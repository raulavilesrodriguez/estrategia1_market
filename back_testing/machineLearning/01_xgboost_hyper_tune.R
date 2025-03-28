# LIBRARIES and DATA ----
library(tidymodels)
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(here)

source(here::here("helpers/Process_data_interactivebrokers.R"))
source(here::here('helpers/Get_stock_data_R_yahoo.R'))
source(here::here("helpers/analisis_calendars.R"))


slice <- dplyr::slice

path_xgboost <- "./datos/Xgboost/buyDips_calibrado.xlsx"
path_calendar_usa <- "./datos/calendars/calendar_usa.xlsx"
path_calendar_stock <- "./datos/calendars/meta_11mar2025.xlsx"
path_data <- "./datos/datos_META.xlsx"
path_data_to_predict <- "./datos/Xgboost/buyDips_META_NUEVOS_21mar25.xlsx"


stock.train <- Process_data_interactivebrokers(path_data)

data_new_to_predict <- read_excel(path_data_to_predict) |>
  clean_names()

symbol_stock <- "META"
start <- "2025-03-06"
end <- "2025-03-24"
stock.predict <- Get_stock_data_R_yahoo(symbol_stock, "5m", start = start, end = end)

# -----Wrangling DATA of CALENDARS ----
calendar_usa_tbl <- read_excel(path_calendar_usa) |>
  clean_names()

calendar_stock_tbl <- read_excel(path_calendar_stock) |>
  clean_names()

# wrangling calendar USA
calendar_usa_tbl <- calendar_usa_tbl |>
  mutate(
    date = mdy(sub("^\\w+, ", "", date)),
    time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S")
  )
calendar_usa_tbl <- calendar_usa_tbl |>
  mutate(
    date = ifelse(
      is.na(time), paste(date, "09:30:00"), paste(date, format(time, "%H:%M:%S"))
    )
  )

calendar_usa_tbl$release <- trimws(calendar_usa_tbl$release)
calendar_usa_tbl$release <- gsub("\\s+", " ", calendar_usa_tbl$release)
calendar_usa_tbl$release <- iconv(calendar_usa_tbl$release, to = "ASCII//TRANSLIT")

which(grepl("Employment Situation for", calendar_usa_tbl$release))
which(grepl("Consumer Price Index for", calendar_usa_tbl$release))

calendar_usa_tbl_filt <- calendar_usa_tbl[
  grepl("Employment Situation for|Consumer Price Index for", calendar_usa_tbl$release),
]

# ---- Wrangling DATA of MODEL -----
signals_tbl <- read_excel(path_xgboost) |>
  clean_names()

indices <- signals_tbl$indices

signals_tbl |> glimpse()
signals_tbl[is.na(signals_tbl)] <- 0

signals_tbl <- signals_tbl |>
  select(-c(indices, name_stock)) |>
  mutate(signals = as.factor(signals))

# ----MODEL AND PREPORCESSOR SPEC ----

xgb_spec_stage_1 <- boost_tree(
  mode   = "classification",
  engine = "xgboost",
  learn_rate = tune()
)

columns_to_normalize <- c("volume", "obv", "ema", "volume_brk_b", "obv_brk_b", "ema_brk_b")

rec_spec <- recipe(signals ~ ., signals_tbl) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_normalize(all_of(columns_to_normalize)) 

rec_spec |> prep() |> juice() |> glimpse()

# 2 STAGE HYPERPARAMETER TUNING PROCESS ----

# STAGE 1: TUNE LEARNING RATE ----

# * Define the stage 1 tuning grid ----
set.seed(123)
grid_stage_1 <- grid_random(
  learn_rate(),
  size = 10
)


# * Create a workflow ----
wflw_xgb_stage_1 <- workflow() |>
  add_model(xgb_spec_stage_1) |>
  add_recipe(rec_spec)

# * Tune the model ----
set.seed(123)
cv_folds <- vfold_cv(signals_tbl, v = 5)
tune_stage_1 <- tune_grid(
  wflw_xgb_stage_1,
  resamples = cv_folds,
  grid      = grid_stage_1,
  metrics   = metric_set(roc_auc),
  control   = control_grid(verbose = TRUE)
)

tune_stage_1 |> collect_metrics() |> arrange(-mean)

# ROC AUC: 0.772 con v=5

# STAGE 2: HOLD LEARN RATE CONSTANT / TUNE OTHER PARAMS ----

# * Get Best Params Stage 1 ----
best_params_stage_1 <- tune_stage_1 |>
  collect_metrics() |>
  arrange(-mean) |>
  slice(1)

# * Model Spec Stage 2: Hold LR constant ----
xgb_spec_stage_2 <- xgb_spec_stage_1 |>
  set_args(
    learn_rate     = best_params_stage_1$learn_rate,
    tree_depth     = tune(),
    loss_reduction = tune(),
    stop_iter      = tune()
  )

wflw_xgb_stage_2 <- wflw_xgb_stage_1 |>
  update_model(xgb_spec_stage_2)


# * Define Stage 2 grid ----
set.seed(123)
grid_stage_2 <- grid_random(
  tree_depth(),
  loss_reduction(),
  stop_iter(),
  size = 10
)


# * Tune stage 2 -----
tune_stage_2 <- tune_grid(
  wflw_xgb_stage_2,
  resamples = cv_folds,
  grid      = grid_stage_2,
  metrics   = metric_set(roc_auc),
  control   = control_grid(verbose = TRUE)
)

tune_stage_2 |> collect_metrics() |> arrange(-mean)

# BEST ROC STAGE 2: 0.796 (YES IMPROVEMENT :) )

# FINAL MODEL ----
# * Select Best Params ----
best_params_stage_2 <- tune_stage_2 |>
  collect_metrics() |>
  arrange(-mean) |>
  slice(1)

# * Update Best Parameters Args ----
xgb_spec_final <- xgb_spec_stage_2 |>
  set_args(
    tree_depth     = best_params_stage_2$tree_depth,
    loss_reduction = best_params_stage_2$loss_reduction,
    stop_iter      = best_params_stage_2$stop_iter,
  )

# * Fit the Final Model ----
wflw_final <- wflw_xgb_stage_2 |>
  update_model(xgb_spec_final) |>
  fit(signals_tbl)

# * Make Predictions ----
resultado_xgboost <- bind_cols(
  wflw_final |> predict(signals_tbl),
  wflw_final |> predict(signals_tbl, type = "prob"),
  signals_tbl
)

# Accuracy
accuracy <- sum(resultado_xgboost$.pred_class == resultado_xgboost$signals)/nrow(resultado_xgboost)
accuracy

resultado_xgboost$indices <- indices
resultado_xgboost <- resultado_xgboost |> relocate(indices, .before = everything())

resultado_xgboost <- resultado_xgboost |> clean_names()

# new data
predicciones_nuevas <- bind_cols(
  wflw_final |> predict(data_new_to_predict),
  wflw_final |> predict(data_new_to_predict, type = "prob"),
  data_new_to_predict
)

predicciones_nuevas <- predicciones_nuevas |> clean_names()

# -----------ANALYSIS with CALENDARS-------
# add dates
resultado_xgboost <- add_dates(resultado_xgboost, stock.train)
resultado_xgboost.2 <- resultado_xgboost

predicciones_nuevas <- add_dates(predicciones_nuevas, stock.predict)

# -----CALENDAR EVENTS SOTCK ---
resultado_xgboost.2 <- analisis_events_stock(calendar_stock_tbl, resultado_xgboost.2)

predicciones_nuevas <- analisis_events_stock(calendar_stock_tbl, predicciones_nuevas)

# -------CALENDAR USA-------
resultado_xgboost.2 <- analisis_events_usa(calendar_usa_tbl_filt, resultado_xgboost.2)

predicciones_nuevas <- analisis_events_usa(calendar_usa_tbl_filt, predicciones_nuevas)

write_xlsx(resultado_xgboost.2, "./datos/Xgboost/resultado_xgboost.xlsx")
write_xlsx(predicciones_nuevas, "./datos/Xgboost/pred_nuevas_xgboost.xlsx")



