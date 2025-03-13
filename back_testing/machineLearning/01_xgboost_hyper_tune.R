# LIBRARIES and DATA ----
library(tidymodels)
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(here)

source(here::here("helpers/Process_data_interactivebrokers.R"))
source(here::here('helpers/Get_stock_data_R_yahoo.R'))

slice <- dplyr::slice

path_xgboost <- "./datos/Xgboost/buyDips_META_10mar25.xlsx"
path_calendar_usa <- "./datos/calendars/calendar_usa.xlsx"
path_calendar_stock <- "./datos/calendars/meta_11mar2025.xlsx"
path_data <- "./datos/datos_META_1year_05mar25.xlsx"

symbol_stock <- "META"
stock <- Process_data_interactivebrokers(path_data)

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

# ---- Wrangling DATA of MODEL -----
signals_tbl <- read_excel(path_xgboost) |>
  clean_names()

indices <- signals_tbl$indices

signals_tbl |> glimpse()
signals_tbl[is.na(signals_tbl)] <- 0

signals_tbl <- signals_tbl |>
  select(-indices) |>
  mutate(signals = as.factor(signals))

# ----MODEL AND PREPORCESSOR SPEC ----

xgb_spec_stage_1 <- boost_tree(
  mode   = "classification",
  engine = "xgboost",
  learn_rate = tune()
)

rec_spec <- recipe(signals ~ ., signals_tbl) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

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

# -----------ANALYSIS with CALENDARS-------
# add dates
resultado_xgboost <- resultado_xgboost |>
  mutate(date = sapply(indices, function(indice){as.character(stock[["date"]][indice])}))
resultado_xgboost <- resultado_xgboost |> select(indices, date, everything())

# -----CALENDAR EVENTS SOTCK ---
coincidences <- sapply(calendar_stock_tbl$date, function(x){
  i <- which(as.Date(x) == as.Date(resultado_xgboost$date))
  if(length(i) == 1) {
    i
  }
})
coincidences <- unlist(unique(coincidences))

resultado_xgboost.2 <- resultado_xgboost

lapply(coincidences, function(i){
  resultado_xgboost.2[i, "pred_class"] <<- "FALSE"
  j <- i + 1
  if(j < nrow(resultado_xgboost.2)){
    resultado_xgboost.2[j, "pred_class"] <<- "FALSE"  
  }
})

# -------CALENDAR USA-------
calendar_usa_tbl$release <- trimws(calendar_usa_tbl$release)
calendar_usa_tbl$release <- gsub("\\s+", " ", calendar_usa_tbl$release)
calendar_usa_tbl$release <- iconv(calendar_usa_tbl$release, to = "ASCII//TRANSLIT")

which(grepl("Employment Situation for", calendar_usa_tbl$release))
which(grepl("Consumer Price Index for", calendar_usa_tbl$release))

calendar_usa_tbl_filt <- calendar_usa_tbl[
  grepl("Employment Situation for|Consumer Price Index for", calendar_usa_tbl$release),
]

coincidences_usa <- sapply(calendar_usa_tbl_filt$date, function(x){
  i <- which(as.Date(x) == as.Date(resultado_xgboost$date))
  if(length(i) == 1) {
    i
  }
})
coincidences_usa <- unlist(unique(coincidences_usa))

lapply(coincidences_usa, function(i){
  resultado_xgboost.2[i, "pred_class"] <<- "FALSE"
  j <- i + 1
  if(j < nrow(resultado_xgboost.2)){
    resultado_xgboost.2[j, "pred_class"] <<- "FALSE"  
  }
})

write_xlsx(resultado_xgboost.2, "./datos/Xgboost/resultado_xgboost.xlsx")





