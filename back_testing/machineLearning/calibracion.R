library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)

path_plt_complet <- "./datos/Xgboost/buyDips_META_10mar25v2.xlsx"

signals_db <- read_excel(path_plt_complet) |>
  clean_names()

# the scenary as 8 should be buy in all moments
# so resultado_xgboost[["signals]] shoud be all in TRUE
signals_db$signals <- map(1:nrow(signals_db), ~ calibracion(.x, escenarios8, signals_db))

calibracion <- function(index, escenarios, signals_db){
  i <- signals_db[["indices"]][index]
  if(escenarios[[i, "profit"]] > 0){
    TRUE
  } else {
    FALSE
  }
}

signals_db <- signals_db |> mutate(signals = as.character(signals))

write_xlsx(signals_db, "./datos/Xgboost/buyDips_calibrado.xlsx")