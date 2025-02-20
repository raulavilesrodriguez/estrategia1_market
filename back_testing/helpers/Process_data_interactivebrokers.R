library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(here)
library(writexl)

Process_data_interactivebrokers <- function(path_data){
  db <- read_excel(path_data)
  colnames(db)[2:6] <- c("open", "high", "low", "close", "volume")
  
  db$date <- with_tz(db$date, tzone = "America/New_York")
  db$date <- as.POSIXct(db$date)
  db$date_ymd <- as.Date(db$date)
  
  
  # Detectar cambios de día
  db <- db |>
    mutate(change_day = as.integer(date_ymd != lag(date_ymd, default = first(date_ymd))))
  
  # Calcular el cambio porcentual
  db <- db |>
    mutate(change = (close - lag(close)) / lag(close))  # pct_change() en R
  
  db[which(is.na(db["change"])), "change"] = 0
  
  db 
}
