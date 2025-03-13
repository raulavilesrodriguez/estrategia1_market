library(tidyverse) 
library(dplyr) 
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(yahoofinancer)
library(lubridate)
library(rvest)
library(httr)
library(readxl)
library(writexl)
library(janitor)

url <- "https://es.marketscreener.com/cotizacion/accion/META-PLATFORMS-INC-10547141/agenda/"

path_calendar_usa <- "./datos/calendars/calendar_usa.xlsx"
path_calendar_stock <- "./datos/calendars/meta_11mar2025.xlsx"

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


resultado_xgboost <- resultado_xgboost |>
  mutate(date = sapply(indices, function(indice){as.character(stock[["date"]][indice])}))
resultado_xgboost <- resultado_xgboost |> select(indices, date, everything())

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
})




