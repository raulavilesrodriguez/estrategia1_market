library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(highcharter) #Interactive Plot
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(devtools)

source(here::here('helpers/Get_stock_data_R_yahoo.R'))
source(here::here('helpers/Save_data.R'))
source(here::here("helpers/Process_data_interactivebrokers.R"))

#-----Data-----
# interactive brokers data
download_interactiveBrokers <- TRUE
path_data <- "./datos/datos_NVDA_1year.xlsx"

# yahoo finance data
download_yahoo <- FALSE
symbol_stock <- "GOOGL"
ultimo.dia <- "" #"2025-02-13"

stock <- if(download_interactiveBrokers){
  Process_data_interactivebrokers(path_data)
} else {
  Get_stock_data_R_yahoo(symbol_stock, "5m")
}

stock <- if(download_yahoo & ultimo.dia == ""){
  stock |> filter(date_ymd != ultimo.dia)
} else {
  stock
}

# Save Data
#Save_data(stock, symbol_stock)

threshold <- 0.012

# thresholds <- seq(0.003, 0.03, 0.001)
trails_loss <- 0.01 #seq(0.008, 0.011, 0.001)
trails_gain <- 0.02 #seq(0.008, 0.01, 0.003)
times_buy <- 0
times_left <- 48
ventana_5min <- 78
capital <- 2000
cost_broker <- 2  # $1 to buy and $1 to sell

Estrategia6 <- function(stock){
  which(grepl("15:55", stock$date))
  stock <- stock |> mutate()
  
}
