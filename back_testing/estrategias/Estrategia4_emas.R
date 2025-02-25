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
source(here::here('algorithm/Calculo_profit4.R'))

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


#---- Estrategia 4 -----
currentPosition <- 0

Estrategia4 <- function(threshold, stock){
  stock <- stock |> mutate(emaFast = EMA(close, n = 9), emaSlow = EMA(close, n = 21))
  
  stock$results <- map(1:nrow(stock), ~ when_buy(.x, stock))
  stock <- stock |> 
    mutate(
      profit = map_dbl(results, ~ pluck(.x, "profit", .default = 0)),
      index_sell = map_dbl(results, ~ pluck(.x, "i", .default = NA)) 
      )
  stock <- stock |>
    mutate(date_sell = case_when(!is.na(index_sell) ~ as_datetime(stock$date[index_sell])))
    
  stock
}

# ----Definition of Signal-----
when_buy <- function(index, stock) {
  i <- min(index + 6, nrow(stock))
  j <- max(index - 1, 1)
  
  if (!exists("currentPosition", envir = .GlobalEnv)) {
    currentPosition <<- 0  # Si no existe, inicializar
  }
  
  if (index != nrow(stock) &&
      !is.na(stock[["emaFast"]][index]) &&
      !is.na(stock[["emaSlow"]][index]) &&
      as.Date(stock[["date"]][i]) == as.Date(stock[["date"]][index]) && 
      as.Date(stock[["date"]][index]) == as.Date(stock[["date"]][j]) &&
      stock[["emaFast"]][index] > (stock[["emaSlow"]][index] + 0.005)
      ) {
    if(index > currentPosition){
      x <- Calculo_profit4(
        index, 
        trails_loss, 
        trails_gain, 
        stock, 
        ventana_5min, 
        capital, 
        cost_broker
      )
      currentPosition <<- x[["i"]]
      print(paste("I want to be millionaire: ", currentPosition))
      return(x)
    } else {
      return(0) 
    }
  }
  return(0)
}

escenarios4 <- Estrategia4(threshold, stock)
resumen <- tibble(
  ingresos = sum(pmax(escenarios4$profit, 0)),
  perdidas = sum(pmin(escenarios4$profit, 0)),
  trades_wins = sum(escenarios4$profit>0),
  trades_loss = sum(escenarios4$profit<0),
  )
resumen <- resumen |> mutate(neto = ingresos + perdidas)

escenarios <- escenarios4 |> mutate(date = as.character(date))
write_xlsx(escenarios, "./datos/escenario4.xlsx")













