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
download_interactiveBrokers <- FALSE
path_data <- "./datos/datos_NVDA_1year.xlsx"

# yahoo finance data
download_yahoo <- TRUE
symbol_stock <- "T"
ultimo.dia <- "" #"2025-02-13"

stock <- if(download_interactiveBrokers){
  Process_data_interactivebrokers(path_data)
} else {
  Get_stock_data_R_yahoo(symbol_stock, interval = "1mo",start="2024-01-01", end="2025-02-21")
}

stock <- if(download_yahoo & ultimo.dia != ""){
  stock |> filter(date_ymd != ultimo.dia)
} else {
  stock
}

cost_buy_ib <- 1
cost_sell_ib <- 1
capital <- 200

Estrategia5 <- function(stock){
  stock$results <- map(1:nrow(stock), ~ Calculo_profit5(.x, stock, cost_buy_ib))
  stock <- stock |>
    mutate(
      num_stocks = map_dbl(results, ~ pluck(.x, "num_stocks", .default = 0)),
      inversion = map_dbl(results, ~ pluck(.x, "inversion", .default = 0))
    )
  
  
  stock
}

Calculo_profit5 <- function(
    indice,
    stock,
    cost_buy_ib
    ) {
  price <- stock[[indice, "high"]]
  num_stocks <- ifelse(capital > 1, floor(capital/price), 1)
  inversion <- (num_stocks*price) + cost_buy_ib
  return(list(num_stocks = num_stocks, inversion = inversion))
}

escenario5 <- Estrategia5(stock)

resumen <- data.frame(
  num_stocks = sum(escenario5$num_stocks),
  inversion = sum(escenario5$inversion),
  price_to_sell = last(escenario5$low)
)
resumen <- resumen |> mutate(profit = num_stocks*price_to_sell - inversion - cost_sell_ib)
resumen <- resumen |> mutate(margen = profit/inversion)
resumen <- resumen |> mutate(valor_final = num_stocks * price_to_sell)
