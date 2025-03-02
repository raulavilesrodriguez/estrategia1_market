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
library(lubridate)

source(here::here('helpers/Get_stock_data_R_yahoo.R'))
source(here::here('helpers/Save_data.R'))
source(here::here("helpers/Process_data_interactivebrokers.R"))
source(here::here('algorithm/Calculo_profit7.R'))

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

threshold <- -0.011

# thresholds <- seq(0.003, 0.03, 0.001)
trails_loss <- 0.009 #seq(0.008, 0.011, 0.001)
trails_gain <- 0.02 #seq(0.008, 0.01, 0.003)
times_buy <- 0
times_left <- 48
ventana_5min <- 78
capital <- 2000
cost_broker <- 2  # $1 to buy and $1 to sell


#---- Estrategia 7 -----
currentPosition <- 0

Estrategia7 <- function(threshold, stock){
  indices <- which(grepl("15:55", stock[["date"]]))
  close <- c()
  
  for(i in indices){
    close <- append(close, stock[[i, "close"]])
  }
  db <- data.frame(indices = indices, close = close)
  db <- db |> mutate(change = (close/lag(close))-1)
  
  signals <- c(FALSE, FALSE)
  for (i in 3:nrow(db)) {
    if(db[[i, "change"]]<0 && db[[i, "change"]]< (-0.01) &&
       db[[i-1, "change"]]<0 && db[[i-1, "change"]]< (-0.01)
       ){
      signals <- append(signals, TRUE)
    } else {
      signals <- append(signals, FALSE)
    }
  }
  db <- db |> mutate(signals = signals)
  
  # filter signals
  w <- db |> filter(signals == TRUE)
  stock$signals <- seq_len(nrow(stock)) %in% w$indices
  
  stock$results <- map(1:nrow(stock), ~ when_buy(.x, stock))
  
  stock <- stock |> 
    mutate(
      profit = map_dbl(results, ~ pluck(.x, "profit", .default = 0)),
      index_sell = map_dbl(results, ~ pluck(.x, "i", .default = NA)) 
    )
  stock <- stock |>
    mutate(date_sell = case_when(!is.na(index_sell) ~ as_datetime(stock$date[index_sell])))
  
  return(list(db=db, stock=stock))
}


when_buy <- function(index, stock){
  
  if(index != nrow(stock) && stock[["signals"]][index]){
    
    if(index > currentPosition){
      x <- Calculo_profit7(
        index, 
        trails_loss, 
        trails_gain, 
        stock, 
        capital, 
        cost_broker
      )
      currentPosition <<- x[["i"]]
      print(paste("I am millionaire: ", currentPosition))
      return(x)
    } else {
      return(0) 
    }
  }
  return(0)
}

results <- Estrategia7(threshold, stock)
db <- results$db
escenarios7 <- results$stock

resumen <- tibble(
  ingresos = sum(pmax(escenarios7$profit, 0)),
  perdidas = sum(pmin(escenarios7$profit, 0)),
  trades_wins = sum(escenarios7$profit>0),
  trades_loss = sum(escenarios7$profit<0),
)
resumen <- resumen |> mutate(neto = ingresos + perdidas)  
resumen <- resumen |> mutate(margen = neto / capital)

