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
source(here::here('algorithm/Calculo_profit6.R'))

#-----Data-----
# interactive brokers data
download_interactiveBrokers <- TRUE
path_data <- "./datos/datos_IAU_1year_05mar25.xlsx"

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
capital <- 2000
cost_broker <- 2  # $1 to buy and $1 to sell

#---- Estrategia 6 -----
currentPosition <- 0

Estrategia6 <- function(threshold, stock){
  
  indices1 <- which(grepl("09:30", stock[["date"]]))
  indices2 <- which(grepl("15:55", stock[["date"]]))
  
  lista <- list()
  for (x in indices1) {
    for (y in indices2) {
      if (format(stock$date[x], "%Y-%m-%d") == format(stock$date[y], "%Y-%m-%d")){
        print(paste("I will be millionaire: ", x, y))
        lista <- append(lista, list(c(x, y))) 
      }
    }
  }
  indices <- as.data.frame(do.call(rbind, lista))
  colnames(indices) <- c("indices1", "indices2")
  
  stock$dips <- (seq_len(nrow(stock)) %in% indices$indices1 & 
            stock$change < threshold)
  
  indices_signal <- c()
  
  for(i in 1:nrow(stock)){
    if(stock[[i, "dips"]]){
      print(paste("I am Byron Raul: ", i))
      index <- which(indices$indices1==i)
      indices_signal <- append(indices_signal, indices[[index, "indices2"]]) 
    }
  }
  
  stock$signals <- seq_len(nrow(stock)) %in% indices_signal
  
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

when_buy <- function(index, stock){
  
  if(index != nrow(stock) && stock[["signals"]][index]){
    
    if(index > currentPosition){
      x <- Calculo_profit6(
        index, 
        trails_loss, 
        trails_gain, 
        stock, 
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


escenarios6 <- Estrategia6(threshold, stock)

resumen <- tibble(
  ingresos = sum(pmax(escenarios6$profit, 0)),
  perdidas = sum(pmin(escenarios6$profit, 0)),
  trades_wins = sum(escenarios6$profit>0),
  trades_loss = sum(escenarios6$profit<0),
)
resumen <- resumen |> mutate(neto = ingresos + perdidas)  
resumen <- resumen |> mutate(margen = neto / capital)  



