library(tidyverse)
library(dplyr)
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

#-----Data-----
stock <- Get_stock_data_R_yahoo("SPY", "5m")

threshold <- 0.005
trail_loss <- 0.01
trail_gain <- 0.01
time_buy <- 4
price <- 0
stop_loss <- 0
ventana_5min <- 78
indice_final_dia_5min <- ventana_5min - time_buy - 2
capital <- 2000
cost_broker <- 2  # $1 to buy and $1 to sell

#---- Estrategia 1: Definition of Signal -----
Liquidez_entrada_premarket <- function(stock){
  # Aplicar la función when_buy
  stock$buy <- sapply(1:nrow(stock), function(i) when_buy(i, stock))
  
  # Desplazar la columna buy hacia abajo por time_buy períodos
  stock$buy <- c(rep(0, time_buy), head(stock$buy, -time_buy))
  
  return(stock)
  
}

when_buy <- function(index, stock) {
  
  cambio <- (stock$close[index])
  
  if (stock$change_day[index] == 1 && stock$change[index] >= threshold) {
    future_index <- index + time_buy
    if (future_index <= nrow(stock)) {
      return(stock$close[future_index])
    }
  }
  return(0)
}

stock <- Liquidez_entrada_premarket(stock)
veamos <- as.xts(stock$close, order.by = stock$date)

#Save_data(stock, "SPY")


highchart(type="stock") |>
  hc_add_series(veamos, 
                name = "Precio",
                color = "green"
                ) |>
  hc_add_series(SMA(na.omit(veamos),n=9),name="SMA(9)", color ="#D70654") |> 
  hc_add_series(SMA(na.omit(veamos),n=20),name="SMA(20)", color = "#3674B5") |>
  hc_title(text="<b>GRAFICO</b>")


#---TRADE---
indices <- which(stock[['close']] == stock[['buy']])


calculo.profit.e1 <- function(indice){
  profit <- 0
  price <- stock[[indice, "close"]]
  stop_loss <- price*(1 - trail_loss)
  stop_gain <- price*(1 + trail_gain)
  next_indice <- indice + 1
  final_indice <- next_indice + indice_final_dia_5min
  we <- price
  num_stokcs <- ifelse(capital > 1, floor(capital/price), 1)
  
  for (i in next_indice:final_indice){
    x <- stock[[i, "close"]]
    profit <- (num_stokcs*(x - stock[[indice, "close"]])) - cost_broker
    
    # Exit when gain
    if(x >= stop_gain){
      return(profit)
    }
    
    # configuration automatic stop loss
    if(x >= price & x >= we){
      stop_loss <- x * (1 - trail_loss)
      we <- x
    } else {
      stop_loss <- we * (1 - trail_loss)
    }
    
    # Exit when lost
    if(x <= stop_loss){
      return(profit)
    }
    
    price <- x
  }
  return(profit)
}


estrategia.liquidez.premarket1 <- function(indices){
  sapply(indices, calculo.profit.e1)
}

resultado.e1 <- estrategia.liquidez.premarket1(indices)

resultados.trades.e1 <- tibble(indices = indices, resultados = resultado.e1)
suma.e1 <- sum(resultados.trades.e1$resultados, na.rm = TRUE)
suma.e1
