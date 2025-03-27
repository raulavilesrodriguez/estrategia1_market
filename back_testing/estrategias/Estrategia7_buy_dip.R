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
path_data <- "./datos/datos_BRK B.xlsx"
path_vix <- "./datos/datos_VIX.xlsx"

# yahoo finance data
symbol_stock <- "BRK B"
ultimo.dia <- "" #"2025-02-13"
start <- "2025-03-06"
end <- "2025-03-24"

stock <- if(download_interactiveBrokers){
  Process_data_interactivebrokers(path_data)
} else {
  Get_stock_data_R_yahoo(symbol_stock, "5m", start = start, end = end)
}

stock <- if(!download_interactiveBrokers & ultimo.dia != ""){
  stock |> filter(date_ymd != ultimo.dia)
} else {
  stock
}

# Save Data
#Save_data(stock, symbol_stock)

#----INPUTS--------
change_dip1 <- (-0.0) #-0.011 to stocks
change_dip2 <- (-0.00) #-0.005 to stocks
threshold_rsi <- 63

# thresholds <- seq(0.003, 0.03, 0.001)
trails_loss <- 0.009 #seq(0.008, 0.011, 0.001)
trails_gain <- 0.01 #seq(0.008, 0.01, 0.003)
capital <- 4000
cost_broker <- 2  # $1 to buy and $1 to sell


#---- Estrategia 7 -----
currentPosition <- 0

Estrategia7 <- function(stock){
  indices <- which(grepl("15:55", stock[["date"]]))
  close <- c()
  
  for(i in indices){
    close <- append(close, stock[[i, "close"]])
  }
  db <- data.frame(indices = indices, close = close)
  db <- db |> mutate(change = (close/lag(close))-1)
  
  signals <- c(FALSE, FALSE)
  for (i in 3:nrow(db)) {
    if(db[[i-1, "change"]]< change_dip1 &&
      db[[i, "change"]]< change_dip2 
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
  
  # Indicators
  stock <- stock |> mutate(rsi = RSI(close, SMA, n = 20))
  stock <- stock |> mutate(obv = OBV(close, volume))
  stock <- stock |> mutate(ema = EMA(obv, n=3))
  stock$ema[is.na(stock$ema)] <- 0
  so <- stoch(stock[,3:5], nFastK  = 10, nFastD  = 3, nSlowD  = 3)
  so <- tibble(so)
  stock <- stock |> 
    mutate(soFastk = so$so[,1], soFastD=so$so[,2], soSlowD=so$so[,3])
  stock$soFastk[is.na(stock$soFastk)] <- 0
  stock$soFastD[is.na(stock$soFastD)] <- 0
  stock$soSlowD[is.na(stock$soSlowD)] <- 0
  macd <- MACD(stock$close, nFast = 12, nSlow = 26, nSig = 9)
  macd.Indicator <- macd[,"macd"]
  macd.Signal <- macd[,"signal"]
  stock <- stock |>
    mutate(macd = macd.Indicator, macdSignal = macd.Signal)
  stock$macd[is.na(stock$macd)] <- 0
  stock$macdSignal[is.na(stock$macdSignal)] <- 0
  
  vix <- Process_data_interactivebrokers(path_vix)
  stock$volatile <- map(1:nrow(stock), ~ volatil(.x, stock, vix))
  db$volatile <- unlist(map(1:nrow(db), ~ stock[["volatile"]][db[["indices"]][.x]]))
  db$rsi <- unlist(map(1:nrow(db), ~ stock[["rsi"]][db[["indices"]][.x]]))
  db$volume <- unlist(map(1:nrow(db), ~ stock[["volume"]][db[["indices"]][.x]])) 
  db$obv <- unlist(map(1:nrow(db), ~ stock[["obv"]][db[["indices"]][.x]]))
  db$ema <- unlist(map(1:nrow(db), ~ stock[["ema"]][db[["indices"]][.x]]))
  db$soFastk <- unlist(map(1:nrow(db), ~ stock[["soFastk"]][db[["indices"]][.x]]))
  db$soFastD <- unlist(map(1:nrow(db), ~ stock[["soFastD"]][db[["indices"]][.x]]))
  db$soSlowD <- unlist(map(1:nrow(db), ~ stock[["soSlowD"]][db[["indices"]][.x]]))
  db$macd <- unlist(map(1:nrow(db), ~ stock[["macd"]][db[["indices"]][.x]]))
  db$macdSignal <- unlist(map(1:nrow(db), ~ stock[["macdSignal"]][db[["indices"]][.x]]))
  
  
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

volatil <- function(index, stock, vix){
  indice_vix <- which(vix[["date"]] %in% stock[["date"]][index])
  if(!is_empty(indice_vix)){
    return(vix[["close"]][indice_vix])
  } else {
    return(vix[["close"]][1])
  }
}

when_buy <- function(index, stock){
  
  if(index != nrow(stock) && 
     stock[["signals"]][index] &&
     !is.na(stock[["rsi"]][index]) &&
     stock[["rsi"]][index] <= threshold_rsi
     ){
    
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

results <- Estrategia7(stock)
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


#----Ploting-----
plt <- escenarios7 |> filter(profit !=0)

# plot density
plt |>
  ggplot(aes(x=profit)) + geom_density(fill = "#059212", alpha=0.5) +
  labs(title = "Densidad de Profit por time_buy",
       x = "Profit",
       y = "Densidad",
       fill = "time_buy") +
  theme_minimal()

db$name_stock <- rep(symbol_stock, nrow(db))
db <- db |> select(name_stock, everything())
write_xlsx(db, "./datos/db.xlsx")

# vix  "BRK-B"
#s <- Ticker$new("^VIX")
#s.data<- s$get_history(interval = "5m", start = start, end = end)
