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
library(janitor)

source(here::here('helpers/Get_stock_data_R_yahoo.R'))
source(here::here('helpers/Save_data.R'))
source(here::here("helpers/Process_data_interactivebrokers.R"))
source(here::here('algorithm/Calculo_profit7.R'))

#-----Data-----
# interactive brokers data
download_interactiveBrokers <- FALSE
path_data <- "./datos/datos_META_1year_05mar25.xlsx"
path_vix <- "./datos/datos_VIX_1year_06mar25.xlsx"
path_xgboost <- "./datos/Xgboost/pred_nuevas_xgboost.xlsx"

resultado_xgboost <- read_excel(path_xgboost) |>
  clean_names()

# yahoo finance data
symbol_stock <- "META"
ultimo.dia <- "" #"2025-04-13"
start <- "2025-03-06"
end <- "2025-03-25"

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
change_dip1 <- (-0.00) #-0.011 to stocks
change_dip2 <- (-0.00) #-0.005 to stocks
threshold_rsi <- 63

# thresholds <- seq(0.003, 0.03, 0.001)
trails_loss <- 0.009 #seq(0.008, 0.011, 0.001)
trails_gain <- 0.01 #seq(0.008, 0.01, 0.003)
capital <- 4000
cost_broker <- 2  # $1 to buy and $1 to sell


#---- Estrategia 8 -----
currentPosition <- 0

Estrategia8 <- function(stock){
  
  # filter signals
  w <- resultado_xgboost |> filter(pred_class == TRUE)
  stock$signals <- seq_len(nrow(stock)) %in% w$indices

  stock$results <- map(1:nrow(stock), ~ when_buy(.x, stock))
  
  stock <- stock |> 
    mutate(
      profit = map_dbl(results, ~ pluck(.x, "profit", .default = 0)),
      index_sell = map_dbl(results, ~ pluck(.x, "i", .default = NA)) 
    )
  stock <- stock |>
    mutate(date_sell = case_when(!is.na(index_sell) ~ as_datetime(stock$date[index_sell])))
  
  return(stock)
}


when_buy <- function(index, stock){
  
  if(index != nrow(stock) && 
     stock[["signals"]][index]
  ){
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
  }
  return(0)
}

escenarios8 <- Estrategia8(stock)


resumen <- tibble(
  ingresos = sum(pmax(escenarios8$profit, 0)),
  perdidas = sum(pmin(escenarios8$profit, 0)),
  trades_wins = sum(escenarios8$profit>0),
  trades_loss = sum(escenarios8$profit<0),
)
resumen <- resumen |> mutate(neto = ingresos + perdidas)  
resumen <- resumen |> mutate(margen = neto / capital)


#----Ploting-----
plt <- escenarios8 |> filter(profit !=0)

# plot density
plt |>
  ggplot(aes(x=profit)) + geom_density(fill = "#059212", alpha=0.5) +
  labs(title = "Densidad de Profit por time_buy",
       x = "Profit",
       y = "Densidad",
       fill = "time_buy") +
  theme_minimal()


