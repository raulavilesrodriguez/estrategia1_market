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
source(here::here('algorithm/Calculo_profit.R'))

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
trails_loss <- 0.009 #seq(0.008, 0.011, 0.001)
trails_gain <- 0.02 #seq(0.008, 0.01, 0.003)
times_buy <- seq(1:30)
times_left <- seq(6,48,6)
ventana_5min <- 78
capital <- 2000
cost_broker <- 2  # $1 to buy and $1 to sell

#---- Estrategia 3 -----
Liquidez_premarket_salida_rapida <- function(threshold, stock){
  
  # Aplicar la función when_buy
  stock$buy <- sapply(1:nrow(stock), function(i) when_buy(i, stock))
  
  # Indices of the Signals
  indices <- tryCatch(
    {
      which(stock[['close']] == stock[['buy']])
    }, error = function(e) {
      cat("No hay signals válidas, por tanto indices NULL", 
          conditionMessage(e),"\n")
      NULL
    }
  )
  
  if(is.null(indices)){
    return(data.frame())
  } 
  combinaciones <- expand.grid(
    i = indices,
    trail_loss = trails_loss,
    trail_gain = trails_gain,
    time_buy = times_buy,
    time_left = times_left 
  )
  
  combinaciones <- combinaciones |> arrange(i)
  
  
  combinaciones <- combinaciones |>
    mutate(
      index_buy = i+time_buy, 
      date_buy = stock[i+time_buy, "date"],
      index_sell = i+time_buy+time_left
      )
  combinaciones <- combinaciones |>
    mutate(date_sell = stock[index_sell, "date"])
  
  combinaciones <- combinaciones |>
    select(i, index_buy, date_buy, everything())
  
  combinaciones$profit <- pmap_dbl(
    combinaciones, ~ Calculo_profit(
      ..1, 
      ..4, 
      ..5, 
      ..6, 
      ..7, 
      stock,
      ventana_5min,
      capital,
      cost_broker
    ))
  
  return(combinaciones)
}

# ----Definition of Signal-----
when_buy <- function(index, stock) {
  
  if (stock$change_day[index] == 1 && stock$change[index] >= threshold) {
    return(stock$close[index])
  }
  return(0)
}



escenarios <- Liquidez_premarket_salida_rapida(threshold, stock)

resumen <- escenarios |> group_by(
  time_buy, time_left
  ) |>
  summarise(
    Profit_Suma = sum(profit),
    Profit_media = mean(profit),
    Profit_max = max(profit),
    Profit_min = min(profit),
    trade_posi = sum(profit >= 0),
    trade_neg = sum(profit < 0),
    conteo = n()
  )


resumen.times <- escenarios |> 
  group_by(
  date_buy, time_buy, time_left, date_sell
) |>
  summarise(
    Profit_Suma = sum(profit),
    Profit_media = mean(profit),
    Profit_max = max(profit),
    Profit_min = min(profit),
    conteo = n()
  )

resumen.trails <- escenarios |> group_by(
  trail_loss, trail_gain
) |>
  summarise(
    Profit_Suma = sum(profit),
    Profit_media = mean(profit),
    Profit_max = max(profit),
    Profit_min = min(profit),
    conteo = n()
  )

# plot density
resumen |>
  ggplot(aes(x=Profit_Suma, fill=factor(time_buy))) + geom_density(alpha=0.5) +
  labs(title = "Densidad de Profit_Suma por time_buy",
       x = "Profit_Suma",
       y = "Densidad",
       fill = "time_buy") +
  theme_minimal()

resumen |> ggplot(aes(x = factor(time_buy), y = Profit_Suma)) +
  geom_violin(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Distribución de Profit_Suma por time_buy",
       x = "time_buy",
       y = "Profit_Suma") +
  theme_minimal()

resumen |> ggplot(aes(x = factor(time_buy), y = Profit_Suma)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6) +
  labs(title = "Distribución de Profit_Suma por time_buy",
       x = "time_buy",
       y = "Profit_Suma") +
  theme_minimal()


resumen |> ggplot(aes(x = factor(time_buy), y = Profit_Suma)) +
  geom_point(color = "blue", alpha = 0.7, size = 2) +
  labs(title = "Gráfico de dispersión de Profit_Suma vs time_buy",
       x = "time_buy",
       y = "Profit_Suma") +
  theme_minimal()


resumen |> ggplot(aes(x = time_buy, y = time_left, size = scale(Profit_Suma), color = scale(Profit_Suma))) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(1, 15)) +  # Ajusta el tamaño de las burbujas
  scale_color_gradient(low = "#F5004F", high = "#06D001") + 
  labs(title = "Gráfico de burbujas: Profit_Suma normalizado",
       x = "time_buy",
       y = "time_left") +
  theme_minimal()

# Plots de todos los escenarios
escenarios |> ggplot(aes(x = factor(time_buy), y = profit, color = profit < 0)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("blue", "red"), labels = c("Profit ≥ 0", "Profit < 0")) +
  labs(title = "Gráfico de dispersión de Profit vs time_buy",
       x = "time_buy",
       y = "Profit_Suma") +
  theme_minimal()


escenarios |> ggplot(aes(x = time_buy, y = time_left, size = scale(profit), color = scale(profit))) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(1, 15)) +  # Ajusta el tamaño de las burbujas
  scale_color_gradient(low = "#F5004F", high = "#06D001") + 
  labs(title = "Gráfico de burbujas: Profit por Escenario",
       x = "time_buy",
       y = "time_left") +
  theme_minimal()

prueba <- escenarios |> filter(time_buy == 2, time_left == 42)
sum(ifelse(prueba$profit <0, prueba$profit, 0))
sum(ifelse(prueba$profit >0, prueba$profit, 0))

sum(ifelse(prueba$profit >0, prueba$profit, 0)) + sum(ifelse(prueba$profit <0, prueba$profit, 0))

