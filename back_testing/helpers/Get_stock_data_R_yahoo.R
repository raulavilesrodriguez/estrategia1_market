library(yahoofinancer)

Get_stock_data_R_yahoo <- function(symbol, interval, start, end){
  stock <- Ticker$new(symbol)
  if(interval == "5m" | interval == "1m"){
    stock.data <- stock$get_history(interval = interval)
  } else {
    stock.data <- stock$get_history(start = start, end = end, interval = interval)
  }
  stock.data$date <- with_tz(stock.data$date, tzone = "America/New_York")
  
  stock.data$date <- as.POSIXct(stock.data$date)
  stock.data$date_ymd <- as.Date(stock.data$date)  # Extraer solo la fecha (YYYY-MM-DD)
  
  # Detectar cambios de día
  stock.data <- stock.data |>
    mutate(change_day = as.integer(date_ymd != lag(date_ymd, default = first(date_ymd))))
  
  # Calcular el cambio porcentual
  stock.data <- stock.data |>
    mutate(change = (close - lag(close)) / lag(close))  # pct_change() en R
  
  stock.data[which(is.na(stock.data["change"])), "change"] = 0
  
  stock.data
}
