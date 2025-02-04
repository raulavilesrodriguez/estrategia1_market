library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)

#-----Data-----
stock <- read.csv("./datos/SPY_20250202_190055.csv")

trail <- 0.01
time_buy <- 3
price <- 0
stop_loss <- 0
indice_final_dia_5min <- 73

indices <- which(stock[['close']] == stock[['buy']])

calculo.profit <- function(indice){
  profit <- 0
  price <- stock[[indice, "close"]]
  stop_loss <- price*(1 - trail)
  next_indice <- indice + 1
  final_indice <- next_indice + indice_final_dia_5min
  we <- price
  
  for (i in next_indice:final_indice){
    x <- stock[[i, "close"]]
    profit <- x - stock[[indice, "close"]]
    
    if(x >= price & x >= we){
      stop_loss <- x * (1 - trail)
      we <- x
    } else {
      ifelse(we > x, 
             stop_loss <- we * (1 - trail), 
             stop_loss <- price * (1 - trail))
    }
    
    if(x <= stop_loss){
      return(profit)
    }
    
    price <- x
  }
  return(profit)
}







