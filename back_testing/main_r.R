library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(IBrokers)

source(here::here('helpers/Get_stock_data_R_yahoo.R'))

accion <- Get_stock_data_R_yahoo("SPY", "5m")





