library(IBrokers)
library(lubridate)
library(writexl)
library(xts)

tws <- twsConnect(port = 7497)

isConnected(tws)

# Definir el contract
symbol <- "BRK B"
accion <- TRUE #could be stock or index
contract <- if(accion){
  twsSTK(symbol)
} else {
  twsIndex(symbol, exch = "CBOE", currency = "USD") 
}
#contract <- twsSTK("GOOGL")
#contract <- twsIndex(symbol, exch = "CBOE", currency = "USD")

end_date <- Sys.time()
end_date <- format(end_date, "%Y%m%d %H:%M:%S")

contract_data <- reqHistoricalData(
  tws, 
  contract,
  endDateTime = "20250305 19:00:00",
  barSize = "5 min", 
  duration = "1 Y"
  )

write_xlsx(
  data.frame(
    date = index(contract_data),
    coredata(contract_data)
    ), 
  "./datos/datos_BRK B_1year_05mar25.xlsx"
  )

