#---TRADE---

Calculo_profit <- function(
    ix, 
    trail_loss, 
    trail_gain, 
    time_buy, 
    time_left,
    stock,
    ventana_5min,
    capital,
    cost_broker
){
  
  profit <- 0
  indice <- ix + time_buy
  price <- stock[[indice, "close"]]
  stop_loss <- price*(1 - trail_loss)
  stop_gain <- price*(1 + trail_gain)
  next_indice <- indice + 1
  indice_final_dia_5min <- ventana_5min - time_buy - 2
  final_indice <- next_indice + indice_final_dia_5min
  we <- price
  num_stokcs <- ifelse(capital > 1, floor(capital/price), 1)
  
  for (i in next_indice:final_indice){
    x <- stock[[i, "close"]]
    profit <- (num_stokcs*(x - stock[[indice, "close"]])) - cost_broker
    
    # Exit when gain
    if(x >= stop_gain | i == indice + time_left){
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
