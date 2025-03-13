#---TRADE---

Calculo_profit7 <- function(
    indice, 
    trail_loss, 
    trail_gain, 
    stock,
    capital,
    cost_broker
){
  profit <- 0
  price <- stock[[indice, "close"]]
  stop_loss <- price*(1 - trail_loss)
  stop_gain <- price*(1 + trail_gain)
  i <- indice + 1 # next indice
  we <- price
  num_stokcs <- ifelse(capital > 1, floor(capital/price), 1)
  
  if(i >= nrow(stock)){
    return(list(profit = 0, i = nrow(stock), num_stokcs=num_stokcs))
  }
  
  while(i < nrow(stock)){
    if(i >= nrow(stock) ){
      break
    }
    
    x <- stock[[i, "close"]]
    profit <- (num_stokcs*(x - stock[[indice, "close"]])) - cost_broker
    
    # Exit when gain
    if(x >= stop_gain){
      return(list(profit = profit, i = i, num_stokcs=num_stokcs))
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
      return(list(profit = profit, i = i, num_stokcs=num_stokcs))
    }
    
    price <- x
    i <- i + 1
  }
  return(list(profit = profit, i = i-1, num_stokcs=num_stokcs))
}

