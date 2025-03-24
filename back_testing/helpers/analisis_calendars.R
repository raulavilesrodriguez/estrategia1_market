
# -----------ANALYSIS with CALENDARS-------

# add dates
add_dates <- function(resultado_xgboost, stock){
  resultado_xgboost <- resultado_xgboost |>
    mutate(date = sapply(indices, function(indice){as.character(stock[["date"]][indice])}))
  resultado_xgboost <- resultado_xgboost |> select(indices, date, everything())
  resultado_xgboost
}

# -----CALENDAR EVENTS SOTCK ---
analisis_events_stock <- function(calendar_stock_tbl, resultado_xgboost){
  coincidences <- sapply(calendar_stock_tbl[["date"]], function(x){
    i <- which(as.Date(x) == as.Date(resultado_xgboost[["date"]]))
    if(length(i) == 1) {
      i
    }
  })
  coincidences <- unlist(unique(coincidences))
  
  lapply(coincidences, function(i){
    resultado_xgboost[i, "pred_class"] <<- "FALSE"
    j <- i + 1
    if(j < nrow(resultado_xgboost)){
      resultado_xgboost[j, "pred_class"] <<- "FALSE"  
    }
  })
  
  resultado_xgboost
}


# -------CALENDAR USA-------
analisis_events_usa <- function(calendar_usa_tbl_filt, resultado_xgboost){
  coincidences_usa <- sapply(calendar_usa_tbl_filt[["date"]], function(x){
    i <- which(as.Date(x) == as.Date(resultado_xgboost[["date"]]))
    if(length(i) == 1) {
      i
    }
  })
  coincidences_usa <- unlist(unique(coincidences_usa))
  
  lapply(coincidences_usa, function(i){
    resultado_xgboost[i, "pred_class"] <<- "FALSE"
    j <- i + 1
    if(j < nrow(resultado_xgboost)){
      resultado_xgboost[j, "pred_class"] <<- "FALSE"  
    }
  })
  
  resultado_xgboost
}




