Save_data <- function(data, symbol, start_date = NULL, end_date = NULL) {
  output_dir <- "datos"
  
  # Crear el directorio si no existe
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Obtener la fecha y hora actual
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Crear el nombre del archivo
  file_path <- file.path(output_dir, paste0(symbol, "_", timestamp, ".csv"))
  
  # Guardar los datos en CSV
  write.csv(data, file_path, row.names = FALSE)
  
  message("Data saved in ", file_path)
}
