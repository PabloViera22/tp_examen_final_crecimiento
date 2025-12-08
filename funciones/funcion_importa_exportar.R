#==============================================================================#
# FUNCION PARA EXPORTAR
#==============================================================================#
exportar_data <- function(data, nombre, carpeta = c("raw", "processed", "clean"),
                          format = "csv") {
  # Validar argumentos
  folder <- match.arg(carpeta)
  format <- tolower(format)
  
  if (!format %in% c("csv", "excel")) {stop("El formato debe ser 'csv' o 'excel'")}
  
  if (!is.data.frame(data)) {stop("El objeto 'data' debe ser un data frame")}
  
  # Determinar extensión y ruta completa
  proyecto_tp_grupal <- file.path (here::here(), "data")
  extension <- ifelse(format == "csv", ".csv", ".xlsx")
  filepath <- file.path(proyecto_tp_grupal,carpeta, paste0(nombre, extension))
  
  # Exportar según el formato
  if (format == "csv") {
    write.csv(data, file = filepath, row.names = FALSE, fileEncoding = "UTF-8")
    message(paste("Archivo CSV exportado:", filepath))
  } else {
    # Verificar si writexl está instalado
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("El paquete 'writexl' es necesario para exportar a Excel. Instálalo con: install.packages('writexl')")
    }
    writexl::write_xlsx(data, path = filepath)
    message(paste("Archivo Excel exportado:", filepath))
  }
  # Retornar ruta del archivo
  invisible(filepath)
}
#==============================================================================#
# FUNCION PARA CARGAR DATOS
#==============================================================================#
importar_datos <- function(nombre_archivo, carpeta = "raw", encoding = "UTF-8") {
  
  # Construir ruta completa
  ruta_carpeta <- switch(carpeta,
                         "raw" = dir_data_raw,
                         "processed" = dir_data_processed, 
                         "clean" = dir_data_clean,
                         stop("Carpeta debe ser 'raw', 'processed' o 'clean'"))
  
  ruta_completa <- file.path(ruta_carpeta, nombre_archivo)
  # Verificar que el archivo existe
  if (!file.exists(ruta_completa)) {
    stop("Archivo no encontrado: ", ruta_completa)
  }
  
  mensaje_proceso(paste("Cargando", nombre_archivo))
  
  # Detectar tipo de archivo y cargar apropiadamente
  extension <- tools::file_ext(nombre_archivo)
  
  datos <- switch(extension,
                  "csv" = read_csv(ruta_completa, locale = locale(encoding = encoding)),
                  "xlsx" = read_excel(ruta_completa, sheet = 3),
                  "rds" = readRDS(ruta_completa),
                  "txt" = read_delim(ruta_completa, locale = locale(encoding = encoding)),
                  stop("Formato no soportado: ", extension))
  
  # Información sobre los datos cargados

  return(datos)
}

