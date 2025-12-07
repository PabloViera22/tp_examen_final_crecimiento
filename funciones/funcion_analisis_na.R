#==============================================================================#
# funcion para analizar de forma automatica los NA por grupo
analizar_na <- function(tabla, grupo) {
  # 1. Validación de parámetros
  if (!grupo %in% c("country", "year", "countrycode")) {
    stop("El formato debe ser 'country' o 'year'.")
  }
  if (!is.data.frame(tabla)) {
    stop("El objeto 'tabla' debe ser un data frame.")
  }
  # 2. Análisis por Grupo (usando funciones base de dplyr/tidyr)
  conteo_na_por_columna <- tabla %>%
    group_by(.data[[grupo]]) %>%
    summarise(
      across(
        .cols = everything(), # Aplica a todas las columnas
        .fns = ~ sum(is.na(.)), # Cuenta los NA
        .names = "na_{.col}" # Renombra la columna a 'na_nombrecolumna'
      ),
      .groups = "drop"
    )
    # 3. Análisis Total (usando naniar si aún lo deseas)
  conteo_na_total <- tabla %>% naniar::miss_var_summary()
    # 4. Devolver resultados
  list(por_columna = conteo_na_por_columna,
    total = conteo_na_total)
}

#==============================================================================#
# funcion para analizar de forma automatica los NA de una tabla sin dividir em grupos
analizar_na_general <- function(tabla) {
  # Validación
  if (!is.data.frame(tabla)) {
    stop("El objeto `tabla` debe ser un data frame.")
  }
  # Cálculo
  conteo_na <- tabla %>%
    miss_var_summary()
  # Salida
  list(por_columna = conteo_na)
}
#==============================================================================#

#==============================================================================#
# misma funcion pero general
analizar_na_grafico_general <- function(tabla) {
  # Validación: debe ser un data frame
  if (!is.data.frame(tabla)) {stop("El objeto 'tabla' debe ser un data frame.")}
  # Lista de variables numéricas sobre las que querés evaluar NA
  variables_key <- c(
    "crecimiento_pbi", "pbi_p_c", "inflacion",
    "formacion_bruta_capital", "consumo_gobierno", "apertura")
  # Verificar que las variables existan
  faltantes <- setdiff(variables_key, names(tabla))
  if (length(faltantes) > 0) {
    stop(paste("Estas variables no existen en la tabla:", paste(faltantes, collapse = ", ")))
  }
  # Gráfico general sin segmentar por grupos
  aggr(
    tabla[, variables_key],
    col        = c("steelblue", "red"),
    numbers    = TRUE,
    sortVars   = TRUE,
    labels     = c("crecimiento", "pbi_p_c", "inflacion", "FMK", "consumo", "apertura"),
    cex.axis   = 0.8,
    gap        = 3,
    ylab       = c("Proporción de faltantes", "Patrón de combinación")
  )
}


#==============================================================================#
# TEST DE LITTLE
#==============================================================================#
# Preparar datos para el test
test_de_little<- function(datos){
  datos_para_test <- datos %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(where(~any(is.na(.))))
  # Realizar test de Little
  test_mcar <- naniar::mcar_test(datos_para_test)
  # Crear dataframe con resultados
  resultados_mcar <- data.frame(
    Estadistico = round(test_mcar$statistic, 2),
    `Grados de libertad` = test_mcar$df,
    `P-value` = format(test_mcar$p.value, scientific = TRUE),
    Conclusion = ifelse(test_mcar$p.value < 0.05, 
                        "Rechazamos H0: Datos NO son MCAR",
                        "No rechazamos H0: Datos podrían ser MCAR"))
  resultados_mcar %>%
    kable(caption = "Resultados del Test de Little para MCAR") %>%
    kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
    column_spec(4, bold = TRUE, 
                color = ifelse(test_mcar$p.value < 0.05, "red", "green"))
  
}





