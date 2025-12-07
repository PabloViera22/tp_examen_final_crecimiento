limpiar <- function(tabla) {
  # 3. Aplicar las transformaciones de limpieza
  df_limpio <- tabla %>%
    dplyr::rename_with(~ stringr::str_remove_all(., '["\']')) %>% #sacar comillas
    dplyr::rename_with(~ stringr::str_to_lower(.)) %>%        # Todo a minúsculas
    dplyr::rename_with(~ stringr::str_replace_all(., " ", "_")) # Espacios a guion bajo "_"
  
  # La función devuelve el data frame limpio (opcional, pero buena práctica)
  return(df_limpio)
}

















