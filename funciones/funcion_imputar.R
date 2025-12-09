#==============================================================================#
# IMPUTACION MULTIPLE
#==============================================================================#
#vector_columna<-c("pop", "rgdpna", "csh_i","pl_x", "pl_m")
# Seleccionar variables para imputación
imputacion_multiple <- function(datos, vector_columna) {
  
  # 1. Seleccionar solo las variables a imputar
  vars_mice <- datos %>%
    dplyr::select(all_of(vector_columna)) # Usamos all_of para mejor manejo de variables
  
  # 2. Configurar y ejecutar MICE
  mice_imp <- mice(vars_mice,
                   m = 5,
                   method = 'pmm', 
                   seed = 2025,
                   printFlag = FALSE)
  
  # Opcional: Ver métodos utilizados (mantener para verificación)
  print("Métodos de imputación utilizados:")
  print(mice_imp$method)
  
  # 3. Extraer la primera imputación
  # Este data frame solo contiene las columnas imputadas (country.x, por ejemplo)
  datos_imp_solo_vars <- complete(mice_imp, 1) 
  
  # --- PASO CRÍTICO: Integrar los valores imputados de vuelta al data frame original ---
  
  # 4. Reemplazar las columnas originales por las imputadas
  # Usamos mutate(across()) para reemplazar los valores en las columnas especificadas
  
  datos_final <- datos %>%
    # Usamos all_of(vector_columna) para asegurarnos de seleccionar las columnas exactas
    mutate(across(all_of(vector_columna), 
                  ~ datos_imp_solo_vars[[cur_column()]]))
  
  return(datos_final)
}


