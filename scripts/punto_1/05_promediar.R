source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_imputar.R"))
source(here::here("funciones", "funcion_lag.R"))

tabla_promediar<-importar_datos(nombre_archivo = "tabla_a_promediar.csv",carpeta = "processed")
names(tabla_promediar)
columnas_a_promediar <- c("ln_rgdp_pc", "inflacion", "ln_rgdp_pc", "csh_i", "dpop", "incremento_pct_tot")


#==============================================================================#
# QUINQUENIOS
df_quinquenal <- tabla_promediar %>%
  # 1. Crear la variable 'quinquenio'
  mutate(
    # Calcula el inicio del periodo de 5 años:
    # (year - 1970) %/% 5 da 0 para 1970-1974, 1 para 1975-1979, etc.
    # Multiplicar por 5 y sumar 1970 da el año de inicio.
    inicio_quinquenio = floor((year - 1970) / 5) * 5 + 1970,
    # Crear la etiqueta del quinquenio (ej: "1970-1974")
    quinquenio = paste0(inicio_quinquenio, "-", inicio_quinquenio + 4)
  )

df_promedios_final <- df_quinquenal %>%
  # Agrupar por país Y por el periodo de 5 años
  group_by(country, quinquenio) %>%
  # Aplicar la función summarise(across()) para promediar las columnas deseadas
  summarise(
    across(all_of(columnas_a_promediar), 
           # Nueva Sintaxis: Aplica mean(x, na.rm=TRUE) a la columna x
           .fns = \(x) mean(x, na.rm = TRUE), 
           .names = "avg_{.col}"),
    
    # También aplica el cambio a las variables individuales
    desvioEstandar_tot = sd(tot, na.rm = TRUE), # Esta está bien
    avg_tot = mean(tot, na.rm = TRUE),          # Esta está bien
    
    conteo_años = n(),
    .groups = 'drop' 
  )
print(head(df_promedios_final))
#==============================================================================#
# AGGREGAR EL SD(TOT)
# Asumiremos que df_quinquenal ya tiene las columnas 'country', 'year', 'quinquenio', y 'tot'
df_promedios_final_con_desvio <- df_quinquenal %>%
  
  # Agrupar por país Y por el periodo de 5 años
  group_by(country, quinquenio) %>%
  
  # Aplicar la función summarise
  summarise(
    # 1. Calcular el Promedio (Media) de las columnas seleccionadas
    across(all_of(columnas_a_promediar), 
           .fns = mean, 
           na.rm = TRUE, 
           .names = "avg_{.col}"),
    # 2. CALCULAR LA DESVIACIÓN ESTÁNDAR DE 'tot'
    desvioEstandar_tot = sd(tot, na.rm = TRUE),
    # 3. Opcional: También podrías querer el promedio de 'tot'
    avg_tot = mean(tot, na.rm = TRUE),
    conteo_años = n(),
    .groups = 'drop' 
  )

print(head(df_promedios_final_con_desvio))

exportar_data(data = df_promedios_final_con_desvio, nombre ="tabla_completa" ,carpeta = "clean")










