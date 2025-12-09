source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_analisis_na.R"))

#importar tabla
tabla_na<-importar_datos(nombre_archivo = "inflacion_mas_banco_sf.csv",carpeta = "processed")

#analisis de na
analisis_na_pais<-analizar_na(tabla = tabla_na,grupo = "country")
analisis<-analisis_na_pais$por_columna%>%arrange(na_om01)
print(analisis,n=Inf)

# Quinqueneos
df_quinquenal <- tabla_na %>%
  # 1. Crear la variable 'quinquenio'
  mutate(
    # Calcula el inicio del periodo de 5 años:
    # (year - 1970) %/% 5 da 0 para 1970-1974, 1 para 1975-1979, etc.
    # Multiplicar por 5 y sumar 1970 da el año de inicio.
    inicio_quinquenio = floor((year - 1970) / 5) * 5 + 1970,
    # Crear la etiqueta del quinquenio (ej: "1970-1974")
    quinquenio = paste0(inicio_quinquenio, "-", inicio_quinquenio + 4)
  )

# ALGO
columnas_a_promediar<-c("inflacion", "di01","dm02", "om01")
df_promedios_final <- df_quinquenal %>%
  # Agrupar por país Y por el periodo de 5 años
  group_by(country, quinquenio) %>%
  # Aplicar la función summarise(across()) para promediar las columnas deseadas
  summarise(
    across(all_of(columnas_a_promediar), 
           # Nueva Sintaxis: Aplica mean(x, na.rm=TRUE) a la columna x
           .fns = \(x) mean(x, na.rm = TRUE), 
           .names = "avg_{.col}"),
    conteo_años = n(),
    .groups = 'drop' 
  )
print(head(df_promedios_final))

exportar_data(data = df_promedios_final, nombre = "quinquenios_p3", carpeta = "clean")







