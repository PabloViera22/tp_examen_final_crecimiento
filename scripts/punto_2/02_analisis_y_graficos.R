source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
tabla_analisis<-importar_datos(nombre_archivo = "datos_bancarios.csv",carpeta = "processed")

names(tabla_analisis)

#==============================================================================#
# grafico de dispersion de 
#==============================================================================#
PAIS_1 <- "Nigeria"
PAIS_2 <- "Australia"
VARIABLE_Y <- "di01" 
# 1. Filtrar y Preprocesar los datos (igual que antes)
df_filtrado_final <- tabla_analisis %>%
  filter(country %in% c(PAIS_1, PAIS_2)) %>%
  drop_na(!!sym(VARIABLE_Y))

min_year <- min(df_filtrado_final$year)
max_year <- max(df_filtrado_final$year)
breaks_quinquenales <- seq(from = min_year, to = max_year, by = 5)


# 3. Crear el gráfico de dispersión usando ggplot2
grafico_dispersion <- ggplot(
  data = df_filtrado_final, 
  aes(x = year, y = !!sym(VARIABLE_Y), color = country)
) +
  geom_point(size = 2) + 
  geom_line() +
  scale_x_continuous(
    breaks = breaks_quinquenales, # Usamos la secuencia creada
    labels = breaks_quinquenales  # Las etiquetas son los mismos años
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = paste("Evolución de acciones negociadas totales como parte del PBI (%) en", PAIS_1, "vs.", PAIS_2),
    x = "Año",
    y = "Acciones Negociadas",
    color = "País"
  )

print(grafico_dispersion)

#==============================================================================#
# grafico de dispersion de 
#==============================================================================#

PAIS_1 <- "Nigeria"
PAIS_2 <- "Australia"
VARIABLE_Z <- "dm02" 

# 1. Filtrar y Preprocesar los datos (igual que antes)
df_filtrado_final <- tabla_analisis %>%
  filter(country %in% c(PAIS_1, PAIS_2)) %>%
  drop_na(!!sym(VARIABLE_Z))

min_year <- min(df_filtrado_final$year)
max_year <- max(df_filtrado_final$year)
breaks_quinquenales <- seq(from = min_year, to = max_year, by = 5)


# 3. Crear el gráfico de dispersión usando ggplot2
grafico_dispersion <- ggplot(
  data = df_filtrado_final, 
  aes(x = year, y = !!sym(VARIABLE_Z), color = country)
) +
  geom_point(size = 2) + 
  geom_line() +
scale_x_continuous(
  breaks = breaks_quinquenales, # Usamos la secuencia creada
  labels = breaks_quinquenales  # Las etiquetas son los mismos años
) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  labs(
    title = paste("Evolución de acciones negociadas totales como parte del PBI (%) en", PAIS_1, "vs.", PAIS_2),
    x = "Año",
    y = "Acciones Negociadas",
    color = "País"
  )

print(grafico_dispersion)

