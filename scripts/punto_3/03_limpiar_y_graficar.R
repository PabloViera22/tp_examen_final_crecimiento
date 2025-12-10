source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))

#importar
para_graficar<-importar_datos(nombre_archivo = "quinquenios_p3.csv", carpeta = "clean")
para_graficar_filtrada<-importar_datos(nombre_archivo = "quinquenios_filtrada_p3.csv", carpeta = "clean")
nrow(para_graficar)
#==============================================================================#
#simulo que aca se limpia

para_graficar %>%
  group_by(quinquenio) %>%
  summarise(
    total_nas = sum(is.na(avg_dm02)),
    total_observaciones = n(),
    # 👈 ¡LA CLAVE ESTÁ AQUÍ!
    media_acciones_negociadas = mean(avg_dm02, na.rm = TRUE),
    media_credito_privado = mean(avg_di01, na.rm = TRUE),
    media_inflacion = mean(avg_inflacion, na.rm = TRUE)
  )



para_graficar %>%
  group_by(quinquenio) %>%
  summarise(
    media_acciones_negociadas = mean(avg_dm02, na.rm = TRUE),
    media_credito_privado = mean(avg_di01, na.rm = TRUE),
    media_inflacion = mean(avg_inflacion, na.rm = TRUE)
  )
install.packages("gt")
library(gt)
#==============================================================================#
#==============================================================================#
#Grafico1 INFLACION Y CREDITO PRIVADO
datos_df <- data.frame(X = para_graficar$avg_inflacion, Y = para_graficar$avg_di01)

# 2. Crear el gráfico de dispersión con ggplot2
ggplot(
  data = datos_df,                    # Especifica los datos
  aes(x = X, y = Y)                   # Mapea las variables a los ejes (Aesthetics)
) +
  geom_point(color = "darkred", size = 3) + # Añade los puntos, define color y tamaño
  labs(
    title = "Gráfico de Dispersión usando ggplot2",
    x = "Variable X (Eje Horizontal)",
    y = "Variable Y (Eje Vertical)"
  ) +
  coord_cartesian(
    xlim = c(-10, 100),  # Limita el Eje X (avg_inflacion) de 0 a 10
    ylim = c(0, 400) )+  # Limita el Eje Y (avg_di01) de 5 a 20
  theme_minimal() # Usa un tema limpio

#==============================================================================#
#==============================================================================#
#Grafico2
datos_df <- data.frame(X = para_graficar$avg_om01, Y = para_graficar$avg_inflacion)

# 2. Crear el gráfico de dispersión con ggplot2
ggplot(
  data = datos_df,                    # Especifica los datos
  aes(x = X, y = Y)                   # Mapea las variables a los ejes (Aesthetics)
) +
  geom_point(color = "darkred", size = 3) + # Añade los puntos, define color y tamaño
  labs(
    title = "Gráfico de Dispersión usando ggplot2",
    x = "Variable X (Eje Horizontal)",
    y = "Variable Y (Eje Vertical)"
  ) +
  coord_cartesian(
    xlim = c(-10, 100),  # Limita el Eje X (avg_inflacion) de 0 a 10
    ylim = c(0, 20) )+  # Limita el Eje Y (avg_di01) de 5 a 20
  theme_minimal() # Usa un tema limpio

#==============================================================================#
#==============================================================================#
#Grafico3 INFLACION Y CREDITO PRIVADO
datos_df <- data.frame(X = para_graficar_filtrada$avg_inflacion, Y = para_graficar_filtrada$avg_di01)

# 2. Crear el gráfico de dispersión con ggplot2
ggplot(
  data = datos_df,                    # Especifica los datos
  aes(x = X, y = Y)                   # Mapea las variables a los ejes (Aesthetics)
) +
  geom_point(
    shape = 21,          # 👈 Usar pch 21 (círculo con relleno y borde)
    fill = "#00BFC4",    # 👈 Color de relleno (el color interior)
    color = "black",     # 👈 Color del contorno (el color del borde)
    size = 3,            # Tamaño del punto
    stroke = 1           # Grosor de la línea del contorno (opcional, 1 es un buen valor)
  ) +
  geom_smooth(
    method = "loess",     # El método de suavizado (LOESS es el default para < 1000 puntos)
    se = TRUE,            # Mostrar el intervalo de confianza (TRUE es el default)
    color = "#F8766D",       # Color de la línea de tendencia
    linewidth = 1.2       # Grosor de la línea
  ) +
  labs(
    title = "Gráfico de Dispersión: Inflación VS Crédito Privado",
    x = "Inflación",
    y = "Crédito Privado de los Bancos de Depósito Sobre el PBI (%)"
  ) +
  coord_cartesian(
    xlim = c(-0, 25),  # Limita el Eje X (avg_inflacion) de 0 a 10
    ylim = c(0, 200) )+  # Limita el Eje Y (avg_di01) de 5 a 20
  theme_minimal() # Usa un tema limpio


#FILTRO AQUELLAS FILAS DONDE FALTA DM02
datos_sin_na_filter <- para_graficar_filtrada %>%
  filter(!is.na(avg_dm02))
nrow(para_graficar_filtrada)#660
nrow(datos_sin_na_filter)#328
# CUENTO 
conteo_distinto_por_columna <- datos_sin_na_filter %>%
  group_by(quinquenio)%>%
  summarise(across(everything(), n_distinct))



#==============================================================================#
#==============================================================================#
#Grafico4 INFLACION VS acciones negociadas, valor total respecto del PBI (%). 
datos_df <- data.frame(X = para_graficar_filtrada$avg_inflacion, Y = para_graficar_filtrada$avg_dm02)

# 2. Crear el gráfico de dispersión con ggplot2
ggplot(
  data = datos_df,                    # Especifica los datos
  aes(x = X, y = Y)                   # Mapea las variables a los ejes (Aesthetics)
) +
  geom_point(
    shape = 21,          # 👈 Usar pch 21 (círculo con relleno y borde)
    fill = "#00BFC4",    # 👈 Color de relleno (el color interior)
    color = "black",     # 👈 Color del contorno (el color del borde)
    size = 3,            # Tamaño del punto
    stroke = 1           # Grosor de la línea del contorno (opcional, 1 es un buen valor)
  ) +
  geom_smooth(
    method = "loess",     # El método de suavizado (LOESS es el default para < 1000 puntos)
    se = TRUE,            # Mostrar el intervalo de confianza (TRUE es el default)
    color = "#F8766D",       # Color de la línea de tendencia
    linewidth = 1.2       # Grosor de la línea
  ) +
  labs(
    title = "Gráfico de Dispersión: Inflación VS Acciones Negociadas",
    x = "Inflación",
    y = "Acciones Negociadas, Valor Total Respecto del PBI (%)"
  ) +
  coord_cartesian(
    xlim = c(0, 20),  # Limita el Eje X (avg_inflacion) de 0 a 10
    ylim = c (0, 50))+  # Limita el Eje Y (avg_di01) de 5 a 20
  theme_minimal() # Usa un tema limpio

#==============================================================================#
#==============================================================================#
#Grafico4
datos_df <- data.frame(X = para_graficar_filtrada$avg_dm02, Y = para_graficar_filtrada$avg_di01)

# 2. Crear el gráfico de dispersión con ggplot2
ggplot(
  data = datos_df,                    # Especifica los datos
  aes(x = X, y = Y)                   # Mapea las variables a los ejes (Aesthetics)
) +
  geom_point(
    shape = 21,          # 👈 Usar pch 21 (círculo con relleno y borde)
    fill = "#00BFC4",    # 👈 Color de relleno (el color interior)
    color = "black",     # 👈 Color del contorno (el color del borde)
    size = 3,            # Tamaño del punto
    stroke = 1           # Grosor de la línea del contorno (opcional, 1 es un buen valor)
  ) +
  geom_smooth(
    method = "loess",     # El método de suavizado (LOESS es el default para < 1000 puntos)
    se = TRUE,            # Mostrar el intervalo de confianza (TRUE es el default)
    color = "#F8766D",       # Color de la línea de tendencia
    linewidth = 1.2       # Grosor de la línea
  ) +
  labs(
    title = "Gráfico de Dispersión: Inflación VS Acciones Negociadas",
    x = "Inflación",
    y = "Acciones Negociadas, Valor Total Respecto del PBI (%)"
  ) +
  coord_cartesian(
    xlim = c(-10, 100),  # Limita el Eje X (avg_inflacion) de 0 a 10
    ylim = c(0, 200) )+  # Limita el Eje Y (avg_di01) de 5 a 20
  theme_minimal() # Usa un tema limpio

#==============================================================================#
#==============================================================================#
#grafico en 3 dimensiones
datos_sin_na_filter <- para_graficar_filtrada %>%
  filter(!is.na(avg_dm02) &  # Columna X
      !is.na(avg_di01))



datos_df <- data.frame(
  X = datos_sin_na_filter$avg_dm02, #acciones negociadas
  Y = datos_sin_na_filter$avg_di01, # Credito privado
  Z = datos_sin_na_filter$avg_inflacion
)


ggplot(
  data = datos_df,
  aes(x = X, y = Y, color = Z)  # Z sigue mapeada al color
) +
  geom_point(size = 3) +
  # 👈 CAPA geom_smooth AGREGADA
  geom_smooth(
    method = "loess",     # Usa el método LOESS para una curva suave
    se = TRUE,            # Mostrar el intervalo de confianza
    color = "black",    # Color de la línea de tendencia (Puedes cambiarlo a negro o gris si prefieres)
    linewidth = 1.2,      # Grosor de la línea
    inherit.aes = FALSE,  # ¡IMPORTANTE! Ignora el mapeo de color=Z para que solo dibuje una línea general
    aes(x = X, y = Y)     # Mapeo específico solo para X e Y
  ) +
  labs(
    title = "Grafico con tres dimensiones",
    x = "Acciones Negociadas, Valor Total Respecto del PBI (%)",
    y = "Crédito Privado de los Bancos de Depósito Sobre el PBI (%)
    ",
    color = "Inflación"
  ) +
  scale_color_gradient(
    low = "#00BFC4",
    high = "#FF403B",
    limits = c(0, 50),
    oob = scales::squish
  ) +
  coord_cartesian(
    xlim = c(-10, 100),
    ylim = c(0, 200)
  ) +
  theme_minimal()



conteo_distinto_por_columna <- datos_sin_na_filter %>%
  group_by(quinquenio)%>%
  summarise(across(everything(), n_distinct))



#ÚLTIMO GRAFICO 3D 
library(plotly)


plot_B <- plot_ly(datos_sin_na_filter, 
                  x = ~avg_di01,
                  y = ~avg_dm02,
                  z = ~avg_inflacion,              
                  colors = c('#00BFC4', '#FF403B'),
                  type = "scatter3d", 
                  mode = "markers",
                  marker = list(size = 5, opacity = 0.8)) %>%
  plotly::layout( 
    title = "Gráfico 3D",
    scene = list(
      xaxis = list(
        title = "Crédito Privado (%PIB)",
        range = c(0, 200) # 👈 Límite para el Eje X (avg_di01)
      ),
      yaxis = list(
        title = "Acciones Negociadas (%PIB)",
        range = c(0, 100)  # 👈 Límite para el Eje Y (avg_dm02)
      ),
      zaxis = list(
        title = "Inflación",
        range = c(0, 25)   # 👈 Límite para el Eje Z (avg_inflacion)
      )))

print(plot_B)

