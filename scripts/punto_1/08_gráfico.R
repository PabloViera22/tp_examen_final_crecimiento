source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))


##############################################################################p#
datos_df <- data.frame(X = datos_grafico$avg_inflacion, Y = datos_grafico$avg_var_pct_pib_pc)
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
    title = "Gráfico de Dispersión: Inflación VS Crecimiento Per Cápita",
    x = "Inflación, Quinquenal",
    y = "Variación (%) Crecimiento Per Cápita, Quinquenal"
  ) +
  coord_cartesian(
    xlim = c(0, 25),  # Limita el Eje X (avg_inflacion) de 0 a 10
    ylim = c (-10, 25))+  # Limita el Eje Y (avg_di01) de 5 a 20
  theme_minimal() # Usa un tema limpio



