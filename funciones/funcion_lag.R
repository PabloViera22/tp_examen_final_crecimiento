incremento<-function(df_datos, variable_x){
df_incremento <- df_datos %>%
  # Paso 1: Agrupar por país
  group_by(country) %>%
  # Paso 2: Asegurar el orden cronológico
  # (Aunque ya estén ordenados, es una buena práctica de seguridad)
  arrange(year) %>%
  # Paso 3: Calcular el incremento
  mutate(
    # Calcula el valor anterior (lag) de la variable X
    valor_anterior = lag(variable_x),
    # Incremento Absoluto: Valor Actual - Valor Anterior
    incremento_abs = variable_x - valor_anterior,
    # Incremento Porcentual: ((Valor Actual / Valor Anterior) - 1) * 100
    incremento_pct = ((variable_x / valor_anterior) - 1) * 100
  ) %>%
  # Paso 4: Desagrupar al terminar
  ungroup()
}
