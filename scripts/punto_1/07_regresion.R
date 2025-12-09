source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
# =============================================================================
# MODELO DE UMBRAL DE PANEL (PANEL THRESHOLD MODEL) - Bick (1996)
# =============================================================================
# Ecuación: Δln(gdp_it) = μ_i + β₁·π̃_it·I(π̃_it≤γ) + δ₁·I(π̃_it≤γ) + 
#                         β₂·π̃_it·I(π̃_it>γ) + φ'·w_it + ε_it
# =============================================================================#
# 1. PREPARACIÓN DE DATOS
# =============================================================================#
# Lee tus datos (ajusta el nombre del archivo según corresponda)
datos <- importar_datos(nombre_archivo = "tabla_limpia.csv", carpeta = "clean")
# Ejemplo de estructura esperada:
# datos debe tener columnas: country, period, gdp_growth, inflation, 
#                            initial, igdp, dpop, dtot, sdtot

# Transformación de inflación (semi-log)
transformar_inflacion <- function(pi) {
  pi_tilde <- ifelse(pi < 1, pi - 1, log(pi))
  return(pi_tilde)
}

# Aplica la transformación
datos$pi_tilde <- transformar_inflacion(datos$avg_inflacion)

# Convierte a panel data
pdata <- pdata.frame(datos, index = c("country", "quinquenio"))

# =============================================================================
# 2. BÚSQUEDA DEL UMBRAL ÓPTIMO (GRID SEARCH)
# =============================================================================

# Define el rango de búsqueda del umbral
# Usualmente se eliminan los percentiles extremos (15% inferior y superior)
pi_sorted <- sort(unique(datos$pi_tilde))
n_pi <- length(pi_sorted)
trim <- 0.15
grid_start <- ceiling(n_pi * trim)
grid_end <- floor(n_pi * (1 - trim))
gamma_grid <- pi_sorted[grid_start:grid_end]

cat("Buscando umbral óptimo en", length(gamma_grid), "puntos...\n\n")

# Función para estimar el modelo dado un umbral
estimar_modelo_umbral <- function(gamma, data) {
  # Crea las variables de régimen
  data$regime_low <- as.numeric(data$pi_tilde <= gamma)
  data$regime_high <- as.numeric(data$pi_tilde > gamma)
  
  # Variables regime-dependent (inflación interactuada)
  data$pi_low <- data$pi_tilde * data$regime_low
  data$pi_high <- data$pi_tilde * data$regime_high
  
  # Estima modelo con efectos fijos
  formula <- gdp_growth ~ pi_low + pi_high + regime_low + 
    initial + igdp + dpop + dtot + sdtot
  
  tryCatch({
    modelo <- plm(formula, data = data, model = "within", effect = "individual")
    ssr <- sum(residuals(modelo)^2)
    return(list(modelo = modelo, ssr = ssr, gamma = gamma, 
                converged = TRUE, n_obs = nobs(modelo)))
  }, error = function(e) {
    return(list(modelo = NULL, ssr = Inf, gamma = gamma, 
                converged = FALSE, n_obs = 0))
  })
}

# Grid search
resultados_grid <- lapply(gamma_grid, function(g) {
  estimar_modelo_umbral(g, pdata)
})

# Extrae SSR para cada umbral
ssr_values <- sapply(resultados_grid, function(x) x$ssr)
gamma_values <- sapply(resultados_grid, function(x) x$gamma)

# Identifica el umbral óptimo (mínimo SSR)
idx_optimo <- which.min(ssr_values)
gamma_optimo <- gamma_values[idx_optimo]
modelo_final <- resultados_grid[[idx_optimo]]$modelo

cat("===============================================\n")
cat("UMBRAL ÓPTIMO ENCONTRADO\n")
cat("===============================================\n")
cat("γ* =", round(gamma_optimo, 4), "\n")
cat("SSR mínimo =", round(min(ssr_values), 4), "\n")
cat("N observaciones =", resultados_grid[[idx_optimo]]$n_obs, "\n\n")

# =============================================================================
# 3. RESULTADOS DEL MODELO FINAL
# =============================================================================

# Recrea las variables con el umbral óptimo
pdata$regime_low <- as.numeric(pdata$pi_tilde <= gamma_optimo)
pdata$regime_high <- as.numeric(pdata$pi_tilde > gamma_optimo)
pdata$pi_low <- pdata$pi_tilde * pdata$regime_low
pdata$pi_high <- pdata$pi_tilde * pdata$regime_high

# Modelo final
formula_final <- avg_ln_rgdp_pc ~ pi_low + pi_high + regime_low + 
  pbi_inicial + avg_csh_i + avg_dpop + avg_incremento_pct_tot + desvioEstandar_tot

modelo_final <- plm(formula_final, data = pdata, 
                    model = "within", effect = "individual")

# Muestra resultados
cat("===============================================\n")
cat("RESULTADOS DEL MODELO FINAL\n")
cat("===============================================\n\n")
print(summary(modelo_final))

#IMPRIMIR IMAGEN
RUTA_ARCHIVO <- file.path(dir_outputs_figures, "resumen_modelo.html")

stargazer(modelo_final,
          type = "html",
          title = "Modelo de Regresión con Umbral (Efectos Fijos)",
          dep.var.labels = "Cambio en Log(GDP)",
          covariate.labels = c("Inflación (Régimen Bajo)", 
                               "Inflación (Régimen Alto)",
                               "Intercepto Régimen Bajo",
                               "Ahorro", 
                               "Crecimiento Población",
                               "Incremento Total (%)",
                               "Desvío Estándar Total"),
          add.lines = list(c("Efectos Fijos Individuales", "Sí")),
          out = RUTA_ARCHIVO # Guarda el archivo en la ruta especificada
)

# Extrae coeficientes
coefs <- coef(modelo_final)
cat("\n===============================================\n")
cat("INTERPRETACIÓN DE COEFICIENTES\n")
cat("===============================================\n")
cat("β₁ (inflación en régimen bajo, π̃≤γ):", round(coefs["pi_low"], 4), "\n")
cat("β₂ (inflación en régimen alto, π̃>γ):", round(coefs["pi_high"], 4), "\n")
cat("δ₁ (intercepto de régimen bajo):", round(coefs["regime_low"], 4), "\n\n")

cat("Variables de control (regime-independent):\n")
cat("  initial:", round(coefs["pbi_inicial"], 4), "\n")
cat("  igdp:", round(coefs["avg_csh_i"], 4), "\n")
cat("  dpop:", round(coefs["avg_dpop"], 4), "\n")
cat("  dtot:", round(coefs["avg_incremento_pct_tot"], 4), "\n")
cat("  sdtot:", round(coefs["desvioEstandar_tot"], 4), "\n\n")

# =============================================================================
# 4. GRÁFICOS DE DIAGNÓSTICO
# =============================================================================

# Gráfico 1: SSR vs Umbral (Likelihood Ratio Function)
df_plot <- data.frame(gamma = gamma_values, ssr = ssr_values)

p1 <- ggplot(df_plot, aes(x = gamma, y = ssr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_vline(xintercept = gamma_optimo, linetype = "dashed", 
             color = "red", linewidth = 1) +
  annotate("text", x = gamma_optimo, y = max(ssr_values) * 0.95, 
           label = paste0("γ* = ", round(gamma_optimo, 3)), 
           color = "red", hjust = -0.1) +
  labs(title = "Función de Razón de Verosimilitud",
       subtitle = "Suma de Cuadrados de Residuos vs Umbral",
       x = "Umbral de Inflación (γ)", 
       y = "SSR") +
  theme_minimal()

print(p1)

# Gráfico 2: Distribución de observaciones por régimen
pdata$regime <- ifelse(pdata$pi_tilde <= gamma_optimo, "Bajo", "Alto")

p2 <- ggplot(pdata, aes(x = pi_tilde, fill = regime)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  geom_vline(xintercept = gamma_optimo, linetype = "dashed", 
             color = "red", linewidth = 1) +
  scale_fill_manual(values = c("Bajo" = "steelblue", "Alto" = "coral")) +
  labs(title = "Distribución de Inflación Transformada por Régimen",
       x = "Inflación Transformada (π̃)", 
       y = "Frecuencia",
       fill = "Régimen") +
  theme_minimal()

print(p2)

# Gráfico 3: Efecto marginal de inflación por régimen
pi_seq_low <- seq(min(pdata$pi_tilde[pdata$pi_tilde <= gamma_optimo]), 
                  gamma_optimo, length.out = 100)
pi_seq_high <- seq(gamma_optimo, 
                   max(pdata$pi_tilde[pdata$pi_tilde > gamma_optimo]), 
                   length.out = 100)

effect_low <- coefs["pi_low"] * pi_seq_low
effect_high <- coefs["pi_high"] * pi_seq_high

df_effects <- data.frame(
  pi = c(pi_seq_low, pi_seq_high),
  effect = c(effect_low, effect_high),
  regime = c(rep("Bajo", length(pi_seq_low)), 
             rep("Alto", length(pi_seq_high)))
)

p3 <- ggplot(df_effects, aes(x = pi, y = effect, color = regime)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = gamma_optimo, linetype = "dashed", 
             color = "black") +
  scale_color_manual(values = c("Bajo" = "steelblue", "Alto" = "coral")) +
  labs(title = "Efecto Marginal de la Inflación sobre el Crecimiento",
       subtitle = "Por Régimen de Inflación",
       x = "Inflación Transformada (π̃)", 
       y = "Efecto sobre Δln(GDP)",
       color = "Régimen") +
  theme_minimal()

print(p3)

# =============================================================================
# 5. TEST DE SIGNIFICANCIA DEL UMBRAL (Bootstrap - opcional)
# =============================================================================

# Función para bootstrap (simplificada)
bootstrap_umbral <- function(data, n_boot = 500) {
  cat("\nRealizando bootstrap con", n_boot, "repeticiones...\n")
  
  boot_lr <- numeric(n_boot)
  
  # SSR del modelo sin umbral (restringido)
  formula_restricted <- gdp_growth ~ pi_tilde + 
    initial + igdp + dpop + dtot + sdtot
  modelo_restricted <- plm(formula_restricted, data = data, 
                           model = "within", effect = "individual")
  ssr_restricted <- sum(residuals(modelo_restricted)^2)
  
  # SSR del modelo con umbral óptimo (no restringido)
  ssr_unrestricted <- sum(residuals(modelo_final)^2)
  
  # LR observado
  lr_obs <- (ssr_restricted - ssr_unrestricted) / ssr_unrestricted
  
  cat("LR observado:", round(lr_obs, 4), "\n")
  
  return(list(lr_obs = lr_obs, p_value = NA))
}

# Descomentar para ejecutar bootstrap (puede tardar varios minutos)
boot_results <- bootstrap_umbral(pdata, n_boot = 500)

cat("\n===============================================\n")
cat("ANÁLISIS COMPLETO FINALIZADO\n")
cat("===============================================\n")

