# Para tener las mismas versiones de paquetes ***EN DUDA***
#install.packages("renv") #solo una vez
#renv::init() # Crea una carpeta para este proyecto
#renv::snapshot() #Para cuando se termine de actualizar los paquetes
#renv::restore() #Para cuando se abra el proyecto por primera vez
# =============================================================================#
# CONFIGURACIÓN GLOBAL DEL PROYECTO
# =============================================================================#
# LIMPIAR ENTORNO
rm(list = ls())

# CONFIGURAR OPCIONES GLOBALES
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notación científica
options(digits = 2) # Decimales a mostrar 

# LIBRERÍAS DEL PROYECTO
library(here)
library(WDI)
library(pwt10)
library(tidyverse)
library(dplyr)
library(readxl)
#library(lubridate)
#library(scales)
#library(readr)
library(naniar) # Librería para analizar datos faltantes
#library(eurostat)
library(VIM)   # Librería clásica para visualizar datos faltantes
library(mice)         # Imputación múltiple
#library(Hmisc)        # Herramientas estadísticas
library(kableExtra)   # Tablas mejoradas
#library(patchwork)    # Combinar gráficos
#library(ggpubr)   
library(zoo)
#library(lmtest)
#library(car)
#library(carData)
#library(MASS)
#library(ggplot2)
#library(mgcv)
#library(dplyr)
#library(writexl)
#library(countrycode)
#library(rvest)
#library(stringr)
#library(purrr)
#library(rlang)
library(ggrepel) # Etiquetas para los grafios de outliers
#library(stargazer)

# DEFINIR DIRECTORIO DE MANERA RERODUCIBLE
if (!exists("proyecto_tp_grupal")) {
  proyecto_tp_grupal <- here::here()  # Usa el paquete 'here'
}

# RUTAS PRINCIPALES
dir_data_raw <- file.path(proyecto_tp_grupal, "data", "raw")
dir_data_clean<-file.path(proyecto_tp_grupal, "data", "clean")
dir_data_processed <- file.path(proyecto_tp_grupal, "data", "processed")
dir_outputs_figures <- file.path(proyecto_tp_grupal, "outputs", "figures")
dir_outputs_tables <- file.path(proyecto_tp_grupal, "outputs", "tables")
dir_scripts<-file.path(proyecto_tp_grupal, "scripts")

# CREAR DIRECTORIOS SI NO EXISTEN
dirs_crear <- c(dir_data_raw, dir_data_clean,dir_data_processed, 
                dir_outputs_figures, dir_outputs_tables,
                dir_scripts)
for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# PARAMETROS DE ANÁLISIS
indicadores <- c(
  "deuda_gob" ="GC.DOD.TOTL.GD.ZS",   # DEBT (Central government debt)
  "crecimiento_pbi" ="NY.GDP.MKTP.KD.ZG",   # GDP_GROWTH (GDP growth (annual %))
  "pbi_p_c" ="NY.GDP.PCAP.KD",      # GDP_P_C (GDP per capita - Constant US$)
  "apertura" ="NE.TRD.GNFS.ZS",      # OPENNESS (Trade (% of GDP))
  "formacion_bruta_capital" ="NE.GDI.FTOT.ZS",      # LTOTAL (Inversión - Gross capital formation)
  "consumo_gobierno" ="NE.CON.GOVT.ZS",      # GOV_EXPEND (Government consumption)
  "interes_real" = "FR.INR.RINR",         # Interest rate (Real interest rate)
  "inflacion" = "NY.GDP.DEFL.KD.ZG",   # Inflation (GDP deflator (annual %))
  "poblacion" = "SP.POP.GROW",          # Population growth
  "gasto_gobierno_porc"="NE.CON.GOVT.ZS"
)

columnas<-c("crecimiento_pbi", "pbi_p_c",  "apertura", 
            "formacion_bruta_capital",  "consumo_gobierno",  
            "interes_real", "inflacion",
            "deuda_pbi", "deuda_deficit")

# Funciones para mensajes consistentes
mensaje_exito <- function(texto) {
  cat("
 ✅
 ", texto, "\n")
}
mensaje_proceso <- function(texto) {
  cat("
 🔄
 ", texto, "...\n")
}
mensaje_exito("Configuración cargada correctamente")

##Carga de configuración en cada script
# Inicio de cada script

