source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
# IMPORTAR DATOS
#==============================================================================#
# DESCARGA DE DATOS DE LA PWT
#==============================================================================#
# 1) Descargar PWT 10.x
pwt<-importar_datos(nombre_archivo = "pwt11.xlsx",carpeta = "raw")
# 2) Seleccionar variables relevantes
pwt_sel <- pwt %>%
  dplyr::select(country, countrycode , year, pop, rgdpna, csh_i, pl_x, pl_m) %>%
  mutate(rgdp_pc = rgdpna / pop,
         ln_rgdp_pc = log(rgdp_pc)) %>%
  group_by(country) %>%# 4) Crecimiento poblacional
  arrange(year) %>%
  mutate(dpop = log(pop) - lag(log(pop))) %>%
  ungroup()%>% 
  mutate(tot=pl_x/pl_m) #agregar tot
# Dataset final
pwt_ready <- pwt_sel
pwt_ready

# EXPORTAMOS ESTOS DATOS A RAW
exportar_data(data = pwt_ready,nombre = "pwt_tabla",carpeta = "raw")

#==============================================================================#
# DESCARGA DE DATOS DEL BANCO MUNDIAL
#==============================================================================#
# 1) Descargar datos WDI
vars <- c(
  inflation = "FP.CPI.TOTL.ZG",
  gdp_pc_ppp = "FP.CPI.TOTL.ZG",
  igdp = "NE.GDI.TOTL.ZS",
  dpop = "SP.POP.GROW",
  indice_terminos_netos_del_intercambio = "TT.PRI.MRCH.XD.WD",
  terminos_netos_del_intercambio = "NE.TRM.TRAD.XU",
  ajuste_de_lo_tot="NY.TTF.GNFS.KN"
)
wdi_raw <- WDI(country = "all", indicator = vars, start = 1960, end = 2024)
# 2) Limpiar
wdi_clean <- wdi_raw %>%
  filter(!is.na(iso2c), iso2c != "NA") %>%
  rename(country = country,
         year = year)
wdi_ready <- wdi_clean
wdi_ready

exportar_data(data = wdi_ready,nombre = "wdi_tabla" ,carpeta = "raw")

#==============================================================================#
# DESCARGA DE DATOS DE **INFLACION** DEL BANCO BANCO MUNDIAL
#==============================================================================#
inflacion_wb<-importar_datos(nombre_archivo = "inflacion_wb.csv", carpeta = "raw")
print(inflacion_wb, n=Inf)

# PIVOTEAR
  tabla_larga_inflacion <- inflacion_wb %>%
    pivot_longer(
      # Excluye las columnas de identificación con el signo '-'
      cols = -c(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`),
      # Los nombres de las columnas pivotadas (los años) irán a la columna 'Year'
      names_to = "Year",
      # Los valores de esas columnas (la tasa de inflación) irán a 'Value'
      values_to = "Value"
    ) %>%
    # Limpieza opcional: Convertir la columna Year a numérica (si no tiene las tildes)
    # y convertir Value a numérica (si es doble)
    mutate(
      Year = as.numeric(gsub("`", "", Year))
    )
  
# ELIMINO AÑOS ANTERIORES A 1970
  tabla_filtrada <- tabla_larga_inflacion %>%
    # Aseguramos que la columna Year sea numérica antes de filtrar
    mutate(Year = as.numeric(Year)) %>%
    # Filtramos para incluir solo los años mayores o iguales a 1970
    filter(Year >= 1970)
  # El resultado se guarda en 'tabla_filtrada'
  head(tabla_filtrada)

  exportar_data(data = tabla_filtrada,nombre = "inflacion" ,carpeta = "raw")
  


#==============================================================================#
# DESCARGA DE DATOS DE **META-DATA** DEL BANCO BANCO MUNDIAL
#==============================================================================#
meta <- WDI_data$country
meta_continente<-meta%>%dplyr::select("iso3c","region", "income")
exportar_data(data = meta_continente,nombre = "meta_continente" ,carpeta = "raw")


#==============================================================================#
# DESCARGA DE DATOS DE OTRA INFLACION DEL BANCO BANCO MUNDIAL
#==============================================================================#
inflacion2<-read_excel(path = "D:/ProyectosR/TP_FINAL_CRECIMIENTO_PROJECT/data/raw/Inflation-data.xlsx")

inflacion2_limpia<-limpiar(tabla = inflacion2)%>%select(-imf_country_code, -series_name)
# PIVOTEAR
tabla_larga_inflacion <- inflacion2_limpia %>%
  tidyr::pivot_longer(
    # Excluye las columnas de identificación Y cualquier columna de texto (como 'note')
    cols = -c(country_code, country, indicator_type, note), # <--- ¡Agrega 'note' aquí!
    
    names_to = "year",
    values_to = "inflacion"
  ) %>%
  # Limpieza opcional: Convertir la columna Year a numérica (si no tiene las tildes)
  # y convertir Value a numérica (si es doble)
  dplyr::mutate(
    year = as.numeric(gsub("`", "", year))
  )
exportar_data(data = tabla_larga_inflacion,nombre = "infalcion2",carpeta = "raw")







