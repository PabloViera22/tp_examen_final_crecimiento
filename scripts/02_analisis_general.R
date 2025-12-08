source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_analisis_na.R"))
source(here::here("funciones", "funcion_limpieza.R"))


pwt_tabla_est<-importar_datos(nombre_archivo = "pwt_tabla.csv",carpeta = "raw")
wdi_tabla_est<-importar_datos(nombre_archivo = "wdi_tabla.csv",carpeta = "raw")
meta_data<-importar_datos(nombre_archivo = "meta_continente.csv",carpeta = "raw")
tabla_inflacion<-importar_datos(nombre_archivo = "inflacion.csv",carpeta = "raw")
tabla_inflacion_2<-importar_datos(nombre_archivo = "infalcion2.csv",carpeta = "raw")

inflacion_arreglo<-limpiar(tabla_inflacion)

#filtrar años
pwt_filer_year<-pwt_tabla_est%>% filter(year>=1970)
wdi_filter_year<-wdi_tabla_est%>%filter(year>=1970)
inflacion_filter_year<-inflacion_arreglo%>%filter(year>=1970)

# Cantidad de países
pwt_filer_year %>%dplyr::distinct(countrycode) %>%dplyr::count()#185 paises
wdi_filter_year %>%dplyr::distinct(iso2c) %>%dplyr::count() #265 PAISES
inflacion_filter_year%>%dplyr::distinct(country_code) %>%dplyr::count() # 266
tabla_inflacion_2%>%dplyr::distinct(country_code) %>%dplyr::count()#205

# CANTIDAD DE AÑOS
pwt_filer_year %>%dplyr::distinct(year) %>%dplyr::count()# 54
wdi_filter_year %>%dplyr::distinct(year) %>%dplyr::count() # 55
inflacion_filter_year%>%dplyr::distinct(Year) %>%dplyr::count() # 55
tabla_inflacion_2%>%dplyr::distinct(year) %>%dplyr::count()#55

#==============================================================================#
# UNION DE TABLA DE PWT Y INFLACION
#==============================================================================#
tabla_unida <- pwt_filer_year %>%
  left_join(tabla_inflacion_2, 
            by = c(
              "countrycode" = "country_code", # Mapea "countrycode" de tabla_uno a "contry_code" de tabla_dos
              "year" = "year"                # Mapea "year" de ambas tablas
            ))%>%select(-indicator_type, -note, -country.y)


#==============================================================================#
# GRAFICO DE DATOS FALTANTES
aggr(tabla_unida,
     # Colores: 'steelblue' para presentes, 'red' para faltantes
     col = c('steelblue', 'red'),
     numbers = TRUE,           # Mostrar el porcentaje en las barras
     sortVars = TRUE,          # Ordenar variables por el número de NA
     cex.axis = 0.8,           # Tamaño de la fuente de los ejes
     gap = 3,                  # Espacio entre los dos gráficos
     main = "Análisis de Valores Faltantes (NA) del Ejemplo", # Título principal
     ylab = c("Proporción de Faltantes", "Patrones de Combinación") # Títulos de los ejes Y
)

aggr(tabla_inflacion_2,
     # Colores: 'steelblue' para presentes, 'red' para faltantes
     col = c('steelblue', 'red'),
     numbers = TRUE,           # Mostrar el porcentaje en las barras
     sortVars = TRUE,          # Ordenar variables por el número de NA
     cex.axis = 0.8,           # Tamaño de la fuente de los ejes
     gap = 3,                  # Espacio entre los dos gráficos
     main = "Análisis de Valores Faltantes (NA) del Ejemplo", # Título principal
     ylab = c("Proporción de Faltantes", "Patrones de Combinación") # Títulos de los ejes Y
)

aggr(inflacion_filter_year,
     # Colores: 'steelblue' para presentes, 'red' para faltantes
     col = c('steelblue', 'red'),
     numbers = TRUE,           # Mostrar el porcentaje en las barras
     sortVars = TRUE,          # Ordenar variables por el número de NA
     cex.axis = 0.8,           # Tamaño de la fuente de los ejes
     gap = 3,                  # Espacio entre los dos gráficos
     main = "Análisis de Valores Faltantes (NA) del Ejemplo", # Título principal
     ylab = c("Proporción de Faltantes", "Patrones de Combinación") # Títulos de los ejes Y
)
#==============================================================================#

#conteo de años y apises
tabla_unida%>%dplyr::distinct(countrycode) %>%dplyr::count()#185
tabla_unida%>%dplyr::distinct(year) %>%dplyr::count()# 54

# analisis de na por pais y por año
p<-analizar_na(tabla_unida,grupo = "year")
q<-analizar_na(tabla_unida,grupo = "countrycode")
print(p$por_columna,n=Inf) # hay datos para trabajar y borrar
print(q$por_columna,n=Inf) # mayoria de los paises no le faltan datos, es sistemico

# algo de proporciones
faltantes_year<-tabla_unida %>%group_by(year)%>%dplyr::summarise(dplyr::across(
  .cols = dplyr::everything(),.fns = ~ sum(is.na(.)) / n() * 100))
faltantes_pais<-tabla_unida %>%group_by(countrycode)%>%dplyr::summarise(dplyr::across(
  .cols = dplyr::everything(),.fns = ~ sum(is.na(.)) / n() * 100))%>% arrange(inflacion)

# EXPORTAMOS LA TABLA JOINEADA
exportar_data(data =tabla_unida ,nombre = "tabla_unida",carpeta = "processed")



