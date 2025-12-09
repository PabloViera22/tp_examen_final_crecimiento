source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_analisis_na.R"))


tabla_na<-importar_datos(nombre_archivo = "inflacion_mas_banco_sf.csv",carpeta = "processed")
analisis_na_pais<-analizar_na(tabla = tabla_na,grupo = "country")
analisis<-analisis_na_pais$por_columna%>%arrange(na_dm02)
print(analisis,n=Inf)
