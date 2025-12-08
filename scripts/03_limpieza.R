source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_limpieza.R"))
source(here::here("funciones", "funcion_analisis_na.R"))

tabla_a_limpiar<-importar_datos(nombre_archivo = "tabla_unida.csv",carpeta = "processed")

tabla_infla_sin_na <- tabla_a_limpiar %>%
  group_by(country.x) %>% filter(sum(is.na(inflacion)) == 0) %>%ungroup()

#conteo de años y apises
tabla_infla_sin_na%>%dplyr::distinct(countrycode) %>%dplyr::count()#146
tabla_infla_sin_na%>%dplyr::distinct(year) %>%dplyr::count()# 54
#==============================================================================#
meta_datos<-importar_datos(nombre_archivo = "meta_continente.csv",carpeta = "raw")

tabla_unida <- tabla_infla_sin_na %>%
  left_join(meta_datos, by = c("countrycode" = "iso3c")) %>% # Mapea "countrycode" de tabla_uno a "contry_code" de tabla_dos
      mutate(nivel_de_desarrollo=case_when(
        income== "High income" ~"pais desarrollado",
        TRUE~"pais en desarrollo"
      ))%>% select(-region, -countrycode)
print(tabla_unida, n=10)

exportar_data(data = tabla_unida,nombre = "tabla_con_desarrollo",carpeta = "processed")

#==============================================================================#
tabla_contar_na<-importar_datos(nombre_archivo = "tabla_con_desarrollo.csv",carpeta = "processed")
tabla_contar_na_nombre_country<-tabla_contar_na%>% rename(country=country)
names(tabla_contar_na)
analizar_na(tabla = tabla_contar_na, grupo = "year")
faltantes<-analizar_na(tabla = tabla_contar_na_nombre_country, grupo = "country")
faltantes$por_columna %>%arrange(desc(na_pop))
paises_a_sacar<-c("Belarus","czechia","Russian Federation", "Ukraine", "Yemen")

tabla_a_imputar<-tabla_contar_na_nombre_country%>%filter(!(country %in% paises_a_sacar))
exportar_data(data = tabla_a_imputar,nombre = "tabla_a_imputar",carpeta = "processed")
















