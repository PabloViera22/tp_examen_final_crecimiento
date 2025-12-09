source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_limpieza.R"))
# que paises necesito
seleccion_desarrollados_29 <- c(
  "Australia", "Austria", "Belgium", "Canada", "Switzerland",
  "Germany", "Denmark", "Spain", "Finland", "United States",
  "France", "United Kingdom", "Greece", "Ireland", "Iceland",
  "Israel", "Italy", "Japan", "Korea, Rep.", "Luxembourg",
  "Malta", "Netherlands", "Norway", "New Zealand", "Portugal",
  "Singapore", "Sweden", "Chile", "Uruguay"
)
seleccion_en_desarrollados_31<- c(
  "Brazil",  "Botswana", "China", "Colombia","Dominican Republic",
  "Costa Rica", "Egypt, Arab Rep.","Ghana","India","Iran, Islamic Rep.",
  "Jamaica","Liberia","Mexico", "Cabo Verde",
  "St. Lucia","Sri Lanka","Morocco", "Mali",
  "Nigeria","Peru","Paraguay","Philippines","Senegal",
  "Sierra Leone","El Salvador","São Tomé and Principe",
  "Turkey","Venezuela, RB","South Africa","Vietnam","Cameroon"
)
suma_de_seleccion<-c(seleccion_desarrollados_29,seleccion_en_desarrollados_31)

# necesito los datos de la inflacion
inflacion<-importar_datos(nombre_archivo = "infalcion2.csv", carpeta ="raw")

inflacion_muestra<-inflacion%>%dplyr::filter(country%in%suma_de_seleccion)%>%
  select(-note,-indicator_type)%>%filter(year<=2020)
n_distinct(inflacion_muestra$country)# 50 paises subio a 58

# datos del sistema financiero
datos_bancarios<-importar_datos(nombre_archivo = "wb_sistema_financiero.xlsx",carpeta = "raw")
datos_banco_muestra<-datos_bancarios%>%select(country,iso3, year, di01,dm02)%>%
  dplyr::filter(year>=1970)%>%filter(country %in% suma_de_seleccion)
n_distinct(datos_banco_muestra$country)#subio a 58, faltan

#JOIN DE AMBAS TABLAS
tabla_unida <- inflacion_muestra %>%
  left_join(datos_banco_muestra, 
            by = c(
              "country" = "country", # Mapea "countrycode" de tabla_uno a "contry_code" de tabla_dos
              "year" = "year"                # Mapea "year" de ambas tablas
            ))%>%select(-iso3)

# Exportamos los datos
exportar_data(data = tabla_unida,nombre = "inflacion_mas_banco",carpeta = "processed")












