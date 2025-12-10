source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_imputar.R"))
source(here::here("funciones", "funcion_lag.R"))

tabla_completa<-importar_datos(nombre_archivo = "tabla_completa.csv", carpeta = "clean")
# TENGO QUE SELECCIONAR LOS 60 PAISES
tabla_elejir_pais<-importar_datos(nombre_archivo = "tabla_a_promediar.csv",carpeta = "processed")
desarrollado<-tabla_elejir_pais%>%dplyr::filter(nivel_de_desarrollo=="pais desarrollado")
en_desarrollo<-tabla_elejir_pais%>%dplyr::filter(nivel_de_desarrollo=="pais en desarrollo")
  #unicos
unique(desarrollado$country)
unique(en_desarrollo$country)
seleccion_desarrollados_29 <- c(
  "Australia", "Austria", "Belgium", "Canada", "Switzerland",
  "Germany", "Denmark", "Spain", "Finland", "United States",
  "France", "United Kingdom", "Greece", "Ireland", "Iceland",
  "Israel", "Italy", "Japan", "Korea, Rep.", "Republic of Korea" , "Luxembourg",
  "Malta", "Netherlands", "Norway", "New Zealand", "Portugal",
  "Singapore", "Sweden", "Chile", "Uruguay"
)
seleccion_en_desarrollados_31<- c(
  "Brazil",  "Botswana", "China", "Colombia","Dominican Republic",
  "Costa Rica", "Egypt, Arab Rep.", "Egypt","Ghana","India","Iran, Islamic Rep.","Iran (Islamic Republic of)",
  "Jamaica","Liberia","Mexico", "Cabo Verde",
  "St. Lucia","Saint Lucia" ,"Sri Lanka","Morocco", "Mali",
  "Nigeria","Peru","Paraguay","Philippines","Senegal",
  "Sierra Leone","El Salvador","São Tomé and Principe","Sao Tome and Principe" ,
  "Turkey","Türkiye","Venezuela, RB", "Venezuela (Bolivarian Republic of)","South Africa","Vietnam", "Viet Nam","Cameroon"
)
suma_de_seleccion<-c(seleccion_desarrollados_29,seleccion_en_desarrollados_31)

# LE TENGO QUE AGREGAR EL PBI PERCAPITA DE 1970
pbi_en_1970<-importar_datos(nombre_archivo = "tabla_a_promediar.csv",carpeta = "processed")

columna_pib_inicial<-pbi_en_1970%>%filter(year==1970)%>%select(country, pbi_inicial=rgdp_pc)

# AGREGO EL PBI INICIAL A LA TABLA
datos_con_pib_inicial <- tabla_completa %>%
  left_join(columna_pib_inicial, by = "country" )

tabla_final_filtrada_60p<-datos_con_pib_inicial%>%filter(country%in%suma_de_seleccion)

exportar_data(data = tabla_final_filtrada_60p,nombre = "tabla_limpia",carpeta = "clean")







