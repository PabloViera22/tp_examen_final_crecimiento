source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))

#importamos datos
datos_bancarios<-importar_datos(nombre_archivo = "wb_sistema_financiero.xlsx",carpeta = "raw")

#filtramo nigeria y australia por separado por porblema tecnico
datos_filtro<-datos_bancarios%>%filter(iso3=="AUS")%>%
  select(country, year,di01,di02,di03,di04,di05)
datos_filtro_nga<-datos_bancarios%>%filter(iso3=="NGA")%>%
  select(country, year,di01,di02,di03,di04,di05,dm02)#GFDD.DM.02: Stocks traded, total value to GDP

#unimos lo anteior
datos_bancario_filtro <- bind_rows(datos_filtro, datos_filtro_nga)%>%select(country, year,di01,dm02)


