source(here::here("config", "parametros.R"))
source(here::here("funciones", "funcion_importa_exportar.R"))
source(here::here("funciones", "funcion_imputar.R"))
source(here::here("funciones", "funcion_analisis_na.R"))
source(here::here("funciones", "funcion_lag.R"))

tabla_a_imputar<-importar_datos(nombre_archivo = "tabla_a_imputar.csv",carpeta = "processed")

vector_columna<-c("pop", "rgdpna", "csh_i","pl_x", "pl_m")
imputada<-imputacion_multiple(datos = tabla_a_imputar, vector_columna = vector_columna)
#==============================================================================#
# SELECCION DE PAISES AL AZAR
paises_azar<-tabla_a_imputar%>%dplyr::filter(year==2023)%>%
  select(country,nivel_de_desarrollo)

azar_en_desarrollo<-paises_azar%>%filter(nivel_de_desarrollo=="pais en desarrollo")
azar_desarrollado<-paises_azar%>%filter(nivel_de_desarrollo=="pais desarrollado")

vector_paises_en_desarrollo <- sample(
  x = azar_en_desarrollo$country, # El vector de donde sacar la muestra
  size = 30           # El tamaño de la muestra que queremos (solo 1)
)

vector_paises_desarrollados <- sample(
  x = azar_desarrollado$country, # El vector de donde sacar la muestra
  size = 30           # El tamaño de la muestra que queremos (solo 1)
)
#suma de vectores
paises_a_seleccionar<-c(vector_paises_en_desarrollo, vector_paises_desarrollados)

#==============================================================================#
#AGREGAMOS INCREMENTO PBI PC
tabla_con_incremento_pbi<-imputada %>%
  group_by(country) %>%
  arrange(year) %>%mutate(
    valor_anterior_pib_pc = dplyr::lag(rgdp_pc),
    var_abs_pib_pc = rgdp_pc - valor_anterior_pib_pc,
    var_pct_pib_pc = ((rgdp_pc / valor_anterior_pib_pc) - 1) * 100
  ) %>%  ungroup()
# AGREGAMOS INCREMENTO DE LOG DGP
tabla_con_incremento_log<-tabla_con_incremento_pbi %>%
  group_by(country) %>%
  arrange(year) %>%mutate(
    valor_anterior =  dplyr::lag(ln_rgdp_pc),
    incremento_abs_pib = ln_rgdp_pc - valor_anterior,
    incremento_pct_pib = ((ln_rgdp_pc / valor_anterior) - 1) * 100
  ) %>%  ungroup()



# analisis de na por pais y por año
p<-analizar_na(tabla_con_incremento_log,grupo = "year")
q<-analizar_na(tabla_con_incremento_log,grupo = "country")
print(p$por_columna,n=Inf) # hay datos para trabajar y borrar
print(q$por_columna,n=Inf) # mayoria de los paises no le faltan datos, es sistemico
# Ressultado Czechia le faltan muchos años en dpop. Eliminar
tabla_filtro_dpop<-tabla_con_incremento_log%>%dplyr::filter(country!="Czechia")

names(tabla_filtro_dpop)

#agregar creicimietno de tot
tabla_con_incremento_tot<-tabla_filtro_dpop %>%
  group_by(country) %>%
  arrange(year) %>%mutate(
    valor_anterior = dplyr::lag(tot),
    incremento_abs_tot = tot - valor_anterior,
    incremento_pct_tot = ((tot / valor_anterior) - 1) * 100
  ) %>%  ungroup()





#exportamos esta ultima tabla
exportar_data(data = tabla_con_incremento_tot, nombre = "tabla_a_promediar",carpeta = "processed")














