### Juan Fernando Contreras Garcia & Mariana Baquero Jara
### Codigo Uniandes: 202011873 & 202015009
### Update: 18/03/2024
### R version 4.3.2 (2023-10-31 ucrt)

#Limpiar el environment
rm(list=ls())

#Saber version de R.
R.version.string
#Obtener ruta al repositorio actual
getwd() 

#instalar/llamar pacman
require(pacman)


#uso la función p_load de pacman para instalar/llamar las librerias que se usaran en el problem set
p_load(rio, # función import/export: permite leer/escribir archivos desde diferentes formatos. 
       skimr, # funcion skim: describe un conjunto de datos
       janitor, # contiene conjuntos de datos
       tidyverse, ## manipular/limpiar conjuntos de datos
       dplyr) # renombar variables

##1. Importar/exportar bases de datos

#1.1 Importe las bases de datos Módulo de sitio o ubicación en un objeto llamdo location y Módulo de identificación en un objeto llamado identification.
#Se importan las bases de datos del repositorio problem set 2 y se colocan en la carpeta input, se cargan desde dicha ubicacion usando import de rio
identification = import(file="input/Módulo de identificación.dta")
location = import(file="input/Módulo de sitio o ubicación.dta")

##-----------
# library(haven)
# identification <- read_dta("~/GitHub/Rproblemset2/input/Módulo de identificación.dta")
# location <- read_dta("~/GitHub/Rproblemset2/input/Módulo de sitio o ubicación.dta")
##------------

#1.2 Exporte a la carpeta output los objetos cargados en el punto anterior, guÃ¡rdelos como location.rds y identification.rds.
#Se exportan a la carpeta output en formato rds
export(x=identification , file="output/identification.rds")
export(x=location , file="output/location.rds")


##2. Generar variables.
identification=mutate(identification,bussiness_type=case_when(identification$GRUPOS4=="01" ~ "Agricultura" ,
                                                   identification$GRUPOS4=="02" ~ "Industria Manufacturera" ,
                                                   identification$GRUPOS4=="03" ~ "Comercio" ,
                                                   identification$GRUPOS4=="04" ~ "Servicios"))

identification=mutate(identification,grupo_etario=case_when(identification$P241<27 ~ "Jovenes" ,
                                                              identification$P241>=27 & identification$P241<40 ~ "Adultos Jovenes" ,
                                                              identification$P241>=40 & identification$P241<60 ~ "Adultos" ,
                                                              identification$P241>=60 ~ "Adultos Mayores"))
location$ambulante = ifelse(test=(location$P3053==3|location$P3053==4|location$P3053==5), yes=1 , no=0)

identification_sub=select(.data=identification,-AREA:-GRUPOS12)

location_sub=select(.data=location,DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,ambulante,P3054,COD_DEPTO,F_EXP)

# 4. Combinar bases de datos

df <- full_join(x=identification_sub,y=identification_sub,by=c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))

# 5. Estadisticas descriptivas

#Resumen estadistico general de la nueva base de datos df
summary(df)

# select + summarize_all 

select(.data = df, DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA) %>% summarise_all(mean)

select(.data = df, grupo_etario.x, grupo_etario.y) %>% summarise_all(median)

#Canridad de NA por columna
CantidadNa <- colSums(is.na(df))

# Agrupadas

df <- mutate(df, Actividad.x = ifelse(bussiness_type.x == "Comercio", "Comercio", "Otros"))

# F_EXP.x promedio por actividad comercio

Prom <- df %>% group_by(Actividad.x) %>% summarise(F_EXP.x_mean = mean(F_EXP.x))

#Sin los NA

PromSinNA <- df %>%
  filter(Actividad.x == "Comercio" | Actividad.x == "Otros") %>%
  group_by(Actividad.x) %>%
  summarise(F_EXP.x_mean = mean(F_EXP.x, na.rm = T))

# F_EXP.x por Comercio y grupo_etario.x

Com_y_grupo <- df %>% group_by(Actividad.x, grupo_etario.x) %>% 
  summarise(F_EXP.x_mean = mean(F_EXP.x))

# F_EXP.x por Comercio y grupo_etario.x sin NA

Com_y_grupo <- df %>%
  filter(Actividad.x == "Comercio" | Actividad.x == "Otros") %>%
  group_by(Actividad.x, grupo_etario.x) %>%
  summarise(F_EXP.x_mean = mean(F_EXP.x, na.rm = T))

#Eliminar NA de df Actividad.x

df_sinNA <- df %>%
  filter(Actividad.x == "Comercio" | Actividad.x == "Otros")

Datos_sin_NA <- df_sinNA %>%
  group_by(Actividad.x, grupo_etario.x) %>%
  summarise(F_EXP.x_mean = mean(F_EXP.x, na.rm = T))

# Adultos jovenes, otros y codigo departamento (cantidad)

df <- mutate(df, Grupo.x = ifelse(grupo_etario.x == "Adultos Jovenes", 
                                  "Adultos Jovenes", "Otros"))

df_adul_jov_dep <- df %>% 
  group_by(Grupo.x) %>% 
  count(COD_DEPTO.x) 

# Solo adultos jovenes y codigo departamento (cantidad)

df_adul_jov_dep <- df %>% 
  filter(grupo_etario.x == "Adultos Jovenes") %>% 
  count(COD_DEPTO.x) 

# Grupos Grupo.x, Actividad.x, junto con codigo departamento (cantidad)

df_adul_jov_dep <- df %>% 
  group_by(Grupo.x, Actividad.x) %>% 
  count(COD_DEPTO.x) 

# Adultos jovenes, comercio y codigo departamento (cantidad)

df_adul_jov_dep <- df %>% 
  group_by(Grupo.x, Actividad.x) %>% 
  filter(grupo_etario.x == "Adultos Jovenes" & Actividad.x == "Comercio") %>% 
  count(COD_DEPTO.x) 

# Histograma de la cantidad de personas con cod_departamento.x 
# segun las personas jovenes que estén en comercio

AdultosJovenes_en_Comercio_y_Depto <- rename(.data = df_adul_jov_dep, "Cantidad Personas" = "n")


hist(AdultosJovenes_en_Comercio_y_Depto$`Cantidad Personas`)

