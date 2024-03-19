### Juan Fernando Contreras Garcia & Mariana Baquero Jara
### Codigo Uniandes: 202011873
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
       janitor, # contiene conjuntos de datos.
       dplyr) # renombar variables

##1. Importar/exportar bases de datos

#1.1 Importe las bases de datos Módulo de sitio o ubicación en un objeto llamdo location y Módulo de identificación en un objeto llamado identification.
#Se importan las bases de datos del repositorio problem set 2 y se colocan en la carpeta input, se cargan desde dicha ubicacion usando import de rio
identification = import(file="input/Módulo de identificación.dta")
location = import(file="input/Módulo de sitio o ubicación.dta")

#1.2 Exporte a la carpeta output los objetos cargados en el punto anterior, guárdelos como location.rds y identification.rds.
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
