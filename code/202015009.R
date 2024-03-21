### Juan Fernando Contreras Garcia & Mariana Baquero Jara
### Codigo Uniandes: 202011873 & 202015009
### Update: 20/03/2024
### R version 4.3.2 (2023-10-31 ucrt)

#Limpiar el environment
rm(list=ls())

#Saber version de R.
R.version.string
#Obtener ruta al repositorio actual
getwd() 

#instalar/llamar pacman
require(pacman)


#uso la funcion p_load de pacman para instalar/llamar las librerias que se usaran en el problem set
p_load(rio, # funcion import/export: permite leer/escribir archivos desde diferentes formatos. 
       skimr, # funcion skim: describe un conjunto de datos
       janitor, # contiene conjuntos de datos
       tidyverse, ## manipular/limpiar conjuntos de datos
       dplyr) # renombar variables

##1. Importar/exportar bases de datos

#1.1 Importe las bases de datos Modulo de sitio o ubicacion en un objeto llamdo location y Modulo de identificacion en un objeto llamado identification.
#Se importan las bases de datos del repositorio problem set 2 y se colocan en la carpeta input, se cargan desde dicha ubicacion usando import de rio
identification = import(file="input/Modulo de identificacion.dta")
location = import(file="input/Modulo de sitio o ubicacion.dta")


#1.2 Exporte a la carpeta output los objetos cargados en el punto anterior, guarrdelos como location.rds y identification.rds.
#Se exportan a la carpeta output en formato rds
export(x=identification , file="output/identification.rds")
export(x=location , file="output/location.rds")


##2. Generar variables.

#Se genera la variable llamada bussiness_type que toma distintos valores dependiendo del valor en la variable grupos de identification.
#Se realizo utilizando mutate y un case_when (condicional) al haber distintos posibles valores. Vale la pena destacar que la variable Grupos4 estaba en texto (a esto se deben los valores numericos en "")
identification=mutate(identification,bussiness_type=case_when(identification$GRUPOS4=="01" ~ "Agricultura" ,
                                                   identification$GRUPOS4=="02" ~ "Industria Manufacturera" ,
                                                   identification$GRUPOS4=="03" ~ "Comercio" ,
                                                   identification$GRUPOS4=="04" ~ "Servicios"))

#Se genera la variable grupo_etario que divide a los invividuos por su edad (que se encuentra registrada en la variable P241)
#En este caso, se dividio en las etapas del ciclo de vida registradas por el Minsterio de Salud y Proteccion Social de Colombia.
#Se realizo utilizando mutate y un case_when (condicional) al haber distintos posibles valores.
#Pd: en el caso de la adultez esta se clasifica entre 27 y 59 anos, decidimos dividir este ciclo en dos para cumplir el requisito de 4 grupos etarios de la variable.
identification=mutate(identification,grupo_etario=case_when(identification$P241<27 ~ "Jovenes" ,
                                                              identification$P241>=27 & identification$P241<40 ~ "Adultos Jovenes" ,
                                                              identification$P241>=40 & identification$P241<60 ~ "Adultos" ,
                                                              identification$P241>=60 ~ "Adultos Mayores"))

#Sobre el objeto location, genere una variable llamada ambulante, que sera igual a 1 si la variable P3053 es igual a 3, 4 o 5.
#En este caso se genera la variable con la funcion $ y se usa | como condicional "o".
location$ambulante = ifelse(test=(location$P3053==3|location$P3053==4|location$P3053==5), yes=1 , no=0)


##3. Eliminar filas/columnas de un conjunto de datos.

#Almacene en un objeto llamado identification_sub las variables DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario, ambulante, COD_DEPTO y F_EXP.
#En este caso se elimino el grupo entre area y grupos 12 ya que las variables deseadas se encontraban en los "extremos" de la tabla, vale la pena destacar que en indentification no se tiene la variable ambulante, por lo que no se tuvo en cuenta.
identification_sub=select(.data=identification,-AREA:-GRUPOS12)

#Del objeto location seleccione solo las variables DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ambulante P3054, P469, COD_DEPTO, F_EXP y guardelo en nuevo objeto llamado location_sub.
#Se seleccionaron "a mano" ya que no se encontro un patron para eliminar las variables no deseadas o seleccionar solo las deseadas.
location_sub=select(.data=location,DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,ambulante,P3054,P469,COD_DEPTO,F_EXP)

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
# segun las personas jovenes que estan en comercio

AdultosJovenes_en_Comercio_y_Depto <- rename(.data = df_adul_jov_dep, "Cantidad Personas" = "n")


hist(AdultosJovenes_en_Comercio_y_Depto$`Cantidad Personas`)

#El analisis revela que en ciertas regiones hay una mayor cantidad de adultos y adultos jovenes, 
#mostrando una clara tendencia en la distribucion por edad y ubicacion. Estos grupos etarios dominan los datos, 
#sugiriendo que son mas activos o posiblemente tienen mas oportunidades en esas areas especaficas. 
#La variabilidad entre las regiones destaca las diferencias en la composicion de la poblacion, 
#reflejando como se distribuyen estos grupos a lo largo del pais.
