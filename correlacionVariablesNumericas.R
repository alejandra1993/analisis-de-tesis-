##########CORRELACION VARIABLES NUMERICAS VS CATEGORICAS
#LIBRERIAS QUE SE CARGARAN EN ESTE ARCHIVO PREVIAMENTE INSTALADAS 
library(dplyr)
library(ggplot2)

# CARGAR EL ARCHIVO CSV EN EL VECTOR SURVEY DESDE LA CARPETA DONDE ESTA UBICADO
setwd("/")
setwd("Users/carri/Desktop/Analisis Seminario/")
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)

#FUNCION STR PARA VER LOS NOMBRES DE TODAS LAS VARIABLE
str(survey)


# en nuestro caso nuestra encuesta solo tiene una variable de tipo numerica 
# Horas_diarias_estudio 
#la cual la correlacionaremos con la variable de tipo categorica
#Reprobacion


#FUNCION SUMMARY PARA LA VARIABLE DE TIPO NUMERICA CON EL OBJETIVO DE VER LA MEDIA PROMEDI ETC
summary(survey$Horas_.diarias_.estudio)

#FUNCION TABLE PARA VER LOS DATOS DE ESTA VARIABLE TIPO NUERICA
table(survey$Horas_.diarias_.estudio)


#FUNCION TABLE Y PROPTABLE PRA VER LOS DATOS Y SUS PORCENTAJES DE LA VARIABLE CATEGORICA
#QUE SERA CORRELACIONADA CON LAVARIABLE DE TIPO NUMERICA
table(survey$Reprobaci.n)
prop.table(table(survey$Reprobaci.n))


#descriptivo
#FUNCIONES QQNORM Y QQLINE PARA VER SI NUESTRA VARIABLE NUMERICA TEINE CALORES ATIPICOS QUE CAUSAN
#UNA DISTRIBUCION ANORMAL DE LA VARIABLE
qqnorm(survey$Horas_.diarias_.estudio)
qqline(survey$Horas_.diarias_.estudio)

#BOXPLOT PARA VER LOS CUARTILES Y IDINTIFICAR VALORES ATIPICOS 
boxplot(survey$Horas_.diarias_.estudio)

#PRUEBA DE SHAPIRO.TES PARA VER SI SU DISTRIBUCION ES NORMAL / ES NORMAL SI P-VALUEES MAYOR 0.05
shapiro.test(survey$Horas_.diarias_.estudio)

#H_O nuestra distribucion es normal
#H_A nuestra distribucion no es normal



#TRANSFORMACION PARA QUITAR VALORES ATIPICOS Y TRATAR DE NORMALIZAR VARIABLE NUMERICA EN SU DISTRIBUCION
survey[survey$Horas_.diarias_.estudio > 4 , "Horas_.diarias_.estudio"] <- median(survey$Horas_.diarias_.estudio)


#EVALUAMOS DE NUEVO LA VARIABLE PARA VER SI QUITARON VALORES ATIPICOS 
qqnorm(survey$Horas_.diarias_.estudio)
qqline(survey$Horas_.diarias_.estudio)
boxplot(survey$Horas_.diarias_.estudio)


#CORREMOS LA PRUEBA DE NUEVO PARA VER SI LA VARIABLE LOGRO UNA DISTRIBUCION NORMAL
shapiro.test(survey$Horas_.diarias_.estudio)

#H_O nuestra distribucion es normal
#H_A nuestra distribucion no es normal
table(survey$Horas_.diarias_.estudio,survey$Reprobaci.n)

#LA VARAIABLE NO TIENE UNA DISTRIBUCION NORMALNO SE PUEDE SEGUIR CON LAS PRUEBAS PARAMETRICAS 
#DEBIDO AQUE SOLO SE APLICA A VARIABLES CON DISTRIBUCION NORMAL


#LAS SIGUIENTES LINEAS SOLO SON DE PRUEBA PARA VER COMO CONTINUARIA EL ANALISIS 


Si_Reprobo <- survey %>% filter(Reprobaci.n == "Si") %>% select(Horas_.diarias_.estudio)
No_Reprobo <- survey %>% filter(Reprobaci.n == "No") %>% select(Horas_.diarias_.estudio)


# Si_reprobo
qqnorm(Si_Reprobo$Horas_.diarias_.estudio)
qqline(Si_Reprobo$Horas_.diarias_.estudio)
boxplot(Si_Reprobo$Horas_.diarias_.estudio)


shapiro.test(Si_Reprobo$Horas_.diarias_.estudio)


# NO_reprobo
qqnorm(No_Reprobo$Horas_.diarias_.estudio)
qqline(No_Reprobo$Horas_.diarias_.estudio)
boxplot(No_Reprobo$Horas_.diarias_.estudio)


shapiro.test(No_Reprobo$Horas_.diarias_.estudio)

str(survey)

