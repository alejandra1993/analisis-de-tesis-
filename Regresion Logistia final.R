
# LIBRERIAS QUE SE CARGARAN EN ESTE ARCHIVO / ESTAS LIBRERIAS DEBEN SER INSTALAS ANTERIORMENTE
library(dplyr)
library(ggplot2)
library(caret)


# CARGAR EL ARCHIVO CSV EN EL VECTOR SURVEY DESDE LA CARPETA DONDE ESTA UBICADO
setwd("/")
setwd("Users/carri/Desktop/Analisis Seminario/")
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)




#FUNCIONES TABLE Y PROPTABLE PARA VER LOS PORCENTAJES DE LA VARIABLE DE PESO
table(survey$Uso_.lms)
prop.table(table(survey$Uso_.lms)) 



#CARGAR LAS VARIABLES DE ACOMPANAMIENTO JUNTO CON LA VARIABLE DE PESO PARA REGRESION LOGISTICA
features <- c(
  "Computadora_.permanente",
  "Conexión_.permanente",
  "Rango_.promedio_.clases",
  "Horas_.diarias_.estudio",
  "Disciplina",
  "Autodidacta",
  "Plataforma",
  "Frecuencia_.de_.uso", 
  "Recursos",
  "Uso_.lms"
)


#CREAMOS UN NUEVO VECTOR SET PARA CARGAR LAS VARIABLES QUE SOLO VAMOS USAR
set <- survey[, names(survey) %in% features ]

set$Uso_.lms <- as.factor(set$Uso_.lms)

#APLICAMOS LA REGRESION LOGISTICA Y CARGAMOS EL RESULTADO EN VECTOR MODEL
model <- glm(Uso_.lms ~ ., data = set, family = "binomial") 
#IMPRIMIMOS EL VECTOR MODEL PARA VER LOS RESULTADOS 
model   

#CREAMOS U NUEVO VECTOR IMPIRTANCIA Y CORREMOS LA FUNCION VARIMP QUE NOS MUESTRA
#EL COEFICIENTE DE IMPORTANCIA POR NOMBRE DE VARIABLE
Importancia <- varImp(model)
#IMPRIMIMOS EL VECTOR IMPORTANCIA PARA VER LOS RESULTADOS 
Importancia

#HACEMOS UNA PeQUENA TRANSFORMACION PARA AGREGAR EL NOMBRE DE IMPORTANCIA A LA COLUMNA
Importancia$col <- row.names(Importancia)


#APLICAMOS UN ARRANGE AL COEFICIENTE DE IMPORTACIA DE VARIABLE OVERALL
Importancia <- Importancia %>% arrange(-Overall)  

#IMPRIMIMOS EL VECTOR IMPORTANCIA PARA VER EL ORDEN DE LAS VARIABLES SEGUN SU IMPORTANCIA
Importancia

#GENERAMOS UN GRAFICO PARA INTERPRETAR DE MEJOR FORMA LOS RESULTADOS DE CORRELACION
#DE CADA UNA DE LAS VARIABLES SEGUN EL ORDEN DE IMPORTANCIA CON LA VARIABLE DE PESO
ggplot(survey)+
  aes(x= Recursos, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#E69F00","#999999"))



#ESTA LINEA SE UTILIZO PARA QUITAR UNOS VALORES ATIPICOS DE LA VARIABLE 
#HORAS DIARIAS DE ESTUDIO PARA INTERPRETAR MEJOR LA GRAFICA
survey[survey$Horas_.diarias_.estudio > 6 , "Horas_.diarias_.estudio"] <- median(survey$Horas_.diarias_.estudio)


ggplot(survey)+
  aes(x= Horas_.diarias_.estudio, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#E69F00","#999999"))


