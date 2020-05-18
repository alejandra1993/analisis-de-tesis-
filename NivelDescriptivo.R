### Introduccion.
En el trabajo que a continuacion se muestra se estara analizando la informacion que se recolecto con la encuesta aplicada a los
estudiantes de la Facultad de Ingenieria en Sistemas, esta con el fin de analizar la problematica de sobrepoblacion que se presenta 
en esta facultad y como los estudiantes se desenvuelven mostrando un buen o mal rendimientio academico, para los docentes y alumnos es
vital conocer que problemas conlleva esta situacion y de igual foema conocer que estrategias o tecnologias implementar para ayudar a aliviar
esta circunstancia.

### Pasos preliminares.

Descargaremos nuestro archivo .csv de la pagina de google forms para darle el siguiente tratamiento.

library(dplyr)

### Leemos nuestro archivo deonde tenemos nuestra encuesta.

Tesis <- read.csv("C:/Users/Junior/Desktop/Tesis.csv", encoding="UTF-8")

### El archivo nombre_columnas es donde tenemos nuestras preguntas asociadas a una variable.

nombre_columnas <- read.csv("nombre_columnas.csv", encoding="UTF8", sep=";")


my.names <- names(Tesis)

### Con las siguientes lineas de codigo aliminamos las preguntas que consideramos redundantes o que no aportan informacion de interes.

my.names <- my.names[!(my.names %in% c("X"))]
my.names <- my.names[!(my.names %in% c("X.Cuantas.clases.matriculaste.este.periodo."))]
my.names <- my.names[!(my.names %in% c("X.En.que.año..académico.te.encuentras."))]
my.names <- my.names[!(my.names %in% c("X.Consideras.que.la.UNAH.debe.proporcionar.mas.recursos.materiales.a.disposición.de.la.facultad.de.ingeniería.en.sistemas."))]
my.names <- my.names[!(my.names %in% c("X.Considera.que.los.recursos.económicos.que.invierte.la.UNAH.en.la.facultad.de.ingeniería.en.sistemas.deben.ser.mayores."))]
my.names <- my.names[!(my.names %in% c("X.Consideras.que.la.UNAH.debe.contratar.mas.docentes.para.la.facultad.de.ingeniería.en.sistemas."))]

my.names

### actualizamos Tesis para que tenga solamente las columnas con las que debe de trabajar 
Tesis <- Tesis[,my.names]

### Vector que servira para remplazar los names del dataset de Tesis
nombre_columnas$alias <- as.character(nombre_columnas$alias)

### Cambiamos nombres en el DF original
names(Tesis) <- nombre_columnas$alias
names(Tesis)
head(Tesis)

### Creamos un nuevo documento .cvs con los cambios realizados.
write.csv(Tesis,"Tesis_cleaned", row.names = F)
 

### Leemos nuestro nuevo documento.

Tesis <- read.csv("C:/Users/Junior/Desktop/Tesis_cleaned.csv", encoding="UTF-8")

### Nivel descriptivo, importante importar la libreria dplyr.

### Realizaremos una transformacion de la variable Mejorar_indice para tener una mejor lectura de datos

df_mejorar <- as.data.frame(prop.table(table(Tesis$Mejorar_..ndice))) %>% arrange(-Freq)
df_mejorar[df_mejorar$Var1 %in% c("Mejor Planificaci?n", "Flexibilidad de horarios", "Mejor Planificaci?n;Flexibilidad de horarios", "Mejor planificaci?n"), "Transformacion"] <- "Organizacion"
df_mejorar[df_mejorar$Var1 %in% c("Otros", "Flexibilidad de horarios;Otros", "Mejor Planificaci?n;Otros", "Mejor Planificaci?n;Flexibilidad de horarios;Otros"), "Transformacion"] <- "otras opciones"

### Al dataframe df_mejorar le asignamos nuestras nuevas columnas con las trasnformaciones realizadas.

df_mejorar <- df_mejorar %>% select(Var1, Transformacion)

### Unimos nuestro nuevo daframe df_mejorar a nuestra encuesta.

Tesis <- left_join(Tesis, df_mejorar, by=c("Mejorar_..ndice"="Var1"))

### Borramos de nuestra encuesta la variable sobre la cual realizamos la transformacion esto ya que depuramos la misma con todo el proceso realizado.

Tesis <- Tesis[,!(names(Tesis) %in% c("Mejorar_..ndice"))]

### Si corremos esta linea de comandos podremos ver que organizacion posee una mayoria de 107 registros que la aopoyan y otras opciones tiene un total de 23 registros que la apoyan.

table(Tesis$Transformacion)

### Nivel descriptivo, resumen estadistico

### Para obtener nuestro resumen estadistico vamos a procesar las proporciones de cada una de nuetras variables teniendo en cuenta las transformaciones que ya hemos realizado, 
### en este caso se realizo una por una de la siguiente manera.

as.data.frame(prop.table(table(Tesis$Edad))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Genero))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Procedencia))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Traslado_.UNAH))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Computadora_.permanente))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Conexi.f3.n_.permanente))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Calidad_.conexi.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estudio_.secundaria))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Cantidad_.carreras))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Rango_.acad.e9.mico))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Transformacion))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Exelencia_academica))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Reprobaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Rango_.promedio_.clases))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Flexibilidad_.horarios))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Horas_.libres))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estimaci.f3.n_.horas_.libres))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Lista_.espera))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Mas_.cupos))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Preferencia_.estudio))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Horas_.diarias_.estudio))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Disciplina))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Autodidacta))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Demora))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Entorno))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Motivo_.estudio))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Seguimiento))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Deserci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Sobrepoblaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Aumento._de._la.poblaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Sobrecarga_.laboral))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Planificaci.f3.n))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estrategias_.educativas))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Recursos))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Plataforma))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Frecuencia_.de_.uso))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Aumento_.de_.uso))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Uso_.lms))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$lms_.utiliza))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Estimaci.f3.n_.lms))) %>% arrange(-Freq)
as.data.frame(prop.table(table(Tesis$Cambio._LMS))) %>% arrange(-Freq)


###Analisis de diagramas y valores atipivos encontrados en el nivel descriptivo.

### Variable edad: Como se podra analizar con las siguientes lineas de comandos encontramos valores atipocos e la variable Edad
### se generaron los diagramas correspondientes para concluir acerca de esta situacion.  

df_edad <- as.data.frame(prop.table(table(Tesis$Edad))) %>% arrange(-Freq)
boxplot(df_edad$Freq)

### Variable Mejorar_indice: Teniendo en cuenta la transformacion realizada anterior mente para esta variable en las siguientes
### lineas de codigo veremos los diagramas que nos ayudaron a analizar y tratar esta variable.

df_mejorar <- as.data.frame(prop.table(table(Tesis$Transformacion))) %>% arrange(-Freq)
boxplot(df_mejorar$Freq)
hist(df_mejorar$Freq)
qqnorm(df_mejorar$Freq)

### Variable Rango_.promedio_.clases: esta variable se analizo ya que se penso podria tener valores atipicos, sin embargo, 
### los diagramas generados a continuacion nos ayudaron a confirmar la validez de la data que almacena esta variable.

df_Rango <- as.data.frame(prop.table(table(Tesis$Rango_.promedio_.clases))) %>% arrange(-Freq)
boxplot(df_Rango$Freq)
hist(df_Rango$Freq)
qqnorm(df_Rango$Freq)

### Variable Horas_diarias_estudio: Esta variable se estudio ya que al ser una variable abierta al encuestado para ser llenada
### se penso que posible mente pdroa mostrar errores en los valores que se llenaron en ella, pero los diagramas muestran todo lo
### contrario ya que verifican que la informacion es correcta.

df_Horas <- as.data.frame(prop.table(table(Tesis$Horas_.diarias_.estudio))) %>% arrange(-Freq)
boxplot(df_Horas$Freq)
hist(df_Horas$Freq)
qqnorm(df_Horas$Freq)

### Variable Disciplina: Debido a que en esta variable se preguntaba con que frecuiencia se cumplian las obligaciones del estudiante
### y se presento un valor dificl de creer ya que un procentaje de los encuestados muy bajo no cumple con sus obligaciones,
### sin embargo los diagramas nos ayudaron a revisar esta informacion y concluir que la misma es correcta.

df_Disciplina <- as.data.frame(prop.table(table(Tesis$Disciplina))) %>% arrange(-Freq)
boxplot(df_Disciplina$Freq)
hist(df_Disciplina$Freq)
qqnorm(df_Disciplina$Freq)

### Variable Planificacion: Debido a las distintas opciones que presenta esta variable en forma de rango se nos hizo interesante
### de estudiar la misma ya que podrian existir valores atipicos, pero los diagramas confirman que los registros son correctos.

df_Planificaion <- as.data.frame(prop.table(table(Tesis$Planificaci.n))) %>% arrange(-Freq)
boxplot(df_Planificaion$Freq)
hist(df_Planificaion$Freq)
qqnorm(df_Planificaion$Freq)

### Variable Estrategias educativas: Debido a las distintas opciones que presenta esta variable en forma de rango se nos hizo interesante
### de estudiar la misma ya que podrian existir valores atipicos, pero los diagramas confirman que los registros son correctos.

df_Estrategias <- as.data.frame(prop.table(table(Tesis$Estrategias_.educativas))) %>% arrange(-Freq)
boxplot(df_Estrategias$Freq)
hist(df_Estrategias$Freq)
qqnorm(df_Estrategias$Freq)

### Variable LMS_utilizado: Con respecto a esta variable podemos decir que en las proporciones o distribucion
### mostraba algunos datos sospechos de que fuesen incorrectos, en efecto al analizar los diagramas nos dimos cuenta de
### que la variable posee registros que son atipicos, para solucionar este problema se decidio reformular la pregunta
### en la seccion de limpieza de daros veremos el proceso que se realizo, con los diagramas que se muestran a continuacion se apreciaran
### los valores atipicos en cuestion.

df_Uso <- as.data.frame(prop.table(table(Tesis$lms_.utiliza))) %>% arrange(-Freq)
boxplot(df_Uso$Freq)
hist(df_Uso$Freq)
qqnorm(df_Uso$Freq)

### Variable Cambio_LMS: esta variable nos parecio interesante de corroborar los registros que posee ya que es una de las 
### que sustenta o aporta informacion a nuestra solucion tecnologica, los diagramas muestran que la informacion que almacena
### es correcta.

df_Cambiolms <- as.data.frame(prop.table(table(Tesis$Cambio._LMS))) %>% arrange(-Freq)
boxplot(df_Cambiolms$Freq)
hist(df_Cambiolms$Freq)
qqnorm(df_Cambiolms$Freq)



