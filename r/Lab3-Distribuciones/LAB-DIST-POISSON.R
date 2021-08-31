#LABORATORIO DE DISTRIBUCION DE POISSON
#Este  archivo contiene 2 partes
# 1.-Demostración de las funciones de  R
# 2.-El trabajo que el estudiante debe desarrollar 

#Introducción  a las funciones de R 
#=======================================
#En R cada tipo de funciones de precedidas por un prefijo, seguido 
#por la palabra pois 
#
#p para "probabilidad", la función de distribución acumulativa (CDF)
#q para "cuantil", el inverso CDF 
#d para "densidad", la función de densidad (PDF)
#r para "aleatorio", para crear una variable que tiene la distribución especificada


#cargamos la librería 
rm(list=ls())
require(ggplot2)
require(reshape2)
library(dplyr)
#################################################
#1.1 rpois
#################################################
#Indica la probabilidad que una cantidad dada de eventos ocurran
#en un tiempo  definido o en un espacio determinado,
#si estos eventos tienen una media conocida (lambda)
#e independiente de la última vez que ocurrió .
#
#1.1 Generar una serie con la distribución de Poisson
#La función rpois() obtiene la serie 
#la sintaxis es: 
#rpois(n, lambda)
#Donde: 
#n   es el número de valores deseados que deseamos 
#lambda  media histórica por unidad de tiempo 

#Ejemplo : Generar una serie de 10  que representa la ocurrencia de un evento, 
#cuya media por unidad de tiempo ha sido 10
p0 <- rpois(n=10 , lambda=10)
p0
mean(p0)
#Note que la media se aproxima a lambda en Poisson 
#
p0 <- rpois(n=1000 , lambda=5)
df_p0 <- data.frame(X=p0)
head(p0)
mean(p0)
#graficando df_p0
ggplot(data=df_p0, aes(x=X)) + geom_histogram()


#Se dice que la distribución de Poisson se aproxima a la distribución normal 
#cuando el lambda crece, veamos si es verdad

p1 <- rpois(n=10000 , lambda=1)
p2 <- rpois(n=10000 , lambda=2)
p3 <- rpois(n=10000 , lambda=5)
p4 <- rpois(n=10000 , lambda=10)
p5 <- rpois(n=10000 , lambda=20)

pos_df <- data.frame(l1=p1, l2=p2, l5=p3, l10=p4, l20=p5)
head(pos_df)
# Para poder plotear mejor vamos a transponer las columnas en filas 
#la función melt pasa valores que están en columnas a filas en una columna
#llamada valor en este caso 

pos_melted <- melt( data=pos_df , variable.name="lambda", value.name="valor")
head(pos_melted)
class(pos_melted$lambda)
#Si la columna lambda fuera de tipo #character", necesitaríamos convertirla en tipo factor 
#para poder manejar el agrupamiento, para lo cual deberemos hacer una conversión,
#si arriba salió tipo factor estamos OK 


head(pos_melted)
tail(pos_melted)
class(pos_melted$lambda)

ggplot(pos_melted , aes(x=valor))+
  geom_density(aes(group=lambda, color=lambda, fill= lambda , alpha=1/2 ) )+
  scale_color_discrete() +
  scale_fill_discrete()
########################################
#1.2 dpois
#######################################
#Obtiene la probabilidad para un valor x (PDF)
#Su sintaxis es:
#dpois(x, lambda, log = FALSE)
#Donde: 
#x       Vector de cuantiles.
#lambda  es la media histórica
#log     TRUE su se desea obtener log(P[X]) FALSE si se desea obtener P[X]

#Ejemplo: ¿Cuál es la probabilidad de hacer de exactamente 4 ventas en una semana
#si la tasa de ventas promedio es de 3 por semana?
#
dpois(4, lambda=3)
#Ejemplo: durante las noches claras es posible ver 5 meteoritos por hora o 1 cada
#12 minutos. Cuál es la probabilidad de observar 5 meteoritos en 15 minutos
#En este caso habrá que poner en la misma unidad de medida
#nuevo lambda= lambda*nueva_unidad_medida/vieja unidad_medida
nuevo_lambda=5*.25/1
dpois(5, lambda=nuevo_lambda)

#Otro ejemplo: Una compañía constructora es responsable por la construcción de un 
#edificio, al terminar el mismo se han detectado 2 defectos por cada piso. 
#Para el nuevo contrato, la contratante desea poner una multa por defectos
#Le preguntan a usted cual es la probabilidad de tener 3 defectos por piso.

dpois(3,lambda=2)

#Otro ejemplo: 
#Como profesores debemos separar tiempo para la tutorías con el estudiante
#El promedio es de 5.2 consultas por hora. El profesor desea saber cual 
#es la probabilidad de que lleguen 7 consultas por hora 
dpois(7, lambda=5.2)

#Último problema con conversión
#Un fabricante de cable de fibra óptica indica que su producto tiene 3 defectos 
#por cada 100 mts. Usted está encargado de QC y desea revisar si esto es verdad
#Usted tiene una muestra de 50 mts. Le preguntan cuál es la probabilidad de 
#encontrar un defecto en esa muestra.
#Se nos presenta un problema, dpois no acepta valores no enteros, primero  porque es una
#distribución discreta  por lo tanto dpois(0.5  ,lambda=3) #nos dará error.
#Y segundo el problema es que Possion es una distribución para valores enteros no negativos
# fíjese la fórmula, no se puede calcular el factorial no entero!
#Pero lambda si puede fraccionario, por lo tanto ajustaremos el lambda a una
#nueva unidad de medida (50mts)
#nuevo_lambda=lambda*nueva_unidad/vieja_unidad
nuevo_lambda= 3*50/100
dpois(1, lambda=1.5)


#################################################
#1.3 ppois
###############################################
#Por otro lado el CDF se calcula usando la función ppois, su sintaxis es:
#ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
#Donde: 
#q 		    vector de cuantiles.
#lambda	  es la media histórica
#lower.tail=TRUE (default), probabilidad de P[X <= q],FALSE P[X >q]  


#Ejemplo: Si usted recibe llamadas con una tasa constante 2 llamadas por hora,
#Usted va al cine, y se olvida de apagar su celular 
#¿cuál es la probabilidad de que en una película de 1.5 horas, 
#su teléfono timbre ? 

#Otra vez se presenta el problema conversión de unidad de medida 
nuevo_lambda=2*1.5/1
ppois(0 , lambda=nuevo_lambda, lower.tail=FALSE)


#para verlo mejor vamos a hacer un gráfico en el que combinamos CDF y PDF
#para ventas del 0 al 10
options(scipen = 999, digits = 2) #opcion para presentacion de nuemros con decimales o exponenciales
#creamos unos vectores
ventas <- 0:10
densidad <- dpois(x = ventas, lambda = 3)
prob <- ppois(q = ventas, lambda = 3, lower.tail = TRUE)
df <- data.frame(ventas, densidad, prob)
ggplot(df, aes(x = factor(ventas), y = densidad, fill="PDF")) +
  geom_col() +
  geom_text(
    aes(label = round(densidad,2), y = densidad + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "PDF y CDF de Poisson ",
       x = "Ventas (x)",
       y = "Densidad") +
  geom_line(data = df, aes(x = ventas, y = prob , color="CDF"))
#Comparemos con el gráfico 
dpois( 1, lambda = 3) # da la probabilidad exacta de 1 venta
dpois( 4, lambda = 3) # da la probabilidad exacta de 4 ventas
ppois(4, lambda=3 , lower.tail=TRUE) #da la probabilidad de 4 ventas o menos
ppois(4, lambda=3 , lower.tail=FALSE)# da la probabilidad de mas de 4 ventas

#la probabilidad entre  2 a 4 ventas
ppois(4, lambda=3 , lower.tail=TRUE)-ppois(1, lambda=3 , lower.tail=TRUE) 

#Ejemplo : ¿Cuál es la probabilidad de hacer de 1 a 4 ventas en una semana
#si la tasa de ventas promedio es de 3 por semana?
#Esta pidiendo la probabilidad cumulativa por lo tanto tenemos que usar ppois
#como nos pide un rango tenemos que obtener la probabilidad <1 y restarla de la <4

menor_o_igual_que_0 <- ppois(0,lambda=3, lower.tail=TRUE)
menor_o_igual_que_4 <- ppois(4,lambda=3, lower.tail=TRUE)
entre_1_4 <-menor_o_igual_que_4 - menor_o_igual_que_0
sprintf("Probabilidad entre 1 y 4 %2.2f%%", entre_1_4*100)

#################################################
#1.4 qpois
###############################################
#Por otro lado es posible que dada una probabilidad, encontrar es el valor de la
#cantidad de eventos que generarían dicha probabilidad dado un lambda.
#En otras palabras es el inverso del CDF 
#qpois(q, lambda, lower.tail = TRUE, log.p = FALSE)
#Donde: 
#q 		    vector de cuantiles.
#lambda	  es la media histórica
#lower.tail=TRUE (default), probabilidad de P[X <= q],FALSE P[X >q]  
#Veamos usando uno de los problemas anteriores. 


#Ejemplo: Si usted recibe llamadas con una tasa constante 2 llamadas por hora,
#Usted va al cine, y se olvida de apagar su celular 
#¿cuál es la probabilidad de que en una película de 1.5 horas, 
#su teléfono timbre ? 

#Otra vez se presenta el problema conversión de unidad de medida 
nuevo_lambda=2*1.5/1
ppois(0 , lambda=nuevo_lambda, lower.tail=FALSE)
#Resultado= 0.95 o 95%
#Ahora veamos al reves
#Si tengo una probabilidad de .95 que timbre de tiembre el telefono , esto corresponde
#a más de cuantas  llamadas? 
qpois(0.95, lambda=3, lower.tail=FALSE)






#######################################
#PARTE 2  TAREA DEL ESTUDIANTE 
######################################
#Problema 1
#Un puente está calculado para soportar 60 toneladas métricas, 
#Si se considera que el vehículo más pesado es de 5000kg (5 toneladas) el puente 
#puede resistir el paso de 12 vehículos,  El constructor pregunta cuál es la tasa de 
#transito que tiene este puente  le indican 10 vehículos al tiempo. Para estar seguro 
#el constructor pregunta cuál es la probabilidad de que más de 12 vehículos circulen


#Cual es la probabilidad de que pasen exactamente 10 


#Problema 2 
#Un call center tiene contratado 15 operadores, su contrato con el cliente indica que 
#debe atender un promedio de  12 llamadas  por minuto . El gerente del call 
#center le pregunta cuál es la probabilidad de que más de 15  personas llamen
#porque en esa circunstancia ya no puede atender



#Por otro lado como los call centers  cobran por llamada le interesa
#saber cuál es la probabilidad de que 10 o menos llamadas por minuto ,porque en 
#esa circunstancia tiene personal ocioso 









