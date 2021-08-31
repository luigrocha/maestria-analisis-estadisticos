#=====================================
#LABORATORIO DE DISTRIBUCION BINOMIAL 
#Este archivo contiene 2 partes
# 1.-Demostración de las funciones de  R
# 2.-El trabajo que el estudiante debe desarrollar 
#PARTE1
#####################################################
#Introducción a la funciones de R 
#=======================================
#En R las funciones de distribución binomial son precedidas por un prefijo, 
#seguido por la palabra binom 
#el prefijo:
#p de "probabilidad", esta es la función de distribución acumulativa (CDF)
#q de "cuantil", el inverso CDF, dada una probabilidad obtener el numero de prueba
#d de "densidad", la función de densidad (PDF)
#r de "aleatorio", para crear una variable aleatoria que tiene una  
# distribución especifica

#En la distribución binomial el  experimento se repite n veces 
#de forma independiente, y se trata de calcular la probabilidad 
#de obtener un determinado número de éxitos.
#Recuerde que esta distribución es para variables discretas!!
##############################################
#rbinom
##############################################
# genera el número requerido de valores aleatorios de
#probabilidad dada a partir de una muestra dada.
#en otras palabras rbinom simula la serie de experimentos, retornando
#una serie de resultados 

#Su sintaxis es: rbinom(n, size, prob)
#Donde
#n=número de observaciones que queremos obtener,
#size es número de pruebas que deben hacerse,
#prob (pi) probabilidad de  éxitos de cada prueba (histórico)

#Veamos un ejemplo : 
#Suponga que esta a  cargo del QC  de una fábrica. 
#La fábrica hace 150 ítems  por día. 
#Los ítems defectuosos deben ser reelaborados. 
#Sabemos que hay una tasa de error histórica del 5%. 
#Queremos estimar cuántos ítems necesitaremos arreglar cada día esta semana (laboral).
#Usamos rbinom para generar una serie con estas características
#porque rbinom y no rnorm, bueno el enunciado es que ya sea el ítem o está  
#bien manufacturado o no, esto hace que usemos la distribución binomial 

#cargamos la librerias 
require(ggplot2)
require(reshape2)
rm(list=ls())

rbinom (n=5 , size=150 , prob=.05)
#por que n=5? Es para poner en la misma unidad , si fabricamos 150 por semana queremos 5 muestras en la semana para  tener una diaria 
#cada muestra representaría la cantidad de pruebas con  ítems malos. 
#size y prob son obvios 
#Veamos casos extremos(tendríamos que reparar casi todos)
rbinom (n=5 , size=150 , prob=.9)
#Para n=1 solo una prueba =Distribución de Bernoulli
rbinom (n=1 , size=10 , prob=.5)

#El siguiente ejemplo, sacar una lista de 10000 pruebas de una base de 10 
#con una probabilidad de 50%
#Graficando
#Creamos un data frame con la serie 
bin_df = data.frame(RB=rbinom(n=10000 , size=10 , prob=.5))
# y ploteamos, como histograma 
ggplot (bin_df ,aes(x=RB)) + geom_histogram(binwidth=.1)
#Entendiendo el histograma, lo que vemos es que el número 5 es el más repetido
#con 2500 veces, sin embargo el número 4 se presenta unas 2000 veces  etc..

#La siguiente prueba demuestra que con una gran cantidad de valores discretos
#la distribución binomial  se aproxima  a la distribución normal 

df_10 <- data.frame(Exitos = rbinom(n=10000, size =10 ,prob=.3 ),Size=10)
df_10
df_100 <- data.frame(Exitos = rbinom(n=10000, size =100 ,prob=.3 ),Size=100)
df_100
df_1000 <- data.frame(Exitos = rbinom(n=10000, size =1000 ,prob=.3 ),Size=1000)
df_1000

todo<- rbind(df_10, df_100, df_1000)
head(todo)
tail(todo)
ggplot (data=todo , aes(x=Exitos))+ geom_histogram() +
  facet_wrap(~Size, scales="free")


#1.2 Obtener la probabilidad de éxito de un ensayo
#En este caso cada ensayo solo tiene 2 posibles resultados éxito o fracaso 
#Si la probabilidad de que la prueba sea exitosa es p 
#entonces la probabilidad  de que se obtengan x cantidad de éxitos 
#en un experimento es de acuerdo a la distribución de probabilidad (PDF)
#La Sintaxis es : dbinom(x, size, prob, log = FALSE)
#Donde : 
#x   	vector de cuantiles(indicaría cuantos éxitos que queremos obtener).
#prob     probabilidad de éxito en cada prueba
#size     número de pruebas.
#log      TRUE si los resultados se expresan en log(p) 
#         FALSE si los resultados es solo p 

#Ej: cual es la probabilidad de obtener 3 éxitos de una prueba de 10 cuando la 
#probabilidad de éxito es del 30%

dbinom(x=3 , size=10 , prob=.3)

#Otro ejemplo : Cómo podríamos calcular la probabilidad 
#de que la moneda salga cara en 5 de las 6 pruebas
#cuando la probabilidad individual es del 50%
dbinom(x=5 , size=6 , prob=.5)

#Último ejemplo: 
#Supongamos que hay 12 preguntas de opción múltiple en un examen de la clase
#Cada pregunta tiene cinco respuestas posibles, y solo una de ellas es correcta.
#Encuentre la probabilidad de tener 4 respuestas correctas si un estudiante
#intenta responder todas las preguntas al azar.
dbinom(4, size=12, prob=0.2)




##########################################
#Por otro lado, hay veces que nos piden las probabilidad acumulada. Por ejemplo
#en el ejercicio anterior, si el enunciado cambia a 4 o menos respuestas correctas
#entonces nos estamos refiriendo al CDF.
#La función CDF binomial es pbinom() y su sintaxis es : 
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#Donde: 
#q es el vector de cuantiles o el valor de X debajo del cual acumulamos las probabilidades
#size es el número de pruebas
#prob es la probabilidad de éxito de cada intento 
#lower.tail =TRUE para calcular menor que, FALSE para calcular mayor que
#Sería equivalente a :
dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2) 

pbinom(4, size=12, prob=0.2) 
#Vamos a graficar, melt es una función que transpone columnas a filas, la necesitamos para poder graficar
df1<- data.frame(cero=dbinom(0, size=12, prob=.2),
                 uno=dbinom(1, size=12, prob=.2),
                 dos=dbinom(2, size=12, prob=0.2),
                 tres=dbinom(3, size=12, prob=0.2),
                 cuatro=dbinom(4, size=12, prob=0.2), 
                 cinco=dbinom(5, size=12, prob=0.2))

dens_examen <-melt(df1, value.name="Prob", variable.name="Cantidad")


ggplot(data=dens_examen,  aes(x=Cantidad, y=Prob)) +
  geom_point()+ 
  geom_vline(xintercept=5, color="red")

#El CDF estaría sumando las probabilidades individuales (dbinoms) a la izquierda
#de la línea


# ahora si queremos saber cuál es la probabilidad de que 3 o menos resultados
#sean exitosos utilizaremos esta forma
pbinom(q=3 , size=12 , prob=.2)



#otro ejemplo: 
#Supongamos que los ítems  producidos en ACME tienen una probabilidad de 0.005 
#de ser defectuosos. 
#Supongamos que los ítems se envían en cajas de cartón con 25 
#ACME fabrica 1500 items diarios. 
#¿Cuál es la probabilidad de que una caja de cartón elegida al azar contenga 
#exactamente un item  defectuoso?
#En este caso los 1500 items no vienen al caso, porque está revisando por caja
dbinom(x=1, size=25, prob=0.005)
#El estudiante podrá jugar con estos valores 


#grafiquemos cual es la probabilidad de obtener  8 caras en 10 lanzamientos

df <- data.frame(X=1:10)
df$den <- dbinom(df$X , size=10 , prob=.5)
ggplot (data=df, aes(x=X , y=den))+geom_line() +
  xlab("Num caras")+ ylab("Probabilidad")+
  geom_vline(xintercept=8)



##1.3 CDF  Usaremos la función pbinom cuando el enunciado del problema habla de 
# mayor que, o menor que o entre determinados valores
#Para esto usaremos la función pbinom
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#Donde : 
#q          vector de cuantiles.
#prob       probabilidad de éxito en cada prueba
#size       número de pruebas.
#lower.tail TRUE (default), probabilidad de  P [ X <= x]  ,   FALSE P [ X >x ] 

#Ejemplo: queremos saber cuál es la probabilidad de que 3 o menos resultados
#sean exitosos en 10 ensayos cuya probabilidad es 30% 
pbinom(q=3 , size=10 , prob=.3)

#Ejemplo: El gerente de seguridad de la empresa está preocupado por la 
#cantidad de personal que no porta identificación.
#En general solo el 20% de los empleados porta identificación
#Le pide a usted que calcule cual es la probabilidad de en una muestra de 10
# empleados por lo menos 4 empleados no porten identificación

pbinom(4 , size=10, prob=.2, lower.tail=FALSE)

#Otro ejemplo: Cual es la probabilidad de obtener 26 caras o menos en 
#50 lanzamientos de la moneda
pbinom(26, size=50,prob=.5)

#Y cuál es la probabilidad de obtener más de 26 caras
pbinom(26, size=50,prob=.5, lower.tail=FALSE)

####################################################
#1.4 qbinom 
#Esta función calcula al revés, cual es la cantidad de pruebas que tengo 
#que hacer para obtener una probabilidad de q, cuando cada prueba tiene 
#una probabilidad de x
#Su sintaxis es : 
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)

#Cual es la probabilidad de 5 éxitos o menos de una base de 10 con 20% 
pbinom(5 , size=10, prob=.2, lower.tail=TRUE)
#Cuantos éxitos acumulados tendré de una base de 10 con 20%
qbinom(p=.99 , size=10 ,prob=.2, lower.tail=TRUE)
#Cuál  es la cantidad de pruebas que tengo que hacer
#para obtener una probabilidad de 30% ,teniendo un universo=10 y cuando cada prueba
#tiene una probabilidad  del 40%
qbinom(p=.3 , size=10 ,prob=.4)

###############################
#PARTE 2  TRABAJO DEL ESTUDIANTE
###############################

#Problema 1:
#Usted es un ingeniero electrónico que se dedica al ensamblaje de computadoras
#Requiere comprar chips de memoria. Un fabricante le ofrece chips en 
#paquetes de 12 unidades. Pero tienen una tasa de fallas del 40%
#Cual es la probabilidad de que 5 de los 12 estén defectuosos?



#Problema No2 :
#Marathon hace un encuesta para averiguar si su marca es preferida o no 
#El 60% de los hogares prefiere la marca
#Se hacen 12 encuestas 
#Cual es la probabilidad de que al revisar 7 encuestas sea Marathon la preferida


#Del mismo problema anterior cual es la probabilidad de que más de 
#2 encuestas prefieran la marca



