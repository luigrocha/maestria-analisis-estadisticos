#=====================================
#LABORATORIO DE DISTRIBUCION BINOMIAL 
#Este archivo contiene 2 partes
# 1.-Demostraci?n de las funciones de  R
# 2.-El trabajo que el estudiante debe desarrollar 
#PARTE1
#####################################################
#Introducci?n a la funciones de R 
#=======================================
#En R las funciones de distribuci?n binomial son precedidas por un prefijo, 
#seguido por la palabra binom 
#el prefijo:
#p de "probabilidad", esta es la funci?n de distribuci?n acumulativa (CDF)
#q de "cuantil", el inverso CDF, dada una probabilidad obtener el numero de prueba
#d de "densidad", la funci?n de densidad (PDF)
#r de "aleatorio", para crear una variable aleatoria que tiene una  
# distribuci?n especifica

#En la distribuci?n binomial el  experimento se repite n veces 
#de forma independiente, y se trata de calcular la probabilidad 
#de obtener un determinado n?mero de ?xitos.
#Recuerde que esta distribuci?n es para variables discretas!!
##############################################
#rbinom
##############################################
# genera el n?mero requerido de valores aleatorios de
#probabilidad dada a partir de una muestra dada.
#en otras palabras rbinom simula la serie de experimentos, retornando
#una serie de resultados 

#Su sintaxis es: rbinom(n, size, prob)
#Donde
#n=n?mero de observaciones que queremos obtener,
#size es n?mero de pruebas que deben hacerse,
#prob (pi) probabilidad de  ?xitos de cada prueba (hist?rico)

#Veamos un ejemplo : 
#Suponga que esta a  cargo del QC  de una f?brica. 
#La f?brica hace 150 ?tems  por d?a. 
#Los ?tems defectuosos deben ser reelaborados. 
#Sabemos que hay una tasa de error hist?rica del 5%. 
#Queremos estimar cu?ntos ?tems necesitaremos arreglar cada d?a esta semana (laboral).
#Usamos rbinom para generar una serie con estas caracter?sticas
#porque rbinom y no rnorm, bueno el enunciado es que ya sea el ?tem o est?  
#bien manufacturado o no, esto hace que usemos la distribuci?n binomial 

#cargamos la librerias 
require(ggplot2)
require(reshape2)
rm(list=ls())

rbinom (n=5 , size=150 , prob=.05)
#por que n=5? Es para poner en la misma unidad , si fabricamos 150 por semana queremos 5 muestras en la semana para  tener una diaria 
#cada muestra representar?a la cantidad de pruebas con  ?tems malos. 
#size y prob son obvios 
#Veamos casos extremos(tendr?amos que reparar casi todos)
rbinom (n=5 , size=150 , prob=.9)
#Para n=1 solo una prueba =Distribuci?n de Bernoulli
rbinom (n=1 , size=10 , prob=.5)

#El siguiente ejemplo, sacar una lista de 10000 pruebas de una base de 10 
#con una probabilidad de 50%
#Graficando
#Creamos un data frame con la serie 
bin_df = data.frame(RB=rbinom(n=10000 , size=10 , prob=.5))
# y ploteamos, como histograma 
ggplot (bin_df ,aes(x=RB)) + geom_histogram(binwidth=.1)
#Entendiendo el histograma, lo que vemos es que el n?mero 5 es el m?s repetido
#con 2500 veces, sin embargo el n?mero 4 se presenta unas 2000 veces  etc..

#La siguiente prueba demuestra que con una gran cantidad de valores discretos
#la distribuci?n binomial  se aproxima  a la distribuci?n normal 

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


#1.2 Obtener la probabilidad de ?xito de un ensayo
#En este caso cada ensayo solo tiene 2 posibles resultados ?xito o fracaso 
#Si la probabilidad de que la prueba sea exitosa es p 
#entonces la probabilidad  de que se obtengan x cantidad de ?xitos 
#en un experimento es de acuerdo a la distribuci?n de probabilidad (PDF)
#La Sintaxis es : dbinom(x, size, prob, log = FALSE)
#Donde : 
#x   	vector de cuantiles(indicar?a cuantos ?xitos que queremos obtener).
#prob     probabilidad de ?xito en cada prueba
#size     n?mero de pruebas.
#log      TRUE si los resultados se expresan en log(p) 
#         FALSE si los resultados es solo p 

#Ej: cual es la probabilidad de obtener 3 ?xitos de una prueba de 10 cuando la 
#probabilidad de ?xito es del 30%

dbinom(x=3 , size=10 , prob=.3)

#Otro ejemplo : C?mo podr?amos calcular la probabilidad 
#de que la moneda salga cara en 5 de las 6 pruebas
#cuando la probabilidad individual es del 50%
dbinom(x=5 , size=6 , prob=.5)

#?ltimo ejemplo: 
#Supongamos que hay 12 preguntas de opci?n m?ltiple en un examen de la clase
#Cada pregunta tiene cinco respuestas posibles, y solo una de ellas es correcta.
#Encuentre la probabilidad de tener 4 respuestas correctas si un estudiante
#intenta responder todas las preguntas al azar.
dbinom(4, size=12, prob=0.2)
##########################################
#Por otro lado, hay veces que nos piden las probabilidad acumulada. Por ejemplo
#en el ejercicio anterior, si el enunciado cambia a 4 o menos respuestas correctas
#entonces nos estamos refiriendo al CDF.
#La funci?n CDF binomial es pbinom() y su sintaxis es : 
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#Donde: 
#q es el vector de cuantiles o el valor de X debajo del cual acumulamos las probabilidades
#size es el n?mero de pruebas
#prob es la probabilidad de ?xito de cada intento 
#lower.tail =TRUE para calcular menor que, FALSE para calcular mayor que
#Ser?a equivalente a :
dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2) 

pbinom(4, size=12, prob=0.2) 
#Vamos a graficar, melt es una funci?n que transpone columnas a filas, la necesitamos para poder graficar
df1<- data.frame(cero=dbinom(0, size=12, prob=.2),
                 uno=dbinom(1, size=12, prob=.2),
                 dos=dbinom(2, size=12, prob=0.2),
                 tres=dbinom(3, size=12, prob=0.2),
                 cuatro=dbinom(4, size=12, prob=0.2), 
                 cinco=dbinom(5, size=12, prob=0.2))
df1
dens_examen <-melt(df1, value.name="Prob", variable.name="Cantidad")


ggplot(data=dens_examen,  aes(x=Cantidad, y=Prob)) +
  geom_point()+ 
  geom_vline(xintercept=5, color="red")

#El CDF estar?a sumando las probabilidades individuales (dbinoms) a la izquierda
#de la l?nea


# ahora si queremos saber cu?l es la probabilidad de que 3 o menos resultados
#sean exitosos utilizaremos esta forma
pbinom(q=3 , size=12 , prob=.2)

#otro ejemplo: 
#Supongamos que los ?tems  producidos en ACME tienen una probabilidad de 0.005 
#de ser defectuosos. 
#Supongamos que los ?tems se env?an en cajas de cart?n con 25 
#ACME fabrica 1500 items diarios. 
#?Cu?l es la probabilidad de que una caja de cart?n elegida al azar contenga 
#exactamente un item  defectuoso?
#En este caso los 1500 items no vienen al caso, porque est? revisando por caja
dbinom(x=1, size=25, prob=0.005)
#El estudiante podr? jugar con estos valores 


#grafiquemos cual es la probabilidad de obtener  8 caras en 10 lanzamientos

df <- data.frame(X=1:10)
df
df$den <- dbinom(df$X , size=10 , prob=.5)
ggplot (data=df, aes(x=X , y=den))+geom_line() +
  xlab("Num caras")+ ylab("Probabilidad")+
  geom_vline(xintercept=8)



##1.3 CDF  Usaremos la funci?n pbinom cuando el enunciado del problema habla de 
# mayor que, o menor que o entre determinados valores
#Para esto usaremos la funci?n pbinom
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#Donde : 
#q          vector de cuantiles.
#prob       probabilidad de ?xito en cada prueba
#size       n?mero de pruebas.
#lower.tail TRUE (default), probabilidad de  P [ X <= x]  ,   FALSE P [ X >x ] 

#Ejemplo: queremos saber cu?l es la probabilidad de que 3 o menos resultados
#sean exitosos en 10 ensayos cuya probabilidad es 30% 
pbinom(q=3 , size=10 , prob=.3)

#Ejemplo: El gerente de seguridad de la empresa est? preocupado por la 
#cantidad de personal que no porta identificaci?n.
#En general solo el 20% de los empleados porta identificaci?n
#Le pide a usted que calcule cual es la probabilidad de en una muestra de 10
# empleados por lo menos 4 empleados no porten identificaci?n

pbinom(4 , size=10, prob=.2, lower.tail=FALSE)

#Otro ejemplo: Cual es la probabilidad de obtener 26 caras o menos en 
#50 lanzamientos de la moneda
pbinom(26, size=50,prob=.5)

#Y cual es la probabilidad de obtener mas de 26 caras
pbinom(26, size=50,prob=.5, lower.tail=FALSE)

####################################################
#1.4 qbinom 
#Esta funcion calcula al reves, cual es la cantidad de pruebas que tengo 
#que hacer para obtener una probabilidad de q, cuando cada prueba tiene 
#una probabilidad de x
#Su sintaxis es : 
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)

#Cual es la probabilidad de 5 exitos o menos de una base de 10 con 20% 
pbinom(5 , size=10, prob=.2, lower.tail=TRUE)
#Cuantos exitos acumulados tendr? de una base de 10 con 20%
qbinom(p=.99 , size=10 ,prob=.2, lower.tail=TRUE)
#Cu?l  es la cantidad de pruebas que tengo que hacer
#para obtener una probabilidad de 30% ,teniendo un universo=10 y cuando cada prueba
#tiene una probabilidad  del 40%
qbinom(p=.3 , size=10 ,prob=.4)

###############################
#PARTE 2  TRABAJO DEL ESTUDIANTE
###############################

#Problema 1:
#Usted es un ingeniero electronico que se dedica al ensamblaje de computadoras
#Requiere comprar chips de memoria. Un fabricante le ofrece chips en 
#paquetes de 12 unidades. Pero tienen una tasa de fallas del 40%
#Cual es la probabilidad de que 5 de los 12 est?n defectuosos?
pbinom(5, size = 12, prob = .4)
#Problema No2 :
#Marathon hace un encuesta para averiguar si su marca es preferida o no 
#El 60% de los hogares prefiere la marca
#Se hacen 12 encuestas 
#Cual es la probabilidad de que al revisar 7 encuestas sea Marathon la preferida
pbinom(7,size=12, p=.6)
#Del mismo problema anterior cual es la probabilidad de que m?s de 
#2 encuestas prefieran la marca
pbinom(7 , size=12, prob=.6, lower.tail=FALSE)

