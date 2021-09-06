#LABORATORIO DE DISTRIBUCION NORMAL
#Este  archivo contiene 2 partes
# 1.-Demostración de las funciones en R
# 2.-El trabajo que el estudiante debe desarrollar 
#Introducción  a las funciones de R 
#=======================================
#En R las funciones de distribución normal están precedidas por un prefijo, 
#seguido por la palabra norm 
#el prefijo:
#p de "probabilidad", esta es la función de distribución acumulativa (CDF)
#q de "cuantil", el inverso CDF, dada una probabilidad obtener Z
#d de "densidad", la función de densidad (PDF)
#r de "aleatorio", para crear una variable aleatoria que tiene una  
# distribución especifica
#cargamos la librería 
rm(list=ls())
require(ggplot2)
require(e1071)
#1.1 :Obtener de una distribución normal y   diagramarla
#En la practica estos son los datos que usted recolecta del negocio
#Para obtener una serie de datos que tienen una distribución normal usaremos
#la función rnorm, su sintaxis es :rnorm(n, mean = 0, sd = 1)
#Donde : 
#n 			número de observaciones. 
#mean		media del vector, por defecto 0
#sd			vector de desviación  std. por defecto 1
#lower.tail   TRUE (default), probabilidad de P [ X <= x], FALSE P[ X >x ] 
serie1 <-  rnorm(10)
serie1
#Verifiquemos las características de la serie
mean(serie1) #debe ser cercanos a 0 para series grandes
sd(serie1) #deberia ser cercano a 1 para series grandes
var(serie1)#deberia ser cercano a 1 para series grandes
print(skewness(serie1))
print(kurtosis(serie1))
#Note que los estadisticos no son los esperados , pero esto se debe a que 
#solo tenemos 10 elementos .Veamos con más elementos.
#la siguiente  serie tiene  una media de 20
serie2 <- rnorm(100, 20 ) 
head(serie2)
#La siguiente serie tiene una media de 10 y sd=12
serie3 <- rnorm(1000 , mean=10, sd=12)
head(serie3)
print(skewness(serie3))
print(kurtosis(serie3))
#Note que el skew y la kurtosis es el esperado 

#Lo obtenido son solamente series, ahora queremos ver su densidad de 
#distribución.
#la densidad representa la probabilidad de sacar aleatoriamente este valor 
#(esta es la función PDF)
den_serie1<-dnorm(serie1)
den_serie2<-dnorm(serie2)
den_serie3<-dnorm(serie3)
head(serie3)
head(den_serie3)
#Primero vamos a graficar estas series, no se ve muy bien con pocos números 
#primero creamos el data frame con los valores 
df1 <- data.frame(x=serie1,y=den_serie1)
df1
#Una forma fácil de visualizar un data.frame es mediante la función plot()
#Esta función no tienen muchas opciones ,pero hace su trabajo 
par(mar=c(3,3,1,1)) #definimos unos márgenes para el plot 
#plotemos la densidad 
plot(df1, type="h")
#plotemos como histograma
ggplot (df1, aes(x=x )) +
  geom_histogram(binwidth = 1) + 
  labs(x="numero ", y= "cuenta")
#Todavía no se ve muy bien el histograma pero es porque se está agrupando
#los valores de X en columnas muy amplias.
#El estudiante podrá jugar un poco con el parámetro 
#binwidth  y subiendo la cantidad de elementos  en la muestra

#ahora ploteamos el valor 
ggplot (df1, aes(x=x , y=y)) +
  geom_point() + 
  labs(x="numero ", y= "densidad")


df2 <- data.frame(x=serie2)
ggplot (df2, aes(serie2 )) +
  geom_histogram(binwidth = 1) + 
  labs(x="numero ", y= "cuenta")

df3 <- data.frame(x=serie3)
ggplot (df3, aes(serie3 )) +
  geom_histogram(binwidth = 1) + 
  labs(x="numero ", y= "cuenta")

serie4 <-rnorm(1000 , mean=50, sd=10)
df4 <- data.frame(X=serie4 , Y=dnorm(serie4, mean=50 ,sd=10))
#grafiquemos  
g<- ggplot(data=df4 , aes(x=X, y=Y))+
  geom_point()+
  geom_line()
g

#Probar skewness 
serie4 <-rnorm(1000 , mean=50, sd=10)
print(skewness(serie4))
print(kurtosis(serie4))
#A esta serie que es normal le vmos a incluir unos valores que tienen una distribucion 
#plana (random)  y veamos su resultado de kurtosis/skewness 
colasuperior <- runif(150 , min=70,max=100)
serie4 <- append(serie4,colasuperior)
print(skewness(serie4))
print(kurtosis(serie4))

df4 <- data.frame(X=serie4 , Y=dnorm(serie4,mean=50, sd=10))
#grafiquemos  
g<- ggplot(data=df4 , aes(x=X, y=Y))+
  geom_point()+
  geom_line()
g

#En el gráfico podemos observar el efecto del Skew y Kurtosis 



#1.2 Como conseguir cuál es la probabilidad de obtener un número aleatorio x, 
# para esto usaremos la función PDF que es  dnorm()
# su sintaxis es 
#dnorm(x, mean = 0, sd = 1, log = FALSE)


#Ejemplo: Tratamos de encontrar cual es la probabilidad para un
#valor dado x 
#######################################################
serie4 <-rnorm(1000 , mean=50, sd=10)
df4 <- data.frame(X=serie4 , Y=dnorm(serie4, mean=50 ,sd=10))
x=40.5
prob_valor <- dnorm(x, mean=50, sd=10)
sprintf("La probabilidad del número %1.2f  es %1.7f  ",x, prob_valor)
g<- ggplot(data=df4 , aes(x=X, y=Y))+
  geom_point()+
  geom_vline(xintercept=x, color="red")
g

# Recuerde que la probabilidad de obtener un valor específico en una variable
# continua es  normalamente baja .El estudiante podrá cambiar 
#el valor de "x" y revisar su salida y gráfico

#Por otro lado, si queremos saber cuál es la probabilidad de obtener un 
#número aleatorio que sea igual o menor(o mayor que) un valor dado, usáremos
#la función cdf. Para distribución normal la función es :  
#pnorm(q, mean = 0, sd = 1, lower.tail=TRUE)

#Ejemplo: Cual es la probabilidad que obtener un número menor que el -1 
# en una la distribución normal  con media 50 y sd=10
pnorm(-1, mean=50 , sd=10)
#Otro ejemplo: generemos un vector aleatorio de 5 elementos entre 0 y 50  
vect1 <- runif(5, 0,50)
vect1 <- sort(vect1, decreasing=FALSE)
#Veamos la probabilidad de obtener un número menor o igual a cada elemento
#del vector

pn_serie1 <- pnorm(vect1, mean=50, sd=10)

head(vect1)
head(pn_serie1)
#Tome en cuenta que por  defecto está usando lower.tail=TRUE, 
#esto quiere decir que está
#calculando el cdf entre -inf al numero del vect1 

vect<- c(-5,50,100)
prob_vector <-pnorm(vect, mean=50 , sd=10)
head(vect)
prob_vector
#salen esto valores : 1.898956e-08 5.000000e-01 9.999997e-01
#quiere decir que la probabilidad que sea menor que -5 es 1.8 e-7
#La probabilidad que menor que 50 es .5 lo cual es correcto porque es la media 
#La probabilidad que sea menor que 100 es .999  es correcto porque es prácticamente toda el 
#area debajo de la curva que es =1 o 100%
#
#Por otro lado:
#Comúnmente nos piden la probabilidad de un rango, ej: que esté entre 10 y 15
#Para esto usaremos la función pnorm, pero hay que recordar que pnorm nos da 
#el CDF (de -inf al numero) por lo tanto se debe proceder con la resta de valores

# Ejemplo asumiendo una distribución normal de media es 0 y  sd = 1
# cual es la probabilidad acumulada entre -3 y -2, lo que hacemos es restar las
#probabilidades de que sea menor que -2  de la probabilidad que sea menor que -30 
#esto representa el área debajo de la curva de estos números
pnorm(-3)- pnorm(-2)
pnorm(-1 ) - pnorm(0)


#para visualizar mejor podemos plotear esta ultima de la siguiente forma, 
#creamos un vector con estas características
V2 = rnorm(10000, mean = 50, sd = 25)
#obtenemos su densidad 
dens <- density(V2)
dd <- with(dens,data.frame(x,y))
dd
qplot(x,y,data=dd,geom="line")+
  geom_ribbon(data=subset(dd,x> 25 & x< 30),aes(ymax=y),ymin=0,
              fill="red",colour=NA,alpha=0.5)
pnorm(30,mean = 50, sd = 25) -pnorm(25,mean = 50, sd = 25)

#El estudiante podrá cambiar el valor de área y el parámetro
#lower.tail= false , esto nos dará el CDF desde el número dado
#hasta + infinito 


#Ejemplo:Cual es la probabilidad de sacar un número mayor  a 60 
#de una serie normal cuya media es 50 y desviación 10 ? 

pnorm(60, mean=50, sd=10 , lower.tail=FALSE)


#1.3 Por otro lado qnorm()  toma  una probabilidad acumulada y nos devuelve el 
#valor debajo del cual se obtendría esta probabilidad.
#qnorm es el contrario de pnorm 
#Obtenemos cual es la probabilidad acumulada del valor -1, para una media 0
#y desvoacion 1
pnorm(-1, mean=0,  sd=1)
#Ahora preguntamos cual es el valor debajo del cual obtenemos el 15.86%
qnorm(.1586)
#En otras  palabras si tengo una probabilidad acumulada del 15.86%  
#cual es el valor de x que nos daría este CDF
#Por ultimo
#qnorm nos ayuda también a encontrar los valores z
sprintf("estadístico z para prob. %1.4f es %1.4f",.9 , qnorm(.9))
#tome en cuenta que está obteniendo el valor de Z cuyo cdf =.9 
#es decir desde -inf a un valor x, se hallan el 90% de las observaciones 
#lo mismo podemos hacer para  95%, 97.5% y 99%
sprintf("estadístico z para prob %1.4f es %1.4f",.95 , qnorm(.95))
sprintf("estadístico z para prob %1.4f es %1.4f",.975 , qnorm(.975))
sprintf("estadístico z para prob %1.4f es %1.4f",.99 , qnorm(.99))

sprintf("estadístico z para prob %1.4f es %1.4f",.05 , qnorm(.05))
sprintf("estadístico z para prob %1.4f es %1.4f",.025 , qnorm(.025))
sprintf("estadístico z para prob %1.4f es %1.4f",.005, qnorm(.0125))
#Note que el valor Z que nos esta dando es desde - inf al argumento de
#qnorm
#PARTE 2  TAREA DEL ESTUDIANTE
#=====================================
#Problema1: El número de quejas por día  de los clientes de la compañía
#Turra CA es la siguiente: 
# 5 24 12 25 43 43 34 21 14 36 30 16 42 10 42 29  5 19 17  8
#Ingrese estos valores a un vector(podría reutilizar serie1)  y calcule 
#la media, sd, y var, plotee el histograma  y la densidad de la  serie
#usando las mismas líneas de la sentencia ggplot usadas para la serie1 
#Indique : Que tipo de distribución tiene
s1 <- c(5, 24, 12, 25, 43, 43, 34, 21, 14, 36, 30, 16, 42, 10, 42, 29,  5, 19, 17,  8)
s1
#Verifiquemos las características de la serie
mean(s1) #debe ser cercanos a 0 para series grandes
sd(s1) #deberia ser cercano a 1 para series grandes
var(s1)#deberia ser cercano a 1 para series grandes
max(s1)
print(skewness(s1))
print(kurtosis(s1))
den_s1<-dnorm(s1)
head(s1)
dfs1 <- data.frame(x=s1,y=den_s1)
par(mar=c(3,3,2,2)) #definimos unos márgenes para el plot 
#plotemos la densidad 
plot(dfs1$x, type="h")
#plotemos como histograma
hist(dfs1$x,
     main="histograma s1",
     xlab="serie 1",
     xlim=c(0,50),
     col="blue",
     freq=FALSE
)
ggplot(dfs1, aes(x=x))+ geom_density()
#Problema2 : La compañía Turra CA desea bajar el tiempo de atención 
#a clientes, El gerente desea que la media de atención sea de 10 días
#usted ha medido el proceso y sabe que tienen una distribución normal 
#cuya media es de 17.5 días y varianza es de 350. 
#Le  preguntan cual es la probabilidad de los casos estén por debajo de
#10 días?
#u=17.5
#ds=sqrt(varp2)
#p(x<10)
varp2 <- 350
sdp2 <- sqrt(varp2)
sdp2
prob_p2a <-1 -pnorm(10, mean=17.5 , sd=sdp2, lower.tail=FALSE)
prob_p2a
#grafico
seriep2 <-rnorm(1000 , mean=17.5, sd=sdp2)
dfp2 <- data.frame(X=seriep2 , Y=dnorm(seriep2, mean=17.5 ,sd=sdp2))
x=10
prob_valor <- dnorm(x, mean=17.5, sd=sdp2)
sprintf("La probabilidad del número %1.2f  es %1.7f  ",x, prob_valor)
g<- ggplot(data=dfp2 , aes(x=X, y=Y))+
  geom_point()+
  geom_vline(xintercept=x, color="red")
g

#Cual es la probabilidad de que los casos estén entre 5 a 10 días
#p(10>x<5)
prob_p2b <- pnorm(5, mean=17.5 , sd=sdp2, lower.tail=FALSE) - pnorm(10, mean=17.5 , sd=sdp2, lower.tail=FALSE)
prob_p2b
#grafico
seriep2 <-rnorm(1000 , mean=17.5, sd=sdp2)
dfp2 <- data.frame(X=seriep2 , Y=dnorm(seriep2, mean=17.5 ,sd=sdp2))
x1=5
x2=10
sprintf("La probabilidad del número entre %1.2f y %1.2f  es %1.7f  ",x1,x2, prob_p2b)
g<- ggplot(data=dfp2 , aes(x=X, y=Y))+
  geom_point()+
  geom_vline(xintercept=x1, color="red")+
geom_vline(xintercept=x2, color="red")
g

#Pregunta: Suponga que los pesos cajas producidos en Acme  
#tienen pesos que normalmente se distribuyen con una media de 17.46 gramos
#y una varianza de 375.67 gramos.
#¿Cuál es la probabilidad de que una muestra elegida al azar pese más de
#19 gramos?
#u=17,46
#p(x>19)= 1-p(x<19)
var3=375.67
sd3=sqrt(var3)
prob_p2c = pnorm(19, mean=17.46 , sd=sd3)
prob_p2c
prob_p2 = pnorm(19, mean=17.46 , sd=sd3, lower.tail=FALSE)
prob_p2
#grafico
seriep2 <-rnorm(1000 , mean=17.46, sd=sd3)
dfp2 <- data.frame(X=seriep2 , Y=dnorm(seriep2, mean=17.46 ,sd=sd3))
x=19
sprintf("La probabilidad de que una muestra pese mas que %1.2f  es %1.7f",x, prob_p2)
g<- ggplot(data=dfp2 , aes(x=X, y=Y))+
  geom_point()+
  geom_vline(xintercept=x, color="red")
g

