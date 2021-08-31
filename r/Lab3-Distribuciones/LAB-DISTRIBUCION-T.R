#LABORATORIO DISTRIBUCION T
#Es un miembro de una familia de distribuciones de probabilidad contínua
#que surge al estimar la media de una población distribuida normalmente en
#situaciones donde el tamaño de la muestra es pequeño y se desconoce
#la desviación estándar de la población.

#Cargando librerías 
rm(list=ls())
require(ggplot2)
require(plyr)
require(stats)
require(graphics)
#########################################
#1.1 Como  obtener  un vector con distribución T
#La función es :rt(n, df, ncp)

#Donde : 
#n:    es la cantidad de observaciones que se desea
#df:   son los grados de libertad
#ncp:  parámetro delta de no centralidad, si se omite usa distribución T central

#Ejemplo : Obtener un vector de 20 observaciones con distribución t con 10 grados
#de libertad
rt(20,df=10)

#Comparando las distribuciones T con distribución normal 
#Realicemos un histograma
#Podemos ver que la distribución T con pocas observaciones difiere de la normal

vec_t <- rt(50 , df=5 )
vec_n <- rnorm(500)
vec_n_df<-data.frame(x=vec_n)
vec_t_df<- data.frame(x=vec_t)

ggplot (data=vec_n_df , aes(x=x))+
  geom_histogram(binwidth=0.5 , color="blue")+
  geom_histogram(data=vec_t_df ,binwidth=0.5, aes(x=x ), fill="red", alpha=.5)+ 
  xlab("Azul Dist Normal/Rojo Dist T")


#Vemos que se parecen pero la distribución T es mucho más aplanada en las colas
#realicemos el mismo gráfico, pero como distribución de densidad
vec_dt <-dt(vec_t, df=4)
head(vec_dt)
vec_dn<- dnorm(vec_n)
head(vec_dn)
#Veamos como varían las distribuciones (PDF) con los grados de libertad
ggplot(data.frame(X=c(-4,4)), aes(x = X)) +
  stat_function(fun = dt, args =list(df=8), aes(color=8) )+
  stat_function(fun = dt, args =list(df=4), aes(color=4))+
  stat_function(fun = dt, args =list(df=2), aes(color=2))+
  stat_function(fun = dt, args =list(df=1), aes(color=1))+
  stat_function(fun=dnorm, color="red")

#Concluimos que la distribución T es parecida a la normal, pero mientras
#grados de libertad bajan la diferencia se hace notoria
######################################################
##1.2 Como obtener la densidad (PDF), obtener la probabilidad en 1 punto determinado 
#la sintaxis de la función es dt(x, df, ncp, log = FALSE)
#Donde : 
#x:      vector de cuantiles.
#df:   grados de libertad.

x_dt <- seq(-10, 10, by = 0.01) 
y_dt <- dt(x_dt, df = 3)  
plot(y_dt , col="green")
#Repito, es similar pero no igual a la distribución gaussiana


#1.3 Probabilidad acumulada (CDF) de un punto hacia la cola esta dado por 
#la función es pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
#Donde: 
#q 	vector de cuantiles o el valor de t
#df	grados de libertad.
#lower.tail 	TRUE (default), probabilidad de P[X <= x], FALSE P[ X >x ] 

x_pt <- seq(- 10, 10, by = 0.01) 
y_pt <- pt(x_pt, df = 5, lower.tail = TRUE) 
plot(y_pt, col="blue", lty=1) 
points(y_dt, col="red", lty=1)

x_pt <- seq(- 10, 10, by = 0.01) 
y_pt <- pt(x_pt, df = 5, lower.tail = FALSE) 
plot(y_pt, col="blue", lty=1) 
points(y_dt, col="red", lty=1)



#1.4  Para obtener una probabilidad con cierto intervalo de confianza 
#y  grados de libertad, el valor del estadístico t lo obtenemos así:
#qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
#Donde:
#p:  vector de probabilidad o intervalo de confianza
#df: grados de libertad
#lower.tail: TRUE (default), probabilidad de P [ X<= x],FALSE P[ X >x ] 
#qt() proporciona valores t criticos.

#Ejemplo: Obtener el estadístico t para 95% de intervalo de confianza y df=10
intconf =.95 #interalo de confianza
gl <-10 # 10 grados de libertad
est_t<-qt(intconf,gl,lower.tail=TRUE)
est_t

#Veamos otro ejemplo con lower tail=false, es decir desde el punto hacia arriba
intconf =0.9 #intervalo de confianza
gl <-18 # 18 grados de libertad
est_t<-qt(intconf,gl, lower.tail=FALSE)
est_t

#Veamos un ejemplo  
#Un fabricante de focos asegura que su producto dura 500 horas 
#Es imposible hacer un muestreo grande para validar (pista que debemos usar dist T)
#Se hace una muestra con 25 focos (n)
#La media obtenida es 505.36 
#La muestra tiene una s de 12.07
#Pregunta: Esta o no garantizada la calidad.

#Calculo el valor de t
n=25
media_muestra=505.36
s=12.07
mu=500

t<- (media_muestra- mu)/(s /sqrt(n))
t
#Obtenemos el valor de t-critico para 95% de confianza, se podría discutir si 95% 
#es suficiente, quizás 90 es suficiente o 99%
intconf =0.95
gl <-24  # 25 -1 grados de libertad
tcritico<-qt(intconf,gl, lower.tail=TRUE)
tcritico
#notar qt nos da el tcritico con UNA  sola cola
#graficando 

ggplot(data.frame(X=c(0,10)), aes(x = X)) +
  stat_function(fun = dt, args =list(df=24))+
  geom_vline(xintercept=t , color ="blue")+
  geom_vline(xintercept=tcritico, color="red")+
  xlab("t-critico (rojo) y T muestra (azul)")

#Podemos ver que el estadístico t obtenido es 2.2 superior al tcritico para 
#tener 95% de confianza
#El estudiante podrá jugar con los valores de desviación e intervalo de confianza 

#Ejemplo 2
#Tenemos una fábrica de tornillos
#los tornillos tiene una longitud media de 20mm 
#y una desviación estándar de 1mm
#queremos saber cuál es la probabilidad de que en una muestra de 25 elementos
#la media sea menor que 20.5mm

#primero calculamos t 

t<- (20.5-20)/(1/sqrt(25))
t
#calculamos el CDF para este valor t mediante la función pt
gl=25-1
ptt <-pt(t,df=gl, lower.tail=TRUE)
ptt
#La probabilidad de que la mu de la población sea menor que 20.5 es 99%,
#esto es lo mismo que revisar la tabla al revés, es decir , dados los 
#grados de libertad, y el valor de t buscar cual es intervalo de confianza


#Otro Ejemplo: Encontrar el valor t para el intervalo de confianza es de 95% 
#y los grados de libertad de la muestra es 20 unidades.
#Se resta siempre 1 a la muestra, entonces gl=20-1= 19  
#Se cruza entonces 19 con 0.95
#El valor t que se marca en la curva es de +/- 2.0932, para dos colas
#1.7 para 1 cola

intconf=0.95
n=20
gl=n-1
tcritico<-qt(intconf,gl)
tcritico
# O viceversa teniendo un  valor t y gl  que porcentaje del área se cubre 
pt(1.73,19)



#Para el siguiente ejercicio hay que recordar que la suma de las probabilidades
#es siempre 1 y que el CDF se calcula desde -inf al valor  o  desde el valor a +inf

#Otro ejemplo: Encontrar un valor probabilidad de las colas para un valor t dado:   
#Aqui estamos viendo la probabilidad acumulada de los 2 alfas
#esto es equivalente a cuando en la distribución normal restábamos 2 pnorms
#para t=1.8 y gl=10

2*(1-pt(1.8,10))
#por que multiplicamos por 2 es porque  hay 2 colas

#graficando para verlo mejor 
#obtenemos el valor de t
intconf=.95
gl=10
t <- qt(intconf,gl)
t
#creamos una distribución T de 20 elementos  con df=10

X<-rt(20,df=gl)
serie <- data.frame(X=X)
head(serie)
# y ploteamos
ggplot(data=serie,  aes(x = X)) +
  stat_function(fun = dt, args =list(df=10), aes(color=8))+
  geom_vline(xintercept=t)+
  geom_vline(xintercept=-t)

#El área debajo de la curva y más allá de las verticales (+-t)
#estaria dado por :
sprintf("la probabilidad acumulada CDF de las dos colas es %2.2f ",2*(1-pt(1.8,10)))
#En otras palabras la probabilidad que un valor sea <-t o >t es del 10%


#PARTE2 TAREA DEL ESTUDIANTE
######################################################################
#Un fabricante produce 2 tipos de luminarias de sodio ,un tipo de   
#de vapor de sodio a baja presión SBP y las de alta presión SAP. 
#Las dos tienen distinta cromática. La EEQ esta interesada en comprar varios miles
#y requiere hacer un análisis de la durabilidad.
#Realiza una muestra  de 15 items SBPs que arroja un tiempo de duración de 
#11.3 semanas con un s=2.5, realiza otra muestra de SAPs con 10 muestras que
#evidencia duración de 8 semanas con un s= 3.2 
#Basado en estas encuestas que tipo debería comprar la EEQ si lo que le interesa es
#la durabilidad



#PROBLEMA 2
#Una fábrica de produce resistencias eléctricas de alta precisión, garantizan 2% de variabilidad 
#Una muestra de 6 ítems  de 2.80 ohms da una media muestral de 2.62 ohmios y una s de la muestra de 0.121 ohmios.
#Le piden su opinión de si esta muestra representa la media de la población que se publica
#O en otras palabra a que nivel de significancia se halla la muestra. 


