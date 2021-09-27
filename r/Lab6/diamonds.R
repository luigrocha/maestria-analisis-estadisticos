############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendrá el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Objetivo : realizar distintos tipos de análisis estadístico de sus variables para intentar
# averiguar algún tipo de comportamiento oculto aparentemente en los datos. 
#
# Para ello os marco los siguientes pasos: tipos de variables, medidas de posición central, 
# medidas de dispersión, 
# distribución y relación entre ellas, más análisis de regresión
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relacion al punto mas ancho 

# Responde cada bloque cubriendo al menos lo indicado:

#install.packages("corrplot")

library(ggplot2)
library(e1071)
library(prettyR)
library(corrplot)
library(car)
library(Rcmdr)
library(zoo)
library(lmtest)
library(MASS)
dt<-as.data.frame(diamonds)

head(dt)
attach(dt)
str(dt) #=> 53.940 observaciones en 10 variables.

# ------------ FUNCIONES ------------ #

AnalisisVariable <- function ( variable ){
  
  par(mfrow=c(2,2))
  boxplot(variable)
  hist(variable)
  
  # muestra los graficos para valorar la normalidad 
  plot(density(variable))
  qqnorm(variable)
  qqline(variable, col=2)
  
  par(mfrow=c(1,1))
}

#Funcion para hacer el test de correlacion entre variables del dataset.
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#########################################################################
#Muestra representativa
# Selecciona una muestra representativa para "cut"
########################################################################
table(cut)  # para ver el numero de filas de cada tipo de cut.
# Fair      Good Very Good   Premium     Ideal 
# 1610      4906     12082     13791     21551 
proporcion<-data.frame(prop.table(table(cut)))
print (proporcion)
nMuestra<-4000

#multiplico el porcentaje de la proporcion por el tamaño de la muestra, para obtener
#cual seria el numero de datos que tendría que tener por cada tipo de corte.
mFair<-round(proporcion[1,2]*nMuestra)
mGood<-round(proporcion[2,2]*nMuestra)
mVgood<-round(proporcion[3,2]*nMuestra)
mPremium<-round(proporcion[4,2]*nMuestra)
mIdeal<-round(proporcion[5,2]*nMuestra)

cat (mFair,mGood,mVgood,mPremium,mIdeal)
#Selecciono datos aleatoriamente por cada tipo de corte.

dtFair<- dt[cut=="Fair",]
numale=sample(1:nrow(dtFair), mFair, replace=FALSE)
dtFair<-dtFair[numale,]
#-----
dtGood<- dt[cut=="Good",]
numale<-sample(1:nrow(dtGood), mGood, replace=FALSE)
dtGood<-dtGood[numale,]
#----
dtVGood<- dt[cut=="Very Good",]
numale<-sample(1:nrow(dtVGood), mVgood, replace=FALSE)
dtVGood<-dtVGood[numale,]
#----
dtPremium<- dt[cut=="Premium",]
numale<-sample(1:nrow(dtPremium), mPremium, replace=FALSE)
dtPremium<-dtPremium[numale,]
#----
dtIdeal<- dt[cut=="Ideal",]
numale<-sample(1:nrow(dtIdeal), mIdeal, replace=FALSE)
dtIdeal<-dtIdeal[numale,]
#----
#Genero un dateframe con los datos seleccionados aleatoriamente.
dtMCut <- rbind(dtFair,dtGood,dtVGood,dtPremium,dtIdeal )
table(dtMCut$cut)  # para ver el numero de filas de cada tipo de cut.
proporcion<-data.frame(prop.table(table(dtMCut$cut))) #tengo la misma proporcion en la muestra.

####################################################################################
#Analisis de las variables
# Analisis descriptivo de las variables: Tipo de variable, distribuci?n y representaci?n
# Deteccion de casos atipicos y su tratamiento
####################################################################################
str(dt)
# A continuacion se indican los tipos de variable.  

# carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...=> Variable continua.
# cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
# color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
# clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
# depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
# table  : num  55 61 65 58 58 57 57 55 61 61 ...
# price  : int  326 326 327 334 335 336 336 337 337 338 ...
# x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
# y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
# z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

#-----------   tipos de variable:
#carat : variable cuantitativa continua
#cut: variable cualitativa
#color : variable cualitativa
#clarity: variable cualitativa
# depth:cuantitativa continua
# table:cuantitativa continua
# price :cuantitativa continua
# x,y,z :cuantitativa continua


#-----------   distribucion y representacion y Deteccion de casos atipicos y su tratamiento

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable carat :# carat: peso del diamante

AnalisisVariable(dtMCut$carat)
summary(dtMCut$carat) # no hay valores invalidos.

# Casos atipicos los podemos ver en el boxplot.
# los buscamos y los quitamos.
maxi<-quantile(dtMCut$carat,.75)+1.5*IQR(dtMCut$carat)  
dtMCut<-dtMCut[dtMCut$carat<maxi,]
AnalisisVariable(dtMCut$carat)
skew(dtMCut$carat) # es positiva, es sesgada a la derecha.
kurtosis(dtMCut$carat)
shapiro.test(dtMCut$carat)


#En boxplot se ve que la varianza esta equilibrada y que la mediana 
#esta en el centro, y luego vemos por los datos que la media y la mediana tienen valores cercanos 
# entre 0.7 y 0.77, pero en el diagrama de densidad se aprecia que el tamaño que mas abunda es cercano
#a 0,5, luego decrece, y luego hay otro pico rondando 1 kilate.. con lo cual no llega a asemejarse del 
#todo a  una distribucion normal.
# en el grafico de qqplot, se puede apreciar, que tenemos bastantes puntos en comun con una 
#distribucion normal, pero nuestra variable tiene picos,y aunque se asemeja no llega a ser del todo normal.
# El test de Kurtosis, devuelve un valor negativo, lo que significa que sigue una distribucion platicurtica.
# Finalmente, el test de shapiro devuelve un valor muy pequeño y < 0,05, por lo que podemos rechazar que siga una
# distribucion normal.



#  *-*-* *-*-* *-*-* *-*-*  variable cut

#cut: variable cualitativa =>  calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# distribucion y representacion y Deteccion de casos atipicos y su tratamiento

# la variable cut es un factor, la convierto a valores, para poder trabajar con ella.
# Pero para analizarlo utilizo otra variable.

table(dtMCut$cut) # aqui podemos ver el numero de corte
DatosCut<-ifelse(dtMCut$cut=="Fair",1,
                 ifelse(dtMCut$cut=="Good",2,
                        ifelse(dtMCut$cut=="Very Good",3,
                               ifelse(dtMCut$cut=="Premium",4,5 )      )     ))

table(DatosCut)
AnalisisVariable(DatosCut)
shapiro.test(DatosCut)


# el histograma, para este tipo de variables, indica lo que ya vemos en
#con los numeros, que el tipo de corte que mas abunda es el Ideal, y luego los de good y very good
#tienen valores muy cercanos.
# He ralizado el test de shapiro, para confirmar que efectivamente no es una distribucion normal.

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable color :# colour: color del diamante (desde D el mejor hasta J el peor)
table(dtMCut$color) # es un factor, con esta instruccion vemos la distribucion por color.
#lo pasamos a numeros, con el criterio que he seguido con el color.. de menos a mas calidad. 
DatosColor=ifelse(dtMCut$color=="D",7,ifelse(dtMCut$color=="E",6,ifelse(dtMCut$color=="F",5,ifelse(dtMCut$color=="G",4,ifelse(dtMCut$color=="H",3,ifelse(dtMCut$color=="I",2,1))))))
table(DatosColor)
AnalisisVariable(DatosColor) # los colores que mas abundan son del 2 al 4.
shapiro.test(DatosColor)
#Al ser variables cualitativas, en los graficos se puede ver que no tiene nada que ver 
#con la distribucion normal. Aun asi, he realizado el test de shapiro, que arroja valor negativo y confirma
#que no es una distribucion normal.



# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
table(dtMCut$clarity) # es un factor, con esta instruccion vemos la distribucion por color.
#lo voy a poner siguiendo el mismo criterio anterior, de menos a mas.

Datosclarity=ifelse(dtMCut$clarity=="I1",1,ifelse(dtMCut$clarity=="SI2",2,ifelse(dtMCut$clarity=="SI1",3,ifelse(dtMCut$clarity=="VS2",4,ifelse(dtMCut$clarity=="VS1",5,ifelse(dtMCut$clarity=="VVS2",6,ifelse(dtMCut$clarity=="VVS1",7,8)))))))
table(Datosclarity)
AnalisisVariable(Datosclarity) #La claridad que mas abunda son la 3 y 4.
shapiro.test(Datosclarity)
#Al ser variables cualitativas, en los graficos se puede ver que no tiene nada que ver 
#con la distribucion normal. Aun asi, he realizado el test de shapiro, que arroja valor negativo y confirma
#que no es una distribucion normal.


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable  price: Precio en dolares americanos
summary(dtMCut$price) # no hay valores invalidos.
AnalisisVariable(dtMCut$price)
#tiene bastante cola a la derecha, ya que tiene bastantes valores atipicos..precios muy altos..
#los vamos a filtrar.
maxi<-quantile(dtMCut$price,.75)+1.5*IQR(dtMCut$price)  
dtMCut<-dtMCut[dtMCut$price<maxi,]
AnalisisVariable(dtMCut$price) 
#Sigue teniendo muchos valores atipicos, vuelvo a filtrar
maxi<-quantile(dtMCut$price,.75)+1.5*IQR(dtMCut$price)  
dtMCut<-dtMCut[dtMCut$price<maxi,]
AnalisisVariable(dtMCut$price) 



skew(dtMCut$price) # es positiva, es sesgada a la derecha.
kurtosis(dtMCut$price)
shapiro.test(dtMCut$price)
# hay mayor cantidad de diamantes en torno a los 2000 dolares, pero como hay diamantes que
# que tienen un precio muy alto, hace que la media suba a 3025, en comparacion a la mediana que son 2140.
# y eso tambien lo podemos apreciar en el boxplot, que se ve que hay varianza, ya que la mediana
#no esta en el "medio" de la caja. 
#Se podria decir que sigue una distribucion con asimetria positiva,y apuntamiento leptocurtico ( el test de Kurtosis es positivo).
#En el grafico de qqplot, vemos que se acerca a la recta de lo que seria la normal, pero no llega 
#a ser del todo normal.
# Con el test de shapiro, vemos que dar un resultado negativo y menor a 0,05 podemos rechazar que la 
# distrubucion sea normal.

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable  x: longitud en mm 

AnalisisVariable(dtMCut$x) 
summary(dtMCut$x) # no hay valores invalidos.
skew(dtMCut$x)  # es positivo, sesgado a la derecha.
kurtosis(dtMCut$x)
shapiro.test(dtMCut$x)
#No tiene casos atipicos que limpiar. 
#no sigue una distribucion clara.  En boxplot se ve que la varianza esta equilibrada y que la mediana 
#esta en el centro, y luego vemos por los datos que la media y la mediana tienen valores cercanos 
# entre 5.380 y 5,49, pero en el diagrama de densidad se aprecia que el tamaño que mas abunda es cercano
#a 4mm, luego decrece, y luego hay otro pico rondando 7 mm.. con lo cual no se asemeja a una 
#distribucion normal.
#en el grafico de qqplot lo podemos ver, que tiene bastantes puntos en comun en el centro, pero se aleja 
# en las colas.
# Con el test de shapiro, vemos que dar un resultado negativo y menor a 0,05 podemos rechazar que la 
# distrubucion sea normal.
#El test de Kurtosis, devuelve un valor negativo, lo que significa que sigue una distribucion platicurtica.


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  variable  y: longitud en mm 
AnalisisVariable(dtMCut$y) 
summary(dtMCut$y) # no hay valores invalidos.
skew(dtMCut$y) # es positivo, sesgado a la derecha.
kurtosis(dtMCut$y)
shapiro.test(dtMCut$y)
#No tiene casos atipicos que limpiar. 
#los datos son casi iguales que los de la variable x, con lo cual las apreciaciones son las mismas.

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******   z: profundidad en mm 

AnalisisVariable(dtMCut$z) 
summary(dtMCut$z) # no hay valores invalidos.
skew(dtMCut$z) # es positivo, sesgado a la derecha.
kurtosis(dtMCut$z)
shapiro.test(dtMCut$z)

#No tiene casos atipicos que limpiar. 
#los datos siguen el mismo patron que las variables x e y, con lo cual las apreciaciones son las mismas.

# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ******  depth: porcentaje total de profundidad 
AnalisisVariable(dtMCut$depth) 
summary(dtMCut$depth) # no hay valores invalidos.

#En el boxplot se ve que hay muchos puntos atipicos, vamos a limpiarlos.
maxi<-quantile(dtMCut$depth,.75)+1.5*IQR(dtMCut$depth)  
dtMCut<-dtMCut[dtMCut$depth<maxi,]
AnalisisVariable(dtMCut$depth)  #hemos limpiado los superiores.

mini<-quantile(dtMCut$depth,.25)-1.5*IQR(dtMCut$depth)  
dtMCut<-dtMCut[dtMCut$depth>mini,]
AnalisisVariable(dtMCut$depth)  #hemos limpiado los inferiores
sd(dtMCut$depth)
skew(dtMCut$depth) # es negativa, sesgado a la izquierda.
kurtosis(dtMCut$depth)
shapiro.test(dtMCut$depth)

#Una vez limpiados los valores, se puede ver que esta variable si que sigue una distribucion normal.
#con una varianza muy cercana a 1, y con una media y mediana practicamente iguales.
#el test de shapiro devuevle un valor positivo, mayor que 0,05, con lo que podemos aceptar que sigue una
#distribucion normal
#Ademas el test de kurtosis, nos da un valor muy pequeño muy cercano a 0, lo que indica que sigue una distribucion normal o mesocurtica


# *-*-* *-*-* *-*-* *-*-* *-*-* *-*-*
# ****** table: anchura de la parte superior de diamante con relacion al punto mas ancho 
AnalisisVariable(dtMCut$table) 
summary(dtMCut$table) # no hay valores invalidos.

#vamos a limpiar los puntos atipicos que se aprecian en el boxplot.
maxi<-quantile(dtMCut$table,.75)+1.5*IQR(dtMCut$table)  
dtMCut<-dtMCut[dtMCut$table<maxi,]
mini<-quantile(dtMCut$table,.25)-1.5*IQR(dtMCut$table)  
dtMCut<-dtMCut[dtMCut$table>mini,]
AnalisisVariable(dtMCut$table)  #hemos limpiado los inferiores
summary(dtMCut$table)
sd(summary(dtMCut$table))
unique(dtMCut$table)
skew(dtMCut$table) # es positiva, sesgado a la derecha
kurtosis(dtMCut$table)
shapiro.test(dtMCut$table)

#No sigue para nada una distribucion normal. Aunque la media y la mediana sean casi iguales, 
#los valores suben y bajan, tal y como se ven en el diagrama de densidad y en el histograma.
#el test de shapiro, devuelve un valor negativo, lo que indica que podemos rechazar que siga una distribucion normal.
# parece una distribucion de Bernoulli, donde la variable parace tomar valores cercanos a 7 valores 
# concretos 54,55,56,57,58,59 y 60. Seria un compartamiento comparable a una variable cuantitativa discreta.
AnalisisVariable(round(dtMCut$table))  #hemos limpiado los inferiores
#El test de Kurtosis, devuelve un valor negativo, lo que significa que sigue una distribucion platicurtica.


####################################################################################
#Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
# Formula un test de hipotesis
####################################################################################
# Carat: peso del diamente, y depth(que sigue dNormal) es porcentaje de profundidad.

#-----  Calcular el intervalo de confianza  (95% ) para carat.

#Formula
#P(media-((Za/2)*desviacion)/5 < X < media+((Za/2)*desviacion)/5) = 0,95
#para calcular el intervarlo de confianza utilizo t.test, por defecto con 0,95

t.test(dtMCut$carat)
intervalo <- t.test(dtMCut$carat)$conf.int[1:2]
intinferior <-intervalo[1]
intsuperior<-intervalo[2]
cat ("el intervalo de confianza para la media de carat esta entre [" , intinferior , "," , intsuperior , "] ")

# #Lo muestro en grafico para verlo mejor.
par(mfrow=c(1,1))

plot(density(dtMCut$carat))
abline(v=mean(dtMCut$carat),col="red",lty=2, lwd=1)
abline(v=intinferior,col="blue",lty=2, lwd=1)
abline(v=intsuperior,col="blue",lty=2, lwd=1)
#http://www.sthda.com/english/wiki/histogram-and-density-plots-r-base-graphs
#http://www.dm.uba.ar/materias/analisis_de_datos/2008/1/teoricas/Teor5.pdf
#-----  Calcular el intervalo de confianza  (95% ) para depth
#P(media-((Za/2)*desviacion)/5 < X < media+((Za/2)*desviacion)/5) = 0,95

#para calcular el intervarlo de confianza utilizo t.test
t.test(dtMCut$depth)
intervalo <- t.test(dtMCut$depth)$conf.int[1:2]
intinferior <-intervalo[1]
intsuperior<-intervalo[2]


cat ("el intervalo de confianza para la media de depth esta entre [", intinferior , "," , intsuperior , "] ")

plot(density(dtMCut$depth))
abline(v=mean(dtMCut$depth),col="red",lty=2, lwd=1)
abline(v=intinferior,col="blue",lty=2, lwd=1)
abline(v=intsuperior,col="blue",lty=2, lwd=1)

#------- Formula un test de hipotesis

#Hipotesis es que la media de los precios (price) de los diamentes de corte Ideal, es igual a la
#media de los precios de los de tipo Premium (cut), con un nivel de confianza de 95%.

#Sigo la formula de contraste de media para dos muestras independientes.

pricePremium<-dtMCut[dtMCut$cut=="Premium", 7]
priceIdeal<-dtMCut[dtMCut$cut=="Ideal", 7]
boxplot(pricePremium,priceIdeal) # vemos en un diagrama los dos variables que vamos a analizar

#la hipotesis nula es que la media de las dos muestras son iguales.


mediaPrimePremium<-mean(pricePremium)
mediaPrimeIdeal<-mean(priceIdeal)
desPrimePremium <- sd(pricePremium)^2
desPrimeIdeal <- sd(priceIdeal)^2
tamanomuestraPremium<-length(pricePremium)
tamanomuestraIdeal<-length(priceIdeal)

t<-(mediaPrimePremium-mediaPrimeIdeal) / sqrt((((tamanomuestraPremium*desPrimePremium)  + (tamanomuestraIdeal*desPrimeIdeal)/(tamanomuestraPremium+tamanomuestraIdeal-2))      )*((1/tamanomuestraPremium)+(1/tamanomuestraIdeal)))
vt <-  qt(0.025,df=tamanomuestraPremium-1)

cat ("Es el valor del estadistico de contraste menor que el valor teorico :" , t<vt)
cat (" Se rechaza la hipotesis nula, la media de los precios de tipo Ideal y Premium no son iguales")

#Si hacemos el test con R, utilizando la funcion t.test
t.test(pricePremium, priceIdeal,  alternative='two.sided',conf.level=.95, paired=FALSE)

#El resultado refleja que la media de los precios de los diamantes Premiun, es superior que los Ideal,
#y eso que el corte Ideal es mejor que el Premium
#Ademas, el p-value ademas muy pequeño, se rechaza la hipotesis nula de que las medias de los precios serian iguales.



#  Voy a utilizar un contraste de independencia
#  formulo la hipotesis de que la variable cut ( calidad del corte) esta relacionada con el color.

dtCulColor <- table(dtMCut$cut , dtMCut$color)
dtCulColor
#chisq.test(dtCulColor,simulate.p.value = TRUE)
chi <- chisq.test(dtCulColor,simulate.p.value = TRUE)
print (chi)
cat ("El resultado de pvalue :", chi$p.value , " es < que 0.05 : " , chi$p.value < 0.05)
#El resultado de p-value es  menor que 0,05. No son independientes.
# por lo que podriamos decir que el corte del diamante depende del color

####################################################################################
#Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlacion)
####################################################################################

#Lo primero que voy a hacer es cambiar los valores categoricos por valores numericos.

dtMCut$cut <- DatosCut<-ifelse(dtMCut$cut=="Fair",1, ifelse(dtMCut$cut=="Good",2, ifelse(dtMCut$cut=="Very Good",3,ifelse(dtMCut$cut=="Premium",4,5 )      )     ))
dtMCut$clarity <- ifelse(dtMCut$clarity=="I1",1,ifelse(dtMCut$clarity=="SI2",2,ifelse(dtMCut$clarity=="SI1",3,ifelse(dtMCut$clarity=="VS2",4,ifelse(dtMCut$clarity=="VS1",5,ifelse(dtMCut$clarity=="VVS2",6,ifelse(dtMCut$clarity=="VVS1",7,8)))))))
dtMCut$color<-ifelse(dtMCut$color=="D",7,ifelse(dtMCut$color=="E",6,ifelse(dtMCut$color=="F",5,ifelse(dtMCut$color=="G",4,ifelse(dtMCut$color=="H",3,ifelse(dtMCut$color=="I",2,1))))))

#par(mfrow=c(2,2))

corrplot(cor(dtMCut), method="number")
corrplot(cor(dtMCut), type="upper")
p.mat <- cor.mtest(dtMCut)

#pinto el grafico , donde se puede ver las variables con mas correlacion, y se tachan aquellas
#que s
corrplot(cor(dtMCut), type="upper", order="hclust",  p.mat = p.mat, sig.level = 0.01)
cor.test(dtMCut$cut,dtMCut$table)

#Segun estos graficos, podemos descartar correlaciones entre las variables de color con cut y table
#asi como depth con price, carat,x,y y color.

#Las correlaciones mas cercanos se producen entre carat,x,y,z, es decir entre los variables que indican las
#medidas del diameante: el peso, longitud, ancho y profundidad 

#----- ANOVA 
# Comprobar si el porcentaje de profundidad ( depth) es igual para cada uno de los tipos de Claridad del diamante.
pmD <- dtMCut[dtMCut$clarity==7,5]
pmE <- dtMCut[dtMCut$clarity==6,5]
pmF <- dtMCut[dtMCut$clarity==5,5]
pmG <- dtMCut[dtMCut$clarity==4,5]
pmH <- dtMCut[dtMCut$clarity==3,5]
pmI <- dtMCut[dtMCut$clarity==2,5]
pmJ <- dtMCut[dtMCut$clarity==1,5]

#Igualo el tamaño de las muestras al minimo de los datos encontrados
mn<-min(length(pmD),length(pmE),length(pmF),length(pmG),length(pmH),length(pmI),length(pmJ))
pmD<-sample(pmD, mn,replace=FALSE)
pmE<-sample(pmE, mn,replace=FALSE)
pmF<-sample(pmG, mn,replace=FALSE)
pmG<-sample(pmG, mn,replace=FALSE)
pmH<-sample(pmH, mn,replace=FALSE)
pmI<-sample(pmI, mn,replace=FALSE)
pmJ<-sample(pmJ, mn,replace=FALSE)

DpCla <- c(pmD,pmE,pmF,pmG,pmH,pmI,pmJ)
FClar <- c(rep("D",mn),rep("E",mn),rep("F",mn),rep("G",mn),rep("H",mn),rep("I",mn),rep("J",mn))
FClar<-factor(FClar)

#Ya tengo mi muestra para hacer el test.
#El anova requiere que las poblaciones sean normales, que las muestras sean independientes, y que 
# tengan igual varianza.


#Compruebe que sigue una distribucion normal. Aunque por el analisis previo realizado anteriormente
#ya vimos que la variable depth lo sigue.

shapiro.test(DpCla)
d<-rnorm(length(DpCla),mean=mean(DpCla),sd=sd(DpCla))
qqplot(DpCla,d)
abline(c(0,1))

#contraste de independencia
#chisq.test(DpCla,FClar,simulate.p.value=TRUE) #son independientes
chi <- chisq.test(DpCla,FClar,simulate.p.value=TRUE)
print (chi)
cat ("El resultado de pvalue :", chi$p.value , " es > que 0.05 : " , chi$p.value > 0.05)

# el p.value es mayor que 0.05, asi que son independientes.


#muestra el valor del poder de transformacion 
splevel <-spreadLevelPlot(DpCla, by=FClar, robust.line=FALSE, xlab="Nivel Depth")
pwt<-round(splevel$PowerTransformation) 
lnDpCla<-(DpCla)**pwt #transformo la variable.

leveneTest(lnDpCla, group=FClar) #Mayor de 0.05, igualdad de varianzas
# EL Resultado indica que las varianzas son iguales ( despues de la transformacion), ya que da un resultado < 0.05

# Se cumplen las tres condiciones para calcular la anova.

boxplot(lnDpCla~FClar)
p.aov<-aov(lnDpCla ~FClar)  
summary(p.aov)

#como el valor de pvalue es mayor que 0.05 , se entiende que no hay diferencias significativas.
#se acepta la hipotesis de que las medias sean iguales.

tukey<-TukeyHSD(p.aov) #los intervalos que no contienen el valor 0 son significativos, p-value<0,05
tukey
plot(tukey) #se puede ver en el grafico que no hay valores significativos.

#por lo que las medias de la variable depth de profundidad es la misma para los distintos valores de claridad.
#dicho de otra forma, todos los diamantes que son de una claridad tienen la misma media del valor de la profundidad del diamante.

####################################################################################
#Analisis de regresion
# Formular un modelo de regresion y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformacion a la regresion y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresion
####################################################################################

#https://www.youtube.com/watch?v=HyJoMcTSomc#t=535.350851

# Para hacer al analisis de las variables a emplear en el modelo, antes hemos visto la correlacion, pero
# ahora vamos a ver la correlacion parcial.
# Mi variable respuesta va a ser el precio, es para la que quiero establecer el modelo.

partial.cor(dtMCut)
# Segun he visto, prize tiene mayores valores con carat, color y clarity. Luego depth y cut podrian interesar o no... vamos a ver.
fit <- lm(dtMCut$price~dtMCut$carat+dtMCut$color+dtMCut$clarity+dtMCut$depth+dtMCut$cut)
summary(fit)
#En este resultado, ya podria ver si cada coeficiente es relevante o significativo, segun si su pvalue es > 0.05 o no. Si es > 0.05 
#no podemos rechazar la hipotesis de que ese valor puede ser 0, por lo que no seria relevante para el estudio.
#Con lo que al a vista de los resultados, podriamos decir que las variables carat, color y clarity con valores muy pequeños si que nos
#aportan al modelo. Depth es mayor > 0.05, con lo cual no aporta, porque podria ser 0, y cut es menor que 0.05, pero no es un valor muy pequeño
#Para salir de dudas, vamos a estudiar el IAC del modelo.

step(fit)
#Segun vemos, el IAC mejora si quitamos la variable depth. Y empeora si quitamos mas variables, asi que la propia funcion nos dice cual es
#el mejor modelo a emplear. 
#lm(formula = dtMCut$price ~ dtMCut$carat + dtMCut$color + dtMCut$clarity + dtMCut$cut)
#curiosamente, la variable cut aporta, pero antes ya habiamos dicho que el corte del diamante depende del color.
#Ademas, los valores fisicos ( anchura, longitud...) no aportan al precio del diamante.¿?
# vamos a incluir variables fisicas para estar seguros de esto..

fit2 <- lm(dtMCut$price~dtMCut$carat+dtMCut$color+dtMCut$clarity+dtMCut$depth+dtMCut$cut+dtMCut$x+dtMCut$y+dtMCut$z+dtMCut$table)
summary(fit2)
#ahora parece que todas aportan con pvalores < 0.05, excepto la variable table.
step(fit2) # valor IAC 41682 41808
#El valor IAC de fit2 es menor que el anterior, con lo que el modelo finalmente es fit2.
#Modelofinal : lm(formula = dtMCut$price ~ dtMCut$carat + dtMCut$color + dtMCut$clarity + dtMCut$depth + dtMCut$cut + dtMCut$x + dtMCut$y + dtMCut$z)
fit2 <- lm(dtMCut$price~dtMCut$carat+dtMCut$color+dtMCut$clarity+dtMCut$depth+dtMCut$cut+dtMCut$x+dtMCut$y+dtMCut$z)
sfit2<-summary(fit2)
sfit2
#Vamos a calcular un intervalo de confianza de los valores de los coeficientes el modelo.
confint(fit2) 
#Aqui tenemos el intervalo de confianza entre los que se mueven los coeficientes que multiplican a cada una de  nuestras variables.

#Bondad del ajuste --------------

#Vamos a ver los graficos del modelo
par(mfrow=c(2,2))
plot(fit2)

#la primera impresion despues de ver los graficos, es que el modelo no esta muy ajustado..

#----- 1. Calculo del error standar residual. 
redStanError <- sfit2$sigma
cv <- 100 * ( redStanError/ mean(dtMCut$price)) #calculamos el coeficiente de variacion asociado.
cv > 10
# Si el coeficiente es mayor que 10, significa que hay mucha desviacion, el modelo no es bueno.
# ya se ve en el grafico que existe mucha variacion entre los residuos y los valores esperados.

#----- 2. Anova, que contesta a la pregunta de si las variables pueden ser 0 o no ( que seria la hipotesis nula)
# Para ello nos fijamos en el pvalue de F-statistic,  en este caso, vemos que tiene un valor inferior a 
# 0.05, y es un valor muy pequeño, por lo que podemos rechazar la hipotesis nula, y ninguno de las variables
#puede valer 0. Lo que indicaria que el modelo puede ser bueno.

#----- 3. Coeficiente de determinacion. El R-squared
#El  Multiple R-squared indica el porcentaje que explica el modelo, esta cercano al 90%, no llega al 95%.
sfit2$r.squared

#Diagnosticos del modelo --------------

#-----1 Normalidad de los valores residuales.
par(mfrow=c(1,1))

e<- residuals(fit2)
d <- e / sfit2$sigma
hist(d,probability = T,xlab="Residuos",xlim = c(-3,3))
lines(d.seq,dnorm(d.seq,mean(d),sd(d)))
d.seq <- seq(-3,3,length=50)
#Con el grafico podemos ver que se asemeja a la normal. 
#Pero para salir de dudas, haremos el test de shapiro.
shapiro.test(e)
#Con un valor inferior a 0.05, podemos rechazar la hipotesis nula de que es normal.
#Asi que los valores residuales no siguen la distribucion normal.
#=>  No se cumple esta condicion de normalidad.

#-----2 Homocedasticidad  ,para cada valor de la variable independiente la varianza de los residuos
#es constante.
#Esto lo vamos a calcular con la funcion bptest, que tien como  hipotesis nula que hay homocedasticidad.
bptest(fit2)

#si el valor es > 0.05 no podriamos rechazar la hipoteses de homocedasticidad, como ha sido un valor
#muy pequeño, podemos rechazar la hipotesis, con lo que no se cumple la homocedasticidad.

#-----3 Incorrelacion. 
#Para ver la incorrelacion del modelo, lo haremos con el test de Durbin-Watson, que tiene como hipotesis nula
#que la correlacion es 0, es decir no hay correlacion.

dwtest(fit2, alternative = "two.sided")
#el p-vale es mayor que 0.05, no se puede rechazar la hipotesis nula. Lo que quiere decir que existe incorrelacion.

#En definitiva, el modelo NO es bueno. 

#-------- Aplica una transformacion a la regresion y analiza los resultados
# Vamos a transformar la variable respuesta (price)
# Para ello, utilizo la funcion boxcox.
bc <- boxCox(fit2,plotit = F)
lambda <- bc$x[which.max(bc$y)]
lambda
#Como lambda da un valor muy proximo a 0, hacemos la transformacion del logaritmo neperiano de la variable price.
Nprice <- log(dtMCut$price)

#Volvemos a calcular el modelo con la variable transformada.
fitT <- lm(Nprice~dtMCut$carat+dtMCut$color+dtMCut$clarity+dtMCut$depth+dtMCut$cut+dtMCut$x+dtMCut$y+dtMCut$z)
sfitT<-summary(fitT)
step(fitT) #Nos indica que la variable z no es representativa.
fitT <- lm(Nprice ~ dtMCut$carat + dtMCut$color + dtMCut$clarity +  dtMCut$depth + dtMCut$cut + dtMCut$x + dtMCut$y)
sfitT<-summary(fitT)
sfitT 
#Vemos que con este modelo, el multiple R squared ha mejorado con respecto al anterior, es decir
#hay un mayor porcentaje de error explicado. 


par(mfrow=c(2,2))
plot(fitT)

#la primera impresion despues de ver los graficos, es que el modelo esta mas ajustado que el anterior.

#----- 1. Calculo del error standar residual. 
redStanError <- sfitT$sigma
cv <- 100 * ( redStanError/ mean(Nprice)) #calculamos el coeficiente de variacion asociado.
cv > 10
# Si el coeficiente es mayor que 10, significa que hay mucha desviacion, el modelo no es bueno.

#----- 2. Anova, que contesta a la pregunta de si las variables pueden ser 0 o no ( que seria la hipotesis nula)
# Para ello nos fijamos en el pvalue de F-statistic,  en este caso, vemos que tiene un valor inferior a 
# 0.05, y es un valor muy pequeño, por lo que podemos rechazar la hipotesis nula, y ninguno de las variables
#puede valer 0. Lo que indicaria que el modelo puede ser bueno.

#----- 3. Coeficiente de determinacion. El R-squared
#El  Multiple R-squared indica el porcentaje que explica el modelo, esta cercano al 95%.
sfitT$r.squared

#Diagnosticos del modelo --------------

#-----1 Normalidad de los valores residuales.
par(mfrow=c(1,1))

e<- residuals(fitT)
d <- e / sfitT$sigma
hist(d,probability = T,xlab="Residuos",xlim = c(-3,3))
lines(d.seq,dnorm(d.seq,mean(d),sd(d)))
d.seq <- seq(-3,3,length=50)
#Con el grafico podemos ver que se asemeja a la normal. 
#Pero para salir de dudas, haremos el test de shapiro.
shapiro.test(e)
#Con un valor inferior a 0.05, podemos rechazar la hipotesis nula de que es normal.
#Asi que los valores residuales no siguen la distribucion normal.
#=>  No se cumple esta condicion de normalidad.

#-----2 Homocedasticidad  ,para cada valor de la variable independiente la varianza de los residuos
#es constante.
#Esto lo vamos a calcular con la funcion bptest, que tien como  hipotesis nula que hay homocedasticidad.
bptest(fitT)

#si el valor es > 0.05 no podriamos rechazar la hipoteses de homocedasticidad, como ha sido un valor
#muy pequeño, podemos rechazar la hipotesis, con lo que no se cumple la homocedasticidad.

#-----3 Incorrelacion. 
#Para ver la incorrelacion del modelo, lo haremos con el test de Durbin-Watson, que tiene como hipotesis nula
#que la correlacion es 0, es decir no hay correlacion.

dwtest(fitT, alternative = "two.sided")
#el p-vale es mayor que 0.05, no se puede rechazar la hipotesis nula. Lo que quiere decir que existe incorrelacion.

#EL Modelo no es bueno, aun transformado.

######probando un modelo mas simple.
#araceli

fitSS <- lm(dtMCut$x~dtMCut$y)
sfitSS<-summary(fitSS)
sfitSS 
#Vemos que con este modelo, el multiple R squared ha mejorado con respecto al anterior, es decir
#hay un mayor porcentaje de error explicado. 


par(mfrow=c(2,2))
plot(fitSS)

#la primera impresion despues de ver los graficos, es que el modelo esta mas ajustado que el anterior.

#----- 1. Calculo del error standar residual. 
redStanError <- sfitSS$sigma
cv <- 100 * ( redStanError/ mean(dtMCut$x)) #calculamos el coeficiente de variacion asociado.
cv > 10
# Si el coeficiente es mayor que 10, significa que hay mucha desviacion, el modelo no es bueno.

#----- 2. Anova, que contesta a la pregunta de si las variables pueden ser 0 o no ( que seria la hipotesis nula)
# Para ello nos fijamos en el pvalue de F-statistic,  en este caso, vemos que tiene un valor inferior a 
# 0.05, y es un valor muy pequeño, por lo que podemos rechazar la hipotesis nula, y ninguno de las variables
#puede valer 0. Lo que indicaria que el modelo puede ser bueno.

#----- 3. Coeficiente de determinacion. El R-squared
#El  Multiple R-squared indica el porcentaje que explica el modelo, esta cercano al 95%.
sfitSS$r.squared

#Diagnosticos del modelo --------------

#-----1 Normalidad de los valores residuales.
par(mfrow=c(1,1))

e<- residuals(fitSS)
d <- e / sfitSS$sigma
hist(d,probability = T,xlab="Residuos",xlim = c(-3,3))
lines(d.seq,dnorm(d.seq,mean(d),sd(d)))
d.seq <- seq(-3,3,length=50)
#Con el grafico podemos ver que se asemeja a la normal. 
#Pero para salir de dudas, haremos el test de shapiro.
shapiro.test(e)
#Con un valor inferior a 0.05, podemos rechazar la hipotesis nula de que es normal.
#Asi que los valores residuales no siguen la distribucion normal.
#=>  No se cumple esta condicion de normalidad.

#-----2 Homocedasticidad  ,para cada valor de la variable independiente la varianza de los residuos
#es constante.
#Esto lo vamos a calcular con la funcion bptest, que tien como  hipotesis nula que hay homocedasticidad.
bptest(fitSS)

#si el valor es > 0.05 no podriamos rechazar la hipoteses de homocedasticidad, como ha sido un valor
#muy pequeño, podemos rechazar la hipotesis, con lo que no se cumple la homocedasticidad.

#-----3 Incorrelacion. 
#Para ver la incorrelacion del modelo, lo haremos con el test de Durbin-Watson, que tiene como hipotesis nula
#que la correlacion es 0, es decir no hay correlacion.

dwtest(fitSS, alternative = "two.sided")
#el p-vale es mayor que 0.05, no se puede rechazar la hipotesis nula. Lo que quiere decir que existe incorrelacion.

#EL Modelo no es bueno, aun transformado.

