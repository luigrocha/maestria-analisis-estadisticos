#LAB ESTADISTICOS PRINCIPALES
#######################################
#Vamos a usar el dataset midwest, contiene una serie de estad?sticas interesantes 
#de la poblacion de los USA . 
#Mas informaci?n en https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/midwest
#Cargamos el dataset con : 

install.packages('ggplot2')
??ggplot2
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
data(midwest)
#Para referirnos a los campos (columnas)  como variables podemos usar el comando:
attach(midwest)
#Para obtener la clase de objeto que es usaremos:
class(midwest)
#El dataset es de tipo tibble, una subclase de dataframe pero tiende a "quejarse" mucho 
#prefiero verlo como data.frame, para esto uso un cast
mid <- as.data.frame(midwest)
class(mid)
#ahora si, veamos que columnas tiene
names(mid)
#veamos cuantas filas tiene el dataset 
NROW(mid)
#Primero me interesa saber cual es la media de los estados respecto
#a personas con educaci?n superior:
mean(mid$percollege)
#Note el signo de $ indica la columna del dataset
#Analicemos cual es el m?nimo y m?ximo de esta columna
max(mid$percollege)
min(mid$percollege)
#Nos llama la atenci?n el m?nimo, me interesa saber en que estado se 
#produce este m?nimo. Utilizaremos subsetting.
mid[mid$percollege==min(mid$percollege),"state" ]
mid[mid$percollege>= 40,"state" ]
mid[mid$percollege <= 20,"state" ]
# Subsetting nos permite ver exactamente filas y columnas de inter?s.
#El formato b?sico es : 
#dataset[filas, columnas]
#Pero podemos poner tambi?n condicionales como en el ejemplo: 
#Si nos interesa una cantidad de filas y/o colunas podemos usar vectores en su lugar
vector_cols<- c("state", "poptotal", "popdensity","percblack","percwhite","percasian", "percother","percamerindan") 
mid[mid$percollege==min(mid$percollege),vector_cols]
#Podemos concluir que el m?nimo se obtiene en un estado donde la poblaci?n
#IndoAmericana es mayoritaria. 
#Analicemos la densidad poblacional de estos estados, nos interesa saber
#la moda
install.packages("modeest")
require(modeest)
moda_densidad<- mlv(mid$popdensity, method="mfv")
moda_densidad
#Nos interesa saber que tan dispersa est?  la densidad poblacional,
#habria que revisar de cuantos estados tenemos informaci?n
unique(mid$state)
var(mid$popdensity)

#calculamos manuamente  a ver si coincide, note la correccion de Bessel 
var1 <- (sum( (mid$popdensity - mean(mid$popdensity) )^2)  /(NROW(mid)-1))
var1
sqrt(var1)
sd(mid$popdensity)
#podemos ver gr?ficamente la distribucion de la densidad de la poblacion 
require(ggplot2)
ggplot(mid , aes(x=popdensity)) +
  geom_density()

#Nos sorprende este n?mero tan alto, veamos los extremos 
max(mid$popdensity)
min(mid$popdensity)
#Entonces concluimos que nuestra poblaci?n est? muy dispersa, 
#tenemos estados muy poblados y otros muy poco
sd(mid$popdensity)
#queremos analizar la distribuci?n de poblaci?n blanca por condado
#no se preocupe mucho si no entiende la l?gica de la funci?n, 
#veremos m?s en detalle cuando veamos  distribuci?n
vect_cuantiles <- c(0.25 , 0.5, 0.75 )
quantile(mid$percwhite, probs=vect_cuantiles)
#esto nos indica que el cuantil m?s bajo  ya es de 94% de poblacion blanca 

#O la distribucion de adultos 
vect_cuantiles <- c(0.10 , 0.25, 0.50, .9 )
quantile(mid$percwhite, probs=vect_cuantiles)
quantile(mid$percblack)

#Podriamos ver estos quantiles en forma  gr?fica 
qwhite <- quantile(mid$percwhite, probs=vect_cuantiles)
qwdf<- as.data.frame(qwhite)
class(qw)
print(qwhite)

ggplot(qwdf, aes(x=qwhite, y=vect_cuantiles)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+coord_flip()



#Nos interesa saber cu?ntos condados tiene poblaci?n de adultos 
#mayores en situaci?n pobreza
count(mid[mid$percelderlypoverty>=10,"county"])
summary(mid$poptotal)




#CORRELACION 
#=====================================
require(ggplot2)
require(corrplot)
require(reshape2)
require(scales)

#vamos a usar un dataset de  carros,
#primero cargamos  la data 
data(mtcars)
#Examinemos la data 
names(mtcars)
class(mtcars)
#Usamos subsetting para  que no salga una matriz de todas las columnas 
#contra todas las columnas. Seleccionamos solo las priemras 4 
#El estudiante podria seleccionar otras de su inter?s 
#1.
mtcars
apply(mtcars, 2, median)
mean(mpg)
summary(mtcars)
median(mtcars$mpg)

max(mtcars$hp)
min(mtcars$qsec)

var(mtcars$disp, na.rm = FALSE)

sd(mtcars$disp, na.rm = FALSE)
quantile(mtcars$wt,probs = seq(0, 1, 1/7))
#head(mtcars$carb)
head(mtcars, c(1L,11L))
v = sqrt(var(mtcars$disp))
v
head(mtcars)
head(mtcars$carb, 1)
tail(mtcars)
tail(mtcars$gear)
head(tail(mtcars$gear),1)
runif(mtcars, 1,7)
cor(mtcars[,1:4])
#Analice los resultados. Indique cuales son las variables que de alta correlaci?n




#Veamos otro ejemplo con el dataset economics
#Cargamos la data 
data(economics)
#Analizamos la data 
class(economics)
NROW(economics)
names(economics)
head(economics)
# las columnas son : fecha , 
#date Month of data collection
#pce personal consumption expenditures, in billions of dollars, http://research.stlouisfed.org/fred2/series/PCE
#pop total population, in thousands, http://research.stlouisfed.org/fred2/series/POP
#psavert personal savings rate, http://research.stlouisfed.org/fred2/series/PSAVERT/
# uempmed median duration of unemployment, in weeks, http://research.stlouisfed.org/fred2/series/UEMPMED
#unemploy number of unemployed in thousands, http://research.stlouisfed.org/fred2/series/UNEMPLOY

#queremos ver si estas variables estan relacionadas 

cor(economics$pce, economics$psavert)
#Que indica este valor? 
cor(economics[,c(2, 4:6)])
#Que indica esta matriz?
cor_economics <-cor(economics[,c(2, 4:6)])
cor_economics
#esto se ve mejor en un mapa de calor ,pero para esto habr? que  poner en un
#una sola columna , como est? ggplot no lo puede visualizar adecuadamente 

econ_melted <- melt(cor_economics, varname=c("X","Y"), value.name="Correlacion")
econ_melted
class(econ_melted)
#ahora le ploteamos como heat map
#antes le ordenamos para que nos salga bonito el grafico
econ_melted <- econ_melted[order(econ_melted$Correlacion),]
econ_melted <- econ_melted[order(Correlacion, decreasing = TRUE)]

ggplot(econ_melted, aes(x=X , y=Y)) + 
  geom_tile(aes(fill=Correlacion))+
  scale_fill_gradient2(low=muted("steelblue"), mid="white" , high="red")+
  theme_minimal() +
  labs(x=NULL , y=NULL)

#Un problema recurrente al momento de correlacionar son los 
#datos con NA, hay opciones para esto 

#cor(df ,use="_______")
# donde el parametro puede ser :
#everything : usa las filas completas no las NAs
#all.abbs todas las filas deben estar completas o dar? un error
#na.or.complete usa todo pero retornar varios  NAs
#pairwise.complete.obs  las 2 filas relacionadas deben estar presentes y sin nas

#VEAMOS UN EJEMPLO DE CORRELACION  ENTRE MATRICES 

mat1 <- c(9,9,NA,3,NA,5,8,1,10,4)
mat2 <- c(2,NA,1,6,6,4,1,1,6,7)
mat3 <- c(8,4,3,9,10,NA,3,NA,9,9)
mat4 <- c(10,10,7,8,4,2,8,5,5,2)
mat <- cbind(mat1,mat2,mat3,mat4)
cor(mat)
cor(mat , use="everything")
cor(mat , use="all.obs")
cor(mat , use="complete.obs")
cor(mat , use="na.or.complete")
cor(mat , use="pairwise.complete.obs")

#correlation no implica causaci?n
#PROBLEMA 1:
#Dado el dataset mtcars encuentre relacionaciones entre variables que indiquen 
#una correlaci?n aunque esto no indicque cuasaci?n 


#PROBLEMA 2
#Dado el siguiente dataset, cargue la data, entienda la data, 
housing <- read.csv(url("https://www.jaredlander.com/data/housing1.csv"))
#Encuentre las correlaciones entre las variables num?rica, indique
#que relaciones son fuertes y cuales son d?biles, indique si hace sentido 

p3 <- c(16.1, 14.3, 16.3, 15.3, 14.9, 14.9, 14.7, 15.5, 13.7, 17.2 )
p3
max(p3)
min(p3)
median(p3)

Vendedor1= c(986, 1337, 2745, 2645, 3658, 1265, 734, 245, 5344, 4867) 
Vendedor2= c(645, 645, 734, 822, 893, 230, 415, 723) 
Vendedor3= c(534,534, 324 ,534, 534, 534, 534, 534) 
median(Vendedor1)
median(Vendedor2)
median(Vendedor3)


library(psych)

pairs.panels(economics[,2:6])

plot(economics$unemploy,economics$pce)


library(tidyr)
cov(economics$unemploy,economics$uempmed)

cov(economics$uempmed,economics$unemploy)
