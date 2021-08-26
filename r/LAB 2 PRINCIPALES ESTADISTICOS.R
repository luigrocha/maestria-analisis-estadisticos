#LAB ESTADISTICOS PRINCIPALES
#######################################
#Vamos a usar el dataset midwest, contiene una serie de estadísticas interesantes 
#de la poblacion de los USA . 
#Mas información en https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/midwest
#Cargamos el dataset con : 
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
#a personas con educación superior:
mean(mid$percollege)
#Note el signo de $ indica la columna del dataset
#Analicemos cual es el mínimo y máximo de esta columna
max(mid$percollege)
min(mid$percollege)
#Nos llama la atención el mínimo, me interesa saber en que estado se 
#produce este mínimo. Utilizaremos subsetting.
mid[mid$percollege==min(mid$percollege),"state" ]
mid[mid$percollege>= 40,"state" ]
mid[mid$percollege <= 20,"state" ]
# Subsetting nos permite ver exactamente filas y columnas de interés.
#El formato básico es : 
#dataset[filas, columnas]
#Pero podemos poner también condicionales como en el ejemplo: 
#Si nos interesa una cantidad de filas y/o colunas podemos usar vectores en su lugar
vector_cols<- c("state", "poptotal", "popdensity","percblack","percwhite","percasian", "percother","percamerindan") 
mid[mid$percollege==min(mid$percollege),vector_cols]
#Podemos concluir que el mínimo se obtiene en un estado donde la población
#IndoAmericana es mayoritaria. 
#Analicemos la densidad poblacional de estos estados, nos interesa saber
#la moda
require(modeest)
moda_densidad<- mlv(mid$popdensity, method="mfv")
moda_densidad
#Nos interesa saber que tan dispersa está  la densidad poblacional,
#habria que revisar de cuantos estados tenemos información
unique(mid$state)
var(mid$popdensity)

#calculamos manuamente  a ver si coincide, note la correccion de Bessel 
var1 <- (sum( (mid$popdensity - mean(mid$popdensity) )^2)  /(NROW(mid)-1))
var1
sqrt(var1)
sd(mid$popdensity)
#podemos ver gráficamente la distribucion de la densidad de la poblacion 
ggplot(mid , aes(x=popdensity)) +
  geom_density()

#Nos sorprende este número tan alto, veamos los extremos 
max(mid$popdensity)
min(mid$popdensity)
#Entonces concluimos que nuestra población está muy dispersa, 
#tenemos estados muy poblados y otros muy poco
sd(mid$popdensity)
#queremos analizar la distribución de población blanca por condado
#no se preocupe mucho si no entiende la lógica de la función, 
#veremos más en detalle cuando veamos  distribución
vect_cuantiles <- c(0.25 , 0.5, 0.75 )
quantile(mid$percwhite, probs=vect_cuantiles)
#esto nos indica que el cuantil más bajo  ya es de 94% de poblacion blanca 

#O la distribucion de adultos 
vect_cuantiles <- c(0.10 , 0.25, 0.50, .9 )
quantile(mid$percwhite, probs=vect_cuantiles)
quantile(mid$percblack)

#Podriamos ver estos quantiles en forma  gráfica 
qwhite <- quantile(mid$percwhite, probs=vect_cuantiles)
qwdf<- as.data.frame(qwhite)
class(qw)
print(qwhite)

ggplot(qwdf, aes(x=qwhite, y=vect_cuantiles)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  coord_flip()



#Nos interesa saber cuántos condados tiene población de adultos 
#mayores en situación pobreza
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
#El estudiante podria seleccionar otras de su interés 

cor(mtcars[,1:4])
#Analice los resultados. Indique cuales son las variables que de alta correlación


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
#esto se ve mejor en un mapa de calor ,pero para esto habrá que  poner en un
#una sola columna , como está ggplot no lo puede visualizar adecuadamente 

econ_melted <- melt(cor_economics, varname=c("X","Y"), value.name="Correlacion")
econ_melted
class(econ_melted)
#ahora le ploteamos como heat map
#antes le orenamos para que nos salga bonito el grafico
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
#all.abbs todas las filas deben estar completas o dará un error
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

#correlation no implica causación
#PROBLEMA 1:
#Dado el dataset mtcars encuentre relacionaciones entre variables que indiquen 
#una correlación aunque esto no indicque cuasación 


#PROBLEMA 2
#Dado el siguiente dataset, cargue la data, entienda la data, 
housing <- read.csv(url("https://www.jaredlander.com/data/housing1.csv"))
#Encuentre las correlaciones entre las variables numérica, indique
#que relaciones son fuertes y cuales son débiles, indique si hace sentido 
