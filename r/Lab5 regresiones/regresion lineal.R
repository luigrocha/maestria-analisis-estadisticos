#Modelos de regresi�n lineal
#######################################
#INTRODUCCI�N
#######################################
# La regresi�n lineal (lm) es la herramienta fundamental de estad�stica 
#por un lado nos indica la l�nea de tendencia de una variable  y por otro lado
#nos permite predecir el valor de la variable de respuesta en base nuevos valores 
#de la variable predictora.
#En estad�stica  las variables toman nombres distintos, 
#variable predictora es aquella  que puede tomar cualquier valor,
#a veces se le llama tambi�n la variable independiente
#Y la variable dependiente aquella que toma un valor en base a la variable independiente 
#a veces llamada variable de respuesta.
#El modelo de regresi�n trata de encontrar una relaci�n matem�tica entre estas
#variables.
#Hagamos una prueba con el dataset father-son 
rm(list=ls())
require(tidyr)
require(reshape2)
require(UsingR)
require(ggplot2)
require(gridExtra)
data(father.son)
#Entender el dataset
names(father.son)
class(father.son)
NROW(father.son)
#Vemos las variables del dataset, muy simple indica la altura de un hijo en
#funci�n de la altura del padre. 
#Visualizemos la data 
ggplot (data=father.son , aes(x=fheight , y= sheight)) +
  geom_point()+
  xlab("Altura Padre")+
  ylab("Altura hijo")
#podemos tambi�n revisar la correlaci�n entre estas variables
cor(father.son)
#encontramos que el �ndice de correlaci�n es medio. 
#Podemos complementar el gr�fico con una l�nea de tendencia 
ggplot (data=father.son , aes(x=fheight , y= sheight)) +
  geom_point()+ 
  geom_smooth(method=lm)+
  xlab("Altura Padre")+
  ylab("Altura hijo")

#Notar primero la distribuci�n de los puntos.
#Segundo  la l�nea de tendencia calculada .
#Tercero una �rea sombreada alrededor de la l�nea de tendencia ,
# La banda gris alrededor de la l�nea muestra la incertidumbre en los ajustes lineales.

#Realicemos un boxplot para ver sus outliers
ggplot (data=father.son , aes(x=fheight , y= sheight)) +
  geom_boxplot()+
  xlab("Altura Padre")+
  ylab("Altura hijo")

#Podemos notar que tiene valores que pueden ser  outliers, 
#pero no sabemos exactamente si lo son,
#tampoco sabemos cuantos son, el diagrama de caja es enga�oso porque coloca todos 
#en el mismo eje.
ggplot (data=father.son , aes(x=fheight , y= sheight)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)+ geom_jitter()
#Ahora vemos que son varios.
#Vamos a crear una funci�n para eliminar los outliers.
#Definimos una funci�n 
is_outlier <- function(x) {
  #obtenemos el vector de cuantiles
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  #Sacamos el valor los cuantiles 1 y 3
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  #definimos los l�mites, los valores para base usados son 1.5 o  3, 
  #depende de que tan extremos queremos ser en el an�lisis (1.5 m�s exigente
  # 3 es menos exigente).
  base=1.5
  extreme.threshold.upper = (iqr * base) + upperq
  extreme.threshold.lower = lowerq - (iqr * base)
  #Creamos un vector l�gico que nos indica si es  outlier
  x > extreme.threshold.upper | x < extreme.threshold.lower
}
vect1 <- is_outlier(father.son$sheight)
#filtramos 
fs1 <- father.son$fheight[!vect1]
fs2 <- father.son$sheight[!vect1]


father_son_sin_outliers <-data.frame(fheight=fs1, sheight=fs2)
NROW(father_son_sin_outliers)
#Con esto validamos que no tenemos valores m�s all� de lo que se 
#define como outliers (con base= 3, pero si con 1.5)
#El siguiente  gr�fico nos indica mejor 
plot1<- ggplot (data=father.son , aes(x=fheight , y= sheight)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter()


plot2 <- ggplot (data=father_son_sin_outliers , aes(x=fheight , y= sheight)) +
  geom_boxplot()+
  geom_jitter()

grid.arrange(plot1, plot2, ncol=2)
#Vamos a crear  el modelo 
#La ecuaci�n de la recta de regresi�n  b=slope , a=Intercept e=epsilon 
padre_hijoLM <- lm(sheight ~fheight , data= father_son_sin_outliers)
class(padre_hijoLM)
names(padre_hijoLM)
padre_hijoLM$coefficients
padre_hijoLM$residuals
padre_hijoLM$fitted.values
summary(padre_hijoLM)

#Note que nos da el valor del intercept y el  slope b que es el fheight
#el primer dato es algo extra�o indicar�a que para un padre de 0 mtrs el hijo ya tiene un
#alto determinado, por esto en ANOVA ten�amos f�rmula -1 !!!. 
#Pr�ximo m�dulo explicaremos m�s de este tema 
#El segundo indica que por cada incremento del fheight el sheight tiene in incremento 
#
summary(padre_hijoLM)
#Nos da los siguientes estad�sticos:
#El valor de residuos (diferencia entre valor observado y valor calculado (fitted) 
#Los residuos  nos indican que tan bueno  es el modelo 
#Los coeficientes y error est�ndar 
#Ya veremos lo de r-cuadrado m�s adelante
# Los grados de libertad nos indica el n�mero de filas del modelo 

#Uno de los problemas con la regresi�n es que si la data contiene varios grupos 
#o factores y cada factor tiene una variabilidad con respecto a otros,
#el modelo no se puede ajustar perfectamente a todos.
#Esta variabilidad la validamos con ANOVA
#vamos a usar el dataset de tips porque en father.son no existen otros factores

#Analizando con ANOVA 
data(tips)
head(tips)
unique(tips$day)
#Hacemos una an�lisis de varianza 
tipsanova <- aov(tip~day-1 , data=tips)
tipsanova

#El indicador m�s importante es suma de cuadrados
#Sum =suma de cuadrados similar al numerador de la varianza indica que tan 
#bueno es el modelo, porque nos da un valor absoluto de la varianza de los residuos
#residuos grandes(mal ajuste) nos dar�n un valor alto.

#Ahora creamos el modelo para dataset tips
tipslm <-lm(tip~day -1 , data=tips)
class(tipslm)
summary(tipslm)


#Nos da mucha m�s informaci�n pero este caso esta desglosado por cada grupo 
#en este caso cada grupo seria un d�a distinto 

#Veamos otro ejemplo con la base de cars
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/cars.html
#speed(mpg dist(ft))
data("cars", package = "datasets")
head(cars)
#En este dataset speed es la velocidad del carro y dist el tiempo de frenado
model <- lm(dist ~ speed, data=cars)
model
#La variable model es un objeto que tiene entre sus propiedades  el intercept y pendiente.
#dist = -17.579 + 3.932*speed.

#Ahora vamos a predecir los valores observados, es decir, aplicaremos 
#la formula a las mismas velocidades observadas y vamos a comprar la distancia de 
#frenada observada con la "fitted"

#Veamos con el argumento de predict
pred.int <- predict(model, interval = "predict" ,level=.90)

#Ponemos todo en un data.frame para ver mejor 
mydata <- cbind(cars, pred.int)
mydata

#Graficamos 
ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)+
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")+
  ggtitle("Intervalo de Predicci�n")





#Ahora podemos predecir para nuevos valores para velocidades de 130,140 etc
new.speeds <- data.frame( speed = c(130,140,150,160))

predicciones <-predict(model, newdata = new.speeds, interval = "predict")
mydata1 <- cbind(new.speeds , predicciones)
mydata1
###################################################
#PARTE 2 
#TAREA DEL ALUMNO 
###################################################

#Enunciado : 
#  R provee es dataset  mtcars
#investigue sobre las variables de este dataset en
#https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
#Queremos desarrollar un modelo  para predecir el consumo de gasolina (mpg) como
#funcion de disp y otro modelo separado como funci�n de hp. 
#Para  esto deber� seguir lo pasos mencionados en la presentaci�n:
#  a) Diagramas de dispersi�n
#vamos a usar un dataset de  carros,
#primero cargamos  la data 
data(mtcars)
#Examinemos la data 
names(mtcars)
class(mtcars)
NROW(mtcars)
#Visualizemos la data 
graphdis1 <- ggplot (data=mtcars , aes(x=disp , y= mpg)) +
  geom_point()+
  xlab("desplazamiento")+
  ylab("millas/galón")
graphdis2 <- ggplot (data=mtcars , aes(x=hp , y= mpg)) +
  geom_point()+
  xlab("Potencia bruta")+
  ylab("millas/galón")
grid.arrange(graphdis1, graphdis2, ncol=2)
#  b) Establecer correlaci�n 
mtcars$cyl <- NULL
mtcars$wt <- NULL
mtcars$qsec <- NULL
mtcars$wt <- NULL
mtcars$am <- NULL
mtcars$vs <- NULL
mtcars$drat <- NULL
mtcars$gear <- NULL
mtcars$carb <- NULL
mtcars
cor(mtcars)
#  c) an�lisis de varianza con Anova
mtcarsanovadisp <- aov(mpg~disp-1 , data=mtcars)
mtcarsanovadisp
summary(mtcarsanovadisp)
mtcarsanovahp <- aov(mpg~hp-1 , data=mtcars)
mtcarsanovahp
summary(mtcarsanovahp)

#  d) revisi�n de outliers
graph1<-ggplot (data=mtcars , aes(x=disp , y= mpg, group = 1)) +
  geom_boxplot()+
  xlab("desplazamiento")+
  ylab("millas/galón")+
  ggtitle("Caja desplazamiento")
graph2<-ggplot (data=mtcars , aes(x=disp , y= mpg, group = 1)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)+ geom_jitter()+
  xlab("desplazamiento")+
  ylab("millas/galón")+
  ggtitle("Outliers desplazamiento")
grid.arrange(graph1, graph2, ncol=2)
graph3<-ggplot (data=mtcars , aes(x=hp , y= mpg, group = 1)) +
  geom_boxplot()+
  xlab("Potencia bruta")+
  ylab("millas/galón")+
  ggtitle("Caja Potencia")
graph4<-ggplot (data=mtcars , aes(x=hp , y= mpg, group = 1)) +
  geom_boxplot(outlier.colour="blue", outlier.shape=16,
               outlier.size=2, notch=FALSE)+ geom_jitter()+
  xlab("Potencia bruta")+
  ylab("millas/galón")+
  ggtitle("Outliers Potencia")
grid.arrange(graph3, graph4, ncol=2)
is_outlier <- function(x) {
  #obtenemos el vector de cuantiles
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  #Sacamos el valor los cuantiles 1 y 3
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  #definimos los l�mites, los valores para base usados son 1.5 o  3, 
  #depende de que tan extremos queremos ser en el an�lisis (1.5 m�s exigente
  # 3 es menos exigente).
  base=1.5
  extreme.threshold.upper = (iqr * base) + upperq
  extreme.threshold.lower = lowerq - (iqr * base)
  #Creamos un vector l�gico que nos indica si es  outlier
  x > extreme.threshold.upper | x < extreme.threshold.lower
}
vect1 <- is_outlier(mtcars$mpg)
#filtramos 
fs1 <- mtcars$disp[!vect1]
fs2 <- mtcars$mpg[!vect1]
fs3 <- mtcars$hp[!vect1]


mtcars_sin_outliers <-data.frame(disp=fs1, mpg=fs2, hp=fs3)
NROW(mtcars_sin_outliers)

plot1<- ggplot (data=mtcars , aes(x=disp , y= mpg)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter()+
  xlab("Desplazamiento")+
  ylab("millas/galón")


plot2 <- ggplot (data=mtcars_sin_outliers , aes(x=disp , y= mpg)) +
  geom_boxplot()+
  geom_jitter()+
  xlab("Desplazamiento")+
  ylab("millas/galón")

plot3<- ggplot (data=mtcars , aes(x=hp , y= mpg)) +
  geom_boxplot(outlier.colour="blue", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter()+
  xlab("Potencia bruta")+
  ylab("millas/galón")


plot4 <- ggplot (data=mtcars_sin_outliers , aes(x=hp , y= mpg)) +
  geom_boxplot()+
  geom_jitter()+xlab("Potencia bruta")+
  ylab("millas/galón")

grid.arrange(plot1, plot2,plot3, plot4, ncol=2)

#  e) Generaci�n del modelo
model1 <- lm(mpg ~ disp, data=mtcars)
model1
class(model1)
names(model1)
model1$coefficients
model1$residuals
model1$fitted.values
summary(model1)
#Veamos con el argumento de predict
pred.int <- predict(model1, interval = "predict" ,level=.90)
pred.int
#Ponemos todo en un data.frame para ver mejor 
mydata1 <- cbind(mtcars, pred.int)
mydata1
#Graficamos 
ggplot(mydata1, aes(disp, mpg)) +
  geom_point() +
  stat_smooth(method = lm)+
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")+
  ggtitle("Intervalo de Prediccion")+
  xlab("Desplazamiento")+
  ylab("millas/galon")
#Ahora podemos predecir para nuevos valores para velocidades de 130,140 etc
new.disp <- data.frame( disp = c(130,140,150,160))

predicciones <-predict(model1, newdata = new.disp, interval = "predict")
mydata2 <- cbind(new.disp , predicciones)
mydata2
#####################################################################
model2 <- lm(mpg ~ hp, data=mtcars)
model2
class(model2)
names(model2)
model2$coefficients
model2$residuals
model2$fitted.values
summary(model2)
#Veamos con el argumento de predict
pred.int2 <- predict(model2, interval = "predict" ,level=.90)
pred.int2
#Ponemos todo en un data.frame para ver mejor 
mydata3 <- cbind(mtcars, pred.int2)
mydata3
#Graficamos 
ggplot(mydata3, aes(hp, mpg)) +
  geom_point() +
  stat_smooth(method = lm)+
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")+
  ggtitle("Intervalo de Prediccion")+
  xlab("Potencia")+
  ylab("millas/galon")
#Ahora podemos predecir para nuevos valores para velocidades de 140,100 etc
new.disp2 <- data.frame( disp = c(140,150,170,100))

predicciones <-predict(model2, newdata = new.disp2, interval = "predict")
mydata3 <- cbind(new.disp2 , predicciones)
mydata3
#  f) Analisis del proceso y resultados

#En el punto f, el alumno debe hacer el an�lisis de los resultados y diagramas 
#explicando que se est� presentado. 
#Se sugiere que utilice las funciones de graficaci�n y solo haga cambios de variables