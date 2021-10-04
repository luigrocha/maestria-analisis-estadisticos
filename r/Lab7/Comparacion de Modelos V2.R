#LAB-COMPARACION DE MODELOS V2
#PARTE 1 DEMOSTRACI�N 
#En este laboratorio revisaremos algunos estimadores usados en 
#la selecci�n de modelos.
###############################################################
#COMPARACI�N DE MODELOS
###############################################################
rm(list=ls())
require(ggplot2)
require(coefplot)
require(reshape2)
require(boot)
#Bajar el archivo de https://www.jaredlander.com/data/
#Cargamos el dataset housing NY
housing <- read.table("https://www.jaredlander.com/data/housingNew.csv" , sep=",", header=TRUE, stringsAsFactors=FALSE)
#Examinamos el dataset
names(housing)
class(housing)
#Creamos un modelo con estas variables predictoras Units, SqFt y Boro
housingmod1 <- lm(ValuePerSqFt ~ Units +SqFt +Boro , data=housing )
#Examinamos el modelo 
class(housingmod1)
summary(housingmod1)
#El estudiante ya puede revisar los principales estad�sticos del modelo 
#Veamos los coeficientes en forma gr�fica
coefplot(housingmod1)
#coefplot nos dibuja un gr�fico donde el punto representa el coeficiente
#la l�nea gruesa representa el intervalo de confianza de valor de 1 
#standard error y la l�nea delgada el intervalo de confianza de
#2 standard errors
#Si se desea "jugar" con los datos del modelo debemos  extraerlos a un 
#data.frame, para  esto podemos usar fortify o tidy

names(fortify(housingmod1))
#Esto es similar a lo hecho en el laboratorio de regresi�n  lineal, pero
#sin tener todos los campos, solo los que nos interesan 
head(fortify(housingmod1))

#En la presentaci�n mencion�bamos que: SST=SSE+SSR
#Importante notar que  errores y residuos son dos cosas distintas.  
#Est�n estrechamente relacionadas y f�cilmente confundidas de la desviaci�n 
#de un valor observado 
#El residuo de un valor observado es la diferencia entre valor observado
#y el valor estimado con el an�lisis de regresi�n.
#Donde los conceptos a veces se denominan errores de regresi�n y residuales 
#de regresi�n y donde conducen al concepto de residuales studentizados.
#Pero, el error (o perturbaci�n) de un valor observado es la desviaci�n del valor
#observado del valor verdadero (no observable) de una cantidad de inter�s
#En general mientras los residuos sean mayores  el modelo es de inferior 
#calidad.

#Veamos un plot de residuos
ggplot (data=housingmod1 , aes(x=.fitted , y=.resid))+
  geom_point() +
  geom_hline(yintercept=0)+
  geom_smooth(aes(color="regresion"),se=FALSE)+
  labs(x="Valor calculado", y="residuals")

#El primer an�lisis aqu� es que el gr�fico no parece que la data tiene 
#una distribuci�n normal, m�s bien una distribuci�n bimodal
#Investigamos un poco m�s cambiando el color seg�n el factor Boro
ggplot (data=housingmod1 , aes(x=.fitted , y=.resid , color=Boro))+
  geom_point() +
  geom_hline(yintercept=0)+
  geom_smooth(aes(color="regresion"),se=FALSE)+
  labs(x="Valor Ajustado", y="residuos")
#Es claro que tenemos 2 grupos
#Veamos una comparaci�n de modelos, creamos varios modelos
housingmod1 <- lm(ValuePerSqFt ~ Units +SqFt +Boro , data=housing )
housingmod2 <- lm(ValuePerSqFt ~ Units * SqFt +Boro , data=housing )
housingmod3 <- lm(ValuePerSqFt ~ Units +SqFt *Boro + Class, data=housing )
housingmod4 <- lm(ValuePerSqFt ~ Units +SqFt +Boro + SqFt*Class , data=housing )
housingmod5  <- lm(ValuePerSqFt ~ Boro + Class , data=housing )
#Comparamos los coeficientes de los modelos.
#El alumno debe encontrar que coeficientes que crea  importantes
#basado en el criterio de 2 SE sobre el valor 0 

multiplot(housingmod1, housingmod2,housingmod3,housingmod4,housingmod5)

#COMPARACI�N CON ANOVA 
###############################
#Para hacer algo m�s matem�tico podemos usar ANOVA .
#ANOVA lo vimos en an�lisis de varianza , ahora lo utilizamos para ver 
#la variabilidad  de los modelos
#En este caso usamos la funci�n ANOVA en lugar de aov porque tenemos m�ltiples
#variables multiples archivos 
anova(housingmod1, housingmod2,housingmod3,housingmod4,housingmod5)
#Uno de los factores que nos da ANOVA es RSS  residual sum of squares (SSR en la presentaci�n),
#RSS indica que porcentaje del valor de variable de respuesta  NO est� explicada
#por el modelo (predictoras). Por lo tanto queremos el modelo que tenga el menor valor 
#De la salida anterior el menor RSS corresponde al modelo 4

#El problema es que cuando aumentamos la cantidad de factores  ANOVA 
#empieza a caer en overfitting y habr� que penalizar los valor que saca 
#Como resultado del sobreajuste puede fallar en la predicci�n.

#Para corregir este comportamiento de  ANOVA existen 2  funciones que
#nos ayudar�n:
#AIC y BIC, estas dos funciones nos entregan  unos estimadores que contienen correcciones al #RSS para darnos un RSS real 
AIC(housingmod1, housingmod2,housingmod3,housingmod4,housingmod5)
BIC (housingmod1, housingmod2,housingmod3,housingmod4,housingmod5)
#AIC se calcula en base al  algoritmo "log-likelihood" o probabilidad-logaritmica
#AIC estima la cantidad relativa de informaci�n perdida por un modelo dado: 
#Cuanta menos informaci�n pierde un modelo, mayor es la calidad de ese modelo.
#AIC estima la cantidad de informaci�n perdida por un modelo, 
#AIC establece un equilibrio entre la bondad de ajuste del modelo y
#la simplicidad del modelo. 
#En otras palabras, AIC se ocupa tanto del riesgo de sobreajuste como del riesgo 
#de falta de ajuste.
#BIC es similar 
#Podemos usar cualquiera de estos ajustes para escoger  el mejor modelo 
#que es el que tiene el estimador de  menor.
#Grafiquemos 
dfaic <-AIC(housingmod1, housingmod2,housingmod3,housingmod4,housingmod5)
dfbic <-BIC(housingmod1, housingmod2,housingmod3,housingmod4,housingmod5)
dfaic$BIC <- dfbic$BIC
min_val_AIC <-min(dfaic$AIC)
min_var_AIC =dfaic[dfaic$AIC==min_val_AIC,1]
min_val_BIC <-min(dfaic$BIC)
min_var_BIC =dfaic[dfaic$BIC==min_val_BIC,1]



ggplot (dfaic, aes(x=df))+
  geom_point(aes(y=AIC, color=AIC), color="blue")+
  geom_line(aes(y=AIC, color=AIC))+
  geom_point(aes(y=BIC, color=BIC), color="red")+
  geom_line(aes(y=BIC, color=BIC))+
  labs(title="AIC/BIC" )+
  ylab("AIC en Azul/BIC en Rojo Value")+
  xlab("Num Variables")+
  geom_point(aes(x=min_var_AIC, y=min_val_AIC, size=4), color="blue")+
  geom_point(aes(x=min_var_BIC, y=min_val_BIC, size=4), color="red")      

#Vemos que 8 o 9 variables nos da los m�nimos estimadores

#VALIDACI�N POR GLM DEVIANCE 
##########################################################
#Otra opci�n es usar glm.
#La funci�n lm() se usa porque es simple y se usa con bastante frecuencia,
#glm tiene la ventaja de poder manejar otros tipos de distribuciones.
#Para distribuci�n gaussiana se obtienen los mismos resultados que lm .
#
#Obtendr� la misma respuesta que lm, pero la diferencia t�cnica es que glm usa 
#un modelo de probabilidad  mientras que lm usa m�nimos cuadrados para encontrar
#la recta de regresi�n.
#Cada distribucion tiene una family y una funci�n  de enlace ,esta funci�n de enlace
#es la que relaciona la variable(s) X con Y 
# Una funci�n de enlace transforma las probabilidades de los niveles de una variable de respuesta categ�rica a una escala continua que es ilimitada

#Las opciones de family son : 
#Family 	Default Link Function
#binomial 	(link = "logit")
#gaussian 	(link = "identity")
#Gamma 	(link = "inverse")
#inverse.gaussian 	(link = "1/mu^2")
#poisson 	(link = "log")
#quasi 	(link = "identity", variance = "constant")
#quasibinomial 	(link = "logit")
#quasipoisson 	(link = "log")



#Veamos un ejemplo con distribuci�n binomial 

#El argumento family es lo que en la presentaci�n llamamos la funci�n de enlace o funci�n link
#Creamos una columna adicional 
housing$exclusivo<-housing$ValuePerSqFt>=150

h1 <- glm(exclusivo ~ Units +SqFt +Boro , data=housing, family=binomial(link="logit") )
h2 <- glm(exclusivo ~ Units * SqFt +Boro , data=housing ,family=binomial(link="logit") )
h3 <- glm(exclusivo ~ Units +SqFt *Boro + Class, data=housing ,family=binomial(link="logit") )
h4 <- glm(exclusivo ~ Units +SqFt +Boro + SqFt*Class , data=housing ,family=binomial(link="logit") )
h5  <- glm(exclusivo ~ Boro + Class , data=housing,family=binomial(link="logit") )

#En glm el factor de comparaci�n se llama  deviance,
#GLM produce 2 deviances:
#Null deviance: La desviaci�n nula muestra qu� tan bien 
#la variable de respuesta es predicha por un modelo que incluye solo la 
#intercepci�n .
#Y el residual deviance de un modelo como el doble de la probabilidad 
#logar�tmica negativa m�s una constante. 
#Esta es una definicion complicada , mas facil :
#La desviaci�n residual es la diferencia entre la desviaci�n del modelo actual
#y la desviaci�n m�xima del modelo ideal donde los valores predichos son 
#id�nticos a los observados. POr lo tanto es el mejor indicador y esperamos que sea
#bajo.Si no es bajo indicar� que los predictores no son los adecuados




#El deviance se usa para comparar dos modelos diferentes, como consecuencia, 
#el deviance siempre es mayor o igual que cero, siendo cero solo si el 
#ajuste del modelo es perfecto.

comp_glm <- data.frame(H1=h1$deviance,H2=h2$deviance,H3=h3$deviance,H4=h4$deviance,H5=h5$deviance)
#Grafiquemos 
comp_melted <-melt(comp_glm)
names(comp_melted) <- c("model", "Deviance")
names(comp_melted)

ggplot(comp_melted, aes( model, Deviance , fill=model)) +
  geom_col()+
  labs(title="Mejor Modelo Deviance m�s bajo")
#Concluimos que si lo que deseo es un modelo para los  departamentos "de lujo"
#(para eso creamos la variable exclusivo) los modelo 1,2,5 no son adecuados

#CROSS VALIDACION 
###############################################
#Un buen link para CV es http://www.milanor.net/blog/cross-validation-for-predictive-analytics-using-r/
#La funci�n CV se utiliza para medir la diferencia de variabilidad de dos series 
#y se define como (sd/mean)*100
#La validaci�n cruzada proporciona una estimaci�n del error de prueba para cada modelo
#La validaci�n cruzada es uno de los m�todos m�s utilizados para la selecci�n del 
#modelo y para elegir los valores de los par�metros de ajuste.
#El par�metro K (conocido como K-fold indica la cantidad de sets de datos con los 
#que validar� el modelo), K=5 es un punto medio, validar� con 5 sets )
require(caret)
# Este paquete contiene una serie de funciones para crear particiones de la data 
# createDataPartition mientras que createResample crea una o m�s muestras de bootstrap
#createFolds divide los datos en k grupos, mientras que createTimeSlices crea una divisi�n
#de validaci�n cruzada para los datos de la serie. 
#groupKFold divide los datos seg�n un factor de agrupaci�n. 
#Refrescamos la data 
housing <- read.table("c:\\users\\alfonso\\Documents\\housingNew.csv" , sep=",", header=TRUE, stringsAsFactors=FALSE)
#creamos el modelo con glm()
g1 <- glm(ValuePerSqFt ~ Units +SqFt +Boro , data=housing, 
          family=gaussian(link="identity") )
g1
#Ejecutamos la cross validacion 
houseCV1 <- cv.glm(housing, g1, K=5)
class(houseCV1)
names(houseCV1)
#El coeficiente que nos interesa se llama delta
#Delta es un vector de longitud dos. El primer componente es el  error de
#predicci�n. Es el error cuadr�tico medio promedio MSE que obtiene al hacer K-veces CV. 
#El segundo componente es un ajuste por el k-fold, y se calcula asi:
#inicialmente, la suma de cuadrados residual (RSS) se calcula bas�ndose en los valores 
#predichos de GLM y los valores de respuesta reales para todo el conjunto de datos.
#A medida que recorre los K pliegues, genera un modelo de entrenamiento y luego calcula
#el RSS entre todo el conjunto de datos de valores y (no solo el conjunto de entrenamiento)
#y los valores predichos del modelo de entrenamiento. Estos valores RSS resultantes luego 
#se restan del RSS inicial. Una vez que haya terminado de revisar sus K pliegues, 
#habr� restado los valores de K del RSS inicial. Este es el segundo componente de delta. 



#Con estos objetos crearemos un data.frame  y lo podemos visualizar
#Seleccionaremos el modelo con el menor valor de error
houseCV1$delta
#Hagamos con m�ltiples modelos
g1 <- glm(ValuePerSqFt ~ Units +SqFt +Boro , data=housing, family=gaussian(link="identity") )
g2 <- glm(ValuePerSqFt ~ Units* SqFt+Boro , data=housing, family=gaussian(link="identity") )
g3 <- glm(ValuePerSqFt ~ Units + SqFt*Boro +Class, data=housing, family=gaussian(link="identity") )
g4 <- glm(ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class, data=housing, family=gaussian(link="identity") )
g5 <- glm(ValuePerSqFt ~ Boro + Class, data=housing, family=gaussian(link="identity") )
houseCV1 <- cv.glm(housing, g1,  K=5)
houseCV2 <- cv.glm(housing ,g2 , K=5)
houseCV3 <- cv.glm(housing ,g3 , K=5)
houseCV4 <- cv.glm(housing ,g4 , K=5)
houseCV5 <- cv.glm(housing ,g5 , K=5)

#Hay dos algoritmos para esta division : K-folds  y LOOCV
#En kfolds  la data se divide en k partes, aleatoriamente las observaciones van a cada fold
# pero no necesariamente todos los folds tienen igual cantidad de  observaciones ya que es random
#entonces tenemos la data dividida en K partes,luego entra en un lazo y hace un c�lculo del
#error usando MSE para todos los datos menos el del 1er fold (MSE es suma residuos al cuadrado )
#Y esto ponderado por la cantidad de observaciones del fold ,luego continuamos asi K veces . 
#Dado que este error de validaci�n cruzada es solo un promedio,
#el error est�ndar de ese promedio tambi�n nos da un error est�ndar de la estimaci�n de
#validaci�n cruzada 

#En LOOCV la cantidad de folds es la misma cantidad de observaciones K=N, training es con todas
#la data menos 1 observacion 
#Esto nos da el "error validado en forma cruzada" entre distintos sets y el valor ajustado del mismo, 
#entonces con K=1 deja 1 afuera  (leaves-out) para testing y prueba con el modelo con el resto,
#luego vuelve a probar el modelo con k-2   dejando 2 para testing ,
#una vez completada las K iteraciones nos presenta cual es el deviance normal(glm) y cual ser�a 
#el ajustado con los k-folds que  seria el valor  de la derecha
#
#Note que si utilizamos un K diferente  podemos obtener distintos resultados
#Note : Cuando los datos tienen una variable de factor, puede darse el caso de que una 
#observaci�n grupal de muestreo aleatorio no cubra todos los niveles de factor. 
#En este caso, se produce un error. Puede intentarlo de nuevo hasta que tenga �xito: 

#sea cual sea el algoritmo el delta[1] es esta SME
#El delta[2] es una correccion que se hace porque no se ha tomado en cuenta todos los datos
#para el entrenamiento.

#Vamos a crear un df para  comparar los valores de CV y ANOVA, AIC , BIC
cvdataframe = as.data.frame(rbind(houseCV1$delta,houseCV2$delta,houseCV3$delta
                                  ,houseCV4$delta,houseCV5$delta))
#write.csv(cvdataframe,"cvdataframe.csv", row.names = FALSE)


names(cvdataframe)
names(cvdataframe)  <-c("XVal_pred_error", "XVal_pred_error_adj")
cvdataframe$Model <- sprintf("houseG%s" , 1:5)
head(cvdataframe)
# De acuerdo a los resultados el modelo de m�s bajos valores es el que m�s se ajusta
ggplot(data=cvdataframe, aes(x=Model , y=XVal_pred_error, color="XVal_pred_error")) + 
  geom_point()+
  geom_point(aes(x=Model , y=XVal_pred_error_adj , color="XVal_pred_error Ajustado")) +
  geom_segment(aes(x =Model , y = XVal_pred_error, xend = Model, yend = XVal_pred_error_adj), data = cvdataframe)


#Podemos hacer una comparacion   con ANOVA 
cvANOVA <- anova(g1,g2,g3,g4,g5)
cvANOVA
#Obtenemos el estimador  AIC 
cvAIC <- AIC(g1,g2,g3,g4,g5)
cvAIC
#Obtenemos el estimador  BIC 
cvBIC <-BIC(g1,g2,g3,g4,g5)
cvBIC
#Vamos a incluirlos en la misma tabla para ver como se ven 
cvdataframe$ANOVA <- cvANOVA$`Resid. Dev`
cvdataframe$AIC <- cvAIC
cvdataframe$BIC <- cvBIC
head(cvdataframe)
cvdataframe
#################################################
#APALANCAMIENTO E INFLUENCIA 
##################################################
rm(list=ls())
data(mtcars)
NROW(mtcars)
head(mtcars)
#Creamos una nueva  tabla con la informaci�n (esto es solo para no da�ar 
#la original)
mycars <- mtcars
#Hagamnos una regresi�n lineal de mpg vs hp
carmodel <- lm(mpg ~ hp, data = mycars)
summary(carmodel)

#Los modelos de regresi�n tienen su propio m�todo para plotear 
#y nos sacan  ya informaci�n estandarizada para evaluar el modelo 
#este informaci�n estandarizada consta de 6  gr�ficos 
#Para verlo adecuadamente ampliar lo m�s posible
#el �rea de panel de plots
#par(mfrow = c(1,1))
par("mar")
par(mar=c(1,1,1,1))
#los captions del gr�fico pueden ser cambiados, sin embargo 
#puede exceder del ancho especificado, en cuyo caso saldra
#un error "margins too large"
plot(carmodel, which=1, id.n=10) # Fitted X vs Residuals Y 
plot(carmodel, which=2) #QQ plot 
plot(carmodel, which=3) # Fitted X vs Residuals std Y 
plot(carmodel, which=4 , id.n=10) #Observaciones vs distancia Cook
plot(carmodel, which=5, id.n=10) #Apalancamiento vs residuos y Distancia Cook
plot(carmodel, which=6, id.n=10) #Apalancamiento vs Distancia Cook
#############################################################
#PARTE 2
#TAREA ESTUDIANTES
#############################################################
#Vamos a hacer un trabajo respecto a predicci�n de demoras en los vuelos en NY
#Primero deber� leer el archivo con descripci�n de los datasets
#https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf

#PREPARACI�N PARA LA TAREA:

rm(list=ls())
require(nycflights13)
#datamunging
data(weather)
names(weather)
class(weather)
#no me gustan los tibbles los convierto a data.frame
w <-as.data.frame(weather)

data(flights)
class(flights)
names(flights)
#igualmente la convierto
f<-as.data.frame(flights)
#redondeamos los minutos de la salida del vuelo para hacer m�s f�cil la comparaci�n
f$dep_time=round(f$dep_time,digits=-2)/100
#revisamos si tenemos na en la columna de temp de w
any(is.na(w$temp))
#Si tenemos NAs, vamos a poner un valor medio en estos casos 
w$temp[is.na(w$temp)] =50 # en farenheit
any(is.na(w$temp))
#creamos un campo adicional en flights,  la columna se llamar� "temp" 
f$temp <-0
#Creamos una funci�n que nos permite llenar esta columna con la temperatura 
#al momento de la hora de despegue
updatewind <- function (row)
{
  origen<-row[13]  #nombre el aeropuerto
  mes<-row[2] 
  dia<-row[3]  #dia de la semana 1-7
  hora<-row[4]  # hora 2:23
  temp<-w[w$origin==origen &
            w$month==as.numeric(mes) &
            w$day==as.numeric(dia) &
            w$hour==as.numeric(hora) ,6]
  
  return(temp)
}
f$temp <- apply(f ,1, updatewind)  # args df, 1=rows  2=cols ,function
library(tidyverse)
library(plyr)
library(dplyr)
require(gridExtra)
fw  <-  dplyr::inner_join(x=flights , y=weather, 
by=c("time_hour"="time_hour", "origin"="origin"))
##########################################
#TAREA DEL ESTUDIANTES
###########################################
#El prop�sito de la tarea es crear varios modelos de predicci�n del retraso
#de los vuelos(columna dep_delay). En el data.frame flights existen varias columnas que podr�an 
#ser consideradas como predictoras, por ejemplo :
#air_time: Ser� que los vuelos m�s largos tienen m�s demoras??
#month:ser� que hay m�s demoras durante los meses de invierno??
#origin: Sera que hay m�s demoras en un aeropuerto en particular??
#Otros factores climatol�gicos podr�an influir
#El instructor ya ha  incluido la temperatura como un posible factor 
#El estudiante puede seguir las instrucciones iniciales  con cualquier otro factor 
#climatologico, por ejemplo visibilidad , precipitaci�n etc.
#que usted considere que puede afectar la puntualidad del vuelo
###########################################################
#ENTREGABLES
#NO ENTREGAR LOS EJEMPLOS DE ESTE LABORATORIO UNICAMENTE LA TAREA
#La calificaci�n ser� basada en el analisis no en el c�digo, puesto que la 
#plantilla del c�digo ya esta  en este laboratorio 
#1.- Deber� crear  3 modelos distintos con diferentes predictoras
head(fw)
names(fw)
ModelA <- lm(fw$dep_delay ~ fw$temp + fw$air_time + fw$day.x )
ModelA
ModelB <- lm(fw$dep_delay ~ fw$month.x * fw$distance +fw$temp)
ModelB
ModelC <- lm(fw$dep_delay ~ fw$origin + fw$air_time + fw$temp)
ModelC
multiplot(ModelA, ModelB,ModelC)
ModelgA <- glm(fw$dep_delay ~ fw$temp + fw$air_time + fw$day.x )
ModelgA
ModelgB <- glm(fw$dep_delay ~ fw$month.x * fw$distance +fw$temp)
ModelgB
ModelgC <- glm(fw$dep_delay ~ fw$origin + fw$air_time + fw$temp)
ModelgC
multiplot(ModelgA, ModelgB,ModelgC)

#2.- Presente el summary de cada modelo, los estad�sticos y su an�lisis
summary(ModelA)

summary(ModelB)

summary(ModelC)

#3.- Presente los estimadores de AIC y BIC,  presente su an�lisis
df1aic <-AIC(ModelA, ModelB,ModelC)
df1aic
df1bic <-BIC(ModelA, ModelB,ModelC)
df1bic
df1aic$BIC <- df1bic$BIC
min_val_AIC <-min(df1aic$AIC)
min_var_AIC =df1aic[df1aic$AIC==min_val_AIC,1]
min_val_BIC <-min(df1aic$BIC)
min_var_BIC =df1aic[df1aic$BIC==min_val_BIC,1]
ggplot (df1aic, aes(x=df))+
  geom_point(aes(y=AIC, color=AIC), color="blue")+
  geom_line(aes(y=AIC, color=AIC))+
  geom_point(aes(y=BIC, color=BIC), color="red")+
  geom_line(aes(y=BIC, color=BIC))+
  labs(title="AIC/BIC" )+
  ylab("AIC en Azul/BIC en Rojo Value")+
  xlab("Num Variables")+
  geom_point(aes(x=min_var_AIC, y=min_val_AIC, size=4), color="blue")+
  geom_point(aes(x=min_var_BIC, y=min_val_BIC, size=4), color="red")  

comp_glmM7 <- data.frame(H1=ModelgA$deviance,H2=ModelgB$deviance,H3=ModelgC$deviance)
#Grafiquemos 
comp_meltedM7 <-melt(comp_glmM7)
names(comp_meltedM7) <- c("model", "Deviance")
names(comp_meltedM7)

ggplot(comp_meltedM7, aes( model, Deviance , fill=model)) +
  geom_col()+
  labs(title="Mejor Modelo Deviance mas bajo")
#4.- Revise los plots de cada uno de los modelos buscando la presencia 
#de outliers que tengan apalacamiento e influencia, realice un an�lisis
#en base a estos gr�ficos (plots) y de ser el caso sugiera outliers que usted considera
#que deben ser eliminados. Presente la salida y su analisis
par("mar")
par(mar=c(2,2,2,2))
plot(ModelA, which=1, id.n=10) # Fitted X vs Residuals Y 
plot(ModelA, which=2) #QQ plot 
plot(ModelA, which=3) # Fitted X vs Residuals std Y 
plot(ModelA, which=4 , id.n=10) #Observaciones vs distancia Cook
plot(ModelA, which=5, id.n=10) #Apalancamiento vs residuos y Distancia Cook
plot(ModelA, which=6, id.n=10) #Apalancamiento vs Distancia Cook

plot(ModelB, which=1, id.n=10) # Fitted X vs Residuals Y 
plot(ModelB, which=2) #QQ plot 
plot(ModelB, which=3) # Fitted X vs Residuals std Y 
plot(ModelB, which=4 , id.n=10) #Observaciones vs distancia Cook
plot(ModelB, which=5, id.n=10) #Apalancamiento vs residuos y Distancia Cook
plot(ModelB, which=6, id.n=10) #Apalancamiento vs Distancia Cook

plot(ModelC, which=1, id.n=10) # Fitted X vs Residuals Y 
plot(ModelC, which=2) #QQ plot 
plot(ModelC, which=3) # Fitted X vs Residuals std Y 
plot(ModelC, which=4 , id.n=10) #Observaciones vs distancia Cook
plot(ModelC, which=5, id.n=10) #Apalancamiento vs residuos y Distancia Cook
plot(ModelC, which=6, id.n=10) #Apalancamiento vs Distancia Cook

#5.- Conclusiones del laboratorio , presente un cuadro en el que indique
#el resultado de cada prueba y la recomendacion final del mejor modelo 

