#LABORATORIO DE ANALISIS DE MODELO REGRESI�N M�LTIPLE
#ANTECEDENTES
########################################
#El prop�sito de la regresi�n m�ltiple es encontrar la regresi�n basado en m�ltiples 
#predictoras.
#Revisaremos los conceptos de estad�sticos b�sicos, coeficientes, error est�ndar
#t-value y p-value, aunque para este prop�sito p-value podr�a no ser indicativo,
#esto fue desarrollado por Fisher hace m�s de 100 a�os y los valores p 
#son subjetivos o arbitrarios)

#=====================================
rm(list=ls())
require(coefplot)
require(ggplot2)
#El estudiante podr� bajarse el archivo de las siguiente URL: https://www.jaredlander.com/data/
#Y deber� ajustar el directorio donde se ha bajado 
housing <- read.table("https://www.jaredlander.com/data/housing.csv" , sep=",", header=TRUE, stringsAsFactors=FALSE)
names(housing)
housing
#Nombre largos o con puntos es mejor cambiarlos 
nombres <- names(housing) 
names(housing) <- c("sector" ,"class", "TUnits" , "Year","GSqFt" ,
                    "Est_GIncome" ,"GIncomexSqFt","Est_Expense" , 
                    "ExpSqFt","NetIncome",   
                    "FullValue" , "valxSqFt" ,"Boro")
#Entendamos el dataset
head(housing)

unique(housing$Boro)

house1 <- lm(valxSqFt ~TUnits+ GSqFt +Boro  , data=housing)
#Tratamos de predecir cual es el valor por pie cuadrado de departamentos como funci�n
#de las variables TUnits(total de departamentos), GSqFT(Total �rea edificio),
#Boro(ciudad). 
#La ~ debe considerarse como decir "se distribuye como" o
# "depende de" cuando se ve en las funciones de regresi�n
#El s�mbolo "+" en una f�rmula no suma realmente dos variables, 
#en su lugar es generalmente una solicitud impl�cita de calcular un coeficiente (s)
#de regresi�n para esa variable en el contexto del resto de las variables 
#que est�n a la derecha de una f�rmula.

summary(house1)
#Note que lm nos presenta los coeficientes de las variables pero adem�s nos presenta
#4 pseudo variables que representa el Boro, esto es porque Boro es una variable
#discreta. Veamos cuantos valores tenemos en Boro
unique(housing$Boro)
#Note en los coeficientes de Boro que falta un boro = Bronks
#Esto es debido a la  implementaci�n de algebra de matrices creando 
#pseudo variable booleanas, la que falta es tomada como referencia
#Esto evitara la  multicolinealidad 

class(house1)
#house1 es un modelo LM , podemos extraer los coeficientes con cualquiera de la siguientes formas:
names(house1)
house1$coefficients
coef(house1)
coefficients(house1)
#
#graficando los coeficientes
coefplot(house1)

#Interpretacion del gr�fico :
#Los puntos indican el valor del  coeficiente 
#Las l�neas indican el nivel de confianza
#La l�nea gruesa indica  el rango de 1 sigma 
#la l�nea fina indica 2 sigmas 
#
#La regla general es que si el 0  est� contenido en las l�neas anteriores, 
#entonces el coeficiente no es significativo. En este caso StatenIsland.
#De hecho usted puede validar que el t es bajo, quiere decir que el std err es alto
#en este caso el coeficiente es 3.6 y el std error es 9.9, es decir la incertidumbre es 
#muy alta. 
#Otra conclusi�n que sale del plot es que Manhattan se aleja de la media del resto
#y deber�amos tratarlo por separado.

#Veamos otros modelos a ver cual se adapta mejor,  en este caso usaremos el  operador  *
#El * indica que no solo queremos cada efecto principal, sino que tambi�n
#queremos un t�rmino de interacci�n entre TUnits y GSqFt.
#Para que sirve esto: Bueno podemos ver que estas variables est�n relacionadas
#mientras m�s cantidad de departamentos tenemos mayor �rea del edificio.
#Esto nos puede llevar al problema de la multicolinealidad.
#entonces al tener como predictora a la multiplicaci�n estoy tomando en cuenta 
#las 2 variables por separadas (con +) caer�amos en multicolinealidad
house2 <- lm(valxSqFt ~TUnits* GSqFt +Boro  , data=housing)
coef(house2)
#Visualicemos estos coeficientes
coefplot(house2)
#Veamos los resultados como matriz
head(model.matrix(valxSqFt ~TUnits*GSqFt, data=housing))
cor(housing$GSqFt, housing$TUnits)
#Si solo queremos su interacci�n, pero no las variables  individuales,
#usamos el s�mbolo de dos puntos (:)
house3 <- lm(valxSqFt ~ TUnits:GSqFt + Boro, data=housing)
coefplot(house3)
summary(house3)
#Note que el ploteo da un punto de 0 de tunits:gsqft indicando que no es importante"
#Veamos  otro modelo  con interacci�n de 3  componentes

house4 <- lm(valxSqFt ~ TUnits*GSqFt*NetIncome , data=housing)
summary(house4)
coef(house4)
coefplot(house4)

house5 <- lm(valxSqFt ~class*Boro , data=housing)
unique(housing$class)
coef(house5)
coefplot(house5)
# Note En este caso para cada tipo de condominio, para cada ciudad nos da un coeficiente 
# La funci�n I() act�a para convertir el argumento a "as.is", 
# es decir que esperas. Entonces I(x ^ 2) devolver�a un vector de valores elevados 
# a la segunda potencia. No podemos usar una multiplicaci�n de escalares porque 
#son vectores

#Veamos porque del operador I 
house7 <- lm(valxSqFt ~ (GSqFt+TUnits)^2 , data=housing)
coef(house7)

#Note que los coeficientes que nos da es uno para Gsqft  y uno para TUnits,
#si se requiere que literalmente sume gsqft + tunits debemos usar la funci�n I

house8 <- lm(valxSqFt ~ I(GSqFt+TUnits)^2 , data=housing)
coef(house8)


#Para comparar los modelos podemos usar multiplot

multiplot(house1, house2, house8)
#Comparando , podemos ver que coeficientes son significativos  por cada variable


#Usando el modelo para predicci�n 
housingData <- read.table("https://www.jaredlander.com/data/housingNew.csv" , sep=",", header=TRUE, stringsAsFactors=FALSE)
NROW(housingData)
#OjO este dataset es m�s peque�o que el anterior y los nombres ya est�n cambiados
names(housingData)

housing1Mod <- lm(ValuePerSqFt ~Units+ SqFt +Boro  , data=housingData)
coef(housing1Mod)
coefplot(housing1Mod)
housePredict <-predict(housing1Mod, newdata=housingData , se.fit=TRUE, interval="prediction", 
                       level=.95)
head(housePredict)

#El intervalo de confianza refleja la incertidumbre en torno a la media de las predicciones
#Para mostrar los intervalos de confianza del 95% alrededor de la media usar
#la  opci�n interval = "confidence"


housePredict <-predict(housing1Mod, housingData , interval="prediction",level=.95)
housePredict
salida <- as.data.frame(housePredict)
salida$ValorReal <-housingData$ValuePerSqFt
salida$Unidades <-housingData$Units
salida$SqFt <-housingData$SqFt
salida$Boro <-housingData$Boro
head(salida)
#Para predecir nuevos valores:
nuevadata<-data.frame(Units=c(14), SqFt=c(8400), Boro=c("Manhattan"))
nuevapred <-predict(housing1Mod, newdata=nuevadata , interval="prediction",level=.95)
nuevapred


#El intervalo de predicci�n da incertidumbre en torno a un solo valor. 

#Que usar prediction o confidence  ?
# interval= prediccion  es mucho m�s amplio que un intervalo de confianza para el mismo valor.
#En general, estamos interesados en predicciones individuales espec�ficas, 
#por lo que un interval="prediction"  ser�a m�s apropiado en este caso.
#Usar un interval="confidence cuando deber�a usar un intervalo de predicci�n subestimar�
#en gran medida la incertidumbre en un valor predicho dado
#los valores negativos indicar�an que el modelo no es el correcto 

#Tambi�n podemos obtener el nivel de confianza para cada coeficiente con 
confint(housing1Mod)

#COMPARACI�N DE MODELOS USANDO ANOVA 
#######################################################################

house1 <- lm(valxSqFt ~TUnits+ GSqFt   , data=housing)
house2 <- lm(valxSqFt ~TUnits+ GSqFt +Boro  , data=housing)
anova(house1, house2)
house3 <- lm(valxSqFt ~ TUnits:GSqFt + Boro, data=housing)
house31 <- lm(valxSqFt ~ TUnits + Boro+ GIncomexSqFt,  data=housing)
anova(house1, house2)
anova(house1, house31)




##########################################
#PARTE 2
#TRABAJO DEL ALUMNO 
#########################################
#Para el presente trabajo usaremos el dataset diamonds,
#el significado de cada campo lo puede encontrar en :
#https://ggplot2.tidyverse.org/reference/diamonds.html
#Este es  un listado de precio de 50k diamantes registrados
#Nuestra variable de respuesta es el precio. Pero como puede
#observar el precio est� en funci�n de varios campos(columnas)
#como el corte (cut), color, claridad etc. 
#Su trabajo es encontrar el modelo m�s apropiado para predecir el 
#precio.
require(coefplot)
require(ggplot2)
data(diamonds)
class(diamonds)
names(diamonds)
nrow(diamonds)
diamonds
diamonds$cut <-as.factor(diamonds$cut)
diamonds$color <-as.factor(diamonds$color)
diamonds$clarity <-as.factor(diamonds$clarity)
cor(diamonds[5:10])
diamantes<- diamonds[c("carat","cut", "price","x","y","z")]
cor(diamantes)
unique(diamonds$cut)
unique(diamonds$color)
unique(diamonds$clarity)

modelA <- lm(price ~ carat + x + y+ z, data=diamonds)
names(modelA)
modelA$coefficients
coef(modelA)
coefficients(modelA)
coefplot(modelA)
summary(modelA)

modelB <- lm(price ~ carat + x + y, data=diamonds)
names(modelB)
modelB$coefficients
coef(modelB)
coefficients(modelB)
coefplot(modelB)
summary(modelB)

modelC <- lm(price ~ color + cut , data=diamonds)
names(modelC)
modelC$coefficients
coef(modelC)
coefficients(modelC)
coefplot(modelC)
summary(modelC)

modelD <- lm(price ~ x + z  , data=diamonds)
names(modelD)
modelD$coefficients
coef(modelD)
coefficients(modelD)
coefplot(modelD)
summary(modelD)
multiplot(modelA, modelB, modelC, modelD)
# Para cada modelo que usted considere deber� realizar un estudio 
#del modelo en base a los estad�sticos.
#Deber� presentar  al menos 4 modelos y su an�lisis

modelA <- lm(price ~ carat + x + y+ z, data=diamonds)
modelB <- lm(price ~ carat + x + y, data=diamonds)
modelC <- lm(price ~ carat + x + z, data=diamonds)
modelD <- lm(price ~ carat + color + y + z, data=diamonds)
multiplot(modelA, modelB, modelC, modelD)

