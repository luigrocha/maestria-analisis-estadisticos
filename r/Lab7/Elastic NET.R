#ELASTIC NET 
##############################################################

#Algunos antecedentes :
#En la regresi�n lineal Y=X*beta + epsilon  nosotros tratamos de calcular los
#coeficientes beta
#La forma com�n de hacerlo es a trav�s de MCO (Minimos cuadrados ordinarios ) encontrando 
#el estad�stico RSS residuals sum of squares (ver labs anteriores)
#Para encontrar el beta debemos tomar en cuenta dos cosas : 
#el bias (sesgo) y la varianza 
#El bias es la diferencia  entre el valor del estad�stico real de la poblacion  y 
#la estimaci�n en base a los estimadores. En este caso el beta o coeficiente de la variable
#Si el bias es bajo  nuestra estimaci�n esta muy cerca de la realidad,
#si el bias es alto nuestra estimaci�n esta desviada de la realidad, no importa 
#cuantas muestras haga siempre arrojar�n un  valor dado.
#La varianza es la medida de la dispersi�n, como ya sabemos, si la estimaci�n del beta
#tiene alta varianza quiere decir que a veces puede ser 1 o  veces puede ser 5 
#Ambos conceptos deberian tratar de mantenerse bajos si queremos que sea buena la predicci�n
#Usando el RSS el bias es bajo, pero la varianza puede ser alta.

#A medida que la complejidad aumenta ( es decir aumentan los coeficientes) el bias  
#va bajando mientras que la varianza va subiendo 

#Cuando la complejidad es baja, la varianza tambi�n es baja pero va aumentando  
#con la complejidad. Complejidad quiere decir n�mero de variables
#Por lo tanto, queremos  encontrar la m�nima cantidad de predictores  que nos dan 
#la m�nima varianza 

#Una forma de hacer esto es asumir que ciertos  coeficientes son  0, en base al
#estad�stico Pr>F asi mantenemos la complejidad baja, esto por supuesto evitar� el overfitting 
#pero de esta forma no podemos saber el impacto de dichas variables 
#En otras palabras. p ej. bajo de 10 a 3 variables, seguro que no hay overfitting, pero 
#no tengo idea cual es el efecto de las variables eliminadas!!!. 

#ELASTIC NET PRESENTA DOS PROCESOS ALTERNATIVOS PARA RESOLVER ESTE PROBLEMA

#A)RIDGE REGRESSION 
#La respuesta ser�a "PENALIZAR" los coeficientes de ciertas variables, de tal manera 
#que las seguimos manteniendo en el modelo, aun cuando su impacto es muy bajo 

#b)LASSO (least absolute shrinkage operator) que traducido ser�a operador de contracci�n m�nima absoluta 
#Lasso es similar, penaliza los coeficientes que no son zeros, pero adem�s penaliza la suma
#de sus valores  absolutos 

#Vamos a revisar  los dos 
#El objetivo es minimizar la parte superior del grafico que mostraremos a continuaci�n,
#cuya primera parte es tipo de regresion lineal (respuesta - predictoras al cuadrado ),
#la segunda parte de la formula superior es la compensaci�n para overfitting 
#la segunda parte lo que hace es  tomar un grupo de variable que tienen alta correlaci�n 
#y divide un coeficiente entre ellas.  
#Es una buena forma de decidir que variables son importantes y cuales no.
rm(list=ls())
require(ggplot2)
require(glmnet)
require(useful)
#Veamos el dataset acs , se trata de un censo 
#Puede descargar la data  de  https://www.jaredlander.com/data/
download.file("https://www.jaredlander.com",
              "data/acsNew.csv")
#Yo ya la tengo descargada. MODIFIQUE LA SIGUIENTE LINEA 
acs <-read.table("C:\\Users\\Alfonso\\Documents\\acsNew.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)
class(acs)
names(acs)
head(acs)
#Glmnet es un paquete que  adapta un  modelo lineal generalizado (glm) a trav�s de 
#penalizar los coeficientes mediante ridge o lasso. 
#Para esto necesitamos crear un par de matrices. 

#build.X es una funci�n que creara la matriz con los coeficientes para el an�lisis .
#el formato de build.X es:  
#build.x(formula, dataframe, contrasts = TRUE, sparse = FALSE)
#La matriz contiene con todas las num�ricas y variables "dummy" , por ejemplo:
#si tenemos un factor Language  y tenemos dos niveles Spanish, English, 
#crear� 2 columnas en la matriz con estos nombres

#La f�rmula es lo mismo que hemos utilizado en lm o glm
#El contrast indica si se eliminar� el nivel base de un factor. Puede ser un valor 
#�nico aplicado a cada nivel o un valor para cada factor.
acsX <- build.x(Income~NumBedrooms +NumChildren+NumPeople+NumRooms+
                  NumUnits+NumVehicles +OwnRent+   YearBuilt +
                  ElectricBill+FoodStamp+HeatingFuel+Insurance+
                  Language-1 , data=acs, contrasts=FALSE)


class(acsX)
dim(acsX) #indica las dimensiones de la matriz, NOTE EL INCREMENTO DE COLUMNAS
topleft(acsX, c=6) #es solo para visualizar parte una matriz grande 
#Hasta aqui sabemos que tenemos al menos 42 variables num�ricas 
#Las variables dummies han sido substituidas por  num�ricas para cada nivel 

#Ahora vamos a crear la matriz Y, en realidad es una matriz de una sola fila.
#Note que el contenido de esta matriz es una variable nominal (Above, Below)

acsY <- build.y(Income~NumBedrooms +NumChildren+NumPeople+NumRooms+
                  NumUnits+NumVehicles +OwnRent+   YearBuilt +
                  ElectricBill+FoodStamp+HeatingFuel+Insurance+
                  Language-1 , data=acs)

class(acsY)
head(acsY)
tail(acsY)

#Ahora vamos a utilizar cross-validaci�n para analizar el efecto de cada variable
#(ver laboratorio LAB-COMPARACION-DE-MODELOS)
#dado que esto es un proceso iterativo, necesitamos inicializar la semilla de random 
set.seed(1863561)
acsCV<- cv.glmnet(x=acsX , y=acsY , family="binomial", nfold=5)
#El uso de glmnet es simple. Acepta datos x, y vrea modelos de regresi�n y produce 
#una regularizaci�n sobre una matriz de valores para un  par�metro de ajuste llamado lambda. 
#Porque utilizamos family=binomial es porque la variable de respuesta Y es nominal 
#Entonces estamos creando un modelo donde X son las 42 variables num�ricas y 
#Y es la respuesta nominal . 

#Nos devuelve un objeto de la clase cv.glmnet
acsCV
class(acsCV)
names(acsCV)
class(acsCV$lambda)
#Note que acsCV tiene  una propiedad lambda, 
#Visualizemos  lambda.min que es el que minimiza el  MSE (mean square error )
acsCV$lambda.min
#dentro de 1 SD tambien obtenemos el lambda.1se, que se encuentra a 1 sd de lambda.min
acsCV$lambda.1se
#Visualizando la salida 
plot(acsCV)
abline(v=log(c(acsCV$lambda.min , acsCV$lambda.1se)), lty=2)
#Que vemos en este plot : 
#REcordando el tema de deviance:La desviaci�n residual es la diferencia entre la desviaci�n 
#del modelo actual y la desviaci�n del modelo ideal donde los valores predichos
#son id�nticos a los observados. Por lo tanto es el mejor indicador y esperamos que sea
#bajo.Si no es bajo indicar� que los predictores no son los adecuados(igual que glm) 

#En la primera l�nea tenemos la cantidad de variables a considerar, originalmente ten�amos
#42 vars o columnas
#En el eje X tenemos el log(lambda), (log es solo para ver en una escala reducida)
#Notamos tambi�n una rayas verticales,  estas rayas  indican la incertidumbre 
#En el eje de las Ys tenemos el deviance.Cada punto rojo  define un valor de  deviance
#que corresponde a al deviance obtenido con el numero correspondiente de variables de arriba

#Tenemos que ubicar el punto  minimo del deviance y ver la cantidad de variables 
#con la que se obtiene este deviance
#Tambi�n marcamos dos lineas verticales , la primera es el lambda en el cual se logran 
#que el MSE sea m�nimo (primera linea vertical )
#La segunda l�nea vertical  es donde se ubica  el lambda.1se 
#lambda.1se: lambda que se encuentra a 1 sd de lambda.min

#Vamos a ver los coeficientes que este CV tiene distintos lambdas , (par�metros indica el valor de lambda) 
coef(acsCV, s="lambda.min")
coef(acsCV, s="lambda.1se")
#Note la cantidad de coeficientes que se tienen en lambda.min y cuantos quedan
#en lambda.1sd
#aquellos que tiene coeficientes significar�a que las variables son importantes
#aquellos que tiene un punto indicar�a que la variable no es importante 
#m�s a�n, para ciertas variables dummies es posible que solo incluya ciertos niveles
#esto es porque realiza una correlacion entre variables y cuando tiene una alta correlaci�n las  
#descarta dejando solo 1

#Entonces la cosa es simple, ubique el punto rojo con el m�nimo deviance y ubique la 
#cantidad de variables con las que se obtiene dicho deviance. 


#Otra forma de ver la misma informaci�n es con el siguiente gr�fico
plot(acsCV$glmnet.fit, xvar="lambda")
abline(v=log(c(acsCV$lambda.min , acsCV$lambda.1se)), lty=2)
#Que vemos en este gr�fico:
#L�nea superior vemos que comenzamos con todas las variables(42 variables).
#Eje de las Ys es el valor de los coeficientes.
#Cada traza representa un lambda, y vemos que en la medida que usamos menos variables 
#los coeficientes son penalizados tendiendo a cero. 
#Debemos escoger  un lambda entre los dos valores de incertidumbre(lambda.min y lambda.1sd)
#donde los valores de los coeficientes van convergiendo sin necesariamente ser 0 

#Podemos iniciar con lambda.min e ir subiendo hasta lambda.1sd

coef(acsCV, s="lambda.min")


#Efecto del Alpha , notemos en la formula que existe un alpha
#alpha  es un par�metro de mezcla (mixing) valor=0 asume ridge penalty , alpha=1 lasso penalty 
#buen link para entender diferencias  https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net


#Vamos a crear un nuevo modelo con alpha=0 , esto es regresi�n  ridge o cresta
acsCV2<- cv.glmnet(x=acsX , y=acsY , family="binomial", nfold=5, alpha=0)
acsCV2
plot(acsCV2$glmnet.fit, xvar="lambda")
abline(v=log(c(acsCV2$lambda.min , acsCV2$lambda.1se)), lty=2)
#que notamos en este  cuadro  que las trazas  convergen , esto es porque en ridge 
#los coeficientes se penalizan pero nunca desaparecen 

#El resto del procedimiento es igual. 


