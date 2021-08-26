# maestria-analisis-estadisticos
maestria-analisis-de-datos-estadisticos

Tarea M�dulo 2
Lectura Previa :
Recursos principales del m�dulo 2
Dataset mtcars link https://rpubs.com/neros/61800
Economics dataset link https://ggplot2.tidyverse.org/reference/economics.html
Enunciado
PROBLEMA 1
??? Para este ejercicio vamos a utilizar el dataset mtcars , informaci�n sobre lo que
representa cada columna puede ser encontrada en el primer link mencionado en el
p�rrafo anterior.
??? Prop�sito de la tarea es familiarizar al alumno con los indicadores estad�sticos
aplicados a este dataset.
??? Conteste las siguientes preguntas
- La media de millas por gal�n mpg de los veh�culos
- Cu�l es el m�ximo hp de los veh�culos
- Cu�l es el tiempo m�nimo de cuarto de milla logrado
- Cu�l es la distribuci�n en septiles de peso
- Cu�l es la varianza de disp
- Cual es la desviaci�n est�ndar de disp.
- Compruebe que la relaci�n entre varianza y desviaci�n std usando la funci�n
sqrt()
- Indique la cantidad de carburadores que tiene el Mazda RX4 (pista usar head())
- Cuantos cambios tiene el Porsche 914 (pista usar tail())
PROBLEMA 2
??? Para este ejercicio vamos a utilizar el dataset economics, informaci�n sobre lo que
representa cada columna puede ser encontrada en el segundo link mencionado en el
p�rrafo inicial.
??? Prop�sito de la tarea es familiarizar al alumno con los indicadores estad�sticos de
correlaci�n usando este dataset.
??? Utilizando la funci�n cor() obtenga la matriz de correlaci�n de este dataset entre
pares de variables y elabore un comentario de cuales variables est�n relacionadas y si
la relaci�n es positiva o negativa. 
??? Conteste las siguientes preguntas:
- Es verdad que si aumenta el desempleo el nivel de consumo personal
disminuye
- Es verdad que si aumenta el desempleo el nivel de ahorros baja
- Es verdad que si aumenta la poblaci�n el nivel de consumo aumenta
- Cual es la covarianza entre el n�mero de desempleados y el tiempo promedio
que esta desempleado
??? Ejecute en su programa es siguiente snippet de c�digo R para graficar la relaci�n de
todos contra todos y pres�ntela y emita un comentario de la utilidad de un diagrama
de calor
require(ggplot2)
require(reshape2)
require(scales)
head(economics)
cor(economics[,2:6])
cor_economics <-cor(economics[,2:6])
cor_economics
#esto se ve mejor en un mapa de calor ,pero para esto habr� que poner en un #una sola columna ,
no como esta
econ_melted <- melt(cor_economics, varname=c("X","Y"),value.name="Correlacion")
econ_melted
class(econ_melted)
#ahora le ploteamos como heat map
#antes le orenamos para que nos salga bonito el grafico
econ_melted <- econ_melted[order(econ_melted$Correlacion),]
ggplot(econ_melted, aes(x=X , y=Y)) +
 geom_tile(aes(fill=Correlacion))+
 scale_fill_gradient2(low=muted("red"), mid="white" , high="steelblue")+
 theme_minimal() +
 labs(x=NULL , y=NULL)
Nota: Si el programa arrojara error durante la ejecuci�n de las sentencia require() , deber�
primero instalar el paquete mediante el men� de Tools->Install Packages
Entregable:
Documento Word con el c�digo ejecutado, la salida del c�digo y su interpretaci�n de los
resultados. 