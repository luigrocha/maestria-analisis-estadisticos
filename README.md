# maestria-analisis-estadisticos
maestria-analisis-de-datos-estadisticos

Tarea Módulo 2
Lectura Previa :
Recursos principales del módulo 2
Dataset mtcars link https://rpubs.com/neros/61800
Economics dataset link https://ggplot2.tidyverse.org/reference/economics.html
Enunciado
PROBLEMA 1
??? Para este ejercicio vamos a utilizar el dataset mtcars , información sobre lo que
representa cada columna puede ser encontrada en el primer link mencionado en el
párrafo anterior.
??? Propósito de la tarea es familiarizar al alumno con los indicadores estadísticos
aplicados a este dataset.
??? Conteste las siguientes preguntas
- La media de millas por galón mpg de los vehículos
- Cuál es el máximo hp de los vehículos
- Cuál es el tiempo mínimo de cuarto de milla logrado
- Cuál es la distribución en septiles de peso
- Cuál es la varianza de disp
- Cual es la desviación estándar de disp.
- Compruebe que la relación entre varianza y desviación std usando la función
sqrt()
- Indique la cantidad de carburadores que tiene el Mazda RX4 (pista usar head())
- Cuantos cambios tiene el Porsche 914 (pista usar tail())
PROBLEMA 2
??? Para este ejercicio vamos a utilizar el dataset economics, información sobre lo que
representa cada columna puede ser encontrada en el segundo link mencionado en el
párrafo inicial.
??? Propósito de la tarea es familiarizar al alumno con los indicadores estadísticos de
correlación usando este dataset.
??? Utilizando la función cor() obtenga la matriz de correlación de este dataset entre
pares de variables y elabore un comentario de cuales variables están relacionadas y si
la relación es positiva o negativa. 
??? Conteste las siguientes preguntas:
- Es verdad que si aumenta el desempleo el nivel de consumo personal
disminuye
- Es verdad que si aumenta el desempleo el nivel de ahorros baja
- Es verdad que si aumenta la población el nivel de consumo aumenta
- Cual es la covarianza entre el número de desempleados y el tiempo promedio
que esta desempleado
??? Ejecute en su programa es siguiente snippet de código R para graficar la relación de
todos contra todos y preséntela y emita un comentario de la utilidad de un diagrama
de calor
require(ggplot2)
require(reshape2)
require(scales)
head(economics)
cor(economics[,2:6])
cor_economics <-cor(economics[,2:6])
cor_economics
#esto se ve mejor en un mapa de calor ,pero para esto habrá que poner en un #una sola columna ,
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
Nota: Si el programa arrojara error durante la ejecución de las sentencia require() , deberá
primero instalar el paquete mediante el menú de Tools->Install Packages
Entregable:
Documento Word con el código ejecutado, la salida del código y su interpretación de los
resultados. 