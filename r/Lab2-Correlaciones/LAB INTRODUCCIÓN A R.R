#?PARTE 1 Introducci?n a R 
##################################################
#En este video vamos a revisar los conceptos b?sicos de uso del RStudio,
#as? como el uso de principales comandos mencionados en el 
#cap?tulo 2 de la maestr?a 
#PANELES EN RSTUDIO
#################################################
#Los paneles son b?sicamente ?reas de la pantalla de RStudio que tiene
#prop?sito especifico. Al momento de realizar la instalaci?n,
#RStudio le permite al usuario #configurar los paneles que desea tener 
#Podr? seleccionar hasta 4 paneles en la aplicaci?n 
#Como m?nimo recomiendo  tener 2, Source y Console, pero otros paneles
#como el de ambientes, historia ,archivos, plots, paquetes
#son muy ?tiles al momento de programar 
#La posici?n de cada panel depende de las preferencias del usuario en 
# View->Panes->Pane Layout
#Ahi puede usted seleccionar la posici?n, y que sub paneles est?n activados
#Podr? notar por ejemplo, el panel de ambiente donde podr? revisar 
#las variables creadas, pero m?s importante podr? visualizar matrices, 
#data.frames, vectores entre otros objetos
#El panel de historia-archivos-conexiones incluye el panel de plot
#Donde aparecer? los gr?ficos programados
#Por ?ltimo note los paneles de Fuentes y Consola, son los principales 
#con los que usted estar? interactuando. 
#En este panel note los ?conos de Run que se hallan en la parte
#superior del panel de Fuentes
#WORKSPACE
############################################################
#Es el espacio de trabajo, es su entorno de trabajo R actual e incluye 
#cualquier objeto definido (vectores, matrices, listas, funciones).
#Los cuales al final de una sesi?n de R, el usuario puede guardar una 
#imagen del espacio de trabajo actual, la cual que se vuelve a cargar autom?ticamente 
#la pr?xima vez que se reinicie R (esto es si al salir ha contestado afirmativamente que
#quiere salvar el archivo rdata).

#PAQUETES EN R
#########################################
#Los paquetes R ampl?an la funcionalidad de R al proporcionar funciones, 
#datos y documentaci?n adicionales. 
#Est?n escritos por una comunidad mundial de usuarios de R y se 
#pueden descargar de forma gratuita desde Internet.
#Los repositorios principales son CRAN y GitHub

#Al momento de instalar RStudio, el paquete Base es cargado, por lo que 
#no hay necesidad de cargarlo expl?citamente. Base incluye todas las funciones 
#principales
#Pero otros paquetes se deber?n cargar expl?citamente 
#Existen dos formas de hacerlo, 
#   Primero a trav?s del panel de Historia 
#   Segundo a trav?s de Tools->Install Packages 
#Veamos un ejemplo
#Voy a cargar el paquete tidyr (note el proceso en la consola)
install.packages("tidyr")

#Tambi?n podemos obtener informaci?n del paquete
help(package ="tidyr")


#AYUDA EN LINEA
#Existen dos formas de buscar ayuda:
#la primera es con el signo de interrogaci?n 
#Por ejemplo, busquemos ayuda sobre 
?matrix
#La segunda forma es mediante el panel de ayuda 

#EL OPERADOR  <-
##############################
#Sirve para crear objetos. 
#Por ejemplo, creemos un vector con valores que vayan de 1 a 10.
x <- 1:10

#Una vez creada la variable, podemos asignarle un nuevo valor con el operador 
#igual "=" , pero la primera vez debe crearse con la flecha izquierda
x
#Para desplegar el valor de la variable simplemente  escribiremos el nombre. 
#El valor se desplegar?  en la consola 

#CREACI?N DE VECTORES Y LISTAS
#Es importante mencionar que en R todo es un vector, incluso una variable
#numerica es considerada un vector de longitud 1
#La forma m?s com?n de crear vectores en R es mediante la 
#funci?n c()
#por ejemplo. 

y <- c(10,20,30,40,50)
#Una vez creado los elementos del vector pueden ser obtenidos mediante
#su ?ndice
y[3]+15
#importante mencionar que las estructuras en R empiezan a numerarse por 1
#Un vector debe ser siempre del mismo tipo, no se pueden mezclar strings 
#con num?ricos si usted lo hace entonces todo el vector ser? considerado
#del tipo string 

#Si usted requiere crear un objeto que contenga distintos tipos, entonces 
#lo que necesita es una lista
#Una lista puede ser creada en distintas formas pero la m?s com?n es mediante
#el operador punto (.)

#Por ejemplo 
amigos <- . ("Pedro", 50,TRUE, "Carlos", 55, FALSE)
amigos

#VARIABLES
#Las variables son tambi?n objetos, todo objeto tiene m?nimo 2 propiedades
#longitud y tipo que pueden ser  vistos por las funciones class() y length() respectivamente
pi= 3.14159
length(pi)
class(pi)
#Existen 4 tipos de variables:  num?ricas, caracter, complejas y l?gicas
descripcion1 <- "estoy de vacaciones "
descripcion2 <- "Maria dijo "
descripcion3 <- paste(descripcion2 , descripcion1)
sub("Maria", x=descripcion3, replacement="MARIA")

continuar <- TRUE
!continuar

complejo1 <- 3-4i
abs(complejo1)

#CONVERSIONES o CASTINGS
#Como todo lenguaje R permite convertir expl?citamente de un tipo a otro

e <- 2.718
class(e)
as.character(e)
existe <- TRUE
descipcion1 <- "Esta variable puede ser convertida"
descripcion2 <- paste(as.character(existe),descipcion1)
descripcion2
#Si tiene dudas del tipo de variable que se trata  siempre puede usar
#las funciones is.*  . Existen muchas, algunos ejemplos:
is.numeric(descripcion1)
is.character(existe)
is.logical(existe)

#TRABAJO CON FECHAS
#Importante saber que existen dos  formas de almacenar fechas. 
#POSIXct  basado en cantidad de segundos  desde enero 1970 
#POSIXlt representa la fecha como una lista en el cual cada elemento 
#contiene informaci?n de la fecha desde 01/01/1900
#Esto ser? importante al momento de leer data de un archivo externo
#R provee las funciones para trabajar con fechas. Ejemplos:
numsec <- 1000000
fecha1 <- as.POSIXct(x=numsec, origin="1970-01-01", tz="EST")
as.numeric(as.POSIXct("2000-01-02 09:22:05"))
fecha2<- as.POSIXct("01/01/2000 13:22:05", format="%d/%m/%Y %H:%M:%S")
fecha3 <- as.POSIXct("080406 10:11", format = "%y%m%d %H:%M")
fecha4 <- as.POSIXct("2008-04-06 10:11:01", format = "%Y-%m-%d %I:%M:%S")

ahorita <- unclass(as.POSIXlt(Sys.time()))
ahorita$sec
ahorita$min
ahorita$hour
#note que el mes lo empieza a contar desde 0 
ahorita$mon
ahorita$wday
ahorita$yday
#Los dos formatos son complicados de usar y requieren conversiones
#Para trabajar con fechas es recomendable usar la librer?a lubridate
#lubridate basado en POSIXct. Carguemos la librer?a
install.packages('lubridate')
require(lubridate)
#provee varias funciones
#Revisar el cheatsheet para ver todas las opciones
hoy<- as.POSIXct(Sys.time())
ymd_hms(hoy)
dmy("14072020")
myd("07142020")
ymd("20200714")
ydm("20201407")

#CAMBIO DE DIRECTORIO DE TRABAJO
#R nos provee facilidad para cambiar el directorio de trabajo 
#El directorio de trabajo es donde  guardamos los programas o datasets 
#que est?n en su disco
getwd()
#para movernos de directorio deber? usar / en lugar de \ de windows
setwd("C:/Users/Alfonso/") 
#o en su lugar comentar los \ de windows  usando  \\
setwd("C:\\Users\\Alfonso\\Documents") 
#Si desde el programa desea listar los archivos del directorio presente: 
list.files(path=".")

