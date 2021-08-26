·PARTE 1 Introducción a R 
##################################################
#En este video vamos a revisar los conceptos básicos de uso del RStudio,
#así como el uso de principales comandos mencionados en el 
#capítulo 2 de la maestría 
#PANELES EN RSTUDIO
#################################################
#Los paneles son básicamente áreas de la pantalla de RStudio que tiene
#propósito especifico. Al momento de realizar la instalación,
#RStudio le permite al usuario #configurar los paneles que desea tener 
#Podrá seleccionar hasta 4 paneles en la aplicación 
#Como mínimo recomiendo  tener 2, Source y Console, pero otros paneles
#como el de ambientes, historia ,archivos, plots, paquetes
#son muy útiles al momento de programar 
#La posición de cada panel depende de las preferencias del usuario en 
# View->Panes->Pane Layout
#Ahi puede usted seleccionar la posición, y que sub paneles están activados
#Podrá notar por ejemplo, el panel de ambiente donde podrá revisar 
#las variables creadas, pero más importante podrá visualizar matrices, 
#data.frames, vectores entre otros objetos
#El panel de historia-archivos-conexiones incluye el panel de plot
#Donde aparecerá los gráficos programados
#Por último note los paneles de Fuentes y Consola, son los principales 
#con los que usted estará interactuando. 
#En este panel note los íconos de Run que se hallan en la parte
#superior del panel de Fuentes
#WORKSPACE
############################################################
#Es el espacio de trabajo, es su entorno de trabajo R actual e incluye 
#cualquier objeto definido (vectores, matrices, listas, funciones).
#Los cuales al final de una sesión de R, el usuario puede guardar una 
#imagen del espacio de trabajo actual, la cual que se vuelve a cargar automáticamente 
#la próxima vez que se reinicie R (esto es si al salir ha contestado afirmativamente que
#quiere salvar el archivo rdata).

#PAQUETES EN R
#########################################
#Los paquetes R amplían la funcionalidad de R al proporcionar funciones, 
#datos y documentación adicionales. 
#Están escritos por una comunidad mundial de usuarios de R y se 
#pueden descargar de forma gratuita desde Internet.
#Los repositorios principales son CRAN y GitHub

#Al momento de instalar RStudio, el paquete Base es cargado, por lo que 
#no hay necesidad de cargarlo explícitamente. Base incluye todas las funciones 
#principales
#Pero otros paquetes se deberán cargar explícitamente 
#Existen dos formas de hacerlo, 
#   Primero a través del panel de Historia 
#   Segundo a través de Tools->Install Packages 
#Veamos un ejemplo
#Voy a cargar el paquete tidyr (note el proceso en la consola)
install.packages("tidyr")

#También podemos obtener información del paquete
help(package ="tidyr")


#AYUDA EN LINEA
#Existen dos formas de buscar ayuda:
#la primera es con el signo de interrogación 
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
#El valor se desplegará  en la consola 

#CREACIÓN DE VECTORES Y LISTAS
#Es importante mencionar que en R todo es un vector, incluso una variable
#numerica es considerada un vector de longitud 1
#La forma más común de crear vectores en R es mediante la 
#función c()
#por ejemplo. 

y <- c(10,20,30,40,50)
#Una vez creado los elementos del vector pueden ser obtenidos mediante
#su índice
y[3]+15
#importante mencionar que las estructuras en R empiezan a numerarse por 1
#Un vector debe ser siempre del mismo tipo, no se pueden mezclar strings 
#con numéricos si usted lo hace entonces todo el vector será considerado
#del tipo string 

#Si usted requiere crear un objeto que contenga distintos tipos, entonces 
#lo que necesita es una lista
#Una lista puede ser creada en distintas formas pero la más común es mediante
#el operador punto (.)

#Por ejemplo 
amigos <- . ("Pedro", 50,TRUE, "Carlos", 55, FALSE)
amigos

#VARIABLES
#Las variables son también objetos, todo objeto tiene mínimo 2 propiedades
#longitud y tipo que pueden ser  vistos por las funciones class() y length() respectivamente
pi= 3.14159
length(pi)
class(pi)
#Existen 4 tipos de variables:  numéricas, caracter, complejas y lógicas
descripcion1 <- "estoy de vacaciones "
descripcion2 <- "Maria dijo "
descripcion3 <- paste(descripcion2 , descripcion1)
sub("Maria", x=descripcion3, replacement="MARIA")

continuar <- TRUE
!continuar

complejo1 <- 3-4i
abs(complejo1)

#CONVERSIONES o CASTINGS
#Como todo lenguaje R permite convertir explícitamente de un tipo a otro

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
#contiene información de la fecha desde 01/01/1900
#Esto será importante al momento de leer data de un archivo externo
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
#Para trabajar con fechas es recomendable usar la librería lubridate
#lubridate basado en POSIXct. Carguemos la librería
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
#que estén en su disco
getwd()
#para movernos de directorio deberá usar / en lugar de \ de windows
setwd("C:/Users/Alfonso/") 
#o en su lugar comentar los \ de windows  usando  \\
setwd("C:\\Users\\Alfonso\\Documents") 
#Si desde el programa desea listar los archivos del directorio presente: 
list.files(path=".")

