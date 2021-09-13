#PARTE1 : Demostraci?n de uso de funciones  de hip?tesis
########################################################
#COMPARACIONES DE MEDIAS CON T-TEST
#El proposito de la comparacion es determinar como se compara
#la media de una muestra  con la de la poblaci?n,
#se podr?a pensar que la media de la muestra que es significativa y que 
#coincide con la media de la media de la poblaci?n.
#Para en este ejercicio utilizaremos prueba de hip?tesis
#Se utiliza una prueba t (funci?n t.test() ) de una muestra para comparar LA MEDIA  de una
#muestra con un valor constante denotado MU. 
#La prueba tiene la hip?tesis nula de que la media de la poblaci?n
#es igual a MU y la hip?tesis alternativa de que no es igual a MU

###################################################
#t.test
##################################################
#T.test tiene 2 sintaxis dependiendo si estamos usando 1 o 2 datasets
# S3 method for default
#Para 1 dataset
#t.test(formula, data,  
#       alternative = c("two.sided", "less", "greater"),
#       mu = 0,  var.equal = FALSE,
#       conf.level = 0.95,.)

#Para dos datasets 
#t.test(x, y = NULL,
#       alternative = c("two.sided", "less", "greater"),
#       mu = 0, paired = FALSE, var.equal = FALSE,
#       conf.level = 0.95, .)

#cargamos las librer?as requeridas para este ejercicio
rm(list=ls())
require(reshape2)
require(ggplot2)
require(plyr)
require(dplyr)
require(magrittr)
require(MASS)
library("ggpubr")
#Cargamos la data
data(tips)

#Entendamos que tiene la data
names(tips)
mean(tips$tip)
unique(tips$sex)
unique(tips$day)

#Entonces suponemos que la propina promedio es 2.5 (exactamente 
#no mayor o menor sino 2.5)
# 2.5 ser?a la hip?tesis nula o H0
t.test(tips$tip, alternative="two.sided", mu=2.5,conf.level=.95)
#Notamos el valor t-value , en este caso es un numero alto,
#si el t value es alto podemos rechazar la hip?tesis  H0 de 
#que es  2.5$
#O visto de otra manera, mientras mayor sea la magnitud de t, 
#mayor ser? la evidencia en contra de la hip?tesis nula.
# La funci?n tambi?n permite definir el intervalo de confianza deseado,
#el valor por defecto es 95%
# Se puede intentar con varios par?metros de mu e intervalo de confianza
#en este caso nosotros sabemos que la media es 3, pero la prueba indica H0 mu=3.5
#t.test mide la diferencia en unidades de error estandar, recordando el error 
#estandar es igual a la SD/sqrt(n), En este caso sd= 1.38 , n=nrow(tips$tip)-1
#el error estandar sale=0.08 . El estadistico t calculado es 0.2988 


#Veamos c?mo se compara la distribuci?n con el estad?stico t-value
randomTdist <- rt(30000, df=NROW(tips)-1 )
head(randomTdist)
tipTest <-t.test(tips$tip, alternative="two.sided", mu=2.95)



class(tipTest)
names(tipTest)
tipTest$statistic
tipTest$null.value
#Recuerde t-value  indica cuanta evidencia hay  de que h0 no sea verdadero

#Ploteamos esta distribuci?n 
ggplot(data.frame(x=randomTdist)) +
  geom_density(aes(x=x),fill="grey",color="blue")+
  geom_vline(xintercept=tipTest$statistic, color="red")+
  geom_vline(xintercept=mean(randomTdist)+c(-1.96,1.96)*sd(randomTdist),linetype=2)+
  annotate("text",x=c(-1.96,1.96),y=.5,label=c("-1.96","+1.96"),hjust=0)

#Qu? vemos en el grafico , 
#Primero el ?rea sombreada es una distribuci?n de Student 
#vemos 2 l?neas que est?n  en +-1.96 sigma  y corresponden al sigma de la dist T para 95% CL,
#por ultimo vemos una l?nea s?lida que corresponde  a la variable statistics 
#(valor t), este valor puede cambiar si cambiamos el mu
# el estudiante podr? jugar con  el mu de la prueba y graficar de nuevo 

#Podemos tambi?n hacer una prueba de "un solo lado" , esto es 
# si el valor es mayor o menor , veamos con mayor 

mayorq <-t.test(tips$tip, alternative="greater", mu=2.5)
#Si t-value es muy peque?o posiblemente H1  no es verdad
mayorq$statistic
mayorq$null.value
mayorq$estimate

ggplot(data.frame(x=randomTdist)) +
  geom_density(aes(x=x),fill="grey",color="grey")+
  geom_vline(xintercept=mayorq$statistic ,color="red")+
  geom_vline(xintercept=mean(randomTdist)+c(1.96)*sd(randomTdist),linetype=2)+
  annotate("text",x=c(1.96),y=.5,label=c("+1.96"),hjust=0)




#El estudiante puede jugar con el valor de MU
################################################################

#t.test nos permite trabajar con 2 bases separadas (x,y)
#pero para esto las varianzas de ambas deben ser similar
#Por ejemplo  queremos comparar la propina que dan hombres y mujeres
#Para esto hagamos una aggregate que b?sicamente permite agrupar (en este caso por
#sexo) y sacar una estad?stica (en este caso nos interesa media y varianza)

#Comprobando con aggregate
aggregate(data=tips, tip~sex , mean)
aggregate(data=tips, tip~sex, var) #las varianzas no son iguales por lo tanto deberemos ajustar el par?metro
#var.eq=FALSE


hombres<- tips %>% filter(sex=="Male")
mujeres<- tips %>% filter(sex=="Female")
t.test(x=hombres$tip, y=mujeres$tip  , var.equal=FALSE )
#Con t= 1.4 nos indica que las medias de los dos subsets no son iguales



#Podemos probar si cada subconjunto es o no distribuido normalmente 
#mediante:
#TEST PARAMETRICOS 
#============================
#Muchos test estad?sticos como la correlaci?n, ANOVA, regresi?n y
#t-test, asumen ciertas caracter?sticas acerca de la data
#Se requiere que la data siga una distribuci?n normal o gaussiana 
#estos tipos de tests se conocen como test param?tricos 
#porque depende de la distribuci?n de la data 

#La normalidad (que sigue distribuci?n normal) y otros supuestos deben ser probados 
#para que las conclusiones sean reales
#Los siguientes test preliminares  son hechos para asegurar que 
#se cumplen las condiciones 

#Hay distintas pruebas de normalidad:
#Kolmogorov-Smirnov (K-S) 
#Shapiro-Wilk's test.

#SHAPIRO
#==================================================
#La formula  es : shapiro.test(x) donde x es un vector de valores 
#La prueba rechaza la hip?tesis de normalidad cuando el 
#valor p es menor o igual a 0.05.
shapiro.test(rnorm(100, mean = 5, sd = 3)) #note el valor de p-value 
shapiro.test(runif(100, min = 2, max = 4)) #note el valor de p-value , porque sale seste valor ? 
shapiro.test(tips$tip)


#p-value sale bajo, no es buena se?al
#investiguemos de cada grupo 
shapiro.test(tips$tip[tips$sex=="Female"])
shapiro.test(tips$tip[tips$sex=="Male"])
#ninguno tiene distribuci?n normal 
#Revisemos  visualmente 
ggplot(tips, aes(x=tip, fill=sex)) +
  geom_histogram(binwidth=.5, alpha=1/2)+
  facet_wrap(~sex)
#En el primer caso mujeres, podemos ver que el histograma tiene varias subidas y bajadas 
#En el segundo caso hombre, podemos ver que el hostograma parece tener un skew negativo 
#Esto comprueba que los p-value calculados < .05 es correcto 
#ANSARI-BRADLEY
#================================================
#Otro test  que se puede usar es el Ansari-Bradley 
#Este test  compara 2 datasets, la hip?tesis H0 o nula es que 
#los dos  dataset tienen la similar  distribuci?n (varianza) 
#y localidad (media)

ansari.test(tip~sex ,tips)


#Esto no da un p-value de 0.37 que es alto indicando que 
#la varianza es similar 

#ahora podemos hacer la comparaci?n de si las medias  por sex son iguales
t.test(tip~sex ,data=tips, var.equal=TRUE)
#El valor t que obtengo es -1.3 esto quiere decir que las medias de los dos
#factores no son iguales, pero la diferencia cae dentro del error estandar 

#Recuerde que t-value se expresa en terminos del error  est?ndar.
#Este es utilizado para definir un rango en el cual caer? cualquier
#valor  que se muestree. 
# En general  el nivel de confianza CL = estadistica +/-  el error estandar
#vamos a crear un peque?o data frame con los valores 
#veamos  graficamente , para esto primero crearemos  un data.frame con los datos que nos interesa
tip_sum <- ddply(tips , "sex", summarize ,
                 tip.mean=mean(tip), tip.sd=sd(tip),
                 conf_menor =tip.mean -2*tip.sd/sqrt(NROW(tip)),
                 conf_mayor=tip.mean+ 2*tip.sd/sqrt(NROW(tip)))

head(tip_sum)

ggplot(data=tip_sum, aes(x=tip.mean, y=sex))+
  geom_point()+
  geom_errorbar(aes(xmin=conf_menor , xmax=conf_mayor))
#Note que estamos mostrando el rango  de la data por sexo tomando el error , 
#esto es la media + 2 veces el error est?ndar (que es la desviaci?n/raiz de N)
#Por lo tanto t.test ten?a raz?n al decir que las medias no provienen de la misma poblaci?n
#Y Ansari ten?a razon al indicar que la localidad (varianza)  son similares 

#Veamos otro ejemplo con  2 datasets
#creamos 2 datasets
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Creamos el data.drame
my_data <- data.frame( 
  group = rep(c("Mujeres", "Hombres"), each = 9),
  weight = c(women_weight,  men_weight)
)



# Probamos si son normales
with(my_data, shapiro.test(weight[group == "Hombres"]))# p = 0.1
with(my_data, shapiro.test(weight[group == "Mujeres"])) # p = 0.6

#Los p-values indican que son relativamente normales (>>.05)

res <- t.test(women_weight, men_weight, var.equal = TRUE)
res

#t-test nos indica que no vienen de la misma distribucion normal( sus medias no son iguales)
#t es el estadistico calculado (t = 2.784),
#df los grados de libertad , en este caso se calcula como:
# n1+n2-2 (df= 16),
#p-value es el valor de significancia (p-value = 0.01327).
#intervalos de confianza es 95% (conf.int = [4.0298, 29.748]);
#las medias calculadas (mean = 68.9888889, 52.1).
#de los datos anteriores concluimos que los 2 datasets no provienen 
#de la misma poblaci?n


#Comprobemos  gr?ficamente 
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )


ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")




######################################################
#ANOVA
######################################################
#Las pruebas anteriores nos han servido para comparar  dos datasets  
#sus varianzas o medias, sin embargo hay veces que debemos comparar 3 o m?s 
#datasets, para esto usaremos  una nueva rutina llamada ANOVA.
#Un an?lisis de varianza (ANOVA) prueba la hip?tesis de que las medias
#de dos o m?s poblaciones son iguales. (mu0=mu1=mu2)
#Los ANOVA eval?an la importancia de uno o m?s factores al comparar
#las medias de la variable de respuesta en los diferentes 
#niveles de los factores.
#La hip?tesis nula establece que todas las varianzas de 
#la poblaci?n (o de los niveles de los factores) son iguales
#mientras que la hip?tesis alternativa establece que al menos una 
#es diferente.
#sintaxis:  aov(formula, data = NULL, projections = FALSE, qr = TRUE,contrasts = NULL, .)

###########
#Creamos dos vectores con media distinta , recuerde H0 => mu1=mu2 , Ha mus distintas
vvvar1 <- rnorm(100, mean=200 , sd= 100)
vvvar2 <- rnorm(100, mean=200 , sd= 100)
vvvdf1 <- data.frame(X="gr1", Y=vvvar1)
vvvdf2 <- data.frame(X="gr2", Y=vvvar2)
dftot <- rbind(vvvdf1, vvvdf2)
vvvaov <- aov(Y~X , dftot)
summary(vvvaov)
#####

tips
tipAnova <- aov(tip~day , tips)
tipIntercepts <- aov(tip~day , tips  ) #esto est? solo para validar porque hay que poner el -1
class(tipAnova)
class(tipIntercepts)
unique(tips$day)
names(tipAnova)
tipAnova$coefficients
#Note que ANOVA ha calculado un coeficiente para cada muestra (d?a) 
tipIntercepts$coefficients 
#Note que con -1 en la f?rmula ANOVA entiende que uno de los valores es intercept 
#que significa el valor de media para x=0 lo que no tiene sentido calcular 
summary(tipAnova)
#Dos estad?sticos nos interesan  F-value y Pr(>F)

#El F-value indica cual es la diferencia en el tratamiento  de las muestras.
#Si este valor es BAJO (tendiente a 0 ) indica que NO existe diferencias
#en la varianza y medias de la muestras m?s alla de lo que puede ser atribuido a 
#temas aleatorios, por lo tanto la hip?tesis H0 NO se rechaza (se mantiene) . 
#por el contrario, si el valor de F se vuelve alto  indicar?a que la medias y varianzas 
#son distintas, es decir hay efecto en el tratamiento , H0 se rechaza

#La palabra tratamiento es utilizada en estad?stica para definir las diferencias a las que se esta 
#sometiendo un muestra. En este caso el tratamiento son los factores  que afectan la cantidad
#de propina a trav?s de los diferentes d?as. Si no hubieran factores que afectan entonces la propina 
#tendr?a igual varianza y media todos los d?as.

#El segundo par?metro es Pr(>F) : 
#El valor P nos dice la probabilidad de obtener este valor de la estad?stica F  
#con respecto su nivel de significancia.
#Si el valor de P es mucho mayor que el nivel de significancia indica que la probabilidad de
#obtener el valor F es alta y por lo tanto H0 se rechaza 

#Si por el contrario  el valor P es menor que nuestro nivel de significancia, rechazamos la hip?tesis nula,
#o aceptamos la hipotesis alternativa (como quiera verse)

#En otras palabras H0 se mantiene si F->0 y Pr->1 (flecha indica tiende a)

#Por ?ltimo grafiquemos a ver como se comparan los 4 d?as

#Usaremos la funci?n ddply;
#esta funcion divide un data.frame, aplica una funci?n y devuelve los resultados en
#otro data.frame. Para cada subconjunto de datos
#en este caso, tips es el data.frame
#day es el subconjunto o factor (hay 4)
#summarize es la funci?n que aplica al subconjunto
#luego crea las columnas con los resultados de la funci?n
tipsxdia <- ddply(tips,"day", summarize , 
                  tip.mean=mean(tip), tip.sd=sd(tip),
                  length=NROW(tip),
                  Lower=tip.mean - 2*tip.sd/sqrt(length),
                  Upper=tip.mean + 2*tip.sd/sqrt(length))

#En este caso estamos usando t=2 para sacar el error estandar 
#pero en realidad el error estandar no es igual para todos los casos ,porque 
#los grados de libertad no son iguales.
#El siguiente data.frame  es calculado con los  t values obtenidos del
#95% de IC y grados = observaciones -1 . Es m?s preciso 
tipsxdia <- ddply(tips,"day", summarize , 
                  tip.mean=mean(tip), tip.sd=sd(tip),
                  length=NROW(tip),
                  frac=qt(p=.95 , df=length-1),
                  Lower=tip.mean - frac*tip.sd/sqrt(length),
                  Upper=tip.mean + frac*tip.sd/sqrt(length))
tips
tipsxdia

ggplot(tipsxdia, aes(x=tip.mean, y=day))+
  geom_point()+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)

#PARTE 2 TAREA :
################################
library(MASS)
data(Boston)
attach(Boston)
names(Boston)
tail(Boston)
head(Boston)

#PROBLEMA 1
###############
#Para este problema  deber? cargar la libreria MASS:
#Estamos interesados en el dataset Boston
#Yo tengo la teor?a que la media del ?ndice de criminalidad de Boston 
#es mayor a 3.7 

#a) Primero compruebe que la data es normal. Interprete y documente el resultado   

hist(Boston$crim)
plot(density(Boston$crim))
shapiro.test(Boston$crim)
#Puede que no lo sea. Esto puede ser normal con una data de m?ltiples factores. 
#b) compruebe si la data es normal para el estrato mayor de 700$ de impuesto (tax)
#Documente el resultado
mayoresa700 <- Boston[ Boston$tax > 700,]
hist(mayoresa700$crim)
plot(density(mayoresa700$crim))

#prueba de shapiro
shapiro.test(mayoresa700$crim)
#para p-value <0.5 la data es normal porque > 0.05

#Si resultara normal, puede proceder con la prueba de hip?tesis mencionada
t.test(mayores700$crim, alternative="greater", mu=3.7,conf.level=.95)
#se rechaza porque el valor es -179
#Si no fuera normal, trate de encontrar un rango de impuesto para el cual la data es normal 
#Los  siguientes gr?ficos podr?a ayudar.

#c) En base a los diagramas de caja , encuentre un rango donde la distribucion no es normal  
#y ejecute la prueba de normalidad con ese rango 
#Cree la instruccion en base a los ejemplos. Ejecute  y documente e interprete el resultado 
ggplot(data=Boston , aes(x=Boston$tax ))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  coord_flip()

Boston_subset <- Boston[ Boston$tax > 300 & Boston$tax< 450,]
ggplot(data=Boston_subset , aes(x=Boston_subset$crim ))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1)+
  coord_flip()

shapiro.test(Boston_subset$crim)
t.test??
#d) Pruebe la hipotesis alternativa  con otras opciones ( greater , less )
t.test(Boston_subset$crim, alternative="greater", mu=3.7, conf.level=.95)
t.test(Boston_subset$crim, alternative="greater", mu=3.7,conf.level=.95)
t.test(Boston_subset$crim, alternative="less", mu=3.7, conf.level=.95)


#PROBLEMA 2
#a) Queremos comparar las medias de criminalidad por estrato social
#como normalmente  tenemos  varios  estratos debemos investigar esto con Anova.
#La base original  no nos provee de una variable categ?rica de estrato 
#asi que le vamos a construir en base a la variable lstat, con el siguiente codigo 
rm(list=ls())
data(Boston)
head(Boston)
Boston1  <- Boston #copiamos el dataset 
Boston1$est_social <-c("nada")  #creamos una nueva columna con nada 
head(Boston1) #veamos los primeros registros 
tail(Boston1)  # veamos los ultimos registros 
#Poblamos los registros 
Boston1 %<>% mutate( est_social= ifelse(Boston1$lstat >10, "alto","ninguno") )
head(Boston1[Boston1$lstat>20,])
Boston1 %<>% plyr::mutate( est_social = ifelse(lstat >= 0 & lstat <= 10, 'bajo',
                                               ifelse(lstat >10 & lstat <=20, 'medio',
                                                      ifelse(lstat >20 & lstat<=30, 'alto', 'muyalto'))))
Boston1
#ejecute el analisis con Anova , cree la instruccion en base a los ejemplos 
#ejecute, Documente y analice el resultado 
est_Anova <- aov(crim~est_social, Boston1) 
est_Intercepts <- aov(crim~est_social -1, Boston1) 
class(est_Anova)
class(est_Intercepts)
unique(Boston1$est_social)
names(est_Anova)
est_Anova$coefficients
est_Intercepts$coefficients 
summary(est_Anova)
crimxstr <- ddply(Boston1,"est_social", summarize , 
                  crim.mean=mean(crim), crim.sd=sd(crim),
                  length=NROW(crim),
                  frac=qt(p=.95 , df=length-1),
                  Lower=crim.mean - frac*crim.sd/sqrt(length),
                  Upper=crim.mean + frac*crim.sd/sqrt(length))

ggplot(crimxstr, aes(x=crim.mean, y=est_social))+
  geom_point()+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)

