#Problema 3

require(ggplot2)
require(reshape2)
require(scales)
head(economics)
cor(economics[,2:6])
cor_economics <-cor(economics[,2:6])
cor_economics

#esto se ve mejor en un mapa de calor ,pero para esto habrá que poner en un #una sola columna ,
#no como esta 

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
