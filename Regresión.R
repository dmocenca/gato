#*******************************************
# Daniel Mocencahua Mora
# 3 de abril de 2020
# Matemáticas para ingeniería 2
# Regresión de COVID-19
# 
#***************************




#Carga la librería para leer Excel
#install.packages("readxl")
library(readxl)

#Instala librería para gráficos
#install.packages("ggplot2")
library(ggplot2)

#en googl drive

COVID19MEXGR <- read_excel("C:/Users/modgu/OneDrive - Benemérita Universidad Autónoma de Puebla/00BUAPMaterias/2020MateIng2/REjer/COVID19MEXGR.xlsx", 
                           range = "A1:G46")

datos<-COVID19MEXGR

datos

dias  <- datos$Dia
class(dias)
casos <- datos$Confirmados
Data<-data.frame(dias,casos)
Data

ggplot(data = Data, aes(x = dias, y = casos)) + geom_point(colour = "blue") +  ggtitle("Diagrama de dispersión") 


