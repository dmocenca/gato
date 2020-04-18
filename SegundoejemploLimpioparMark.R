#*******************************************
# Dr. Daniel Mocencahua Mora
# 25 de marzo de 2020
# Matemáticas para ingeniería 2
# Segundo ejemplo 
#*******************************************

#Preparación de datos ----

#Carga las librerías
  library(readxl)
  library(ggplot2)
  library(modeest)
  library(tidyverse)

 #Asigna el archivo a una tabla
 datos <- read_excel("Eje01.xlsx")
 
 View(datos)

 datos #ver datos en consola
 class(datos) #te idnica el tipo del cual es datos

 names(datos) #Muestra las variables en datos

 edad=c(datos$AGE) #genera el vector edad desde la tabla de datos

 edad
 

 genero=data.frame(datos$'GENDER (1=M)')
 genero 

 

#Medidas de tendencia central----

  #Media
  mean(edad)

  #Mediana
  median(edad)

  #Moda
  mfv(edad) #Indica el o los valores con más frecuencia
  
#Medidas de dispersión ----
  range(edad) #rango de edades
  min(edad)
  max(edad)
  max(edad)-min(edad)  #rango
  var(edad) #varianza
  sd(edad) #dispersión estandard
  
  
  #Prueba Kolmogorov Smirnov (muestras grandes)
  ks.test(edad, "pnorm", mean(edad), sd(edad))

  summary(edad)
  
# Tabla de frecuencias ----
  fabs<-table(edad) #tabla de frecuencias absolutas
  fabs  
  
  frel<- fabs/300 #tabla de frecuencias relativas
  frel 
  
  fabsacum<-cumsum(fabs) #freceuncias acumuladas absolutas
  fabsacum
  
  frelacum<- cumsum(frel) #freceuncias acumuladas relativas
  frelacum
  
  tablafrecuencias<-cbind(fabs, frel,fabsacum,frelacum)
  tablafrecuencias #Tabla con estas frecuencias
  
# Gráficos----
  
  #Histograma----
  hist(datos$AGE) #Histograma de las edades en la tabla datos
  
  hist(datos$AGE, breaks = 12, col = "lightblue", border = "black") #Agregando color

  library(ggplot2) #PAra usar esta librería los datos siempre deben estar en dataframes
  df = datos
  ggplot(df,aes(x=edad))+geom_bar()  #Diagrama de barras
  
  ggplot(df,aes(x=edad))+geom_bar(fill="steelblue")  
  
  ggplot(df, aes(x=edad))+geom_histogram(color="darkblue", fill="lightblue")
  
  
  ggplot(df,aes(x=edad))+geom_bar(fill="steelblue")+labs(title="Histograma de edades",x="Edad (años)", y = "Frecuencia")
  
  ed<-data.frame(fabs)
    ggplot(ed,aes(x=edad, y=Freq))+geom_point()
    
    
  
  
    
    
    
    
    #Pastel ----
 
  
  
  sexo<-table(genero)
  sexo
  sexo<-data.frame(sexo)
  sexo
  
  sexos=sexo
  sexos 
  genero=c("mujer","hombre")
  genero
  NuevosDatos = data.frame(genero,sexos)
  NuevosDatos
  colnames(NuevosDatos)= c('Género','x','freq')
  NuevosDatos
  NuevosDatos$x=NULL
  NuevosDatos
  sexos<-NuevosDatos
  sexos
  
  
  sexos$freq<-sexos$freq/300
  sexos$freq<-sexos$freq*100
  
  sexos
  
  # Pastel con ggplot2
  
  past1<-ggplot(sexos, aes(x=1, y=freq,fill=Género)) + geom_bar(stat='identity', colour='black') + coord_polar(theta='y')
  
  past2<-ggplot(sexos,aes(x="",y=freq, fill=Género))+ geom_bar(stat = "identity",color="white")+geom_text(aes(label=freq),position=position_stack(vjust=0.5),color="white",size=6)+coord_polar(theta="y")
  past2
  
  past2 + theme_bw() + theme(axis.ticks= element_blank(), axis.title= element_blank(), axis.text.y= element_blank())

  #Pastel solo con R
  
  porcentajes <- as.numeric(sexos$freq)
  porcentajes
  
  etiqueta<-c("Hombre", "Mujer")
  
  etiqueta
  
  etiquetas <- paste(etiqueta, porcentajes)
  etiquetas
  
  etiquetas <- paste(etiquetas, "%", sep = "")
  etiquetas
  

  pie(porcentajes, etiquetas, main = "Gráfico de pastel", sub = "Porcentaje de género")  
  
  