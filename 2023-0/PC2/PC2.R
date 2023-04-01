d=read.csv("C:\\Users\\thexn\\Downloads\\ESTADISTICA\\PC2\\ozono.csv",sep = ";",
           stringsAsFactors = T)

attach(d)
library(agricolae)
library(EnvStats)



##  A)

## Humedad (%)
  OvsH<-cor(O3,humidity)

## Temperatura (°F)
  OvsT<-cor(O3,temp)

## Visibilidad (millas)
  OvsV<-cor(O3,vis)

##Graficas de dispersión  
  
  par(mfrow=c(2,2))
  
    plot(humidity,O3,xlab = "Humedad (%)", ylab="Nivel de ozono")
    plot(temp,O3,xlab = "Temperatura (°F)", ylab="Nivel de ozono")
    plot(vis,O3,xlab = "VIsibilidad (millas)", ylab="Nivel de ozono")
    
  par(mfrow=c(1,1))
  
#Comaparación entre coeficientes de correlación  
  
  tabla_a<-as.table(cbind(OvsH,OvsT,OvsV))
  colnames(tabla_a)<-c("Humedad","Temperatura","Visibilidad")
  rownames(tabla_a)<-"Coef. correlación"
  tabla_a
  write.table(x=tabla_a, file = "Tabla_a.txt", sep = ",", 
              row.names = F, col.names = T)##PARA EXPORTAR TABLA
  
  
## B) Modelo de O3 y la temperatura

  rltemp<-lm(O3~temp)
  summary(rltemp)
  
  
## C)

  ##Variable Indice de Temperatura y Humedad
  
  ITH<-temp-(0.55-0.55*humidity/100)*(temp-58)
  OvsITH<-cor(O3,ITH)
  
  ##Comparación de correlaciones del nivel de ozono con respecto al ITH y a la
  ##temperatura
  
  tabla_c<-as.table(cbind(OvsT,OvsITH))
  colnames(tabla_c)<-c("Temperatura","ITH")
  rownames(tabla_c)<-"Coef. correlación"
  write.table(x=tabla_c, file = "Tabla_c.txt", sep = ",", 
              row.names = F, col.names = T)##PARA EXPORTAR TABLA
  
  ## Estimación del modelo lineal
  
  rlITH<-lm(O3~ITH)
  summary(rlITH)
  
  plot(ITH,O3,xlab = "Índice de Temperatura y Humedad", ylab="Nivel de Ozono",
       main="Grafica N°2: Modelo de regresión lineal del nivel de ozono \ny el ITH")
  abline(rlITH,col="red",lwd=3)

## D)
  
  est_ITH<-54-(0.55-0.55*51/100)*(54-58)
  est_ITH<-data.frame(ITH=c(est_ITH))
  
  predict(object = rlITH, newdata = est_ITH)
  
## E)
  
  #variable de los Umbrales
  
  umbralITH<-ITH
  
  ##Definition de los umbrales
  
  umbralITH[72>=ITH]="Normal"
  umbralITH[ITH>72 & ITH<=78]="Alerta"
  umbralITH[ITH>78 & ITH<=83]="Peligro"
  umbralITH[ITH>83]="Emergencia"
  
  ##Jerarquia de los niveles de ITH
  
  orden<-c("Normal","Alerta","Peligro","Emergencia")
  umbralITH <- factor(umbralITH,levels = orden,ordered = T)
  
  ##Diagrama de cajas del nivel de ozono según el nivel de ITH
  
  boxplot(O3~umbralITH, horizontal = T, xlab= "Nivel de ozono",
          ylab = "Niveles de ITH",main="Gráfica N°3: Nivel de ozono según los niveles de ITH", 
          col=c("#5EC30D","#D6DA0A","#E4A10F","#C3270B"))
  
##F
  
  tabla_f<-summaryFull(O3[umbralITH=="Normal"])
  write.table(x=tabla_f, file = "Tabla_f.txt", sep = ",", 
              row.names = T, col.names = T)##PARA EXPORTAR TABLA
  