d <- read.csv("C:\\Users\\thexn\\Downloads\\ESTADISTICA\\PC\\Encuesta.csv",
              stringsAsFactors = TRUE)
d$educacion <- with(d, factor(educacion, levels=c('Bachillerato incompleto',
                                                  'Bachillerato completo', 'Universitario incompleto',
                                                  'Universitario completo', 'Post-universitario'), ordered=TRUE))
d$satlab <- with(d, factor(satlab, levels=c('Muy insatisfecho', 'Insatisfecho', 'Neutro',
                                            'Satisfecho', 'Muy satisfecho'), ordered=TRUE))
library(agricolae)
library(psych)
library(gmodels)
library(EnvStats)
library(fdth)

#Pregunta 1

familia<-d$familia
  ## COLUMNAS PARA ELABORAR LA TABLA DE FRECUENCIAS
numero<-c(min(familia):max(familia))
f<-table(familia)
fr<-prop.table(table(familia))
P<-prop.table((table(familia)))*100
  ## ELABORACIÓN DE LA TABLA DE FRECUENCIAS
tabla1<-cbind(as.character(numero),as.character(f),round(fr,5),round(P,2))
colnames(tabla1)<-c("Numero de integrantes", "Numero de familias",
                    "Proporcion de familias","Porcentaje de familias")
write.table(x=tabla1, file = "TABLA1.txt", sep = ",", 
            row.names = FALSE, col.names = TRUE)##PARA EXPORTAR TABLA
  ## Grafica de varas 
plot(table(familia),xlab="Número de integrantes de la familia",
     ylab="Número de trabajadores",
     main="Gráfica N°1: Número de integrantes de familia \nde los trabajadores")

#Pregunta 2

ingrmens<-d$ingrmens
 ##Determinacion de intervalos de misma amplitud a partir del numero de categorias
 ##propuesto
range<-max(ingrmens)-min(ingrmens)
Amplitud<-ceiling(range/10)
intervalos<-seq(min(ingrmens),min(ingrmens)+10*Amplitud,Amplitud)
  ##Histograma de frecuencias 
histograma<-hist(ingrmens,breaks = intervalos,xlab = "Ingreso mensual",ylab="Numero de trabajadores",
     main="Histograma de ingreso mensual de trabajadores",)
  ##Obtención de la tabla de frecuencias simples y acumuladas
tabla2<-table.freq(histograma)
colnames(tabla2)<-c("Desde","Hasta","Marca de clase","Numero de trabajadores","Porcentaje",
                    "Numero de trabajadores acumulado", "Porcentaje acumulado")
tabla2
write.table(x=tabla2, file = "TABLA2.txt", sep = ",", 
            row.names = FALSE, col.names = TRUE)##PARA EXPORTAR TABLA
  ##Ojiva ~ grafico de porcentajes acumuladas
ls<-c(min(tabla2$Desde),tabla2$Hasta)
acum=c(0,tabla2$`Porcentaje acumulado`)
plot(ls,acum,xlab="Ingreso mensual",ylab="Porcentaje acumulado",
     main="Ojiva de frecuencias de ingreso mensual",col="red",)
lines(ls,acum, col="red")

#Pregunta 3

##Graficos de frecuencias relativas
par(mfrow=c(3,2))
plot(fdt(d$ingrmens[d$educacion=="Bachillerato incompleto"]),type='rfh',
     xlab = "Ingreso mensual",ylab="Proporción de trabajadores", 
     main="Bachillerato incompleto",
     col="red")
plot(fdt(d$ingrmens[d$educacion=="Bachillerato completo"]),type='rfh',
     xlab = "Ingreso mensual",ylab="Proporción de trabajadores", 
     main="Bachillerato completo",
     col="orange")
plot(fdt(d$ingrmens[d$educacion=="Universitario incompleto"]),type='rfh',
     xlab = "Ingreso mensual",ylab="Proporción de trabajadores", 
     main="Universitario incompleto",
     col="yellow")
plot(fdt(d$ingrmens[d$educacion=="Universitario completo"]),type='rfh',
     xlab = "Ingreso mensual",ylab="Proporción de trabajadores", 
     main="Universitario completo",
     col="green")
plot(fdt(d$ingrmens[d$educacion=="Post-universitario"]),type='rfh',
     xlab = "Ingreso mensual",ylab="Proporción de trabajadores", 
     main="Post-universitario",
     col="darkgreen")
par(mfcol=c(1,1))
##medidas de tendencia centrañ y dispercion
summaryFull(d$ingrmens~d$educacion)


#Pregunta 4

  ##Creaccion de la variable
vector<-ingrmens>=5000
vector<-as.numeric(vector)
  ##Proporción usando directamente datos de la variable
CrossTable(vector,format = 'SPSS')
  ##Proporción usando la tabla de frecuencias de la Pregunta 2
    ### el limite superior del segundo intervalo es 5000, por ende se puede usar 
    ### el porcentaje acumulado para conocer la proporcion
  menos_5000<-tabla2[2,7]
  mas_5000<-100-menos_5000
  tabla4<-matrix(c(menos_5000,mas_5000))
  rownames(tabla4)<-c("Menos de 5000","Más de 5000")
  colnames(tabla4)<-c("Porcentaje (%)")
  tabla4<-as.table(tabla4)
  tabla4
  ## Grafico sectores circulares
  etiquetas<-c("Menor a \n5000 soles ","Mayor a \n5000 soles")
  pie(table(vector),labels=etiquetas, col=c("white","red"),
      main="Grafica N°6: Proporcion de trabajadores de \ningresos menor o mayor a 5000 soles")
#Pregunta 5

Q1<-quantile(ingrmens,probs = 0.25,names = FALSE)
Q3<-quantile(ingrmens,probs=0.75,names = FALSE)
RIC<-Q3-Q1
LS<-Q3+1.5*RIC
mean(ingrmens[ingrmens>LS])

#pregunta 6

boxplot(d$edad~d$genero,main="Grafica N°7: Edad de los trabajadores según su género",
        xlab="Género",ylab="Edad del trabajador")
summaryFull(d$edad~d$genero)


#Pregunta 7

  #a)
boxplot(d$edad,main="Grafico N° 8: Edad de los trabajadores",ylab="Edad")
describe(d$edad)
  #b)
describeBy(d$edad,group = d$genero)
par(mfrow=c(1,2))
hist(d$edad[d$genero=="Hombre"],xlab="Edad",ylab="Número de trabajadores",main="Hombre")
hist(d$edad[d$genero=="Mujer"],xlab="Edad",ylab="Número de trabajadores",main="Mujer")
title(main="Grafico N° 9: Histograma de frecuencia de la edad según genero",x = 3,y=2,cex.main=1,line=-1,outer =T)
par(mfrow=c(1,1))
  #c)
tblSatLab<-prop.table(table(d$satlab))*100
tblSatLab<-cumsum(tblSatLab)
cbind(tblSatLab)

