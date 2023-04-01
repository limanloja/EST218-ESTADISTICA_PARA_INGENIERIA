# PREGUNTA 1: 

## A)

### Probabilidad de que el consumo de una familia del distrito A sea mayor a  
### 50 m3

p1<-1-pexp(5,0.25)
p1

### Probabilidad de que la últimavivienda elegida ocurra después de la selección 
### 23, pero antes de la trigésimasegunda selección

P1a<-pgeom(30,p1)-pgeom(22,p1)  ##Dado que en el programa se considera el ultimo 
                                ##intento antes de obtener el exitp, se realizó la 
                                ## la transformación correspondiente.
P1a

## B)

### Probabilidad de que el consumo de una familia del distrito B sea mayor a  
### 50 m3

pA=p1
pA

pB<-1-pnorm(5,4,8)
pB

p1b<-(1-pA)*pB+(1-pB)*pA
p1b

# PREGUNTA 2

### A)

### B)

#PREGUNTA 3

p3X<-1-ppois(1,2.25)
p3X
p3Y<-1-ppois(1,1.5)
p3Y

p3<-p3X+p3Y-p3Y*p3X
p3
