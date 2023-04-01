library(readxl)
HoledNotchedUTS <- read_excel("C:\\\\Users\\\\thexn\\\\Downloads\\\\HoledNotchedUTS.xlsx")
##View(HoledNotchedUTS)

attach(HoledNotchedUTS)

library(DescTools)

##PREGUNTA 1

## Nota: Se debe asumir que las distribuciones porvienen de 2 distribuciones normales

## A)

HOLED<-UTS[tipo=="Holed"]
NOTCHED<-UTS[tipo=="Notched"]

##VHOLED<-var(UTS[tipo=="Holed"])
##VNOTCHED<-var(UTS[tipo=="Notched"])

var.test(NOTCHED,HOLED,conf.level = 0.98) 

t.test(HOLED,NOTCHED,conf.level=0.98,var.equal = TRUE)

## B)

mHOLED<-mean(UTS[tipo=="Holed"])
mNOTCHED<-mean(UTS[tipo=="Notched"])

H<-UTS[tipo=="Holed"]>mHOLED
H<-as.numeric(H)
table(H)

pH<-12/30

N<-UTS[tipo=="Notched"]>mNOTCHED
N<-as.numeric(N)
table(N)

pN<-16/30

BinomDiffCI(12,30,16,30,conf.level = 0.92,method="wald")






