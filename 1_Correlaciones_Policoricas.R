
library(foreign)

lapop<-read.dta("Datos/LAPOP_2012.dta")
udp  <-read.dta("Datos/UDP_2023.dta")
cep  <-read.dta("Datos/CEP_2023.dta")

library(polycor)

?polyserial

summary(lapop$q10new)
summary(lapop$q10g)
summary(lapop$edlevel)

library(car)

q10new #Ingreso Familiar
q10g   #Ingreso Personal


ingreso1<-recode(lapop$q10new,"
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor=T)

ingreso2<-recode(lapop$q10g,"'INAP (No trabaja ni est? jubilado)'=NA;
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor=T)

ingreso3<-recode(udp$P94,"'No Sabe'=NA;'No Contesta'=NA")


educacion<-recode(lapop$edlevel,"
'Ninguno'='B?sica';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor=T)

summary(udp$P82)
educacion1<-recode(udp$P82,"
'Sin Estudios'='B?sica';'B?sica Incompleta'='B?sica';'B?sica Completa'='B?sica';
'Media Incompleta'='Media';'Media Completa'='Media';
'T?cnica Superior No Universitaria Incompleta'='T?cnica';
'T?cnica Superior No Universitaria Completa'='T?cnica';
'Universitaria Incompleta'='Universitaria';
'Universitaria Completa'='Universitaria';
'Postgrado'='Universitaria';'No Contesta'=NA")





summary(ingreso1)
summary(ingreso2)
summary(educacion)

summary(ingreso3)
summary(educacion1)

?polychor


polychor(ingreso1,educacion,std.err=T)
polychor(ingreso2,educacion,std.err=T)

polychor(ingreso3,educacion1,std.err=T)






cor.test(as.numeric(ingreso1),as.numeric(educacion))
cor.test(as.numeric(ingreso2),as.numeric(educacion))

library(Hmisc)

matriz1<-data.frame(ingreso1,educacion)
matriz2<-data.frame(ingreso2,educacion)

rcorr(as.matrix(matriz1))
rcorr(as.matrix(matriz2))


library(ggm)
?pcor

summary(lapop$chi60)

chi60<-recode(lapop$chi60,"
'NS'=NA;'NR'=NA;'DK'=NA;'N/A'=NA")
summary(chi60)

matriz1<-data.frame(ingreso1,educacion,chi60)
matriz2<-data.frame(ingreso2,educacion,chi60)

matriz1a<-na.omit(matriz1)

head(matriz1a)
pcor(c("ingreso1","chi60","educaci?n"),var(matriz1a))
