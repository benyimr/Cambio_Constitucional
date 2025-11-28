#SCRIPT DE REPLICACION DE MODELOS PNUD

#PAQUETES UTILIZADOS

library(foreign)
library(car)
library(Zelig)
library(ZeligChoice)
library(ordinal)
library(memisc)
library(survey)
library(effects)

#1) Analisis con LAPOP 2012

lapop2012<-read.dta(file.choose())

#################################################################################
#Construccion de las Variables de Resultado

lapop2012$chi60a<-Recode(lapop2012$chi60,"'Muy de acuerdo'='5';
'De acuerdo'='4';'Ni de acuerdo ni en desacuerdo'='3';'En desacuerdo'='2';
'Muy en desacuerdo'='1';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",
as.factor.result=T)

lapop2012$chi61a<-Recode(lapop2012$chi61,"
'Que el Congreso, como representante de la sociedad, decida a favor/en contra de'='0';
'Que la ciudadania decida votando a favor/en contra de dichos proyectos.'='1';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#################################################################################
#Construccion de las Variables Explicativas

#a)Sexo
lapop2012$q1a<-Recode(lapop2012$q1,"'Hombre'='1';'Mujer'='0';'NS'=NA;
'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#b1)Edad
lapop2012$q2a<-Recode(lapop2012$q2,"'17'=NA",as.numeric.result=T)
lapop2012$q2a<-as.numeric(lapop2012$q2a)

#b2)Edad en Tramos 
lapop2012$q2b<-Recode(lapop2012$q2,"'17'=NA;c(18,19,20,21,22,23,24,25,26,27,28,
29)='0';c(30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)='1';c(46,47,48,49,50,
51,52,53,54,55,56,57,58,59,60)='2';c(61,62,63,64,65,66,67,68,69,70,71,72,73,74,
75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90)='3'",as.factor.result=T)

#c1)Nivel Educacional en 5 Categorias
lapop2012$edlevela<-Recode(lapop2012$edlevel,"'Ninguno'='1';'Basica'='2';
'Media'='3';'Superior no universitaria'='4';'Universitaria'='5';'NS'=NA;
'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#c2)Nivel Educacional en 4 Categorias
lapop2012$edlevelb<-Recode(lapop2012$edlevel,"'Ninguno'='1';'Basica'='1';
'Media'='2';'Superior no universitaria'='3';'Universitaria'='4';'NS'=NA;
'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#d)Pertenencia a Pueblo Originario
lapop2012$chi50a<-Recode(lapop2012$chi50,"'Aymara'='1';'Rapa Nui'='1';
'Quechua'='1';'Mapuche'='1';'Atacameno'='1';'Coya'='1';'Kawesqar'='1';
'Yagan'='1';'Diaguita'='1';'No pertenece a ningun pueblo'='0';'NS'=NA;
'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#e1)Auto-posicionamiento Ideologico Izquierda-Derecha Categorica
lapop2012$l1a<-Recode(lapop2012$l1,"'1 Izquierda'='1';'2'='2';'3'='3';'4'='4';
'5'='5';'6'='6';'7'='7';'8'='8';'9'='9';'10 Derecha'='10';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)
lapop2012$l1a<-factor(lapop2012$l1a,levels=c('1','2','3','4','5','6','7','8',
'9','10'))

#e2)Auto-posicionamiento Ideologico Izquierda-Derecha Continua
lapop2012$l1b<-as.numeric(lapop2012$l1a)

#e3)Auto-posicionamiento Ideologico Izquierda-Derecha en Tramos
lapop2012$l1c<-Recode(lapop2012$l1,"'1 Izquierda'='1';c(2,3)='1';c(4,5,6,7)='2';
c(8,9)='3';'10 Derecha'='3';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",
as.factor.result=T)

#f1)Autoposicionamiento en el Eje Liberal-Conservador Categorica
lapop2012$chi2a<-Recode(lapop2012$chi2,"'1 Conservador'='1';'10 Liberal'='10';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)
lapop2012$chi2a<-factor(lapop2012$chi2a,levels=c('1','2','3','4','5','6','7','8',
'9','10'))

#f2)Autoposicionamiento en el Eje Liberal-Conservador Continua
lapop2012$chi2b<-as.numeric(lapop2012$chi2a)

#f3)Autoposicionamiento en el Eje Liberal-Conservador en Tramos
lapop2012$chi2c<-Recode(lapop2012$chi2,"'1 Conservador'='1';c(2,3)='1';c(4,5,6,
7)='2';c(8,9)='3';'10 Liberal'='3';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",
as.factor.result=T)

#g1)Apoyo a la Democracia en 3 valores
lapop2012$dem2a<-Recode(lapop2012$dem2,"
'A la gente como uno, le da lo mismo un regimen democratico que uno NO democratic'='1';
'La democracia es preferible a cualquier otra forma de gobierno, o'='2';
'En algunas circunstancias un gobierno autoritario puede ser preferible a uno dem'='0';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#g2)Apoyo a la Democracia en 2 valores
lapop2012$dem2b<-Recode(lapop2012$dem2,"
'A la gente como uno, le da lo mismo un regimen democratico que uno NO democratic'='0';
'La democracia es preferible a cualquier otra forma de gobierno, o'='1';
'En algunas circunstancias un gobierno autoritario puede ser preferible a uno dem'='0';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#h1)Satisfaccion con la Democracia en Escala
lapop2012$pn4a<-Recode(lapop2012$pn4,"'Muy satisfecha'='4';'Satisfecha'='3';
'INsatisfecha'='2';'Muy INsatisfecha'='1';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;
'N/A'=NA",as.factor.result=T)

#h2)Satisfaccion con la Democracia Dicotomica
lapop2012$pn4b<-Recode(lapop2012$pn4,"'Muy satisfecha'='1';'Satisfecha'='1';
'INsatisfecha'='0';'Muy INsatisfecha'='0';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;
'N/A'=NA",as.factor.result=T)

#i1)Respeto a Instituciones en Tramos
lapop2012$b2a<-Recode(lapop2012$b2,"'1 Nada'='1';c(2,3)='1';'4'='2';c(5,6)='3';
'7 Mucho'='3';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#i2)Respeto a Instituciones en Escala
lapop2012$b2b<-Recode(lapop2012$b2,"'1 Nada'='1';'7 Mucho'='7';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)
lapop2012$b2b<-as.numeric(lapop2012$b2b)

#Indice de Confianza en las Instituciones
#La construccion se basa en un Analisis de Componentes Principales. 
#Dichos resultados se encuentran en el Anexo Metodologico. 
#La solucion incluye confianza en Partidos, Presidente, Corte Suprema y Parlamento

#Recodificacion de Parlamento
b13x<-Recode(lapop2012$b13,"'1 Nada'='1';'7 Mucho'='7';'NS'=NA;'NR'=NA;'DK'=NA;
'NR'=NA;'N/A'=NA",as.factor.result=T)

#Recodificacion de Partidos
b21x<-Recode(lapop2012$b21,"'1 Nada'='1';'7 Mucho'='7';'NS'=NA;'NR'=NA;'DK'=NA;
'NR'=NA;'N/A'=NA",as.factor.result=T)

#Recodificacion de Presidente Republica
b21ax<-Recode(lapop2012$b21a,"'1 Nada'='1';'7 Mucho'='7';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#Recodificacion de Corte Suprema de Justicia
b31x<-Recode(lapop2012$b31,"'1 Nada'='1';'7 Mucho'='7';'NS'=NA;'NR'=NA;'DK'=NA;
'NR'=NA;'N/A'=NA",as.factor.result=T)

#j1)Construccion Indice Aditivo 28 puntos
lapop2012$indice<-as.numeric(b21x)+as.numeric(b21ax)+as.numeric(b13x)+as.numeric(b31x)

#j2)Construccion Indice Aditivo 3 tramos Categorico
lapop2012$indice2<-Recode(lapop2012$indice,"c(4,5,6,7,8,9,10,11,12)='1';c(13,14,
15,16,17,18,19,20)='2';c(21,22,23,24,25,26,27,28)='3'",as.factor=T)

#j3)Construccion Indice Aditivo 3 tramos Continua
lapop2012$indice3<-as.numeric(lapop2012$indice2)

#k1)Apoyo al Sistema Politico en Tramos
lapop2012$b6a<-Recode(lapop2012$b6,"'1 Nada'='1';c(2,3)='1';'4'='2';c(5,6)='3';
'7 Mucho'='3';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#k2)Apoyo al Sistema Politico en Escala
lapop2012$b6b<-Recode(lapop2012$b6,"'1 Nada'='1';'7 Mucho'='7';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)
lapop2012$b6b<-as.numeric(lapop2012$b6b)

#l)Interes en la Politica
lapop2012$pol1a<-Recode(lapop2012$pol1,"'Mucho'='4';'Algo'='3';'Poco'='2';
'Nada'='1';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#m)Simpatia con Partidos Politicos
lapop2012$vb10a<-Recode(lapop2012$vb10,"'Si'='1';'No'='0';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#n)Participacion en Protestas
lapop2012$prot3a<-Recode(lapop2012$prot3,"'No ha participado'='0';
'Si ha participado'='1';'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#o1)Satisfaccion con Escuelas Publicas en 4 valores
lapop2012$sd3new2a<-Recode(lapop2012$sd3new2,"'Muy satisfecha'='4';
'Satisfecha'='3';'INsatisfecha'='2';'Muy INsatisfecha'='1';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#o2)Satisfaccion con Escuelas Publicas en 2 valores
lapop2012$sd3new2b<-Recode(lapop2012$sd3new2,"'Muy satisfecha'='1';
'Satisfecha'='1';'INsatisfecha'='0';'Muy INsatisfecha'='0';'NS'=NA;'NR'=NA;
'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#p)Percepcion de Calidad Educacion Publica
lapop2012$soc3a<-Recode(lapop2012$soc3,"'Buena'='3';'Regular'='2';'Mala'='1';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#q)Participacion Electoral Retrospectiva
lapop2012$vb2a<-Recode(lapop2012$vb2,"'Si voto'='1';'No voto'='0';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#r1)Participacion Electoral Prospectiva V1
lapop2012$vb20a<-Recode(lapop2012$vb20,"'No votaria'='0';
'Votaria por el candidato o partido del actual presidente'='1';
'Votaria por algun candidato o partido diferente del actual gobierno'='1';
'Iria a votar pero dejaria la boleta en blanco o la anularia'='0';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#r2)Participacion Electoral Prospectiva V2
lapop2012$vb20b<-Recode(lapop2012$vb20,"'No votaria'='0';
'Votaria por el candidato o partido del actual presidente'='1';
'Votaria por algun candidato o partido diferente del actual gobierno'='1';
'Iria a votar pero dejaria la boleta en blanco o la anularia'=NA;
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#s1)Probabilidad de Votar
lapop2012$chi59a<-Recode(lapop2012$chi59,"'1 No votara'='1';'7 Si votara'='7';
'NS'=NA;'NR'=NA;'DK'=NA;'NR'=NA;'N/A'=NA",as.factor.result=T)

#################################################################################
#Chequeo de Variables

summary(lapop2012$chi60a)    #VD: 5 NIVELES RECHAZO-APOYO
summary(lapop2012$chi61a)    #VD: BINARIA
summary(lapop2012$q1a)       #SEXO: 1 HOMBRE
summary(lapop2012$q2a)       #EDAD CONTINUA (18 EN ADELANTE)
summary(lapop2012$q2b)       #EDAD EN 4 TRAMOS CRECIENTE
summary(lapop2012$edlevela)  #NIVEL EDUCACIONAL  CRECIENTE (5)
summary(lapop2012$edlevelb)  #NIVEL EDUCACIONAL  CRECIENTE (4)
summary(lapop2012$chi50a)    #INDIGENA 1
summary(lapop2012$l1a)       #IZQUIERDA-DERECHA 10 CATEGORICO
summary(lapop2012$l1b)       #IZQUIERDA-DERECHA 10 CONTINUO
summary(lapop2012$l1c)       #IZQUIERDA-CENTRO-DERECHA 3 NIVELES
summary(lapop2012$chi2a)     #CONSERVADOR-LIBERAL 10 CATEGORICO
summary(lapop2012$chi2b)     #CONSERVADOR-LIBERAL 10 CONTINUO
summary(lapop2012$chi2c)     #CONSERVADOR-MODERADO-LIBERAL 3 NIVELES
summary(lapop2012$dem2a)     #APOYO DEMOCRACIA 3 NIVELES CRECIENTE
summary(lapop2012$dem2b)     #APOYO DEMOCRACIA 1
summary(lapop2012$pn4a)      #SATISFACCION DEMOCRACIA 4 NIVELES 
summary(lapop2012$pn4b)      #SATISFACCION DEMOCRACIA 2 NIVELES 1 SATISFECHO
summary(lapop2012$b2a)       #RESPETO INSTITUCIONES 3 NIVELES   CRECIENTE
summary(lapop2012$b2b)       #RESPETO INSTITUCIONES 7 CONTINUO  CRECIENTE
summary(lapop2012$indice)    #INDICE CONFIANZA INSTITUCIONES CONTINUO   CR
summary(lapop2012$indice2)   #INDICE CONFIANZA INSTITUCIONES 3 NIVELES  CONT 
summary(lapop2012$indice3)   #INDICE CONFIANZA INSTITUCIONES 3 NIVELES  CAT
summary(lapop2012$b6a)       #APOYO SISTEMA 3 NIVELES           CRECIENTE
summary(lapop2012$b6b)       #APOYO SISTEMA 7 CONTINUO          CRECIENTE
summary(lapop2012$pol1a)     #INTERES EN LA POLITICA 4 NIVELES          CR 
summary(lapop2012$vb10a)     #SIMPATIZA CON UN PARTIDO POLITICO 1 SI LO HACE
summary(lapop2012$prot3a)    #PARTICIPACION PROTESTAS 1 SI
summary(lapop2012$sd3new2a)  #SATISFACCION CON ED. PUBLICA CRECIENTE 4 NIVELES
summary(lapop2012$sd3new2b)  #SATISFACCION CON ED. PUBLICA 1 SI
summary(lapop2012$soc3a)     #PERCEPCION CALIDAD ED. PUBLICA CRECIENTE
summary(lapop2012$vb2a)      #PARTICIPACION ELECTORAL PASADA 
summary(lapop2012$vb20a)     #INTENCION PARTICIPACION ELECTORAL
summary(lapop2012$vb20b)     #INTENCION PARTICIPACION ELECTORAL MIN 
summary(lapop2012$chi59a)    #PROBABILIDAD VOTAR

################################################################################
#Consolidacion de Datos

datoslapop<-subset(lapop2012,select=c(estratopri,upm,wt,chi60a,chi61a,q1a,q2a,q2b,
              edlevela,edlevelb,chi50a,l1a,l1b,l1c,dem2a,dem2b,b2a,b2b,b6a,b6b,
              indice,indice2,indice3,pol1a,vb10a,prot3a,vb2a,vb20a,vb20b)) 
datoslapopb<-na.omit(datoslapop)

################################################################################
#Construccion de Modelos de Regresion

#Version Presentada
modelo1<-clm(chi60a~q1a+q2a+edlevelb+chi50a+l1b+dem2b+b2b+b6b+indice+pol1a+
                    vb10a+prot3a+vb2a,data=datoslapopb)
mtable(modelo1)

#Version con Categorizacion de Variables
modelo1a<-clm(chi60a~q1a+q2b+edlevelb+chi50a+l1c+dem2b+b2a+b6a+indice2+pol1a+
                    vb10a+prot3a+vb2a,data=datoslapopb)
mtable(modelo1a)

#Version con Diseno Complejo
disenolapop<-svydesign(ids=~upm,probs=~wt,strata=~estratopri,data=datoslapopb)

modelo1b<-svyolr(chi60a~q1a+q2a+edlevelb+chi50a+l1b+dem2b+b2b+b6b+indice+
                   pol1a+vb10a+prot3a+vb2a,design=disenolapop)
summary(modelo1b)

#Significancia Estadistica (H0: B=0) de Coeficientes
linearHypothesis(modelo1b,"q1a1=0")        # 
linearHypothesis(modelo1b,"q2a=0")         #   
linearHypothesis(modelo1b,"edlevelb2=0")   # 
linearHypothesis(modelo1b,"edlevelb3=0")   # 
linearHypothesis(modelo1b,"edlevelb4=0")   # **
linearHypothesis(modelo1b,"chi50a1=0")     #  
linearHypothesis(modelo1b,"l1b=0")         # .
linearHypothesis(modelo1b,"dem2b1=0")      # ***
linearHypothesis(modelo1b,"b2b=0")         # 
linearHypothesis(modelo1b,"b6b=0")         # 
linearHypothesis(modelo1b,"indice=0")      #  
linearHypothesis(modelo1b,"pol1a2=0")      # 
linearHypothesis(modelo1b,"pol1a3=0")      # .
linearHypothesis(modelo1b,"pol1a4=0")      # 
linearHypothesis(modelo1b,"vb10a1=0")      #
linearHypothesis(modelo1b,"prot3a1=0")     #
linearHypothesis(modelo1b,"vb2a1=0")       # *

################################################################################
#Probabilidades Predichas

nuevos.datos1<-with(datoslapopb,data.frame(q1a=c("0","1"),q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos2<-with(datoslapopb,data.frame(q1a="0",q2a=c(18,90),edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos3<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb=c("1","4"),
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos4<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a=c("0","1"),l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos5<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=c(1,10),dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos6<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b=c("0","1"),b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos7<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=c(1,7),b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos8<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=c(1,7),indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos9<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=c(4,28),pol1a="2",
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos10<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a=c("1","4"),
vb10a="0",prot3a="0",vb2a="1"))

nuevos.datos11<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a=c("0","1"),prot3a="0",vb2a="1"))

nuevos.datos12<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a=c("0","1"),vb2a="1"))

nuevos.datos13<-with(datoslapopb,data.frame(q1a="0",q2a=45.38,edlevelb="2",
chi50a="0",l1b=4.96,dem2b="1",b2b=4.582,b6b=4.399,indice=15.48,pol1a="2",
vb10a="0",prot3a="0",vb2a=c("0","1")))

a<-predict(modelo1,newdata=nuevos.datos1,type="prob")
b<-predict(modelo1,newdata=nuevos.datos2,type="prob")
c<-predict(modelo1,newdata=nuevos.datos3,type="prob")
d<-predict(modelo1,newdata=nuevos.datos4,type="prob")
e<-predict(modelo1,newdata=nuevos.datos5,type="prob")
f<-predict(modelo1,newdata=nuevos.datos6,type="prob")
g<-predict(modelo1,newdata=nuevos.datos7,type="prob")
h<-predict(modelo1,newdata=nuevos.datos8,type="prob")
i<-predict(modelo1,newdata=nuevos.datos9,type="prob")
j<-predict(modelo1,newdata=nuevos.datos10,type="prob")
k<-predict(modelo1,newdata=nuevos.datos11,type="prob")
l<-predict(modelo1,newdata=nuevos.datos12,type="prob")
m<-predict(modelo1,newdata=nuevos.datos13,type="prob")

tabla<-rbind(a$fit,a$fit[2,]-a$fit[1,],b$fit,b$fit[2,]-b$fit[1,],c$fit,
c$fit[2,]-c$fit[1,],d$fit,d$fit[2,]-d$fit[1,],e$fit,e$fit[2,]-e$fit[1,],f$fit,
f$fit[2,]-f$fit[1,],g$fit,g$fit[2,]-g$fit[1,],h$fit,h$fit[2,]-h$fit[1,],i$fit,
i$fit[2,]-i$fit[1,],j$fit,j$fit[2,]-j$fit[1,],k$fit,k$fit[2,]-k$fit[1,],l$fit,
l$fit[2,]-l$fit[1,],m$fit,m$fit[2,]-m$fit[1,])
tabla<-round(tabla,digits=3)
colnames(tabla)<-c("Muy en Desacuerdo","En Desacuerdo","Neutral","De Acuerdo",
"Muy de Acuerdo")
rownames(tabla)<-c("Mujer","Hombre","Diferencia","Edad:18","Edad:90",
"Diferencia","Ed. Basica","Ed. Universitaria","Diferencia","No Indigena",
"Indigena","Diferencia","Izquierda (1)","Derecha (10)","Diferencia",
"No Apoya Democracia","Si Apoya Democracia","Diferencia",
"Respeto Instituciones 1","Respeto Instituciones 7","Diferencia",
"Apoyo Instituciones 1","Apoyo Instituciones 7","Diferencia",
"Indice Confianza Instituciones Min.","Indice Confianza Instituciones Max.",
"Diferencia","Interes Politica: Nada","Interes Politica: Mucho","Diferencia",
"No Simpatiza con Partido","Si Simpatiza con Partido","Diferencia",
"No Participo en Protestas","Si Participo en Protestas","Diferencia",
"No Voto el 2009","Si Voto el 2009","Diferencia")
tabla

#################################################################################
#Graficos de Efectos

modelox<-polr(chi60a~q1a+q2a+edlevelb+chi50a+l1b+dem2b+b2b+b6b+indice+pol1a+
                    vb10a+prot3a+vb2a,data=datoslapopb)

#Grafico de Efecto de Edad sobre Probabilidad de Respuesta
plot(effect("q2a",modelox,xlevels=list(18:90),given.values=c(q1a1=0,edlevelb2=1,
edlevelb3=0,edlevelb4=0,chi50a1=0,l1b=4.96,dem2b1=1,b2b=4.582,b6b=4.399,
indice=15.48,pol1a2=1,pol1a3=0,pol1a4=0,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura 1: Efecto de la Edad sobre el Apoyo a 
la realizacion de Reformas Constitucionales",xlab="Edad",ylab="Probabilidad de Respuesta",
ylim=c(0,1),layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#Grafico de Efecto de Ideologia sobre Probabilidad de Respuesta
plot(effect("l1b",modelox,xlevels=list(1:10),given.values=c(q1a1=0,q2a=45.38,
edlevelb2=1,edlevelb3=0,edlevelb4=0,chi50a1=0,dem2b1=1,b2b=4.582,b6b=4.399,
indice=15.48,pol1a2=1,pol1a3=0,pol1a4=0,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura 2: Efecto del Auto-Posicionamiento en el Eje 
Izquierda-Derecha sobre el Apoyo a la realizacion 
de Reformas Constitucionales",xlab="Auto-Posicionamiento Ideologico",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#######NO APORTAN NADA
#Grafico de Efecto de Educacion sobre Probabilidad de Respuesta (No Presentado)
plot(effect("edlevelb",modelox,xlevels=list(1:4),given.values=c(q1a1=0,q2a=45.38,
chi50a1=0,l1b=4.96,dem2b1=1,b2b=4.582,b6b=4.399,indice=15.48,pol1a2=1,pol1a3=0,
pol1a4=0,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura X: Efecto de la Educacion sobre el Apoyo a 
la realizacion de Reformas Constitucionales",xlab="Nivel Educativo",ylab="Probabilidad de Respuesta",ylim=c(0,0.6),
layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#Grafico de Efecto de Respeto a Inst. sobre Probabilidad de Respuesta (No Presentado)
plot(effect("b2b",modelox,xlevels=list(1:7),given.values=c(q1a1=0,q2a=45.38,
edlevelb2=1,edlevelb3=0,edlevelb4=0,chi50a1=0,l1b=4.96,dem2b1=1,b6b=4.399,
indice=15.48,pol1a2=1,pol1a3=0,pol1a4=0,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura X: Efecto del Respeto a las Instituciones sobre el 
Apoyo a la realizacion de Reformas Constitucionales",xlab="Escala de Respeto",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#Grafico de Efecto de Apoyo a Inst. sobre Probabilidad de Respuesta (No Presentado)
plot(effect("b6b",modelox,xlevels=list(1:7),given.values=c(q1a1=0,q2a=45.38,
edlevelb2=1,edlevelb3=0,edlevelb4=0,chi50a1=0,l1b=4.96,dem2b1=1,b2b=4.582,
indice=15.48,pol1a2=1,pol1a3=0,pol1a4=0,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura X: Efecto del Apoyo a las Instituciones sobre el 
Apoyo a la realizacion de Reformas Constitucionales",xlab="Escala de Respeto",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#Grafico de Efecto de Confianza a Inst. sobre Probabilidad de Respuesta (No Presentado)
plot(effect("indice",modelox,xlevels=list(4:28),given.values=c(q1a1=0,q2a=45.38,
edlevelb2=1,edlevelb3=0,edlevelb4=0,chi50a1=0,l1b=4.96,dem2b1=1,b2b=4.582,
b6b=4.399,pol1a2=1,pol1a3=0,pol1a4=0,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura X: Efecto del Apoyo a las Instituciones sobre el 
Apoyo a la realizacion de Reformas Constitucionales",xlab="Escala de Respeto",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#Grafico de Efecto de Interes en Politica sobre Probabilidad de Respuesta (No Presentado)
plot(effect("pol1a",modelox,xlevels=list(1:4),given.values=c(q1a1=0,q2a=45.38,
edlevelb2=1,edlevelb3=0,edlevelb4=0,chi50a1=0,l1b=4.96,dem2b1=1,b2b=4.582,
b6b=4.399,indice=15.48,vb10a1=0,prot3a1=0,vb2a1=1)),rug=F,
main="Figura X: Efecto del Apoyo a las Instituciones sobre el 
Apoyo a la realizacion de Reformas Constitucionales",xlab="Escala de Respeto",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(5,1),factor.names=T,sub="Fuente: LAPOP,2012.")

#################################################################################
#Modelo de Regresion para Decision Ciudadania/Congreso. NO REPORTADO 

logitdata<-subset(lapop2012,select=c(chi61a,q1a,q2a,edlevelb,chi50a,l1b,chi2b,
                  dem2b,b2b,b6b,indice,pol1a,vb10a,prot3a,vb2a))

logit<-glm(chi61a~q1a+q2a+edlevelb+chi50a+l1b+chi2b+dem2b+b2b+b6b+indice+pol1a+
                  vb10a+prot3a+vb2a,family=binomial(link=logit),data=logitdata)
summary(logit)
mtable(logit)

dim(logitdata1)
summary(logitdata1$chi61a)
#No hay variables que tengan mayor poder explicativo.
#13% de las respuestas son Congreso ?Hay suficiente variabilidad?

#################################################################################
#################################################################################

#2) Analisis con UDP 2013

udp<-read.dta(file.choose())

#################################################################################
#Construccion de las Variables de Resultado

udp$p64g<-Recode(udp$P64_G,"'Nada Importante'='1';'Muy Importante'='7';'Ns'=NA;
'Nc'=NA",as.factor.result=T) 

#################################################################################
#Construccion de las Variables Explicativas

#a)Sexo
udp$sexo<-Recode(udp$SEXO,"'Hombre'='1';'Mujer'='0'",as.factor.result=T)

#b1)Edad
udp$p78<-as.numeric(udp$p78)
udp$p78b<-udp$p78+17

#b2)Edad en Tramos
udp$p78c<-udp$edad

#c)Nivel Educacional en 4 Categorias
udp$p82<-Recode(udp$P82,"'Sin Estudios'='1';'Basica Incompleta'='1';
'Basica Completa'='1';'Media Incompleta'='2';'Media Completa'='2';
'Tecnica Superior No Universitaria Incompleta'='3';
'Tecnica Superior No Universitaria Completa'='3';'Universitaria Incompleta'='4';'Universitaria Completa'='4';
'Postgrado'='4';'No Contesta'=NA",as.factor.result=T)

#d)Pertenencia a Pueblo Originario
udp$p80<-Recode(udp$P80,"'No Pertenece A Ninguno De Ellos'='0';'Si, Aymara'='1';
'Nui'='1';'Si, Quechua'='1';'Si, Mapuche'='1';'Si, Atacameno'='1';'Si, Coya'='1';
'Si, Kawaskar'='1';'Si, Yagan'='1';'Diaguita'='1'",as.factor.result=T)

#e1)Auto-posicionamiento Ideologico Izquierda-Derecha Categorica
udp$p65<-Recode(udp$P65,"'Mas Izquierda'='1';'Mas Derecha'='10';
'Ninguno (No Leer)'=NA")
udp$p65<-factor(udp$p65,levels=c('1','2','3','4','5','6','7','8','9','10'))

#e2)Autoposicionamiento Ideologico Izquierda-Derecha Continua
udp$p65b<-as.numeric(udp$p65)

#e3)#e3)Auto-posicionamiento Ideologico Izquierda-Derecha en Tramos
udp$p20<-Recode(udp$P20,"'Derecha'='3';'Centro'='2';'Izquierda'='1';
'Ninguna'='0';'Ns'=NA;'Nc'=NA")

#Indice de Confianza en las Instituciones
#La Construccion se basa en un Analisis de Componentes Principales. 
#Dichos resultados se encuentran en el Anexo Metodologico. 
#La solucion incluye confianza en Partidos, Gobierno, Tribunales y Congreso

#Recodificacion de Congreso
udp$p21a<-Recode(udp$P21_5,"'Nada'='1';'Poco'='2';'Bastante'='3';'Mucho'='4';
'Ns'=NA;'Nc'=NA")

#Recodificacion de Partidos Politico
udp$p21b<-Recode(udp$P21_6,"'Nada'='1';'Poco'='2';'Bastante'='3';'Mucho'='4';
'Ns'=NA;'Nc'=NA")

#Recodificacion de Gobierno
udp$p21c<-Recode(udp$P21_4,"'Nada'='1';'Poco'='2';'Bastante'='3';'Mucho'='4';
'Ns'=NA;'Nc'=NA")

#Recodificacion de Tribunales de Justicia
udp$p21d<-Recode(udp$P21_7,"'Nada'='1';'Poco'='2';'Bastante'='3';'Mucho'='4';
'Ns'=NA;'Nc'=NA")

#f1)Construccion Indice Aditivo 12 puntos
udp$indice<-as.numeric(udp$p21a)+as.numeric(udp$p21b)+as.numeric(udp$p21c)+
as.numeric(udp$p21d)

#f2)Construccion Indice Aditivo 3 tramos Categorico
udp$indice2<-Recode(udp$indice,"c(4,5,6,7)='1';c(8,9,10,11)='2';c(12,13,14,
16)='3'",as.factor.result=T)

#f3)Construccion Indice Aditivo 3 tramos Continua
udp$indice3<-as.numeric(udp$indice2)

#g)Interes en la Politica
udp$p12<-Recode(udp$P12,"'Muy Interesado'='4';'Algo Interesado'='3';
'Poco Interesado'='2';'Nada Interesado'='1';'Ns'=NA;'Nc'=NA",as.factor.result=T)

#h)Simpatia con Partidos Politicos
udp$p18<-Recode(udp$P18,"'Partido Comunista (Pc)'='1';'Partido Humanista (Ph)'='1';
'Partido Democrata Cristiano (Pdc)'='1';'Partido Por La Democracia (Ppd)'='1';
'Partido Radical Social Democrata (Prsd)'='1';
'Partido Regionalista De Los Independientes (Pri)'='1';
'Partido Progresista (Pro)'='1';'Partido Socialista (Ps)'='1';
'Renovacion Nacional (Rn)'='1';'Union Democrata Independiente (Udi)'='1';
'Ninguno [No Leer]'='0';'Ns'=NA;'Nc'=NA")

#i)Participacion Electoral Retrospectiva
udp$p37<-Recode(udp$P37,"'Si'='1';'No'='0';'Ns'=NA;'Nc'=NA",as.factor.result=T)

#j)Participacion Electoral Prospectiva 
udp$p42<-Recode(udp$P42,"'Si'='1';'No'='0';'Ns'=NA;'Nc'=NA",as.factor.result=T)

#k)Probabilidad de Votar
udp$p44<-Recode(udp$P44,"'Totalmente Seguro'='4';'Seguro'='3';'Poco Seguro'='2';
'Nada Seguro'='1';'Ns'=NA;'Nc'=NA",as.factor.result=T)            

#################################################################################
#Chequeo de Variables

summary(udp$p64g)      #VD: 7 NIVELES CRECIENTE
summary(udp$sexo)      #SEXO: 1 HOMBRE
summary(udp$p78b)      #EDAD CONTINUA 18 EN ADELANTE
summary(udp$edad)      #EDAD EN 4 TRAMOS CRECIENTE
summary(udp$p82)       #NIVEL EDUCACIONAL CRECIENTE (4)
summary(udp$p80)       #INDIGENA 1 SI
summary(udp$p65)       #IZQUIERDA-DERECHA 10 CATEGORICO
summary(udp$p65b)      #IZQUIERDA-DERECHA 10 CONTINUO
summary(udp$p20)       #NINGUNA-IZQUIERDA-CENTRO-DERECHA (0 A 4)
summary(udp$indice)    #INDICE CONFIANZA INSTITUCIONES CONTINUO
summary(udp$indice2)   #INDICE CONFIANZA INSTITUCIONES 3 NIVELES CATEGORICO
summary(udp$indice3)   #INDICE CONFIANZA INSTITUCIONES 3 NIVELES CONTINUO
summary(udp$p12)       #INTERES EN LA POLITICA 4 NIVELES CRECIENTE
summary(udp$p18)       #SIMPATIZA CON UN PARTIDO POLITICO 1 SI LO HACE
summary(udp$p37)       #PARTICIPACION ELECTORAL PASADA
summary(udp$p42)       #INTENCION PARTICIPACION ELECTORAL
summary(udp$p44)       #PROBABILIDAD VOTAR

################################################################################
#Consolidacion de Datos

datosudp<-subset(udp,select=c(macrozona,POND_MUESTRAL,p64g,sexo,p78b,p78c,p82,
p80,p65,p65b,p20,indice,indice2,indice3,p12,p18,p37,p42,p44))
datosudpb<-na.omit(datosudp)

################################################################################
#Construccion de Modelos de Regresion

#Version Presentada
modelo2<-clm(p64g~sexo+p78b+p82+p80+p65b+p12+p18+indice3+p42,data=datosudp)
mtable(modelo2)

#Version con Categorizacion de Variables
modelo2a<-clm(p64g~sexo+p78c+p82+p80+p20+p12+p18+indice2+p42,data=datosudp)
mtable(modelo2a)

#Version con Diseno Complejo

disenoudp<-svydesign(ids=~1,probs=~POND_MUESTRAL,strata=~macrozona,data=datosudp)

modelo2b<-svyolr(p64g~sexo+p78b+p82+p80+p65b+p12+p18+indice3+p42,design=disenoudp)
summary(modelo2b)

AIC(modelo2b)
BIC(modelo2b)

#Significancia Estadistica (H0: B=0) de Coeficientes
linearHypothesis(modelo2b,"sexo1=0")    # 
linearHypothesis(modelo2b,"p78b=0")     # 
linearHypothesis(modelo2b,"p822=0")     # 
linearHypothesis(modelo2b,"p823=0")     # 
linearHypothesis(modelo2b,"p824=0")     # 
linearHypothesis(modelo2b,"p801=0")     # *
linearHypothesis(modelo2b,"p65b=0")     # .
linearHypothesis(modelo2b,"p122=0")     # 
linearHypothesis(modelo2b,"p123=0")     # 
linearHypothesis(modelo2b,"p124=0")     # 
linearHypothesis(modelo2b,"p181=0")     # **
linearHypothesis(modelo2b,"indice3=0")  # **
linearHypothesis(modelo2b,"p421=0")     # 

################################################################################
#Probabilidades Predichas

nuevos.datos1b<-with(datosudp,data.frame(sexo=c("0","1"),p78b=48.22,p82="2",
p80="0",p65b=5.101,p12="2",p18="0",indice3=1.47,p42="1"))

nuevos.datos2b<-with(datosudp,data.frame(sexo="1",p78b=c(18,90),p82="2",
p80="0",p65b=5.101,p12="2",p18="0",indice3=1.47,p42="1"))

nuevos.datos3b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82=c("1","4"),
p80="0",p65b=5.101,p12="2",p18="0",indice3=1.47,p42="1"))

nuevos.datos4b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82="2",
p80=c("0","1"),p65b=5.101,p12="2",p18="0",indice3=1.47,p42="1"))

nuevos.datos5b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82="2",
p80="0",p65b=c(1,10),p12="2",p18="0",indice3=1.47,p42="1"))

nuevos.datos6b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82="2",
p80="0",p65b=5.101,p12=c("1","4"),p18="0",indice3=1.47,p42="1"))

nuevos.datos7b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82="2",
p80="0",p65b=5.101,p12="2",p18=c("0","1"),indice3=1.47,p42="1"))

nuevos.datos8b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82="2",
p80="0",p65b=5.101,p12="2",p18="0",indice3=c(1,3),p42="1"))

nuevos.datos9b<-with(datosudp,data.frame(sexo="1",p78b=48.22,p82="2",
p80="0",p65b=5.101,p12="2",p18="0",indice3=1.47,p42=c("0","1")))

ab<-predict(modelo2,newdata=nuevos.datos1b,type="prob")
bb<-predict(modelo2,newdata=nuevos.datos2b,type="prob")
cb<-predict(modelo2,newdata=nuevos.datos3b,type="prob")
db<-predict(modelo2,newdata=nuevos.datos4b,type="prob")
eb<-predict(modelo2,newdata=nuevos.datos5b,type="prob")
fb<-predict(modelo2,newdata=nuevos.datos6b,type="prob")
gb<-predict(modelo2,newdata=nuevos.datos7b,type="prob")
hb<-predict(modelo2,newdata=nuevos.datos8b,type="prob")
ib<-predict(modelo2,newdata=nuevos.datos9b,type="prob")

tablab<-rbind(ab$fit,ab$fit[2,]-ab$fit[1,],bb$fit,bb$fit[2,]-bb$fit[1,],cb$fit,
cb$fit[2,]-cb$fit[1,],db$fit,db$fit[2,]-db$fit[1,],eb$fit,eb$fit[2,]-eb$fit[1,],
fb$fit,fb$fit[2,]-fb$fit[1,],gb$fit,gb$fit[2,]-gb$fit[1,],hb$fit,
hb$fit[2,]-hb$fit[1,],ib$fit,ib$fit[2,]-ib$fit[1,])
tablab<-round(tablab,digits=2)

colnames(tablab)<-c("Nada Importante","2","3","4","5","6","Muy Importante")
rownames(tablab)<-c("Mujer","Hombre","Diferencia","Edad:18","Edad:90",
"Diferencia","Ed. Basica","Ed. Universitaria","Diferencia","No Indigena",
"Indigena","Diferencia","Izquierda (1)","Derecha (10)","Diferencia",
"Interes Politica: Nada","Interes Politica: Mucho","Diferencia",
"No Simpatiza con Partido","Si Simpatiza con Partido","Diferencia",
"Indice Confianza Instituciones Min.","Indice Confianza Instituciones Max.",
"Diferencia","Votara: No","Votara: Si","Diferencia")
tablab

#################################################################################
#Graficos de Efectos

modelobx<-polr(p64g~sexo+p78b+p82+p80+p65b+p12+p18+indice3+p42,data=datosudp)

#Grafico de Efecto de Edad sobre Probabilidad de Respuesta
plot(effect("indice3",modelobx,xlevels=list(1:3),given.values=c(sexo1=0,p78b=48.22,
p822=1,p823=0,p824=0,p801=0,p65b=5.101,p122=1,p123=0,p124=0,p181=0,p421=1)),
rug=F,
main="Figura 3: Efecto de la Confianza en Instituciones sobre la
Importancia de una Nueva Constitucion",xlab="Indice de Confianza Institucional",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(7,1),factor.names=T,
sub="Fuente: Encuesta Nacional de Opinion Publica UDP,2013.")

#Grafico de Efecto de Ideologia sobre Probabilidad de Respuesta
plot(effect("p65b",modelobx,xlevels=list(1:10),given.values=c(sexo1=0,p78b=48.22,
p822=1,p823=0,p824=0,p801=0,indice3=1.47,p122=1,p123=0,p124=0,p181=0,p421=1)),
rug=F,
main="Figura 4: Efecto de la Ideologia sobre la
Importancia de una Nueva Constitucion",xlab="Indice de Confianza Institucional",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(7,1),factor.names=T,
sub="Fuente: Encuesta Nacional de Opinion Publica UDP,2013.")

#Grafico de Efecto de Edad sobre Probabilidad de Respuesta (NO INTERESANTE)
plot(effect("p78b",modelobx,xlevels=list(18:90),given.values=c(sexo1=0,p65b=5.101,
p822=1,p823=0,p824=0,p801=0,indice3=1.47,p122=1,p123=0,p124=0,p181=0,p421=1)),
rug=F,
main="Figura X: Efecto de la Edad sobre la
Importancia de una Nueva Constitucion",xlab="Indice de Confianza Institucional",
ylab="Probabilidad de Respuesta",ylim=c(0,1),layout=c(7,1),factor.names=T,
sub="Fuente: Encuesta Nacional de Opinion Publica UDP,2013.")

#################################################################################
#################################################################################

#3 Analisis con CEP 2013

cep<-read.dta(file.choose())

summary(cep$te2p11a)
summary(cep$ddp01)
summary(cep$ddp02)
summary(cep$ddp04)
summary(cep$ddp22)
summary(cep$ddp23)
summary(cep$mbp14)
summary(cep$te1p05)
summary(as.factor(cep$te2p05))

#################################################################################
#Construccion de las Variables de Resultado

cep$ac<-Recode(cep$te2p11a,"
'de acuerdo'='1';'en desacuerdo'='0';'no sabe'=NA;'no contesta'=NA",as.factor.result=T)

cep$ac1<-Recode(cep$te2p11a,"
'de acuerdo'='3';'en desacuerdo'='2';'no sabe'='1';'no contesta'='1'",as.factor.result=T)

#################################################################################
#Construccion de las Variables Explicativas

#a)Sexo
cep$hombre<-Recode(cep$ddp01,"'hombre'='1';'mujer'='0'",as.factor.result=T)

#b)Edad
cep$edad<-cep$ddp02

#c)Nivel Educativo 4 
cep$educ<-Recode(cep$ddp04,"'no estudio'='1';'educacion Basica incompleta'='1';
'educacion Basica completa'='1';'educacion media incompleta'='2';
'educacion media completa'='2';
'educacion superior no universitaria incompleta'='3';
'educacion superior no universitaria completa'='3';
'educacion universitaria incompleta'='4';'educacion universitaria completa'='4';
'estudios de post grado, Master, doctorado'='4';
'no contesta'=NA",as.factor.result=T)

#d)Pertenencia a Pueblo Originario
cep$ind<-Recode(cep$ddp23,"'alacalufe ( kawashkar)'='1';'Atacameno'='1';'aimara'='1';
'colla'='1';'mapuche'='1';'quechua'='1';'rapa nui'='1';'yamana (Yagan)'='1';
'ninguno de los anteriores'='0';'no contesta'=NA",as.factor.result=T)

#e1)Auto-posicionamiento Ideologico Izquierda-Derecha Categorica
cep$ideol<-Recode(cep$te2p05,"'88'=NA;'99'=NA",as.factor.result=T)

#e2)Auto-posicionamiento Ideologico Izquierda-Derecha Continua
cep$ideol2<-as.numeric(cep$ideol)

#e3)Auto-posicionamiento Ideologico Izquierda-Derecha en Tramos
cep$ideol3<-Recode(cep$te2p05,"c(1,2,3)='1';c(4,5,6,7)='2';c(8,9,10)='3';
'88'=NA;'99'=NA",as.factor.result=T)

#f)Simpatia con Partidos Politicos
cep$part<-Recode(cep$mbp14,"
'partido democrata cristiano'='1';'union democrata independiente'='1';
'partido comunista de chile'='1';'renovacion nacional'='1';
'partido socialista de chile'='1';'partido radical socialdemocrata'='1';
'partido por la democracia'='1';'partido humanista'='1';
'partido regionalista independiente'='1';'partido progresista'='1';
'otro partido'='1';'ninguno'='0';'no sabe'=NA;'no contesta'=NA",as.factor.result=T)

#g)Participacion Electoral Pasada
cep$voto<-Recode(cep$ddp22,"'si'='1';'no'='0';'no contesta'=NA",as.factor.result=T)

#h)Intencion de Participacion Electoral Futura
cep$votara<-Recode(cep$te1p05,"'Si, con toda seguridad ira a votar'='3';
'probablemente si'='2';'probablemente no'='2';
'no, con toda seguridad no ira a votar'='1';
'no sabe'=NA;'no contesta'=NA",as.factor.result=T)

names(cep)
datoscep<-subset(cep,select=c(ac,ac1,hombre,edad,educ,ind,ideol,ideol2,ideol3,
                 part,voto,votara,pond))
datoscepb<-na.omit(datoscep)

modelo3<-glm(ac~hombre+edad+educ+ind+ideol2+part+voto,
family=binomial(link="logit"),data=datoscep)
modelo3<-glm(ac~hombre+edad+educ+ind+ideol2+part+voto,family=binomial,data=datoscepb)

mtable(modelo3)

library(nnet)

g1<-multinom(ac1~hombre+edad+educ+ind+ideol2+part+voto,data=datoscep)
summary(g1)

effect("hombre",g1)


################################################################################
#MODO MAS FACIL PARA CALCULAR PROBABILIDADES PREDICHAS CON DISENO COMPLEJO

invlogit=function(x)exp(x)/(1+exp(x))
invlogit(logit)
logit$zeta
modelox$zeta
modelo1$alpha
modelo1b$zeta-
modelo1b$coef

summary(modelo1)

x<-c(0,45.38,1,0,0,0,4.96,1,4.582,4.399,15.48,1,0,0,0,0,1)
xb<-x%*%modelo1b$coef
phat1<-invlogit(modelo1b$zeta[1]-xb)
phat2<-invlogit(modelo1b$zeta[2]-xb)-invlogit(modelo1b$zeta[1]-xb)
phat3<-invlogit(modelo1b$zeta[3]-xb)-invlogit(modelo1b$zeta[2]-xb)
phat4<-invlogit(modelo1b$zeta[4]-xb)-invlogit(modelo1b$zeta[3]-xb)
phat5<-1-invlogit(modelo1b$zeta[4]-xb)

round(data.frame(phat1,phat2,phat3,phat4,phat5),digits=4)
round(data.frame(a),digits=4)

