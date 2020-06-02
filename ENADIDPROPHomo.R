###
### Proporciones de discapacidad- ENADID 2014
### Proyecto de Tesis
### Autora: Paola Vazquez Castillo
### Ultima modificacion: 17 mayo 2020

### Informacion recuperada de INEGI el 8 de mayo 2020

rm(list=ls())

library(tidyverse)
library(haven)
library(foreign)

setwd("~/Mirror/Maestría/TESIS/Datos/Discapacidad/ENADID2014")

BASE<-read.dbf("TSDem.dbf")%>%
  select(ENT,SEXO,EDAD,"P3_9A1","P3_9A2","P3_9A5","P3_9A7","P3_9A8","C_LIMDISC","FAC_VIV")%>%
  rename(FACTOR="FAC_VIV")

BaseNorte<-BASE%>%
  filter(ENT=="02"|ENT=="03"|ENT=="05"|ENT=="08"|ENT=="10"|ENT=="19"|ENT=="25"|ENT=="26"|ENT=="28"|ENT=="32")

BaseNorte$EDAD<-as.numeric(levels(BaseNorte$EDAD))[BaseNorte$EDAD]
BaseNorte$EDAD[BaseNorte$EDAD==999]<-NA
BaseNorte$EDAD[BaseNorte$EDAD>=109]<-109

BaseNorte$P3_9A1<-as.numeric(levels(BaseNorte$P3_9A1))[BaseNorte$P3_9A1]
BaseNorte$P3_9A2<-as.numeric(levels(BaseNorte$P3_9A2))[BaseNorte$P3_9A2]
BaseNorte$P3_9A5<-as.numeric(levels(BaseNorte$P3_9A5))[BaseNorte$P3_9A5]
BaseNorte$P3_9A7<-as.numeric(levels(BaseNorte$P3_9A7))[BaseNorte$P3_9A7]
BaseNorte$P3_9A8<-as.numeric(levels(BaseNorte$P3_9A8))[BaseNorte$P3_9A8]


BaseNorte$Dis[BaseNorte$P3_9A1==1|BaseNorte$P3_9A1==2|
              BaseNorte$P3_9A2==1|BaseNorte$P3_9A2==2|
              BaseNorte$P3_9A5==1|BaseNorte$P3_9A5==2|
              BaseNorte$P3_9A7==1|BaseNorte$P3_9A7==2|
              BaseNorte$P3_9A8==1|BaseNorte$P3_9A8==2]<-1

BaseNorte2<-BaseNorte%>%
  select(ENT,FACTOR,SEXO,EDAD,Dis)%>%
  group_by(SEXO,EDAD)%>%
  mutate(Total=sum(FACTOR))%>%
  ungroup()%>%
  group_by(SEXO,EDAD,Dis, Total)%>%
  summarise(Discap=sum(FACTOR))%>%
  ungroup()%>%
  mutate(Prop=Discap/Total)%>%
  filter(Dis==1)%>%
  select(SEXO,EDAD,Prop)%>%
  filter(is.na(EDAD)==FALSE)%>%
  spread(key = SEXO, value = Prop)

names(BaseNorte2)<-c("EDAD","Hombres","Mujeres")

muj.R1<-smooth.spline(c(0:89),
                      BaseNorte2$Mujeres[1:90])
R1.F<-predict(muj.R1,c(0:109))

# matplot(cbind(BaseNorte2$Mujeres,
#               R1.F$y),type="l")

# Prueba de ajuste (PRESS)
# muj.R1$cv.crit

hom.R1<-smooth.spline(c(0:94),
                      BaseNorte2$Hombres[1:95],spar = 0.85)
R1.H<-predict(hom.R1,c(0:109))

# matplot(cbind(BaseNorte2$Hombres,
#               R1.H$y),type="l")

rm(BaseNorte,BaseNorte2)

#### CENTRO

BaseCentro<-BASE%>%
  filter(ENT=="01"|ENT=="06"|ENT=="11"|ENT=="13"|
           ENT=="14"|ENT=="15"|ENT=="16"|ENT=="18"|ENT=="22"|ENT=="24"|ENT=="29")

BaseCentro$EDAD<-as.numeric(levels(BaseCentro$EDAD))[BaseCentro$EDAD]
BaseCentro$EDAD[BaseCentro$EDAD==999]<-NA
BaseCentro$EDAD[BaseCentro$EDAD>=109]<-109

BaseCentro$P3_9A1<-as.numeric(levels(BaseCentro$P3_9A1))[BaseCentro$P3_9A1]
BaseCentro$P3_9A2<-as.numeric(levels(BaseCentro$P3_9A2))[BaseCentro$P3_9A2]
BaseCentro$P3_9A5<-as.numeric(levels(BaseCentro$P3_9A5))[BaseCentro$P3_9A5]
BaseCentro$P3_9A7<-as.numeric(levels(BaseCentro$P3_9A7))[BaseCentro$P3_9A7]
BaseCentro$P3_9A8<-as.numeric(levels(BaseCentro$P3_9A8))[BaseCentro$P3_9A8]


BaseCentro$Dis[BaseCentro$P3_9A1==1|BaseCentro$P3_9A1==2|
                BaseCentro$P3_9A2==1|BaseCentro$P3_9A2==2|
                BaseCentro$P3_9A5==1|BaseCentro$P3_9A5==2|
                BaseCentro$P3_9A7==1|BaseCentro$P3_9A7==2|
                BaseCentro$P3_9A8==1|BaseCentro$P3_9A8==2]<-1


BaseCentro2<-BaseCentro%>%
  select(ENT,FACTOR,SEXO,EDAD,Dis)%>%
  group_by(SEXO,EDAD)%>%
  mutate(Total=sum(FACTOR))%>%
  ungroup()%>%
  group_by(SEXO,EDAD,Dis, Total)%>%
  summarise(Discap=sum(FACTOR))%>%
  ungroup()%>%
  mutate(Prop=Discap/Total)%>%
  filter(Dis==1)%>%
  select(SEXO,EDAD,Prop)%>%
  filter(is.na(EDAD)==FALSE)%>%
  spread(key = SEXO, value = Prop)

names(BaseCentro2)<-c("EDAD","Hombres","Mujeres")


muj.R2<-smooth.spline(c(0:94),
                      BaseCentro2$Mujeres[1:95])
R2.F<-predict(muj.R2,c(0:109))

# matplot(cbind(BaseCentro2$Mujeres,
#               R2.F$y),type="l")

hom.R2<-smooth.spline(c(0:95),
                      BaseCentro2$Hombres[1:96])
R2.H<-predict(hom.R2,c(0:109))

# matplot(cbind(BaseCentro2$Hombres,
#               R2.H$y),type="l")

rm(BaseCentro,BaseCentro2)

#### CDMX 
BaseCDMX<-BASE%>%
  filter(ENT=="09")

BaseCDMX$EDAD<-as.numeric(levels(BaseCDMX$EDAD))[BaseCDMX$EDAD]
BaseCDMX$EDAD[BaseCDMX$EDAD==999]<-NA
BaseCDMX$EDAD[BaseCDMX$EDAD>=109]<-109

BaseCDMX$P3_9A1<-as.numeric(levels(BaseCDMX$P3_9A1))[BaseCDMX$P3_9A1]
BaseCDMX$P3_9A2<-as.numeric(levels(BaseCDMX$P3_9A2))[BaseCDMX$P3_9A2]
BaseCDMX$P3_9A5<-as.numeric(levels(BaseCDMX$P3_9A5))[BaseCDMX$P3_9A5]
BaseCDMX$P3_9A7<-as.numeric(levels(BaseCDMX$P3_9A7))[BaseCDMX$P3_9A7]
BaseCDMX$P3_9A8<-as.numeric(levels(BaseCDMX$P3_9A8))[BaseCDMX$P3_9A8]


BaseCDMX$Dis[BaseCDMX$P3_9A1==1|BaseCDMX$P3_9A1==2|
                 BaseCDMX$P3_9A2==1|BaseCDMX$P3_9A2==2|
                 BaseCDMX$P3_9A5==1|BaseCDMX$P3_9A5==2|
                 BaseCDMX$P3_9A7==1|BaseCDMX$P3_9A7==2|
                 BaseCDMX$P3_9A8==1|BaseCDMX$P3_9A8==2]<-1

BaseCDMX2<-BaseCDMX%>%
  select(ENT,FACTOR,SEXO,EDAD,Dis)%>%
  group_by(SEXO,EDAD)%>%
  mutate(Total=sum(FACTOR))%>%
  ungroup()%>%
  group_by(SEXO,EDAD,Dis, Total)%>%
  summarise(Discap=sum(FACTOR))%>%
  ungroup()%>%
  mutate(Prop=Discap/Total)%>%
  filter(Dis==1)%>%
  select(SEXO,EDAD,Prop)%>%
  filter(is.na(EDAD)==FALSE)%>%
  spread(key = SEXO, value = Prop)

names(BaseCDMX2)<-c("EDAD","Hombres","Mujeres")

# Mujeres
# Imputar el valor 52 hace una serie muy larga
BaseCDMX2$Mujeres[52]<-(BaseCDMX2$Mujeres[51]+BaseCDMX2$Mujeres[53])/2
  
muj.R3<-smooth.spline(c(37:90),
                      BaseCDMX2$Mujeres[38:91],spar = 0.8)
R3.F<-predict(muj.R3,c(0:109))

# matplot(cbind(BaseCDMX2$Mujeres,
#               R3.F$y),type="l")
# abline(h=0,col="gray")

# Hombres Aqui lo voy a hacer por pedazos porque la primera parte no fit bien 
# Voy a completar el valor 20 porque arma serie mas larga para la estimacion 
BaseCDMX2$Hombres[20]<-(BaseCDMX2$Hombres[19]+BaseCDMX2$Hombres[21])/2
BaseCDMX2$Hombres[10]<-(BaseCDMX2$Hombres[9]+BaseCDMX2$Hombres[11])/2
BaseCDMX2$Hombres[82]<-(BaseCDMX2$Hombres[81]+BaseCDMX2$Hombres[83])/2
BaseCDMX2$Hombres[25]<-(BaseCDMX2$Hombres[24]+BaseCDMX2$Hombres[26])/2

hom.R3.1<-smooth.spline(c(7:20),
                        BaseCDMX2$Hombres[8:21])
R3.H.1<-predict(hom.R3.1,c(0:109))
matplot(cbind(BaseCDMX2$Hombres,
              hom.R3.1$y),type="l")

hom.R3.2<-smooth.spline(c(35:86),
                        BaseCDMX2$Hombres[36:87], spar = 0.97)
R3.H.2<-predict(hom.R3.2,c(0:109))

# matplot(cbind(BaseCDMX2$Hombres,
#               R3.H.2$y),type="l")

R3.H<-tibble()
R3.H<-c(R3.H.1$y[1:40],R3.H.2$y[41:110])

# matplot(cbind(BaseCDMX2$Hombres,
#         R3.H),
#         type = "l")
#####

rm(BaseCDMX, BaseCDMX2)

BaseSur<-BASE%>%
  filter(ENT=="04"|ENT=="07"|ENT=="12"|ENT=="17"|ENT=="20"|ENT=="21"|ENT=="23"|ENT=="27"|ENT=="30"|ENT=="31")

BaseSur$EDAD<-as.numeric(levels(BaseSur$EDAD))[BaseSur$EDAD]
BaseSur$EDAD[BaseSur$EDAD==999]<-NA
BaseSur$EDAD[BaseSur$EDAD>=109]<-109

BaseSur$P3_9A1<-as.numeric(levels(BaseSur$P3_9A1))[BaseSur$P3_9A1]
BaseSur$P3_9A2<-as.numeric(levels(BaseSur$P3_9A2))[BaseSur$P3_9A2]
BaseSur$P3_9A5<-as.numeric(levels(BaseSur$P3_9A5))[BaseSur$P3_9A5]
BaseSur$P3_9A7<-as.numeric(levels(BaseSur$P3_9A7))[BaseSur$P3_9A7]
BaseSur$P3_9A8<-as.numeric(levels(BaseSur$P3_9A8))[BaseSur$P3_9A8]

BaseSur$Dis[BaseSur$P3_9A1==1|BaseSur$P3_9A1==2|
              BaseSur$P3_9A2==1|BaseSur$P3_9A2==2|
              BaseSur$P3_9A5==1|BaseSur$P3_9A5==2|
              BaseSur$P3_9A7==1|BaseSur$P3_9A7==2|
              BaseSur$P3_9A8==1|BaseSur$P3_9A8==2]<-1

BaseSur2<-BaseSur%>%
  select(ENT,FACTOR,SEXO,EDAD,Dis)%>%
  group_by(SEXO,EDAD)%>%
  mutate(Total=sum(FACTOR))%>%
  ungroup()%>%
  group_by(SEXO,EDAD,Dis, Total)%>%
  summarise(Discap=sum(FACTOR))%>%
  ungroup()%>%
  mutate(Prop=Discap/Total)%>%
  filter(Dis==1)%>%
  select(SEXO,EDAD,Prop)%>%
  filter(is.na(EDAD)==FALSE)%>%
  spread(key = SEXO, value = Prop)

names(BaseSur2)<-c("EDAD","Hombres","Mujeres")

muj.R4<-smooth.spline(c(0:97),
                      BaseSur2$Mujeres[1:98], spar = 0.9)
R4.F<-predict(muj.R4,c(0:109))

# matplot(cbind(BaseSur2$Mujeres,
#               R4.F$y),type="l")

hom.R4<-smooth.spline(c(0:97),
                      BaseSur2$Hombres[1:98])
R4.H<-predict(hom.R4,c(0:109))

# matplot(cbind(BaseSur2$Hombres,
#               R4.H$y),type="l")

rm(BaseSur,BaseSur2)

# Tabla de valores finales
PropH14.F<-tibble(Edad=c(0:109))
PropH14.F$R1<-R1.F$y
PropH14.F$R2<-R2.F$y
PropH14.F$R3<-R3.F$y
PropH14.F$R4<-R4.F$y

PropH14.F<-PropH14.F%>%
  gather(key = "Region",value = "Prop",-"Edad")

PropH14.M<-tibble(Edad=c(0:109))
PropH14.M$R1<-R1.H$y
PropH14.M$R2<-R2.H$y
PropH14.M$R3<-R3.H
PropH14.M$R4<-R4.H$y

PropH14.M<-PropH14.M%>%
  gather(key = "Region",value = "Prop",-"Edad")

save(PropH14.M,PropH14.F,file = "Prop14Homo.RData")

# A<-as.data.frame(cbind(PropD14.M$Prop,PropD14.F$Prop))
# write_csv(A,"Prop2014Homo.csv")

