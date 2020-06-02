###
### Proporciones de discapacidad- ENADID 2018 HOMO
### Proyecto de Tesis
### Autora: Paola Vazquez Castillo
### Ultima modificacion: 18 mayo 2020

### Informacion recuperada de INEGI el 8 de mayo 2020

rm(list=ls())

library(tidyverse)
library(haven)
library(foreign)

setwd("~/Mirror/Maestría/TESIS/Datos/Discapacidad/ENADID2018")

BASE<-read.dbf("TSDem.dbf")%>%
  select(ENT,SEXO,EDAD,"P3_11_1A","P3_11_2A","P3_11_5A","P3_11_7A","P3_11_8A","FAC_VIV")%>%
  rename(FACTOR="FAC_VIV")

BaseNorte<-BASE%>%
  filter(ENT=="02"|ENT=="03"|ENT=="05"|ENT=="08"|ENT=="10"|ENT=="19"|ENT=="25"|ENT=="26"|ENT=="28"|ENT=="32")

BaseNorte$EDAD<-as.numeric(levels(BaseNorte$EDAD))[BaseNorte$EDAD]
BaseNorte$EDAD[BaseNorte$EDAD==999]<-NA
BaseNorte$EDAD[BaseNorte$EDAD>=109]<-109

BaseNorte$P3_11_1A<-as.numeric(levels(BaseNorte$P3_11_1A))[BaseNorte$P3_11_1A]
BaseNorte$P3_11_2A<-as.numeric(levels(BaseNorte$P3_11_2A))[BaseNorte$P3_11_2A]
BaseNorte$P3_11_5A<-as.numeric(levels(BaseNorte$P3_11_5A))[BaseNorte$P3_11_5A]
BaseNorte$P3_11_7A<-as.numeric(levels(BaseNorte$P3_11_7A))[BaseNorte$P3_11_7A]
BaseNorte$P3_11_8A<-as.numeric(levels(BaseNorte$P3_11_8A))[BaseNorte$P3_11_8A]

BaseNorte$Dis[BaseNorte$P3_11_1A==1|BaseNorte$P3_11_1A==2|
              BaseNorte$P3_11_2A==1|BaseNorte$P3_11_2A==2|
              BaseNorte$P3_11_5A==1|BaseNorte$P3_11_5A==2|
              BaseNorte$P3_11_7A==1|BaseNorte$P3_11_7A==2|
              BaseNorte$P3_11_8A==1|BaseNorte$P3_11_8A==2]<-1

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

muj.R1<-smooth.spline(c(0:96),
                      BaseNorte2$Mujeres[1:97])
R1.F<-predict(muj.R1,c(0:109))

# matplot(cbind(BaseNorte2$Mujeres,
#               R1.F$y),type="l")

# Prueba de ajuste (PRESS)
# muj.R1$cv.crit

hom.R1<-smooth.spline(c(0:98),
                      BaseNorte2$Hombres[1:99])
R1.H<-predict(hom.R1,c(0:109))

# matplot(cbind(BaseNorte2$Hombres,
#               R1.H$y),type="l")
# hom.R1$cv.crit

rm(BaseNorte,BaseNorte2)

#### CENTRO

BaseCentro<-BASE%>%
  filter(ENT=="01"|ENT=="06"|ENT=="11"|ENT=="13"|
           ENT=="14"|ENT=="15"|ENT=="16"|ENT=="18"|ENT=="22"|ENT=="24"|ENT=="29")

BaseCentro$EDAD<-as.numeric(levels(BaseCentro$EDAD))[BaseCentro$EDAD]
BaseCentro$EDAD[BaseCentro$EDAD==999]<-NA
BaseCentro$EDAD[BaseCentro$EDAD>=109]<-109

BaseCentro$P3_11_1A<-as.numeric(levels(BaseCentro$P3_11_1A))[BaseCentro$P3_11_1A]
BaseCentro$P3_11_2A<-as.numeric(levels(BaseCentro$P3_11_2A))[BaseCentro$P3_11_2A]
BaseCentro$P3_11_5A<-as.numeric(levels(BaseCentro$P3_11_5A))[BaseCentro$P3_11_5A]
BaseCentro$P3_11_7A<-as.numeric(levels(BaseCentro$P3_11_7A))[BaseCentro$P3_11_7A]
BaseCentro$P3_11_8A<-as.numeric(levels(BaseCentro$P3_11_8A))[BaseCentro$P3_11_8A]

BaseCentro$Dis[BaseCentro$P3_11_1A==1|BaseCentro$P3_11_1A==2|
                BaseCentro$P3_11_2A==1|BaseCentro$P3_11_2A==2|
                 BaseCentro$P3_11_5A==1|BaseCentro$P3_11_5A==2|
                 BaseCentro$P3_11_7A==1|BaseCentro$P3_11_7A==2|
                 BaseCentro$P3_11_8A==1|BaseCentro$P3_11_8A==2]<-1

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


muj.R2<-smooth.spline(c(0:98),
                      BaseCentro2$Mujeres[1:99])
R2.F<-predict(muj.R2,c(0:109))

# matplot(cbind(BaseCentro2$Mujeres,
#               R2.F$y),type="l")

hom.R2<-smooth.spline(c(0:89),
                      BaseCentro2$Hombres[1:90], spar = 0.75)
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

BaseCDMX$P3_11_1A<-as.numeric(levels(BaseCDMX$P3_11_1A))[BaseCDMX$P3_11_1A]
BaseCDMX$P3_11_2A<-as.numeric(levels(BaseCDMX$P3_11_2A))[BaseCDMX$P3_11_2A]
BaseCDMX$P3_11_5A<-as.numeric(levels(BaseCDMX$P3_11_5A))[BaseCDMX$P3_11_5A]
BaseCDMX$P3_11_7A<-as.numeric(levels(BaseCDMX$P3_11_7A))[BaseCDMX$P3_11_7A]
BaseCDMX$P3_11_8A<-as.numeric(levels(BaseCDMX$P3_11_8A))[BaseCDMX$P3_11_8A]

BaseCDMX$Dis[BaseCDMX$P3_11_1A==1|BaseCDMX$P3_11_1A==2|
             BaseCDMX$P3_11_2A==1|BaseCDMX$P3_11_2A==2|
             BaseCDMX$P3_11_5A==1|BaseCDMX$P3_11_5A==2|
             BaseCDMX$P3_11_7A==1|BaseCDMX$P3_11_7A==2|
             BaseCDMX$P3_11_8A==1|BaseCDMX$P3_11_8A==2]<-1


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

# Mujeres  En pedazos porque no hay buen fit
muj.R3.2<-smooth.spline(c(26:89),
                        BaseCDMX2$Mujeres[27:90], spar = 0.7)
R3.F.2<-predict(muj.R3.2,c(0:109))

#muj.R3$cv.crit

# voy a imputar el elemento 12 como el promedio del 11 y el 13 para poder completar una serie mas larga
# tambien imputo el 17 para lo mismo
BaseCDMX2$Mujeres[12]<-(BaseCDMX2$Mujeres[11]+BaseCDMX2$Mujeres[13])/2
BaseCDMX2$Mujeres[17]<-(BaseCDMX2$Mujeres[16]+BaseCDMX2$Mujeres[18])/2

muj.R3.1<-smooth.spline(c(9:21),
                        BaseCDMX2$Mujeres[10:22], spar = 0.8)
R3.F.1<-predict(muj.R3.1,c(0:109))

R3.F<-tibble()
R3.F<-c(R3.F.1$y[0:24],R3.F.2$y[25:110])

# matplot(cbind(BaseCDMX2$Mujeres,
#               R3.F.2$y,
#               R3.F.1$y,
#               R3.F),type="l")
# abline(h=0,col="gray")

# Hombres Aqui hay huecos puntuales que voy a llenar con el promedio de los alrededores

BaseCDMX2$Hombres[70]<-(BaseCDMX2$Hombres[71]+BaseCDMX2$Hombres[69])/2
BaseCDMX2$Hombres[29]<-(BaseCDMX2$Hombres[30]+BaseCDMX2$Hombres[28])/2
BaseCDMX2$Hombres[22]<-(BaseCDMX2$Hombres[21]+BaseCDMX2$Hombres[23])/2
BaseCDMX2$Hombres[20]<-(BaseCDMX2$Hombres[21]+BaseCDMX2$Hombres[19])/2
BaseCDMX2$Hombres[31]<-(BaseCDMX2$Hombres[30]+BaseCDMX2$Hombres[32])/2

hom.R3<-smooth.spline(c(0:81),
                      BaseCDMX2$Hombres[1:82], spar = 0.75)
R3.H<-predict(hom.R3,c(0:109))

# matplot(cbind(BaseCDMX2$Hombres,
#               R3.H$y),type="l")

rm(BaseCDMX,BaseCDMX2)
#####

BaseSur<-BASE%>%
  filter(ENT=="04"|ENT=="07"|ENT=="12"|ENT=="17"|ENT=="20"|ENT=="21"|ENT=="23"|ENT=="27"|ENT=="30"|ENT=="31")

BaseSur$EDAD<-as.numeric(levels(BaseSur$EDAD))[BaseSur$EDAD]
BaseSur$EDAD[BaseSur$EDAD==999]<-NA
BaseSur$EDAD[BaseSur$EDAD>=109]<-109

BaseSur$P3_11_1A<-as.numeric(levels(BaseSur$P3_11_1A))[BaseSur$P3_11_1A]
BaseSur$P3_11_2A<-as.numeric(levels(BaseSur$P3_11_2A))[BaseSur$P3_11_2A]
BaseSur$P3_11_5A<-as.numeric(levels(BaseSur$P3_11_5A))[BaseSur$P3_11_5A]
BaseSur$P3_11_7A<-as.numeric(levels(BaseSur$P3_11_7A))[BaseSur$P3_11_7A]
BaseSur$P3_11_8A<-as.numeric(levels(BaseSur$P3_11_8A))[BaseSur$P3_11_8A]

BaseSur$Dis[BaseSur$P3_11_1A==1|BaseSur$P3_11_1A==2|
              BaseSur$P3_11_2A==1|BaseSur$P3_11_2A==2|
              BaseSur$P3_11_5A==1|BaseSur$P3_11_5A==2|
              BaseSur$P3_11_7A==1|BaseSur$P3_11_7A==2|
              BaseSur$P3_11_8A==1|BaseSur$P3_11_8A==2]<-1


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

muj.R4<-smooth.spline(c(0:99),
                      BaseSur2$Mujeres[1:100])
R4.F<-predict(muj.R4,c(0:109))

# matplot(cbind(BaseSur2$Mujeres,
#               R4.F$y),type="l")
# abline(h=1, col="gray")

hom.R4<-smooth.spline(c(0:91),
                      BaseSur2$Hombres[1:92], spar = 0.85)
R4.H<-predict(hom.R4,c(0:109))

# matplot(cbind(BaseSur2$Hombres,
#               R4.H$y),type="l")

rm(BaseSur,BaseSur2)

# Tabla de valores finales
PropH18.F<-tibble(Edad=c(0:109))
PropH18.F$R1<-R1.F$y
PropH18.F$R2<-R2.F$y
PropH18.F$R3<-R3.F
PropH18.F$R4<-R4.F$y
PropH18.F<-PropH18.F%>%
  gather(key = "Region",value = "Prop",-"Edad")

PropH18.M<-tibble(Edad=c(0:109))
PropH18.M$R1<-R1.H$y
PropH18.M$R2<-R2.H$y
PropH18.M$R3<-R3.H$y
PropH18.M$R4<-R4.H$y
PropH18.M<-PropH18.M%>%
  gather(key = "Region",value = "Prop",-"Edad")

save(PropH18.M,PropH18.F,file = "Prop18Homo.RData")

# A<-as.data.frame(cbind(PropH18.M$Prop,PropH18.F$Prop))
# write_csv(A,"Prop2018Homo.csv")

