###
### Proporciones de discapacidad 2000
### Proyecto de Tesis
### Autora: Paola Vazquez Castillo
### Ultima modificacion: 16 mayo 2020

### Informacion recuperada de INEGI el 6 de mayo 2020

### Acuerdate cambiar las proporciones a la correcta

rm(list=ls())

library(tidyverse)
library(haven)
library(foreign)


setwd("~/Mirror/Maestría/TESIS/Datos/Discapacidad/Censo2010")
# Estados de la region norte
E02<-read.dbf("Personas_02.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E03<-read.dbf("Personas_03.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E05<-read.dbf("Personas_05.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E08<-read.dbf("Personas_08.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E10<-read.dbf("Personas_10.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E19<-read.dbf("Personas_19.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E25<-read.dbf("Personas_25.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E26<-read.dbf("Personas_26.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E28<-read.dbf("Personas_28.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E32<-read.dbf("Personas_32.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")

BaseNorte<-rbind(E02,E03,E05,E08,E10,E19,E25,E26,E28,E32)

# BaseNorte$Dis[BaseNorte$DISCAP1==10|BaseNorte$DISCAP2==11|BaseNorte$DISCAP3==12|BaseNorte$DISCAP4==13|BaseNorte$DISCAP5==14|BaseNorte$DISCAP6==15|BaseNorte$DISCAP7==16]<-1

BaseNorte$Dis[BaseNorte$DISCAP1==10|BaseNorte$DISCAP2==11|BaseNorte$DISCAP3==12|BaseNorte$DISCAP4==13|BaseNorte$DISCAP7==16]<-1

BaseNorte$EDAD<-as.numeric(levels(BaseNorte$EDAD))[BaseNorte$EDAD]
BaseNorte$EDAD[BaseNorte$EDAD==999]<-NA
BaseNorte$EDAD[BaseNorte$EDAD>=109]<-109

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


# Suavizamiento de las proporciones porque al final se ponen muy locas
# Tomo los primeros 94 years para hacer el modelo
muj.R1<-smooth.spline(c(0:94),
                      BaseNorte2$Mujeres[1:95], spar = 0.5)
R1.F<-predict(muj.R1,c(0:109))

# matplot(cbind(BaseNorte2$Mujeres,
#               R1.F$y),type="l")

hom.R1<-smooth.spline(c(0:97),
                      BaseNorte2$Hombres[1:98], spar = 0.65)
R1.H<-predict(hom.R1,c(0:109))

# matplot(cbind(BaseNorte2$Hombres,
#               R1.H$y),type="l")

rm(E02,E03,E05,E08,E10,E19,E25,E26,E28,E32,BaseNorte,BaseNorte2)

### Estados de la region centro

E01<-read.dbf("Personas_01.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E06<-read.dbf("Personas_06.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E11<-read.dbf("Personas_11.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E13<-read.dbf("Personas_13.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E14<-read.dbf("Personas_14.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E15<-read.dbf("Personas_15.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E16<-read.dbf("Personas_16.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E18<-read.dbf("Personas_18.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E22<-read.dbf("Personas_22.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E24<-read.dbf("Personas_24.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E29<-read.dbf("Personas_29.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")

BaseCentro<-rbind(E01,E06,E11,E13,E14,E15,E16,E18,E22,E24,E29)

# BaseCentro$Dis[BaseCentro$DISCAP1==10|BaseCentro$DISCAP2==11|BaseCentro$DISCAP3==12|BaseCentro$DISCAP4==13|BaseCentro$DISCAP5==14|BaseCentro$DISCAP6==15|BaseCentro$DISCAP7==16]<-1

BaseCentro$Dis[BaseCentro$DISCAP1==10|BaseCentro$DISCAP2==11|BaseCentro$DISCAP3==12|BaseCentro$DISCAP4==13|BaseCentro$DISCAP7==16]<-1

BaseCentro$EDAD<-as.numeric(levels(BaseCentro$EDAD))[BaseCentro$EDAD]
BaseCentro$EDAD[BaseCentro$EDAD==999]<-NA
BaseCentro$EDAD[BaseCentro$EDAD>=109]<-109

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

muj.R2<-smooth.spline(c(0:106),
                      BaseCentro2$Mujeres[1:107], spar = 0.6)
R2.F<-predict(muj.R2,c(0:109))

# matplot(cbind(BaseCentro2$Mujeres,
#         R2.F$y), type = "l")

hom.R2<-smooth.spline(c(0:105),
                      BaseCentro2$Hombres[1:106])
R2.H<-predict(hom.R2,c(0:109))

# matplot(cbind(BaseCentro2$Hombres,
#         R2.H$y), type = "l")

# matplot(c(0:109),
#         cbind(BaseCentro2$Mujeres,
#               R2.F$y,
#               BaseCentro2$Hombres,
#               R2.H$y),type="l")
# abline(v=95,lty=2,col="darkgray")

rm(E01,E06,E11,E13,E14,E15,E16,E18,E22,E24,E29,BaseCentro,BaseCentro2)

### REGION CDMX 

E09<-read.dbf("Personas_09.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
BaseCDMX<-rbind(E09)

# BaseCDMX$Dis[BaseCDMX$DISCAP1==10|BaseCDMX$DISCAP2==11|BaseCDMX$DISCAP3==12|BaseCDMX$DISCAP4==13|BaseCDMX$DISCAP5==14|BaseCDMX$DISCAP6==15|BaseCDMX$DISCAP7==16]<-1

BaseCDMX$Dis[BaseCDMX$DISCAP1==10|BaseCDMX$DISCAP2==11|BaseCDMX$DISCAP3==12|BaseCDMX$DISCAP4==13|BaseCDMX$DISCAP7==16]<-1

BaseCDMX$EDAD<-as.numeric(levels(BaseCDMX$EDAD))[BaseCDMX$EDAD]
BaseCDMX$EDAD[BaseCDMX$EDAD==999]<-NA
BaseCDMX$EDAD[BaseCDMX$EDAD>=109]<-109

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

muj.R3<-smooth.spline(c(0:94),
                      BaseCDMX2$Mujeres[1:95])
R3.F<-predict(muj.R3,c(0:109))

# matplot(cbind(BaseCDMX2$Mujeres,
#               R3.F$y),type="l")

hom.R3<-smooth.spline(c(0:94),
                      BaseCDMX2$Hombres[1:95], spar = 0.8)
R3.H<-predict(hom.R3,c(0:109))

# matplot(cbind(BaseCDMX2$Hombres,
#               R3.H$y),type="l")

# matplot(c(0:104),
#         cbind(BaseCDMX2$Mujeres[1:105],
#               R3.F$y[1:105],
#               BaseCDMX2$Hombres[1:105],
#               R3.H$y[1:105]),type="l")
# 
# matplot(c(0:109),
#         cbind(R3.F$y,
#               R3.H$y),type = "l")

rm(E09,BaseCDMX,BaseCDMX2)

### REGION SUR 

E04<-read.dbf("Personas_04.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E07<-read.dbf("Personas_07.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E12<-read.dbf("Personas_12.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E17<-read.dbf("Personas_17.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E20<-read.dbf("Personas_20.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E21<-read.dbf("Personas_21.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E23<-read.dbf("Personas_23.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E27<-read.dbf("Personas_27.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E30<-read.dbf("Personas_30.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")
E31<-read.dbf("Personas_31.dbf")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISCAP1","DISCAP2","DISCAP3","DISCAP4","DISCAP5","DISCAP6","DISCAP7")

BaseSur<-rbind(E04,E07,E12,E17,E20,E21,E23,E27,E30,E31)

# BaseSur$Dis[BaseSur$DISCAP1==10|BaseSur$DISCAP2==11|BaseSur$DISCAP3==12|BaseSur$DISCAP4==13|BaseSur$DISCAP5==14|BaseSur$DISCAP6==15|BaseSur$DISCAP7==16]<-1

BaseSur$Dis[BaseSur$DISCAP1==10|BaseSur$DISCAP2==11|BaseSur$DISCAP3==12|BaseSur$DISCAP4==13|BaseSur$DISCAP7==16]<-1

BaseSur$EDAD<-as.numeric(levels(BaseSur$EDAD))[BaseSur$EDAD]
BaseSur$EDAD[BaseSur$EDAD==999]<-NA
BaseSur$EDAD[BaseSur$EDAD>=109]<-109

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

muj.R4<-smooth.spline(c(0:95),
                      BaseSur2$Mujeres[1:96])
R4.F<-predict(muj.R4,c(0:109))

# matplot(cbind(BaseSur2$Mujeres,
#               R4.F$y),type = "l")

hom.R4<-smooth.spline(c(0:105),
                      BaseSur2$Hombres[1:106])
R4.H<-predict(hom.R4,c(0:109))

# matplot(cbind(BaseSur2$Hombres,
#               R4.H$y),type="l")

# matplot(c(0:109),
#         cbind(BaseSur2$Mujeres[1:110],
#               R4.F$y[1:110],
#               BaseSur2$Hombres[1:110],
#               R4.H$y[1:110]),type="l")

# matplot(c(0:109),
#         cbind(R4.F$y,
#               R4.H$y),type = "l")
# 

rm(E04,E07,E12,E17,E20,E21,E23,E27,E30,E31,BaseSur,BaseSur2)

PropH10.F<-tibble(Edad=c(0:109))
PropH10.F$R1<-R1.F$y
PropH10.F$R2<-R2.F$y
PropH10.F$R3<-R3.F$y
PropH10.F$R4<-R4.F$y

PropH10.F<-PropH10.F%>%
  gather(key = "Region",value = "Prop",-"Edad")

PropH10.M<-tibble(Edad=c(0:109))
PropH10.M$R1<-R1.H$y
PropH10.M$R2<-R2.H$y
PropH10.M$R3<-R3.H$y
PropH10.M$R4<-R4.H$y

PropH10.M<-PropH10.M%>%
  gather(key = "Region",value = "Prop",-"Edad")

save(PropH10.M,PropH10.F,file = "PropHomo.RData")

A<-as.data.frame(cbind(PropH10.M$Prop,PropH10.F$Prop))
write_csv(A,"Prop2010Homo.csv")