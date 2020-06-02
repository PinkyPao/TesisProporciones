###
### Proporciones de discapacidad 2000
### Proyecto de Tesis
### Autora: Paola Vazquez Castillo
### Ultima modificacion: 15 mayo 2020

### Informacion recuperada de INEGI el 4 de mayo 2020

rm(list=ls())

library(tidyverse)
library(haven)
library(foreign)


setwd("~/Mirror/Maestría/TESIS/Datos/Discapacidad/Censo2000")

# Estados de la region norte
E02<-read.dbf("PER_F02.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E03<-read.dbf("PER_F03.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E05<-read.dbf("PER_F05.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E08<-read.dbf("PER_F08.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E10<-read.dbf("PER_F10.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E19<-read.dbf("PER_F19.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E26<-read.dbf("PER_F26.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E25<-read.dbf("PER_F25.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E28<-read.dbf("PER_F28.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

E32<-read.dbf("PER_F32.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

BaseNorte<-rbind(E02,E03,E05,E08,E10,E19,E25,E26,E28,E32)

BaseNorte$Dis[BaseNorte$DISMOV==1|BaseNorte$DISOIR==3|BaseNorte$DISMUDO==4|BaseNorte$DISVER==5|BaseNorte$DISMENT==6|BaseNorte$DISOTRA_C==131|BaseNorte$DISOTRA_C==199|BaseNorte$DISOTRA_C==299|BaseNorte$DISOTRA_C==399|BaseNorte$DISOTRA_C==421|BaseNorte$DISOTRA_C==422|BaseNorte$DISOTRA_C==430|BaseNorte$DISOTRA_C==499|BaseNorte$DISOTRA_C==960|BaseNorte$DISOTRA_V==7]<-1

BaseNorte$EDAD<-as.numeric(levels(BaseNorte$EDAD))[BaseNorte$EDAD]
BaseNorte$EDAD[BaseNorte$EDAD==999]<-NA
BaseNorte$EDAD[BaseNorte$EDAD>=109]<-109
BaseNorte$FACTOR<-as.numeric(levels(BaseNorte$FACTOR))[BaseNorte$FACTOR]

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
                      BaseNorte2$Mujeres[1:95])
R1.F<-predict(muj.R1,c(0:109))

# matplot(cbind(BaseNorte2$Mujeres,
#               R1.F$y),type="l")

hom.R1<-smooth.spline(c(0:97),
                      BaseNorte2$Hombres[1:98], spar = 0.8)
R1.H<-predict(hom.R1,c(0:109))

# matplot(cbind(BaseNorte2$Hombres,
#               R1.H$y),type="l")

# matplot(c(0:94),cbind(
#          BaseNorte2$Hombres[1:95],
#          R1.H$y[1:95],
#          BaseNorte2$Mujeres[1:95],
#          R1.F$y[1:95]),type="l")
# # 
# 
# matplot(c(0:109),cbind(
#          R1.H$y,
#          R1.F$y),type="l")

# Entonces R1.H y R1.F tienen las proporciones para la region 1

rm(E02,E03,E05,E08,E10,E19,E25,E26,E28,E32,BaseNorte,BaseNorte2)

# Region centro

E01<-read.dbf("PER_F01.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E06<-read.dbf("PER_F06.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E11<-read.dbf("PER_F11.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E13<-read.dbf("PER_F13.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E14<-read.dbf("PER_F14.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E15<-read.dbf("PER_F15.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E16<-read.dbf("PER_F16.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E18<-read.dbf("PER_F18.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E22<-read.dbf("PER_F22.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E24<-read.dbf("PER_F24.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E29<-read.dbf("PER_F29.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR",
         "DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

BaseCentro<-rbind(E01,E06,E11,E13,E14,E15,E16,E18,E22,E24,E29)

BaseCentro$Dis[BaseCentro$DISMOV==1|BaseCentro$DISOIR==3|BaseCentro$DISMUDO==4|BaseCentro$DISVER==5|BaseCentro$DISMENT==6|BaseCentro$DISOTRA_C==131|BaseCentro$DISOTRA_C==199|BaseCentro$DISOTRA_C==299|BaseCentro$DISOTRA_C==399|BaseCentro$DISOTRA_C==421|BaseCentro$DISOTRA_C==422|BaseCentro$DISOTRA_C==430|BaseCentro$DISOTRA_C==499|BaseCentro$DISOTRA_C==960|BaseCentro$DISOTRA_V==7]<-1

BaseCentro$EDAD<-as.numeric(levels(BaseCentro$EDAD))[BaseCentro$EDAD]
BaseCentro$EDAD[BaseCentro$EDAD==999]<-NA
BaseCentro$EDAD[BaseCentro$EDAD>=109]<-109
BaseCentro$FACTOR<-as.numeric(levels(BaseCentro$FACTOR))[BaseCentro$FACTOR]

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

muj.R2<-smooth.spline(c(0:90),
                      BaseCentro2$Mujeres[1:91], spar = 0.8)
R2.F<-predict(muj.R2,c(0:109))

# matplot(cbind(BaseCentro2$Mujeres,
#               R2.F$y),type="l")

hom.R2<-smooth.spline(c(0:91),
                      BaseCentro2$Hombres[1:92])
R2.H<-predict(hom.R2,c(0:109))

# matplot(cbind(BaseCentro2$Hombres,
#               R2.H$y),type="l")

# matplot(c(0:109),
#         cbind(BaseCentro2$Mujeres,
#               R2.F$y,
#               BaseCentro2$Hombres,
#               R2.H$y),type="l")
# abline(v=95,lty=2,col="darkgray")

rm(E01,E06,E11,E13,E14,E15,E16,E18,E22,E24,E29,BaseCentro,BaseCentro2)

# CDMX

E09<-read.dbf("PER_F09.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

BaseCDMX<-rbind(E09)

BaseCDMX$Dis[BaseCDMX$DISMOV==1|BaseCDMX$DISOIR==3|BaseCDMX$DISMUDO==4|BaseCDMX$DISVER==5|BaseCDMX$DISMENT==6|BaseCDMX$DISOTRA_C==131|BaseCDMX$DISOTRA_C==199|BaseCDMX$DISOTRA_C==299|BaseCDMX$DISOTRA_C==399|BaseCDMX$DISOTRA_C==421|BaseCDMX$DISOTRA_C==422|BaseCDMX$DISOTRA_C==430|BaseCDMX$DISOTRA_C==499|BaseCDMX$DISOTRA_C==960|BaseCDMX$DISOTRA_V==7]<-1

BaseCDMX$EDAD<-as.numeric(levels(BaseCDMX$EDAD))[BaseCDMX$EDAD]
BaseCDMX$EDAD[BaseCDMX$EDAD==999]<-NA
BaseCDMX$EDAD[BaseCDMX$EDAD>=109]<-109
BaseCDMX$FACTOR<-as.numeric(levels(BaseCDMX$FACTOR))[BaseCDMX$FACTOR]

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

muj.R3<-smooth.spline(c(0:97),
                      BaseCDMX2$Mujeres[1:98])
R3.F<-predict(muj.R3,c(0:109))

# matplot(cbind(BaseCDMX2$Mujeres,
#               R3.F$y),type="l")

hom.R3<-smooth.spline(c(0:98),
                      BaseCDMX2$Hombres[1:99])
R3.H<-predict(hom.R3,c(0:109))

# matplot(cbind(BaseCDMX2$Hombres,
#               R3.H$y),type="l")

# matplot(c(0:100),
#         cbind(BaseCDMX2$Mujeres[1:101],
#               R3.F$y[1:101],
#               BaseCDMX2$Hombres[1:101],
#               R3.H$y[1:101]),type="l")
# 
# matplot(c(0:109),
#         cbind(
#               R3.F$y,
#               R3.H$y),type="l")
# abline(v=95,lty=2,col="darkgray")

rm(E09,BaseCDMX,BaseCDMX2)

# Region sur 
E04<-read.dbf("PER_F04.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E07<-read.dbf("PER_F07.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E12<-read.dbf("PER_F12.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E17<-read.dbf("PER_F17.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E20<-read.dbf("PER_F20.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E21<-read.dbf("PER_F21.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E23<-read.dbf("PER_F23.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E27<-read.dbf("PER_F27.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E30<-read.dbf("PER_F30.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")
E31<-read.dbf("PER_F31.DBF")%>%
  select(ENT,FACTOR,SEXO,EDAD,"DISMOV","DISBRA","DISOIR","DISMUDO","DISVER","DISMENT","DISOTRA_C","DISOTRA_V","DISNOTIE")

BaseSur<-rbind(E04,E07,E12,E17,E20,E21,E23,E27,E30,E31)

BaseSur$Dis[BaseSur$DISMOV==1|BaseSur$DISOIR==3|BaseSur$DISMUDO==4|BaseSur$DISVER==5|BaseSur$DISMENT==6|BaseSur$DISOTRA_C==131|BaseSur$DISOTRA_C==199|BaseSur$DISOTRA_C==299|BaseSur$DISOTRA_C==399|BaseSur$DISOTRA_C==421|BaseSur$DISOTRA_C==422|BaseSur$DISOTRA_C==430|BaseSur$DISOTRA_C==499|BaseSur$DISOTRA_C==960|BaseSur$DISOTRA_V==7]<-1

BaseSur$EDAD<-as.numeric(levels(BaseSur$EDAD))[BaseSur$EDAD]
BaseSur$EDAD[BaseSur$EDAD==999]<-NA
BaseSur$EDAD[BaseSur$EDAD>=109]<-109
BaseSur$FACTOR<-as.numeric(levels(BaseSur$FACTOR))[BaseSur$FACTOR]

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

muj.R4<-smooth.spline(c(0:96),
                      BaseSur2$Mujeres[1:97])
R4.F<-predict(muj.R4,c(0:109))

matplot(cbind(BaseSur2$Mujeres,
              R4.F$y),type="l")

hom.R4<-smooth.spline(c(0:105),
                      BaseSur2$Hombres[1:106])
R4.H<-predict(hom.R4,c(0:109))

# matplot(cbind(BaseSur2$Hombres,
#               R4.H$y),type="l")

# matplot(c(0:109),
#         cbind(BaseSur2$Mujeres,
#               R4.F$y,
#               BaseSur2$Hombres,
#               R4.H$y),type="l")
# matplot(c(0:109),
#         cbind(
#           R4.F$y,
#           R4.H$y),type="l")

rm(E04,E07,E12,E17,E20,E21,E23,E27,E30,E31,BaseSur,BaseSur2)

PropH00.F<-tibble(Edad=c(0:109))
PropH00.F$R1<-R1.F$y
PropH00.F$R2<-R2.F$y
PropH00.F$R3<-R3.F$y
PropH00.F$R4<-R4.F$y

PropH00.F<-PropH00.F%>%
  gather(key = "Region",value = "Prop",-"Edad")

PropH00.M<-tibble(Edad=c(0:109))
PropH00.M$R1<-R1.H$y
PropH00.M$R2<-R2.H$y
PropH00.M$R3<-R3.H$y
PropH00.M$R4<-R4.H$y

PropH00.M<-PropH00.M%>%
  gather(key = "Region",value = "Prop",-"Edad")

save(PropH00.M,PropH00.F,file = "PropHomo.RData")

# A<-as.data.frame(cbind(PropH00.M$Prop,PropH00.F$Prop))
# write_csv(A,"Prop2000Homo.csv")