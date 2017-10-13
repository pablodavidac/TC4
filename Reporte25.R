library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("xtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("arules", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggrepel", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("caret", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library(ROCR)
library("stargazer", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")





load("d:/Users_info/ALBANPD/My Documents/Bases/Reporte25.RData")

#clientes<-subset(Reporte25, FechaCorte=="2015-12-31" | FechaCorte=="2015-09-30" | FechaCorte=="2015-06-30" | FechaCorte=="2014-12-31")

Base<-subset(Reporte25, FechaCorte=="2015-12-31")  #256,082
#Base<-subset(Base, !(Identificacion==1001083979 & Afinidad=="CUOTA FACIL"))
#Base<-subset(Base, !duplicated(Identificacion)) #256080

##############################tasas de crecimiento##################################

#####################################t/t-12################################################
Dic15<-subset(Reporte25,FechaCorte=="2015-12-31")
Dic14<-subset(Reporte25,FechaCorte=="2014-12-31")
Auxiliar<-left_join(Dic14,Dic15,by=c("Identificacion","IdCuentaTarjeta"))
#Auxiliar<-subset(Auxiliar, EstadoCuentaTarjeta.x=="ACTIVA" & EstadoCuentaTarjeta.y=="ACTIVA") #252388
#Auxiliar<-subset(Auxiliar,!duplicated(Identificacion)) #252384
Auxiliar["T12.CupoAprobado"]<-(Auxiliar$CupoAprobado.y-Auxiliar$CupoAprobado.x)/Auxiliar$CupoAprobado.x
Auxiliar["T12.CupoAprobadoNormal"]<-(Auxiliar$CupoAprobadoNormal.y-Auxiliar$CupoAprobadoNormal.x)/Auxiliar$CupoAprobadoNormal.x
Auxiliar["T12.CupoAprobadoSuperAvance"]<-(Auxiliar$CupoAprobadoSuperAvance.y-Auxiliar$CupoAprobadoSuperAvance.x)/Auxiliar$CupoAprobadoSuperAvance.x
Auxiliar["T12.CupoUtilizado"]<-(Auxiliar$CupoUtilizado.y-Auxiliar$CupoUtilizado.x)/Auxiliar$CupoUtilizado.x
Auxiliar["T12.CupoUtilizadoAvance"]<-(Auxiliar$CupoUtilizadoAvance.y-Auxiliar$CupoUtilizadoAvance.x)/Auxiliar$CupoUtilizadoAvance.x
Auxiliar["T12.CupoUtilizadoNormal"]<-(Auxiliar$CupoUtilizadoNormal.y-Auxiliar$CupoUtilizadoNormal.x)/Auxiliar$CupoUtilizadoNormal.x
Auxiliar["T12.CupoUtilizadoSuperAvance"]<-(Auxiliar$CupoUtilizadoSuperAvance.y-Auxiliar$CupoUtilizadoSuperAvance.x)/Auxiliar$CupoUtilizadoSuperAvance.x
Auxiliar["T12.DiasMora"]<-(Auxiliar$DiasMora.y-Auxiliar$DiasMora.x)/Auxiliar$DiasMora.x


###############
Temporal<-subset(Auxiliar, select=c("Identificacion", 
                                    "IdCuentaTarjeta",
                                    "T12.CupoAprobado",
                                    "T12.CupoAprobadoNormal",
                                    "T12.CupoAprobadoSuperAvance",
                                    "T12.CupoUtilizado",
                                    "T12.CupoUtilizadoNormal",
                                    "T12.CupoUtilizadoAvance",
                                    "T12.CupoUtilizadoSuperAvance",
                                    "T12.DiasMora"))

Base<-left_join(Base,Temporal,by=c("Identificacion","IdCuentaTarjeta"))

#####################################t/t-6################################################

Dic15<-subset(Reporte25,FechaCorte=="2015-12-31")
Jun15<-subset(Reporte25,FechaCorte=="2015-06-30")
Auxiliar<-left_join(Jun15,Dic15,by=c("Identificacion","IdCuentaTarjeta"))
#Auxiliar<-subset(Auxiliar, EstadoCuentaTarjeta.x=="ACTIVA" & EstadoCuentaTarjeta.y=="ACTIVA") #253272
#Auxiliar<-subset(Auxiliar,!duplicated(Identificacion)) #253268
Auxiliar["T6.CupoAprobado"]<-(Auxiliar$CupoAprobado.y-Auxiliar$CupoAprobado.x)/Auxiliar$CupoAprobado.x
Auxiliar["T6.CupoAprobadoNormal"]<-(Auxiliar$CupoAprobadoNormal.y-Auxiliar$CupoAprobadoNormal.x)/Auxiliar$CupoAprobadoNormal.x
Auxiliar["T6.CupoAprobadoSuperAvance"]<-(Auxiliar$CupoAprobadoSuperAvance.y-Auxiliar$CupoAprobadoSuperAvance.x)/Auxiliar$CupoAprobadoSuperAvance.x
Auxiliar["T6.CupoUtilizado"]<-(Auxiliar$CupoUtilizado.y-Auxiliar$CupoUtilizado.x)/Auxiliar$CupoUtilizado.x
Auxiliar["T6.CupoUtilizadoAvance"]<-(Auxiliar$CupoUtilizadoAvance.y-Auxiliar$CupoUtilizadoAvance.x)/Auxiliar$CupoUtilizadoAvance.x
Auxiliar["T6.CupoUtilizadoNormal"]<-(Auxiliar$CupoUtilizadoNormal.y-Auxiliar$CupoUtilizadoNormal.x)/Auxiliar$CupoUtilizadoNormal.x
Auxiliar["T6.CupoUtilizadoSuperAvance"]<-(Auxiliar$CupoUtilizadoSuperAvance.y-Auxiliar$CupoUtilizadoSuperAvance.x)/Auxiliar$CupoUtilizadoSuperAvance.x
Auxiliar["T6.DiasMora"]<-(Auxiliar$DiasMora.y-Auxiliar$DiasMora.x)/Auxiliar$DiasMora.x

###############
Temporal<-subset(Auxiliar, select=c("Identificacion", 
                                    "IdCuentaTarjeta",
                                    "T6.CupoAprobado",
                                    "T6.CupoAprobadoNormal",
                                    "T6.CupoAprobadoSuperAvance",
                                    "T6.CupoUtilizado",
                                    "T6.CupoUtilizadoNormal",
                                    "T6.CupoUtilizadoAvance",
                                    "T6.CupoUtilizadoSuperAvance",
                                    "T6.DiasMora"))

Base<-left_join(Base,Temporal,by=c("Identificacion","IdCuentaTarjeta"))

#####################################t/t-3################################################

Dic15<-subset(Reporte25,FechaCorte=="2015-12-31")
Sep15<-subset(Reporte25,FechaCorte=="2015-09-30")
Auxiliar<-left_join(Sep15,Dic15,by=c("Identificacion","IdCuentaTarjeta"))
#Auxiliar<-subset(Auxiliar, EstadoCuentaTarjeta.x=="ACTIVA" & EstadoCuentaTarjeta.y=="ACTIVA") #255462
#Auxiliar<-subset(Auxiliar,!duplicated(Identificacion)) #255458
Auxiliar["T3.CupoAprobado"]<-(Auxiliar$CupoAprobado.y-Auxiliar$CupoAprobado.x)/Auxiliar$CupoAprobado.x
Auxiliar["T3.CupoAprobadoNormal"]<-(Auxiliar$CupoAprobadoNormal.y-Auxiliar$CupoAprobadoNormal.x)/Auxiliar$CupoAprobadoNormal.x
Auxiliar["T3.CupoAprobadoSuperAvance"]<-(Auxiliar$CupoAprobadoSuperAvance.y-Auxiliar$CupoAprobadoSuperAvance.x)/Auxiliar$CupoAprobadoSuperAvance.x
Auxiliar["T3.CupoUtilizado"]<-(Auxiliar$CupoUtilizado.y-Auxiliar$CupoUtilizado.x)/Auxiliar$CupoUtilizado.x
Auxiliar["T3.CupoUtilizadoAvance"]<-(Auxiliar$CupoUtilizadoAvance.y-Auxiliar$CupoUtilizadoAvance.x)/Auxiliar$CupoUtilizadoAvance.x
Auxiliar["T3.CupoUtilizadoNormal"]<-(Auxiliar$CupoUtilizadoNormal.y-Auxiliar$CupoUtilizadoNormal.x)/Auxiliar$CupoUtilizadoNormal.x
Auxiliar["T3.CupoUtilizadoSuperAvance"]<-(Auxiliar$CupoUtilizadoSuperAvance.y-Auxiliar$CupoUtilizadoSuperAvance.x)/Auxiliar$CupoUtilizadoSuperAvance.x
Auxiliar["T3.DiasMora"]<-(Auxiliar$DiasMora.y-Auxiliar$DiasMora.x)/Auxiliar$DiasMora.x


###############
Temporal<-subset(Auxiliar, select=c("Identificacion", 
                                    "IdCuentaTarjeta",
                                    "T3.CupoAprobado",
                                    "T3.CupoAprobadoNormal",
                                    "T3.CupoAprobadoSuperAvance",
                                    "T3.CupoUtilizado",
                                    "T3.CupoUtilizadoNormal",
                                    "T3.CupoUtilizadoAvance",
                                    "T3.CupoUtilizadoSuperAvance",
                                    "T3.DiasMora"))

Base<-left_join(Base,Temporal,by=c("Identificacion","IdCuentaTarjeta"))
#############################################################################


#####################################t/t-1################################################

Dic15<-subset(Reporte25,FechaCorte=="2015-12-31")
Nov15<-subset(Reporte25,FechaCorte=="2015-11-30")
Auxiliar<-left_join(Nov15,Dic15,by=c("Identificacion","IdCuentaTarjeta"))
#Auxiliar<-subset(Auxiliar, EstadoCuentaTarjeta.x=="ACTIVA" & EstadoCuentaTarjeta.y=="ACTIVA") #255462
#Auxiliar<-subset(Auxiliar,!duplicated(Identificacion)) #255458
Auxiliar["T1.CupoAprobado"]<-(Auxiliar$CupoAprobado.y-Auxiliar$CupoAprobado.x)/Auxiliar$CupoAprobado.x
Auxiliar["T1.CupoAprobadoNormal"]<-(Auxiliar$CupoAprobadoNormal.y-Auxiliar$CupoAprobadoNormal.x)/Auxiliar$CupoAprobadoNormal.x
Auxiliar["T1.CupoAprobadoSuperAvance"]<-(Auxiliar$CupoAprobadoSuperAvance.y-Auxiliar$CupoAprobadoSuperAvance.x)/Auxiliar$CupoAprobadoSuperAvance.x
Auxiliar["T1.CupoUtilizado"]<-(Auxiliar$CupoUtilizado.y-Auxiliar$CupoUtilizado.x)/Auxiliar$CupoUtilizado.x
Auxiliar["T1.CupoUtilizadoAvance"]<-(Auxiliar$CupoUtilizadoAvance.y-Auxiliar$CupoUtilizadoAvance.x)/Auxiliar$CupoUtilizadoAvance.x
Auxiliar["T1.CupoUtilizadoNormal"]<-(Auxiliar$CupoUtilizadoNormal.y-Auxiliar$CupoUtilizadoNormal.x)/Auxiliar$CupoUtilizadoNormal.x
Auxiliar["T1.CupoUtilizadoSuperAvance"]<-(Auxiliar$CupoUtilizadoSuperAvance.y-Auxiliar$CupoUtilizadoSuperAvance.x)/Auxiliar$CupoUtilizadoSuperAvance.x
Auxiliar["T1.DiasMora"]<-(Auxiliar$DiasMora.y-Auxiliar$DiasMora.x)/Auxiliar$DiasMora.x


###############
Temporal<-subset(Auxiliar, select=c("Identificacion", 
                                    "IdCuentaTarjeta",
                                    "T1.CupoAprobado",
                                    "T1.CupoAprobadoNormal",
                                    "T1.CupoAprobadoSuperAvance",
                                    "T1.CupoUtilizado",
                                    "T1.CupoUtilizadoNormal",
                                    "T1.CupoUtilizadoAvance",
                                    "T1.CupoUtilizadoSuperAvance",
                                    "T1.DiasMora"))

Base<-left_join(Base,Temporal,by=c("Identificacion","IdCuentaTarjeta"))
####################################t-3/t-6##############################################
Sep15<-subset(Reporte25,FechaCorte=="2015-09-30")
Jun15<-subset(Reporte25,FechaCorte=="2015-06-30")
Auxiliar<-left_join(Jun15,Sep15,by=c("Identificacion","IdCuentaTarjeta"))
#Auxiliar<-subset(Auxiliar, EstadoCuentaTarjeta.x=="ACTIVA" & EstadoCuentaTarjeta.y=="ACTIVA") #255462
#Auxiliar<-subset(Auxiliar,!duplicated(Identificacion)) #255458
Auxiliar["T36.CupoAprobado"]<-(Auxiliar$CupoAprobado.y-Auxiliar$CupoAprobado.x)/Auxiliar$CupoAprobado.x
Auxiliar["T36.CupoAprobadoNormal"]<-(Auxiliar$CupoAprobadoNormal.y-Auxiliar$CupoAprobadoNormal.x)/Auxiliar$CupoAprobadoNormal.x
Auxiliar["T36.CupoAprobadoSuperAvance"]<-(Auxiliar$CupoAprobadoSuperAvance.y-Auxiliar$CupoAprobadoSuperAvance.x)/Auxiliar$CupoAprobadoSuperAvance.x
Auxiliar["T36.CupoUtilizado"]<-(Auxiliar$CupoUtilizado.y-Auxiliar$CupoUtilizado.x)/Auxiliar$CupoUtilizado.x
Auxiliar["T36.CupoUtilizadoAvance"]<-(Auxiliar$CupoUtilizadoAvance.y-Auxiliar$CupoUtilizadoAvance.x)/Auxiliar$CupoUtilizadoAvance.x
Auxiliar["T36.CupoUtilizadoNormal"]<-(Auxiliar$CupoUtilizadoNormal.y-Auxiliar$CupoUtilizadoNormal.x)/Auxiliar$CupoUtilizadoNormal.x
Auxiliar["T36.CupoUtilizadoSuperAvance"]<-(Auxiliar$CupoUtilizadoSuperAvance.y-Auxiliar$CupoUtilizadoSuperAvance.x)/Auxiliar$CupoUtilizadoSuperAvance.x
Auxiliar["T36.DiasMora"]<-(Auxiliar$DiasMora.y-Auxiliar$DiasMora.x)/Auxiliar$DiasMora.x


###############
Temporal<-subset(Auxiliar, select=c("Identificacion", 
                                    "IdCuentaTarjeta",
                                    "T36.CupoAprobado",
                                    "T36.CupoAprobadoNormal",
                                    "T36.CupoAprobadoSuperAvance",
                                    "T36.CupoUtilizado",
                                    "T36.CupoUtilizadoNormal",
                                    "T36.CupoUtilizadoAvance",
                                    "T36.CupoUtilizadoSuperAvance",
                                    "T36.DiasMora"))

Base<-left_join(Base,Temporal,by=c("Identificacion","IdCuentaTarjeta"))

####################################t-6/t-12##############################################

Jun15<-subset(Reporte25,FechaCorte=="2015-06-30")
Dic14<-subset(Reporte25,FechaCorte=="2014-12-31")
Auxiliar<-left_join(Dic14,Jun15,by=c("Identificacion","IdCuentaTarjeta"))
#Auxiliar<-subset(Auxiliar, EstadoCuentaTarjeta.x=="ACTIVA" & EstadoCuentaTarjeta.y=="ACTIVA") #255462
#Auxiliar<-subset(Auxiliar,!duplicated(Identificacion)) #255458
Auxiliar["T612.CupoAprobado"]<-(Auxiliar$CupoAprobado.y-Auxiliar$CupoAprobado.x)/Auxiliar$CupoAprobado.x
Auxiliar["T612.CupoAprobadoNormal"]<-(Auxiliar$CupoAprobadoNormal.y-Auxiliar$CupoAprobadoNormal.x)/Auxiliar$CupoAprobadoNormal.x
Auxiliar["T612.CupoAprobadoSuperAvance"]<-(Auxiliar$CupoAprobadoSuperAvance.y-Auxiliar$CupoAprobadoSuperAvance.x)/Auxiliar$CupoAprobadoSuperAvance.x
Auxiliar["T612.CupoUtilizado"]<-(Auxiliar$CupoUtilizado.y-Auxiliar$CupoUtilizado.x)/Auxiliar$CupoUtilizado.x
Auxiliar["T612.CupoUtilizadoAvance"]<-(Auxiliar$CupoUtilizadoAvance.y-Auxiliar$CupoUtilizadoAvance.x)/Auxiliar$CupoUtilizadoAvance.x
Auxiliar["T612.CupoUtilizadoNormal"]<-(Auxiliar$CupoUtilizadoNormal.y-Auxiliar$CupoUtilizadoNormal.x)/Auxiliar$CupoUtilizadoNormal.x
Auxiliar["T612.CupoUtilizadoSuperAvance"]<-(Auxiliar$CupoUtilizadoSuperAvance.y-Auxiliar$CupoUtilizadoSuperAvance.x)/Auxiliar$CupoUtilizadoSuperAvance.x
Auxiliar["T612.DiasMora"]<-(Auxiliar$DiasMora.y-Auxiliar$DiasMora.x)/Auxiliar$DiasMora.x


###############
Temporal<-subset(Auxiliar, select=c("Identificacion", 
                                    "IdCuentaTarjeta",
                                    "T612.CupoAprobado",
                                    "T612.CupoAprobadoNormal",
                                    "T612.CupoAprobadoSuperAvance",
                                    "T612.CupoUtilizado",
                                    "T612.CupoUtilizadoNormal",
                                    "T612.CupoUtilizadoAvance",
                                    "T612.CupoUtilizadoSuperAvance",
                                    "T612.DiasMora"))

Base<-left_join(Base,Temporal,by=c("Identificacion","IdCuentaTarjeta"))

################
rm(Temporal)
rm(Auxiliar)
##########################################################

################################Variables Promedios de cupos utilizado################################3
#################################Promedio con 12 meses#############################
Doce<-subset(Reporte25, 
              FechaCorte=="2015-12-31" | 
              FechaCorte=="2015-11-30" |
              FechaCorte=="2015-10-31" |
              FechaCorte=="2015-09-30" |
              FechaCorte=="2015-08-31" |
              FechaCorte=="2015-07-31" |
              FechaCorte=="2015-06-30" |
              FechaCorte=="2015-05-31" |
              FechaCorte=="2015-04-30" |
              FechaCorte=="2015-03-31" |
              FechaCorte=="2015-02-28" |
              FechaCorte=="2015-01-31" )

#Doce<-subset(Doce,EstadoCuentaTarjeta=="ACTIVA")  #3365008
Auxiliar<-Doce %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizado= sum(CupoUtilizado)/12)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Doce %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoAvance= sum(CupoUtilizadoAvance)/12)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Doce %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoNormal= sum(CupoUtilizadoNormal)/12)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))


Auxiliar<-Doce %>% 
  group_by(IdCuentaTarjeta) %>% 
    summarise(Prom.CupoUtilizadoSuperAvance= sum(CupoUtilizadoSuperAvance)/12)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Doce %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.DiasMora= sum(DiasMora)/12)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))
#################################Promedio con 6 meses#############################
Seis<-subset(Reporte25, 
             FechaCorte=="2015-12-31" | 
               FechaCorte=="2015-11-30" |
               FechaCorte=="2015-10-31" |
               FechaCorte=="2015-09-30" |
               FechaCorte=="2015-08-31" |
               FechaCorte=="2015-07-31")

#Seis<-subset(Seis,EstadoCuentaTarjeta=="ACTIVA")  #3365008
Auxiliar<-Seis %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizado6= sum(CupoUtilizado)/6)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Seis %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoAvance6= sum(CupoUtilizadoAvance)/6)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Seis %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoNormal6= sum(CupoUtilizadoNormal)/6)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))


Auxiliar<-Seis %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoSuperAvance6= sum(CupoUtilizadoSuperAvance)/6)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Seis %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.DiasMora6= sum(DiasMora)/6)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

#################################Promedio con 3 meses#############################
Tres<-subset(Reporte25, 
             FechaCorte=="2015-12-31" | 
               FechaCorte=="2015-11-30" |
               FechaCorte=="2015-10-31")

#Tres<-subset(Tres,EstadoCuentaTarjeta=="ACTIVA")  #3365008
Auxiliar<-Tres %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizado3= sum(CupoUtilizado)/3)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Tres %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoAvance3= sum(CupoUtilizadoAvance)/3)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Tres %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoNormal3= sum(CupoUtilizadoNormal)/3)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))


Auxiliar<-Tres %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.CupoUtilizadoSuperAvance3= sum(CupoUtilizadoSuperAvance)/3)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))

Auxiliar<-Tres %>% 
  group_by(IdCuentaTarjeta) %>% 
  summarise(Prom.DiasMora3= sum(DiasMora)/3)

Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta"="IdCuentaTarjeta"))




###########################Creacion de variable de porcentajes#################

Base["Cupo.Utilizado/Cupo.Aprobado"]<-Base$CupoUtilizado/Base$CupoAprobado
Base["Cupo.Ut.Normal/Cupo.Apro.Normal"]<-Base$CupoUtilizadoNormal/Base$CupoAprobadoNormal
Base["Cupo.Ut.Avances/Cupo.Aprobado"]<-Base$CupoUtilizadoAvance/Base$CupoAprobado
Base["Cupo.Ut.SuperAvances/Cupo.Apro.SuperAvance"]<-Base$CupoUtilizadoSuperAvance/Base$CupoAprobadoSuperAvance
Base["CupoUtilizado/CupoUtilizadoNormal"]<-Base$CupoUtilizado/Base$CupoUtilizadoNormal
Base["CupoUtilizado/Cupo.Apro.Normal"]<-Base$CupoUtilizado/Base$CupoAprobadoNormal
Base["Prom.CupoUtilizado/Cupo.Aprobado"]<-Base$Prom.CupoUtilizado/Base$CupoAprobado

#########################################################################


Base<-subset(Base,!duplicated(IdCuentaTarjeta))







################Cruzar con la variable objetivo#################################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Desertores<-read.table("Desertores.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)

Desertores<-read.table("Desertores.csv",header=TRUE,sep=",",na.strings = "NA", colClasses =c(
  "integer",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer"))

Desertores<-subset(Desertores,Reestructura==0 & EstadoTarjeta=="ACTIVA")
Desertores %>% count(Mesdesercion)
Desertores["Objetivo"]<-1

Auxiliar<-subset(Desertores,select=c("Identificacion","IdCuentaTarjeta","Mesdesercion","Objetivo"))
Base$Identificacion<-as.factor(Base$Identificacion)
Base<-Base %>% left_join(Auxiliar, by = c("IdCuentaTarjeta" = "IdCuentaTarjeta", "Identificacion"="Identificacion"))  #8250 desertorees de 8370 xq 120 abrieron en 2016

Base[which(Base$Mesdesercion>4),]$Objetivo<-0
#Base[which(Base$Mesdesercion<5),]$Objetivo<-0
Base[which(is.na(Base$Objetivo)),]$Objetivo<-0
Base$Objetivo<-as.factor(Base$Objetivo)
Base$Identificacion<-as.factor(Base$Identificacion)
Base$FechaCorte<-as.Date("2015-12-31")
  
table(Desertores$Mesdesercion)
table(Base$Mesdesercion)
table(Base$Objetivo)

SaldosDiciembre<-Base
save(SaldosDiciembre,file = "SaldosDiciembre.Rdata")
#################################################################################
##################Filtro: Propietario de la cartera: Solo Solidario  y Estado tarjeta activo
B<-subset(Base,EstadoTarjeta=="ACTIVA")
B<-subset(B, PropietariaCartera=="BANCO SOLIDARIO")
nrow(B[duplicated(B$Identificacion.x),])

#######################Balanceo de la Base##########################
#######################Buenos
muestra<-sample_n(subset(B,Objetivo==0),5606,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("Identificacion","filtro"))
Buenos<-subset(B,Objetivo==0) %>%left_join(muestra,by=c("Identificacion" = "Identificacion"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-96]
########################Malos
n<-23
Malos<-do.call("rbind", replicate(n, subset(B,Objetivo==1), simplify = FALSE))

Balanceada<-rbind(Malos,Buenos)
Balanceada %>% count(Objetivo)
###################################################################################




##########################Ananlisis Extadistico####################################
##############################Densidad################################################
range(Balanceada$CupoUtilizado,na.rm = TRUE)
plot(density(subset(Balanceada,Objetivo==1 & !is.na(CupoUtilizado))$CupoUtilizado),xlim=c(0,2000),lty=2,col="blue", xlab='', ylab='Densidad',
     main = "Cupo Utilizado")
lines(density(subset(Balanceada,Objetivo==0 & !is.na(CupoUtilizado))$CupoUtilizado),xlim=c(0,2000),lty=2,col="chocolate3")
legend("topright", legend = c("Desertores","No desertores"), lty = c(2,2),col=c("blue","chocolate3")) # optional legend

##############################Distribución######################################

require(graphics)

Fm<-ecdf(subset(B,Objetivo==1)$CupoUtilizado)
Fb<-ecdf(subset(B,Objetivo==0)$CupoUtilizado)

plot(Fm,lty=2,col="blue", xlab='', ylab='Distribución',xlim=c(0,2000),
     main = "Cupo Aprobado")
lines(Fb,lty=2,col="chocolate3")
legend("bottomright", legend = c("Desertores","No desertores"), lty = c(2,2),col=c("blue","chocolate3")) # optional legend

m<-array(c(0,0),dim = c(max(B$CupoUtilizado)+1,2))
for (i in 0:max(B$CupoUtilizado)) {
  m[i,1]=i
  m[i,2]=Fm(i)-Fb(i)
  }
plot(m,xlab = "CupoUtilizado",ylab = "Diferencia")
max(m[,2])
which.max(m[,2])
plot(m)
##############################Arbol####################################
B$Prom.DiasMora


Balanceada$Prom
fit <- rpart(as.factor(Objetivo) ~Prom.DiasMora,
             data=Balanceada)
#rpart.plot(fit)
rpart.plot(fit,box.palette="Blues", branch.lty=6,extra=104,type = 4,
            cex=0.75,
           tweak=1)

##############################Intervalos######################################################
range(subset(B,!is.infinite(T6.CupoAprobadoNormal) & !is.na(T6.CupoAprobadoNormal))$T6.CupoAprobadoNormal)
#range(as.numeric(B$CupoUtilizadoNormal),na.rm = TRUE)
##Malos

range(subset(B,!is.infinite(`Cupo.Ut.Avances/Cupo.Aprobado`) & !is.na(`Cupo.Ut.Avances/Cupo.Aprobado`))$`Cupo.Ut.Avances/Cupo.Aprobado`)

I<-c(-0.0001,0.013,0.4,1.1)
#Buenos
duration.cut = cut(as.numeric(subset(B,Objetivo==0)$`Cupo.Ut.Avances/Cupo.Aprobado`), I, right=TRUE) 
View(table(duration.cut) )
summary(subset(B,Objetivo==0)$`Cupo.Ut.Avances/Cupo.Aprobado`)
Aux<-subset(B,Objetivo==0 )
nrow(subset(Aux,T6.CupoAprobadoNormal=="Inf"))

#Malos

duration.cut = cut(as.numeric(subset(B,Objetivo==1)$`Cupo.Ut.Avances/Cupo.Aprobado`), I, right=TRUE) 
View(table(duration.cut) )
summary(subset(B,Objetivo==1)$`Cupo.Ut.Avances/Cupo.Aprobado`)
Aux<-subset(B,Objetivo==1)
nrow(subset(Aux,`Cupo.Ut.Avances/Cupo.Aprobado`==Inf))


Tabla<-array(c(93505,
               80058,
               4654,
               14517,
               5070,
               2179,
               444,
               443),dim=c(4,2))

summary(subset(B,Objetivo==0)$`Cupo.Ut.Avances/Cupo.Aprobado`)
head(table(subset(B,Objetivo==0)$T12.CupoUtilizado))
View(subset(BaseClientes20151231,!is.na(CupoAprobadoSuperAvance)))

######################Pruebas de hipotesis
########################### H0: u=v   H1: u>v(v>u)
##One-sample test
library("signmedian.test", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
x<-c(-5,-3,-2,1,5,6,3,9,10,15,20,21)
signmedian.test(x,alternative = "greater",exact=TRUE)
signmedian.test(x,mu=3,alternative="two.sided",exact=FALSE)
##Two-sample test(paired data)
x<-c(1,2,6,3,2)
y<-c(2,2,5,9,10)
x<-y-x
signmedian.test(x,alternative = "greater",exact=TRUE)



###############Mood's median test

median.test <- function(x, y){
  z <- c(x, y)
  g <- rep(1:2, c(length(x), length(y)))
  m <- median(z)
  fisher.test(z < m, g)$p.value
  result <- list(p.value=fisher.test(z > m, g)$p.value, Int.conf=fisher.test(z > m, g)$conf.int)
  return(result)
}

median.test(x, y)

###########################Estadístico chi^{2}##################### (poder predictivo)

prueba.chi2 <- function(x){
  z<-as.data.frame(x)
  z["Total"]<-as.data.frame(apply(z, 1, sum))
  z<-rbind(z, t(as.data.frame(apply(z, 2, sum))))
  dz<-dim(z)
  y<-t(t(as.vector(z[dim(z)[1],]))%*%(as.vector(z[,dim(z)[2]]))/z[dim(z)[1],dim(z)[2]])
  w<-(z-y)^2/y
  w<-w[-dz[1],]
  w<-w[,-dz[2]]
  w<-rbind(w, t(as.data.frame(apply(w, 2, sum))))
  row.names(w) <- NULL
  v<-w[-dim(w)[1],]
  contribucion<-as.data.frame(v[,1]+v[,2])
  result <- list(w=w,Contribucion=(contribucion)
                 #p.value=chisq.test(x,correct=FALSE)$p.value,
                 #statistic=chisq.test(x,correct=FALSE)$statistic
                 )
  return(result)
}
#####################Prueba Chi^2 de heterogeneidad#####################
View(prueba.D(Tabla)$Contribucion)
chisq.test(Tabla,correct=FALSE)


########################Estadistico  F##################################
##################la entrada tabla de contingencia con la primera columna con clientes buenos
prueba.F <- function(x){
  z<-as.data.frame(x)
  z<-t(t(z)/colSums(z))
  WoE<-log(z[,1]/z[,2])
  F=(z[,1]-z[,2])%*%WoE
  Dif<-z[,1]-z[,2]
  V=t(Dif)*WoE
  result <- list(F=F, Contribucion=t(V),WoE=WoE,z=z)
  return(result)
}
Tabla<-array(c(5000,10000,20000,2000,2000,2000),dim = c(3,2))

Tabla<-array(c(100,1950,950,6000,60,540,100,300),dim = c(4,2))
prueba.F(Tabla)

########################Estadistico  D de Gini##################################
##################la entrada tabla de contingencia con la primera columna con clientes buenos
prueba.D <- function(x){
  z<-as.data.frame(x)
  z<-t(t(z)/colSums(z))
  z2<-apply(z, 2, cumsum)
  z2<-apply(z2, 2, cumsum)
  z<-cbind(apply(z2, 2, cumsum)[,1],t(t(z)/colSums(z))[,2])
  Contribucion=t(z2[,1]*t(z[,2]))
  D=1-sum(Contribucion)
  result <- list(D=D, Contribucion=Contribucion)
  return(result)
}
Tabla<-array(c(6000,950,1600,350,90,10,300,100,400,140,50,10),dim = c(6,2))
Tabla<-array(c(5000,45000,200000,2000,2000,2000),dim = c(3,2))
Tabla<-array(c(99070,93664,6583,1553),dim = c(2,2))
Tabla<-
prueba.D(Tabla)
##################################Tabla de contingencia##############################
#format(12345.6789, decimal.mark=".",big.mark=",", nsmall=2)

range(subset(B,!is.infinite(CupoAprobado) & !is.na(CupoAprobado))$CupoAprobado)
#range(as.numeric(B$CupoUtilizadoNormal),na.rm = TRUE)
##Malos

format(as.data.frame(table(duration.cut)), decimal.mark=".",big.mark=",", nsmall=2)
format(as.data.frame(prueba.D(Tabla)$Contribucion),digits = 9, decimal.mark=".",big.mark=",", nsmall=2)

summary(subset(B,Objetivo==1)$CupoAprobadoNormal)
Aux<-subset(B,Objetivo==1)
nrow(subset(Aux,CupoAprobadoNormal==Inf))


va<-"CupoUtilizado"
I<-c(-0.0001,486,12241)
tabla.contingencia <- function(va,I,B){
duration.cut = cut(as.numeric(B[[va]]), I, right=TRUE) 
tt<-as.data.frame(table(duration.cut))
duration.cut = cut(as.numeric(subset(B,Objetivo==0)[[va]]), I, right=TRUE) 
tb<-as.data.frame(table(duration.cut))
duration.cut = cut(as.numeric(subset(B,Objetivo==1)[[va]]), I, right=TRUE) 
tm<-as.data.frame(table(duration.cut))
x2<-prueba.chi2(cbind(tb[,2],tm[,2]))$Contribucion
F<-prueba.F(cbind(tb[,2],tm[,2]))$Contribucion
D<-prueba.D(cbind(tb[,2],tm[,2]))$Contribucion
T<-cbind(tt[,-1],tb[,2],tm[,2],tb[,2]/tm[,2],x2,F,D)
T2<-rbind(T,T[1,]+T[2,])
colnames(T2)<-c("Total","No desertores","Desertores","Odds","X2","F","D")
rownames(T2)<-c(as.character(tt[1,1]),as.character(tt[2,1]),"Total")
T2[dim(T2)[1],4]<-T2[dim(T2)[1],2]/T2[dim(T2)[1],3]
T2<-round(as.data.frame(T2),digits = 2)
T3<-format(T2, decimal.mark=".",big.mark=",")
result <- list(Tabla=T2, imp=T3)
return(result)
}
t<-tabla.contingencia(va,I,B)
print(xtable(t$imp),  include.colnames = TRUE)



tabla.contingencia2 <- function(va,I,B){
  duration.cut = cut(as.numeric(B[[va]]), I, right=TRUE) 
  tt<-as.data.frame(table(duration.cut))
  duration.cut = cut(as.numeric(subset(B,Objetivo==0)[[va]]), I, right=TRUE) 
  tb<-as.data.frame(table(duration.cut))
  duration.cut = cut(as.numeric(subset(B,Objetivo==1)[[va]]), I, right=TRUE) 
  tm<-as.data.frame(table(duration.cut))
  x2<-prueba.chi2(cbind(tb[,2],tm[,2]))$Contribucion
  F<-prueba.F(cbind(tb[,2],tm[,2]))$Contribucion
  D<-prueba.D(cbind(tb[,2],tm[,2]))$Contribucion
  T<-cbind(tt[,-1],tt[,-1]*100/sum(tt[,-1]),tb[,2],tb[,2]*100/sum(tb[,2]),tm[,2],tm[,2]*100/sum(tm[,2]),tb[,2]/tm[,2],x2,F,D)
  T2<-rbind(T,T[1,]+T[2,])
  colnames(T2)<-c("Tarjetas","%","Tarjetas","%" ,"Tarjetas","%","Odds","X2","F","D")
  rownames(T2)<-c(as.character(tt[1,1]),as.character(tt[2,1]),"Total")
  T2[dim(T2)[1],7]<-T2[dim(T2)[1],3]/T2[dim(T2)[1],5]
  T2[dim(T2)[1],dim(T2)[2]]<-prueba.D(cbind(tb[,2],tm[,2]))$D
  T2<-round(as.data.frame(T2),digits = 2)
  T3<-format(T2, decimal.mark=".",big.mark=",")
  T2<-T2[-dim(T2)[1],c(4,6)]*0.01
  #T2<-round(cbind(T2[2],T2[1]),digits = 3)
  #colnames("Desertores", "No")
  result <- list(Tabla=T2, imp=T3)
  return(result)
}

B$CupoUtilizado
va<-"CupoUtilizado"
range(subset(B,!is.infinite(Prom.DiasMora) & !is.na(Prom.DiasMora))$Prom.DiasMora)

I<-c(-0.1,672,12372)
t2<-tabla.contingencia2(va,I,B)
print(xtable(t2$imp),  include.colnames = TRUE)
View(t2$Tabla)
chisq.test(cbind(t2$Tabla[-3,c(3,5)]),correct=FALSE)


##########################Tablas de estadisticos Manual###################################3
B$T3.CupoAprobadoNormal
va<-"T3.CupoAprobadoNormal"
range(subset(B,!is.infinite(T3.CupoAprobadoNormal) & !is.na(T3.CupoAprobadoNormal))$T3.CupoAprobadoNormal)


I<-c(-1.1,-0.63,32300)
#Buenos
M<-subset(B,Objetivo==0)
duration.cut = cut(as.numeric(M[[va]]), I, right=FALSE) 
View(table(duration.cut) )
summary(M[[va]])
Aux<-subset(B,Objetivo==0 )
nrow(subset(Aux,T3.CupoAprobadoNormal=="Inf"))

#Malos
M<-subset(B,Objetivo==1)
duration.cut = cut(as.numeric(M[[va]]), I, right=FALSE) 
View(table(duration.cut) )
summary(M[[va]])
Aux<-subset(B,Objetivo==1)
nrow(subset(Aux,T3.CupoAprobadoNormal==Inf))

Tabla<-array(c(
  134353,
  58381,
  4781,
  3355
  ),dim = c(2,2))
View(prueba.chi2(Tabla)$Contribucion)
View(prueba.F(Tabla)$Contribucion)
View(prueba.D(Tabla)$Contribucion)
#######################Categorizacion de Variables ########################
Codificada<-subset(B,select = c("Identificacion",
                                "T6.CupoUtilizado",
                                "T3.CupoUtilizadoNormal",
                                "T3.CupoAprobado",
                                "T3.CupoAprobadoNormal",
                                "CupoUtilizadoNormal",
                                "CupoUtilizado",
                                "T612.CupoUtilizado",
                                "CupoAprobado",
                                "CupoAprobadoNormal",
                                "Prom.CupoUtilizado3",
                                "Prom.CupoUtilizado",
                                "Cupo.Ut.Avances/Cupo.Aprobado",
                                "Prom.DiasMora",
                                "Cupo.Utilizado/Cupo.Aprobado",
                                "Objetivo"
                                ))


Codificada$C.T6.CupoUtilizado<- cut(Codificada$T6.CupoUtilizado, 
                              breaks = c(-1.1,-0.4,1122), right = FALSE, labels = c("a","b"))
Codificada$C.T6.CupoUtilizado<-as.character(Codificada$C.T6.CupoUtilizado)
Codificada$C.T6.CupoUtilizado[is.infinite(Codificada$T6.CupoUtilizado) 
                              |is.na(Codificada$T6.CupoUtilizado)]<-"c"
    


Codificada$C.T3.CupoUtilizadoNormal<- cut(Codificada$T3.CupoUtilizadoNormal, 
                                    breaks = c(-1.1,-0.24,988), right = FALSE, labels = c("a","b"))
Codificada$C.T3.CupoUtilizadoNormal<-as.character(Codificada$C.T3.CupoUtilizadoNormal)
Codificada$C.T3.CupoUtilizadoNormal[is.infinite(Codificada$T3.CupoUtilizadoNormal) 
                              |is.na(Codificada$T3.CupoUtilizadoNormal)]<-"c"



Codificada$C.T3.CupoAprobado<- cut(Codificada$T3.CupoAprobado, 
                                          breaks = c(-1.1,-0.63,0.0014,324), right = FALSE, labels = c("a","b","c"))
Codificada$C.T3.CupoAprobado<-as.character(Codificada$C.T3.CupoAprobado)
Codificada$C.T3.CupoAprobado[is.infinite(Codificada$T3.CupoAprobado) 
                                    |is.na(Codificada$T3.CupoAprobado)]<-"c"


Codificada$C.T3.CupoAprobadoNormal<- cut(Codificada$T3.CupoAprobadoNormal, 
                                   breaks = c(-1.1,-0.63,324), right = FALSE, labels = c("a","b"))
Codificada$C.T3.CupoAprobadoNormal<-as.character(Codificada$C.T3.CupoAprobadoNormal)
Codificada$C.T3.CupoAprobadoNormal[is.infinite(Codificada$T3.CupoAprobadoNormal) 
                             |is.na(Codificada$T3.CupoAprobadoNormal)]<-"b"


Codificada$C.T3.CupoAprobadoNormal<- cut(Codificada$T3.CupoAprobadoNormal, 
                                         breaks = c(-1.1,-0.63,324), right = FALSE, labels = c("a","b"))
Codificada$C.T3.CupoAprobadoNormal<-as.character(Codificada$C.T3.CupoAprobadoNormal)
Codificada$C.T3.CupoAprobadoNormal[is.infinite(Codificada$T3.CupoAprobadoNormal) 
                                   |is.na(Codificada$T3.CupoAprobadoNormal)]<-"b"



Codificada$C.CupoUtilizado<- cut(Codificada$CupoUtilizado, 
                                         breaks = c(-1.1,672,12372), right = FALSE, labels = c("a","b"))

Codificada$C.CupoUtilizadoNormal<- cut(Codificada$CupoUtilizadoNormal, 
                                       breaks = c(-1.1,470,12218), right = FALSE, labels = c("a","b"))



Codificada$C.T612.CupoUtilizado<- cut(Codificada$T612.CupoUtilizado, 
                                         breaks = c(-1.1,-0.27,2602), right = FALSE, labels = c("a","b"))
Codificada$C.T612.CupoUtilizado<-as.character(Codificada$C.T612.CupoUtilizado)
Codificada$C.T612.CupoUtilizado[is.infinite(Codificada$T612.CupoUtilizado) 
                                   |is.na(Codificada$T612.CupoUtilizado)]<-"c"



Codificada$C.CupoAprobado<- cut(Codificada$CupoAprobado, 
                                      breaks = c(-1.1,486,13108), right = FALSE, labels = c("a","b"))



Codificada$C.CupoAprobadoNormal<- cut(Codificada$CupoAprobadoNormal, 
                                      breaks = c(-1.1,469,12241), right = FALSE, labels = c("a","b"))

Codificada$C.Prom.CupoUtilizado<- cut(Codificada$Prom.CupoUtilizado, 
                                      breaks = c(-1.1,867,12178), right = FALSE, labels = c("a","b"))
Codificada$C.Prom.CupoUtilizado3<- cut(Codificada$Prom.CupoUtilizado3, 
                                      breaks = c(-1.1,661,12425), right = FALSE, labels = c("a","b"))


Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado<- cut(Codificada$`Cupo.Ut.Avances/Cupo.Aprobado`, 
                                      breaks = c(-1.1,0.013,0.40,100), right = TRUE, labels = c("a","b","c"))
Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado<-as.character(Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado)
Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado[is.infinite(Codificada$`Cupo.Ut.Avances/Cupo.Aprobado`) 
                                |is.na(Codificada$`Cupo.Ut.Avances/Cupo.Aprobado`)]<-"c"


Codificada$C.Prom.DiasMora<- cut(Codificada$Prom.DiasMora, 
                                       breaks = c(-0.1,0.88,454), right = FALSE, labels = c("a","b"))


Codificada$C.Cupo.Utilizado.Cupo.Aprobado<- cut(Codificada$`Cupo.Utilizado/Cupo.Aprobado`, 
                                                 breaks = c(-1.1,0.23,1), right = TRUE, labels = c("a","b"))
Codificada$C.Cupo.Utilizado.Cupo.Aprobado<-as.character(Codificada$C.Cupo.Utilizado.Cupo.Aprobado)
Codificada$C.Cupo.Utilizado.Cupo.Aprobado[is.infinite(Codificada$`Cupo.Utilizado/Cupo.Aprobado`) 
                                           |is.na(Codificada$`Cupo.Utilizado/Cupo.Aprobado`)]<-"b"


######################Matriz de correlaciones############################


mydata <- Codificada[, c(2:15)]
mydata<-subset(mydata,!T612.CupoUtilizado==Inf & !T6.CupoUtilizado==Inf &!T3.CupoUtilizadoNormal==Inf & !T3.CupoAprobadoNormal==Inf & !"Cupo.Ut.Avances/Cupo.Aprobado"==Inf  & !"Cupo.Utilizado/Cupo.Aprobado"==Inf)
mydata<-na.omit(mydata)

cormat <- round(cor(mydata),2)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}




reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}



# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)




ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())+
  ggtitle("Matriz de correlaciones")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#################################Crear a factor
Codificada$Objetivo<-as.factor(Codificada$Objetivo)
Codificada$C.T6.CupoUtilizado<-as.factor(Codificada$C.T6.CupoUtilizado)
Codificada$C.T3.CupoUtilizadoNormal<-as.factor(Codificada$C.T3.CupoUtilizadoNormal)
Codificada$C.T3.CupoAprobado<-as.factor(Codificada$C.T3.CupoAprobado)
Codificada$C.T3.CupoAprobadoNormal<-as.factor(Codificada$C.T3.CupoAprobadoNormal)
Codificada$C.T612.CupoUtilizado<-as.factor(Codificada$C.T612.CupoUtilizado)
Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado<-as.factor(Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado)
Codificada$C.Cupo.Utilizado.Cupo.Aprobado<-as.factor(Codificada$C.Cupo.Utilizado.Cupo.Aprobado)


###########################################
Codificada$C.T6.CupoUtilizado<- cut(Codificada$T6.CupoUtilizado, 
                                    breaks = c(-1.1,-0.4,1122), right = FALSE, labels = c("-100%,-40%","-40%,112200%"))
Codificada$C.T6.CupoUtilizado<-as.character(Codificada$C.T6.CupoUtilizado)
Codificada$C.T6.CupoUtilizado[is.infinite(Codificada$T6.CupoUtilizado) 
                              |is.na(Codificada$T6.CupoUtilizado)]<-"Sin.Cup.uti"



Codificada$C.T3.CupoUtilizadoNormal<- cut(Codificada$T3.CupoUtilizadoNormal, 
                                          breaks = c(-1.1,-0.24,988), right = FALSE, labels = c("-100%,-24%","-24%,98800%"))
Codificada$C.T3.CupoUtilizadoNormal<-as.character(Codificada$C.T3.CupoUtilizadoNormal)
Codificada$C.T3.CupoUtilizadoNormal[is.infinite(Codificada$T3.CupoUtilizadoNormal) 
                                    |is.na(Codificada$T3.CupoUtilizadoNormal)]<-"Sin.Cupo.Uti"



Codificada$C.T3.CupoAprobado<- cut(Codificada$T3.CupoAprobado, 
                                   breaks = c(-1.1,-0.63,0.0014,324), right = FALSE, labels = c("-100%,-63%","-63%,0.14%","0.14%,32400%.y.Sin.Cupo.Apro"))
Codificada$C.T3.CupoAprobado<-as.character(Codificada$C.T3.CupoAprobado)
Codificada$C.T3.CupoAprobado[is.infinite(Codificada$T3.CupoAprobado) 
                             |is.na(Codificada$T3.CupoAprobado)]<-"0.14%,32400%.y.Sin.Cupo.Apro"


Codificada$C.T3.CupoAprobadoNormal<- cut(Codificada$T3.CupoAprobadoNormal, 
                                         breaks = c(-1.1,-0.63,324), right = FALSE, labels = c("-100%,-63%","-63%,32400%.y.sin.cupo"))
Codificada$C.T3.CupoAprobadoNormal<-as.character(Codificada$C.T3.CupoAprobadoNormal)
Codificada$C.T3.CupoAprobadoNormal[is.infinite(Codificada$T3.CupoAprobadoNormal) 
                                   |is.na(Codificada$T3.CupoAprobadoNormal)]<-"-63%,32400%.y.sin.cupo"


Codificada$C.T3.CupoAprobadoNormal<- cut(Codificada$T3.CupoAprobadoNormal, 
                                         breaks = c(-1.1,-0.63,324), right = FALSE, labels = c("-100%,-63%","-63%,32400%.y.sin.cupo"))
Codificada$C.T3.CupoAprobadoNormal<-as.character(Codificada$C.T3.CupoAprobadoNormal)
Codificada$C.T3.CupoAprobadoNormal[is.infinite(Codificada$T3.CupoAprobadoNormal) 
                                   |is.na(Codificada$T3.CupoAprobadoNormal)]<-"-63%,32400%.y.sin.cupo"



Codificada$C.CupoUtilizado<- cut(Codificada$CupoUtilizado, 
                                 breaks = c(-1.1,672,12372), right = FALSE, labels = c("$0,$672","$672,$12,372"))

Codificada$C.CupoUtilizadoNormal<- cut(Codificada$CupoUtilizadoNormal, 
                                       breaks = c(-1.1,470,12218), right = FALSE, labels = c("$0,$470","$470,$12,218"))



Codificada$C.T612.CupoUtilizado<- cut(Codificada$T612.CupoUtilizado, 
                                      breaks = c(-1.1,-0.27,2602), right = FALSE, labels = c("-100%,-27%","-27%,2602%"))
Codificada$C.T612.CupoUtilizado<-as.character(Codificada$C.T612.CupoUtilizado)
Codificada$C.T612.CupoUtilizado[is.infinite(Codificada$T612.CupoUtilizado) 
                                |is.na(Codificada$T612.CupoUtilizado)]<-"sin.cupo.uti"



Codificada$C.CupoAprobado<- cut(Codificada$CupoAprobado, 
                                breaks = c(-1.1,486,13108), right = FALSE, labels = c("$0,$486,","$486,$13,108"))



Codificada$C.CupoAprobadoNormal<- cut(Codificada$CupoAprobadoNormal, 
                                      breaks = c(-1.1,469,12241), right = FALSE, labels = c("$0,$469","$469,$12241"))

Codificada$C.Prom.CupoUtilizado<- cut(Codificada$Prom.CupoUtilizado, 
                                      breaks = c(-1.1,867,12178), right = FALSE, labels = c("$0,$867","$867,$12,178"))
Codificada$C.Prom.CupoUtilizado3<- cut(Codificada$Prom.CupoUtilizado3, 
                                       breaks = c(-1.1,661,12425), right = FALSE, labels = c("$0,$661","$661,$12425"))


Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado<- cut(Codificada$`Cupo.Ut.Avances/Cupo.Aprobado`, 
                                                 breaks = c(-1.1,0.013,0.40,100), right = TRUE, labels = c("0%,1.3%","1.3%,40%","40%,100%.y.sin.cupo.apro.uti"))
Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado<-as.character(Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado)
Codificada$C.Cupo.Ut.Avances.Cupo.Aprobado[is.infinite(Codificada$`Cupo.Ut.Avances/Cupo.Aprobado`) 
                                           |is.na(Codificada$`Cupo.Ut.Avances/Cupo.Aprobado`)]<-"40%,100%.y.sin.cupo.apro.uti"


Codificada$C.Prom.DiasMora<- cut(Codificada$Prom.DiasMora, 
                                 breaks = c(-0.1,0.88,454), right = FALSE, labels = c("0,0.88","0.88,454"))


Codificada$C.Cupo.Utilizado.Cupo.Aprobado<- cut(Codificada$`Cupo.Utilizado/Cupo.Aprobado`, 
                                                breaks = c(-1.1,0.23,1), right = TRUE, labels = c("0%,0.23%","23%,100%.y.sin.cupo.apro.uti"))
Codificada$C.Cupo.Utilizado.Cupo.Aprobado<-as.character(Codificada$C.Cupo.Utilizado.Cupo.Aprobado)
Codificada$C.Cupo.Utilizado.Cupo.Aprobado[is.infinite(Codificada$`Cupo.Utilizado/Cupo.Aprobado`) 
                                          |is.na(Codificada$`Cupo.Utilizado/Cupo.Aprobado`)]<-"23%,100%.y.sin.cupo.apro.uti"
###################################







#######################Balanceo de la Base  regreción logistica##########################
#######################Buenos
muestra<-sample_n(subset(Codificada,Objetivo==0),5606,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("Identificacion","filtro"))
Buenos<-subset(Codificada,Objetivo==0) %>%left_join(muestra,by=c("Identificacion" = "Identificacion"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-31]
########################Malos
n<-23
Malos<-do.call("rbind", replicate(n, subset(Codificada,Objetivo==1), simplify = FALSE))

Balanceada<-rbind(Malos,Buenos)
Balanceada %>% count(Objetivo)








#######################Regresión logistica #########
Train <- createDataPartition(Balanceada$Objetivo, p=0.7, list=FALSE)
training <- Balanceada[ Train, ]
testing <- Balanceada[ -Train, ]

mod_fit_one <- glm(Objetivo ~ C.T6.CupoUtilizado+ 
                     C.T3.CupoUtilizadoNormal +
                     C.T3.CupoAprobado +
                     #C.T3.CupoAprobadoNormal +
                     C.CupoUtilizado+
                     C.CupoUtilizadoNormal + 
                     C.T612.CupoUtilizado+
                     C.CupoAprobado+
                     #C.CupoAprobadoNormal+
                     C.Prom.CupoUtilizado+
                     #C.Prom.CupoUtilizado3+
                     C.Cupo.Ut.Avances.Cupo.Aprobado+
                     C.Prom.DiasMora+
                     C.Cupo.Utilizado.Cupo.Aprobado
                    , data=training, family="binomial")

summary(mod_fit_one)
####################valores ajustados####################33
View(varImp(mod_fit_one))
varImpPlot(mod_fit_one,type=2)

fitted.results <- predict(mod_fit_one,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.52,1,0)

pr <- prediction(fitted.results, testing$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
########################
stargazer(m1, m2, m3, m4, type=
            "
          html
          ", 
          dep.var.labels
          =c("Miles/(US) 
             gallon","Fast
             car (=1)"), 
          covariate.labels
          =c("Gross 
             horsepower","Rear
             axle 
             ratio","Four
             foward
             gears",
             "Five forward 
             gears","Type
             of transmission (manual=1)"), out="
          models.htm
          ")

stargazer(mod_fit_one,type="html",out = "models.htm")
