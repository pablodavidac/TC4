library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("xtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("arules", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ggrepel", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("caret", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ROCR", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("stargazer", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("scales", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("gtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("randomForest", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("reshape2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("arm", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("tree", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("party", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plotROC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("DescTools", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("RODBCext", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")


setwd("d:/Users_info/ALBANPD/My Documents/Bases")

Base20160229II<-read.table("Base 2016 02 29II.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA",
colClasses =c(
"factor",
"integer",
"numeric",
"numeric",
"integer",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"integer",
"integer",
"factor",
"factor",
"factor",
"factor",
"integer",
"numeric",
"factor",
"factor",
"factor",
"numeric",
"factor",
"factor",
"factor",
"numeric",
"integer",
"numeric",
"numeric",
"integer",
"numeric",
"factor",
"integer",
"integer",
"integer",
"integer",
"integer",
"integer",
"factor",
"factor",
"numeric",
"numeric",
"integer",
"integer",
"integer",
"factor",
"factor",
"factor",
"integer",
"numeric"))


Base20160229<-read.table("Base 2016 02 29.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA",

colClasses =c(
  "factor",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "numeric",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "factor",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "integer",
  "integer",
  "integer",
  "factor",
  "logical",
  "logical",
  "factor",
  "factor",
  "integer",
  "factor",
  "integer"))


Base20150831<-read.table("Base 2015 08 31.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA")
#c<-(sapply(Base20160229II, function(x) class(x)[[1]]))
#write.csv(c,file="Names.csv")
Febrero<-subset(Base20160229,CuentaTarjetaMonitor==1 & ActivadaFebrero2016==0)

###################################################################################
FebreroI<-subset(Base20160229II,select=c("Identificacion",
                                         "IdCuentaTarjeta",
                                         "Afinidad"))

Febrero<-subset(Base20160229,select=c("Identificacion",
                                 "IdCuentaTarjeta",
                                 "CupoUtilizado",
                                 "CupoUtilizadoAvance",
                                 "NumeroEntidadesSF",
                                 "Segmento_TDC"))
Febrero<-left_join(Febrero,FebreroI,by=c("Identificacion",
                                         "IdCuentaTarjeta"))

##################################Demograficas##################################
################Informacion del cliente################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Cliente")

ClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM ClienteNatural WHERE IdInstitucion = 2", fetch = TRUE)

Cliente <- sqlExecute(odbcChannel, "SELECT * FROM Cliente", fetch = TRUE)

InformacionDemografica<- sqlExecute(odbcChannel, "SELECT * FROM InformacionDemografica", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)

auxClienteNatural<-subset(ClienteNatural,select = c("IdClienteNatural","IdCliente"))
auxCliente<-subset(Cliente,select = c("IdCliente","Identificacion","TipoIdentificacion"))
auxInformacionDemografica<-subset(InformacionDemografica,select = c("IdClienteNatural",
                                                                    "EstadoCivil"))



Demografica<-left_join(auxCliente,auxClienteNatural)
Demografica<-left_join(Demografica,auxInformacionDemografica)
rm(InformacionDemografica)

#####################Provincia Domicilio##########################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Cliente")

DireccionClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM DireccionClienteNatural", fetch = TRUE)

DireccionClienteNatural<-subset(DireccionClienteNatural,select=c("IdClienteNatural","UbicacionGeografica1"))

odbcClose(odbcChannel)



#################Codificacion de Provinca Domicilio#####################
odbcChannel <-odbcConnect("Configuracion") #Databases
sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "UbicacionGeografica1")

UbicacionGeografica <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica1", fetch = TRUE)
colnames(UbicacionGeografica)[1]<-"UbicacionGeografica1"
odbcClose(odbcChannel)
UbicacionGeografica<-subset(UbicacionGeografica,select=c(
  "UbicacionGeografica1",
  "Nombre"))
colnames(UbicacionGeografica)[2]<-"Provincia"


DireccionClienteNatural$UbicacionGeografica1<-as.character(DireccionClienteNatural$UbicacionGeografica1)
DireccionClienteNatural$UbicacionGeografica1<-as.integer(DireccionClienteNatural$UbicacionGeografica1)

Provincia<-left_join(DireccionClienteNatural,UbicacionGeografica,by="UbicacionGeografica1")
Demografica<-left_join(Demografica,Provincia,by="IdClienteNatural")
#############################Autorizaciones############################

odbcChannel <-odbcConnect("Tarjeta") #Databases
sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Autorizacion")

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2016-03-01' 
                                  and FechaConciliacion > '2015-09-01'", fetch = TRUE)

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))



AutorizacionesFebrero <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2016-03-01' 
                                  and FechaConciliacion > '2016-01-31'", fetch = TRUE)

SoloSeguro<-subset(AutorizacionesFebrero,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesFebrero,TipoConsumo=="TRJAVANCE" | 
                      TipoConsumo=="TRJCONSUMO" | 
                      TipoConsumo=="TRJDEBCON" |
                      TipoConsumo=="TRJSUPERAVCE" |
                      TipoConsumo=="TRJTAIRE"  )
aux3<-SoloConsumo %>% group_by(IdCuentaTarjeta) %>% mutate(Consumos = n())
aux3<-subset(aux3,select=c("IdCuentaTarjeta","Consumos"))
aux3<-subset(aux3,!duplicated(IdCuentaTarjeta))

Autorizaciones<-full_join(aux,aux2,by="IdCuentaTarjeta")
Autorizaciones<-full_join(Autorizaciones,aux3,by="IdCuentaTarjeta")
#########################################################################################

##############################BAse Febrero################################
Febrero<-left_join(Febrero,Demografica,by="Identificacion")
Febrero<-subset(Febrero,!duplicated(IdCuentaTarjeta))
Febrero<-left_join(Febrero,Autorizaciones,by="IdCuentaTarjeta")
summary(Febrero$Provincia)


######################Cargar Abril por Prvincia Domicilio#############
Abril<-read.table("Reporte-RIEG-INCO-00042A.txt",
                           header=TRUE,
                           sep="\t",
                           na.strings = "NA")
Abril<-subset(Abril,select=c("IdCuentaTarjeta","ProvinciaDomicilio","EstadoCivil"))
#####
Febrero<-Febrero[,-which(names(Febrero) %in% c("Provincia",
                                   #"FechaConciliacionUltimoConsumo",
                                   "EstadoCivil",
                                   "UbicacionGeografica1"))]
Febrero<-left_join(Febrero,Abril,by="IdCuentaTarjeta")
###################################Remplazar NA por ceros##################
Febrero[which(is.na(Febrero$Autorizaciones6M)),]$Autorizaciones6M<-0
Febrero[which(is.na(Febrero$ConsumosSeguro)),]$ConsumosSeguro<-0
Febrero[which(is.na(Febrero$Consumos)),]$Consumos<-0
Febrero<-Febrero[,-13]
Febrero<-Febrero[,-12]
###########################Cambiar codigos####################
Febrero$EstadoCivil<-as.character(Febrero$EstadoCivil)
Febrero[which(Febrero$EstadoCivil!="ESTCIVC" & 
                Febrero$EstadoCivil!="ESTCIVD" &
                Febrero$EstadoCivil!="ESTCIVS" &
                Febrero$EstadoCivil!="ESTCIVU" &
                Febrero$EstadoCivil!="ESTCIVV" ),]$EstadoCivil<-NA

Febrero[which(Febrero$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Febrero[which(Febrero$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorsiado"
Febrero[which(Febrero$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Febrero[which(Febrero$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Union Libre"
Febrero[which(Febrero$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"
##################Cupo Utilizado t-6################
Agosto15<-subset(Base20150831,select=c("IdCuentaTarjeta","CupoUtilizado"))
colnames(Agosto15)[2]<-"CupoUtilizadoT6"
Febrero<-left_join(Febrero,Agosto15,by="IdCuentaTarjeta")

Febrero[which(is.na(Febrero$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0


save(Febrero,file="FebreroII.Rdata")

