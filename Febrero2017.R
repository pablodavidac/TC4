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

Base20160831<-read.table("Base 2016 08 31.csv",
                           header=TRUE,
                           sep=",",
                           na.strings = "NA")


#c<-(sapply(Base20160229II, function(x) class(x)[[1]]))
#write.csv(c,file="Names.csv")
Diciembre<-subset(Base20161231,CuentaTarjetaMonitor==1)

###################################################################################
Diciembre<-subset(Diciembre,select=c("Identificacion",
                                      "IdCuentaTarjeta",
                                      "Afinidad",
                                      "CupoUtilizado",
                                      "CupoUtilizadoAvance",
                                      "NumeroEntidadesSF",
                                      "Segmento_TDCNoviembre2016"))

Diciembre<-subset(Diciembre,!duplicated(IdCuentaTarjeta))

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

DireccionClienteNatural$UbicacionGeografica1<-as.integer(DireccionClienteNatural$UbicacionGeografica1)
Provincia<-left_join(DireccionClienteNatural,UbicacionGeografica,by="UbicacionGeografica1")
Demografica<-left_join(Demografica,Provincia,by="IdClienteNatural")
#############################Autorizaciones############################

odbcChannel <-odbcConnect("Tarjeta") #Databases
sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Autorizacion")

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-01-01' 
                               and FechaConciliacion > '2016-07-01'", fetch = TRUE)

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))



AutorizacionesDiciembre <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-01-01' 
                                    and FechaConciliacion > '2016-12-01'", fetch = TRUE)

SoloSeguro<-subset(AutorizacionesDiciembre,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesDiciembre,TipoConsumo=="TRJAVANCE" | 
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

##############################BAse Diciembre################################
Diciembre<-left_join(Diciembre,Demografica,by="Identificacion")
Diciembre<-subset(Diciembre,!duplicated(IdCuentaTarjeta))
Diciembre<-left_join(Diciembre,Autorizaciones,by="IdCuentaTarjeta")
summary(Diciembre$Provincia)


###################################Remplazar NA por ceros##################
Diciembre[which(is.na(Diciembre$Autorizaciones6M)),]$Autorizaciones6M<-0
Diciembre[which(is.na(Diciembre$ConsumosSeguro)),]$ConsumosSeguro<-0
Diciembre[which(is.na(Diciembre$Consumos)),]$Consumos<-0
Diciembre<-Diciembre[,-13]
Diciembre<-Diciembre[,-12]
###########################Cambiar codigos####################
Diciembre$EstadoCivil<-as.character(Diciembre$EstadoCivil)
Diciembre[which(Diciembre$EstadoCivil!="ESTCIVC" & 
                Diciembre$EstadoCivil!="ESTCIVD" &
                Diciembre$EstadoCivil!="ESTCIVS" &
                Diciembre$EstadoCivil!="ESTCIVU" &
                Diciembre$EstadoCivil!="ESTCIVV" ),]$EstadoCivil<-NA

Diciembre[which(Diciembre$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Diciembre[which(Diciembre$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorsiado"
Diciembre[which(Diciembre$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Diciembre[which(Diciembre$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Union Libre"
Diciembre[which(Diciembre$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"
##################Cupo Utilizado t-6################
Junio2016<-subset(Base20160630,select=c("IdCuentaTarjeta","CupoUtilizado"))
colnames(Junio2016)[2]<-"CupoUtilizadoT6"
Diciembre<-left_join(Diciembre,Junio2016,by="IdCuentaTarjeta")

Diciembre[which(is.na(Diciembre$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0




########################################################
Diciembre["SegmentoNuevo"]<-Diciembre$`Segmento_TDC Nuevo`
Diciembre<-subset(Diciembre,
                  SegmentoNuevo!="4. Movilización B" &
                    SegmentoNuevo!="5. Normalización A" &
                    SegmentoNuevo!="6. Normalización B" &
                    SegmentoNuevo!="7. Inactivos")