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


Base20160131<-read.table("Base20160131.csv",
                            header=TRUE,
                            sep=",",
                            na.strings = "NA")
Base20150731<-subset(Reporte25,FechaCorte=="2015-07-31")


load("d:/Users_info/ALBANPD/My Documents/Bases/Reporte25.RData")
#c<-(sapply(Base20160131II, function(x) class(x)[[1]]))
#write.csv(c,file="Names.csv")
Enero<-subset(Base20160131,CuentaTarjetaMonitor==1)

###################################################################################

Enero<-subset(Enero,select=c("Identificacion",
                                      "IdCuentaTarjeta",
                                      "CupoUtilizadoTotal",
                                      "CupoUtilizadoAvance",
                                      "NumeroEntidadesSF",
                             "Afinidad",
                                      "Segmento_TDC"))
Enero["CupoUtilizado"]<-Enero$CupoUtilizadoTotal
Enero$Identificacion<-as.character(Enero$Identificacion)
Enero[which(nchar(Enero$Identificacion)==9),]$Identificacion<-paste0("0",Enero[which(nchar(Enero$Identificacion)==9),]$Identificacion)

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

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2016-02-01' 
                               and FechaConciliacion > '2015-08-01'", fetch = TRUE)

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))



AutorizacionesEnero <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2016-02-01' 
                                    and FechaConciliacion > '2015-12-31'", fetch = TRUE)

SoloSeguro<-subset(AutorizacionesEnero,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesEnero,TipoConsumo=="TRJAVANCE" | 
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

##############################BAse Enero################################
Enero<-left_join(Enero,Demografica,by="Identificacion")
Enero<-subset(Enero,!duplicated(IdCuentaTarjeta))
Enero<-left_join(Enero,Autorizaciones,by="IdCuentaTarjeta")
###################################Remplazar NA por ceros##################
Enero[which(is.na(Enero$Autorizaciones6M)),]$Autorizaciones6M<-0
Enero[which(is.na(Enero$ConsumosSeguro)),]$ConsumosSeguro<-0
Enero[which(is.na(Enero$Consumos)),]$Consumos<-0
###########################Cambiar codigos####################
Enero$EstadoCivil<-as.character(Enero$EstadoCivil)
Enero[which(Enero$EstadoCivil!="ESTCIVC" & 
                Enero$EstadoCivil!="ESTCIVD" &
                Enero$EstadoCivil!="ESTCIVS" &
                Enero$EstadoCivil!="ESTCIVU" &
                Enero$EstadoCivil!="ESTCIVV" ),]$EstadoCivil<-NA

Enero[which(Enero$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Enero[which(Enero$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorsiado"
Enero[which(Enero$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Enero[which(Enero$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Union Libre"
Enero[which(Enero$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"
#############################################################################
Enero$ProvinciaDomicilio<-Enero$Provincia
Enero["Provincia"]<-"NA"
Enero[which(Enero$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Enero[which(Enero$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Enero[which(Enero$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Enero[which(Enero$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Enero[which(Enero$Provincia=="NA"),]$Provincia<-0

####################CupoUtilizadoT6##########################
Julio<-subset(Base20150731,select=c("Identificacion","CupoUtilizado"))
colnames(Julio)[2]<-"CupoUtilizadoT6"
Julio<-subset(Julio,CupoUtilizadoT6>0)

Enero<-left_join(Enero,Julio,by="Identificacion")
Enero<-subset(Enero,!duplicated("Identificacion"))
Enero[which(is.na(Enero$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0


######################Codificacion variables#######################
Enero["T6CupoUtilizado"]<-0
Enero$T6CupoUtilizado<-(Enero$CupoUtilizadoTotal-Enero$CupoUtilizadoT6)/Enero$CupoUtilizadoT6

NROW(Enero[which(is.infinite(Enero$T6CupoUtilizado)),])
NROW(Enero[which(is.na(Enero$T6CupoUtilizado)),])

Enero[which(is.infinite(Enero$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Enero[which(is.na(Enero$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
########################Cupo Utlizado Avances#############
Enero["CupoUtilizadoAvanceIguala0"]<-0
Enero[which(Enero$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1


########################Estado Civil############################
Enero["CodEstadoCivil"]<-1
Enero[which(Enero$EstadoCivil=="Casado"),]$CodEstadoCivil<-1
Enero[which(Enero$EstadoCivil=="Divorsiado"),]$CodEstadoCivil<-1


Enero[which(Enero$EstadoCivil=="Soltero"),]$CodEstadoCivil<-0
Enero[which(Enero$EstadoCivil=="Union Libre"),]$CodEstadoCivil<-0
Enero[which(Enero$EstadoCivil=="Viudo"),]$CodEstadoCivil<-0

Enero[which(Enero$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1

########################Afinidad#############################
Enero["AfinidadTarjeta"]<-"PLATINUM"
Enero$AfinidadTarjeta<-Enero$Afinidad
Enero$AfinidadTarjeta<-as.character(Enero$AfinidadTarjeta)
Enero$Afinidad<-as.character(Enero$Afinidad)

Enero[which(is.na(Enero$AfinidadTarjeta)),]$AfinidadTarjeta<-"PLATINUM"
#Enero["Afinidad"]<-"NA"
Enero[which(Enero$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-1
Enero[which(Enero$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Enero[which(Enero$AfinidadTarjeta=="GOLD     "),]$Afinidad<-1

Enero[which(Enero$AfinidadTarjeta=="BLACK"),]$Afinidad<-0
Enero[which(Enero$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-0
Enero[which(Enero$Afinidad=="NA"),]$Afinidad<-0
Enero[which(Enero$Afinidad==""),]$Afinidad<-0

########################Otra tarjeta################################
Enero["Otra_Tarjeta"]<-"SI"
Enero[which(Enero$NumeroEntidadesSF==0),]$Otra_Tarjeta<-"NO"
########################Solo Seguro#################################
Enero["SoloSeguro"]<-"NO"
Enero[which(Enero$Consumos==0 & Enero$ConsumosSeguro>0),]$SoloSeguro<-"SI"

#######################Autorizaciones 6 meses#############################
Enero["Menos8Autorizaciones6M"]<-0
Enero[which(Enero$Autorizaciones6M<8.5),]$Menos8Autorizaciones6M<-1

##########################T6 Cupo Utilizado##################################

Enero["CodT6CupoUtilizado"]<-1
Enero[which(Enero$T6CupoUtilizado>=(-0.31)),]$CodT6CupoUtilizado<-0

##############################Cupo Normalizado#############################
Enero<-subset(Enero,Segmento_TDC=="0. Nuevos +6M" |
                Segmento_TDC=="0. Nuevos 6M" |
                Segmento_TDC=="1. VIP" |
                Segmento_TDC=="2. Preferente" |
                Segmento_TDC=="3. Movilización A" |
                Segmento_TDC=="4. Movilización B" )


Enero["NormCupoUtilizado"]<-(Enero$CupoUtilizado-mean(Enero$CupoUtilizado))/sd(Enero$CupoUtilizado)

#Enero<-left_join(Enero,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
EneroCompleto<-Enero
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")


####################Deciles###########################
fitted.results <- predict(modA2,newdata=Enero,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Enero["Score"]<-predict(modA2,newdata=Enero,type='response')
Enero["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Enero[which(Enero$Score <= quan[2]),]$Decil<-"P1"
Enero[which(Enero$Score <=quan[3] & Enero$Score > quan[2]),]$Decil<-"P2"
Enero[which(Enero$Score <=quan[4] & Enero$Score > quan[3]),]$Decil<-"P3"
Enero[which(Enero$Score <=quan[5] & Enero$Score > quan[4]),]$Decil<-"P4"
Enero[which(Enero$Score <=quan[6] & Enero$Score > quan[5]),]$Decil<-"P5"
Enero[which(Enero$Score <=quan[7] & Enero$Score > quan[6]),]$Decil<-"P6"
Enero[which(Enero$Score <=quan[8] & Enero$Score > quan[7]),]$Decil<-"P7"
Enero[which(Enero$Score <=quan[9] & Enero$Score > quan[8]),]$Decil<-"P8"
Enero[which(Enero$Score <=quan[10] & Enero$Score > quan[9]),]$Decil<-"P9"
Enero[which(Enero$Score >=quan[10]),]$Decil<-"P10"
View(table(Enero$Decil))


Enero<-Enero[order(-Enero$Score),]
Aux<-Enero[1:21534,]

save(Enero,file = "Enero16.Rdata")
