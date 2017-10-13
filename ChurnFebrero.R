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


####################Tarjetas activas###########################################################
odbcChannel <-odbcConnect("DWH_CRM") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "SB_TDCActivas")

TDCFebrero <- sqlExecute(odbcChannel, "SELECT * FROM SB_TDCActivas", fetch = TRUE)

odbcClose(odbcChannel)

Febrero<-subset(TDCFebrero,CuentaTarjetaMonitor==1)
Febrero$FechaApertura <- as.Date( as.character(Febrero$FechaApertura))
#Febrero<-subset(Febrero,FechaApertura > as.Date("2016-12-31"))


###################################################################################
Febrero<-subset(Febrero,select=c("Identificacion",
                                     "IdCuentaTarjeta",
                                     "Afinidad",
                                     "CupoUtilizado",
                                     "CupoUtilizadoAvance",
                                     "NumeroEntidadesSF",
                                     "Segmento_TDCEnero2017",
                             "FechaApertura"))

Febrero<-subset(Febrero,!duplicated(IdCuentaTarjeta))

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

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-03-01' 
                               and FechaConciliacion > '2016-09-01'", fetch = TRUE)




aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))



AutorizacionesFebrero <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-03-01' 
                                      and FechaConciliacion > '2017-02-01'", fetch = TRUE)

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

##############################Base Febrero################################
Febrero<-left_join(Febrero,Demografica,by="Identificacion")
Febrero<-subset(Febrero,!duplicated(IdCuentaTarjeta))
Febrero<-left_join(Febrero,Autorizaciones,by="IdCuentaTarjeta")
summary(Febrero$Provincia)

##############################################################################


###################################Remplazar NA por ceros##################
Febrero[which(is.na(Febrero$Autorizaciones6M)),]$Autorizaciones6M<-0
Febrero[which(is.na(Febrero$ConsumosSeguro)),]$ConsumosSeguro<-0
Febrero[which(is.na(Febrero$Consumos)),]$Consumos<-0
#Febrero<-Febrero[,-13]
#Febrero<-Febrero[,-12]
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
#########################T6 Cupo Utilizado########################################
Agosto<-read.table("Base 2016 08 31.csv",
                  header=TRUE,
                  sep=",",
                  na.strings = "NA")

Agosto<-subset(Agosto,select=c("IdCuentaTarjeta","CupoUtilizado"))
colnames(Agosto)[2]<-"CupoUtilizadoT6"
#Agosto[which(is.na(Agosto$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Febrero<-left_join(Febrero,Agosto,by="IdCuentaTarjeta")

Febrero$AfinidadTarjeta<-Febrero$Afinidad
######################Codificacion variables#######################
Febrero["T6CupoUtilizado"]<-0
Febrero$T6CupoUtilizado<-(Febrero$CupoUtilizado-Febrero$CupoUtilizadoT6)/Febrero$CupoUtilizadoT6

NROW(Febrero[which(is.infinite(Febrero$T6CupoUtilizado)),])
NROW(Febrero[which(is.na(Febrero$T6CupoUtilizado)),])

Febrero[which(is.infinite(Febrero$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Febrero[which(is.na(Febrero$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
########################Cupo Utlizado Avances#############
Febrero["CupoUtilizadoAvanceIguala0"]<-0
Febrero[which(Febrero$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1

########################Estado Civil############################
Febrero["CodEstadoCivil"]<-1
Febrero[which(Febrero$EstadoCivil=="Casado"),]$CodEstadoCivil<-1
Febrero[which(Febrero$EstadoCivil=="Divorsiado"),]$CodEstadoCivil<-1


Febrero[which(Febrero$EstadoCivil=="Soltero"),]$CodEstadoCivil<-0
Febrero[which(Febrero$EstadoCivil=="Union Libre"),]$CodEstadoCivil<-0
Febrero[which(Febrero$EstadoCivil=="Viudo"),]$CodEstadoCivil<-0

Febrero[which(Febrero$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1

########################Afinidad#############################

Febrero["Afinidad"]<-"NA"
Febrero[which(Febrero$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-1
Febrero[which(Febrero$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Febrero[which(Febrero$AfinidadTarjeta=="GOLD     "),]$Afinidad<-1

Febrero[which(Febrero$AfinidadTarjeta=="BLACK"),]$Afinidad<-0
Febrero[which(Febrero$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-0
Febrero[which(Febrero$Afinidad=="NA"),]$Afinidad<-0

########################Variable provincia#############################
Febrero["ProvinciaDomicilio"]<-Febrero$Provincia
Febrero["Provincia"]<-"NA"
Febrero[which(Febrero$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Febrero[which(Febrero$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Febrero[which(Febrero$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Febrero[which(Febrero$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Febrero[which(Febrero$Provincia=="NA"),]$Provincia<-0
########################Otra tarjeta################################
Febrero["Otra_Tarjeta"]<-"SI"
Febrero[which(Febrero$NumeroEntidadesSF==0),]$Otra_Tarjeta<-"NO"
########################Solo Seguro#################################
Febrero["SoloSeguro"]<-"NO"
Febrero[which(Febrero$Consumos==0 & Febrero$ConsumosSeguro>0),]$SoloSeguro<-"SI"

#######################Autorizaciones 6 meses#############################
Febrero["Menos8Autorizaciones6M"]<-0
Febrero[which(Febrero$Autorizaciones6M<8.5),]$Menos8Autorizaciones6M<-1

##########################T6 Cupo Utilizado##################################

Febrero["CodT6CupoUtilizado"]<-1
Febrero[which(Febrero$T6CupoUtilizado>=(-0.31)),]$CodT6CupoUtilizado<-0

##############################Cupo Normalizado#############################
Febrero["NormCupoUtilizado"]<-(Febrero$CupoUtilizado-mean(Febrero$CupoUtilizado))/sd(Febrero$CupoUtilizado)

#Febrero<-left_join(Febrero,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
FebreroCompleto<-Febrero
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")

Febrero<-subset(Febrero,Segmento_TDCEnero2017=="0. NUEVOS +6M" |
                Segmento_TDCEnero2017=="0. NUEVOS 6M" |
                  Segmento_TDCEnero2017=="1. VIP" |
                  Segmento_TDCEnero2017=="2. PREFERENTE" |
                  Segmento_TDCEnero2017=="3. MOVILIZACION A")
####################Deciles###########################
fitted.results <- predict(modA2,newdata=Febrero,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Febrero["Score"]<-predict(modA2,newdata=Febrero,type='response')
Febrero["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Febrero[which(Febrero$Score <= quan[2]),]$Decil<-"P1"
Febrero[which(Febrero$Score <=quan[3] & Febrero$Score > quan[2]),]$Decil<-"P2"
Febrero[which(Febrero$Score <=quan[4] & Febrero$Score > quan[3]),]$Decil<-"P3"
Febrero[which(Febrero$Score <=quan[5] & Febrero$Score > quan[4]),]$Decil<-"P4"
Febrero[which(Febrero$Score <=quan[6] & Febrero$Score > quan[5]),]$Decil<-"P5"
Febrero[which(Febrero$Score <=quan[7] & Febrero$Score > quan[6]),]$Decil<-"P6"
Febrero[which(Febrero$Score <=quan[8] & Febrero$Score > quan[7]),]$Decil<-"P7"
Febrero[which(Febrero$Score <=quan[9] & Febrero$Score > quan[8]),]$Decil<-"P8"
Febrero[which(Febrero$Score <=quan[10] & Febrero$Score > quan[9]),]$Decil<-"P9"
Febrero[which(Febrero$Score >=quan[10]),]$Decil<-"P10"



View(table(Febrero$Decil))
write.csv(Febrero,file="Febrero2017.csv")
########################Cruses de Grupos################################
Febrero["SegmentoDesercion"]<-NA


Febrero[which(Febrero$CodEstadoCivil==1),]$SegmentoDesercion<-"5. Estado Civil"

Febrero[which(Febrero$CupoUtilizadoAvance==0),]$SegmentoDesercion<-"4. Avances 0"

Febrero[which(Febrero$Otra_Tarjeta=="SI"),]$SegmentoDesercion<-"3. Otra tarjeta"

Febrero[which(Febrero$SoloSeguro=="SI"),]$SegmentoDesercion<-"2. Solo seguro"

Febrero[which(Febrero$CodT6CupoUtilizado==1 | Febrero$Menos8Autorizaciones6M==1),]$SegmentoDesercion<-"1. Decrecio en el cupo Utilizado o Bajo Consumo"



################Cruzar telefonos#############################
odbcChannel <-odbcConnect("Negocio") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "ControlAcceso")

ControlAcceso <- sqlExecute(odbcChannel, "SELECT * FROM ControlAcceso", fetch = TRUE)

ControlAcceso["Identificacion"]<-as.character(ControlAcceso$NumeroIdentificacion)

odbcClose(odbcChannel)

rm(odbcChannel)
ControlAcceso<-subset(ControlAcceso,select=c("Identificacion","CorreoElectronicoRegistrado","NumeroCelularRegistrado"))
ControlAcceso$CorreoElectronicoRegistrado<-as.character(ControlAcceso$CorreoElectronicoRegistrado)
ControlAcceso$NumeroCelularRegistrado<-as.character(ControlAcceso$NumeroCelularRegistrado)

NROW(ControlAcceso[which(is.na(ControlAcceso$CorreoElectronicoRegistrado)),])

ControlAcceso<-cbind(ControlAcceso,
                     nchar(as.character(ControlAcceso$CorreoElectronicoRegistrado))
                     ,nchar(as.character(ControlAcceso$NumeroCelularRegistrado)))
colnames(ControlAcceso)[4]<-"nCorreo"
colnames(ControlAcceso)[5]<-"nTelefono"
ControlAcceso[which(ControlAcceso$nCorreo==0),]$CorreoElectronicoRegistrado<-NA
ControlAcceso[which(ControlAcceso$nTelefono==0),]$NumeroCelularRegistrado<-NA
ControlAcceso<-subset(ControlAcceso,select=c("Identificacion","CorreoElectronicoRegistrado","NumeroCelularRegistrado"))


###########Dispositivos de Ubicabilidad#################

odbcChannel <-odbcConnect("DWH_Reporte") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Rep_TelefonoClienteTotal")

Rep_Telefono <- sqlExecute(odbcChannel, "SELECT * FROM Rep_TelefonoClienteTotal", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)

Rep_Telefono$Identificacion<-as.character(Rep_Telefono$Identificacion)
Rep_Telefono<-subset(Rep_Telefono,select=c("Identificacion","Telefono_01"
                                           ,"Telefono_02"
                                           ,"Telefono_03"
                                           ,"Telefono_04"
                                           ,"Telefono_05"))
Rep_Telefono<-cbind(Rep_Telefono,
                    nchar(as.character(Rep_Telefono$Telefono_01)),
                    nchar(as.character(Rep_Telefono$Telefono_02)),
                    nchar(as.character(Rep_Telefono$Telefono_03)),
                    nchar(as.character(Rep_Telefono$Telefono_04)),
                    nchar(as.character(Rep_Telefono$Telefono_05)))


Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_01))`==0),]$Telefono_01<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_02))`==0),]$Telefono_02<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_03))`==0),]$Telefono_03<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_04))`==0),]$Telefono_04<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_05))`==0),]$Telefono_05<-NA

Rep_Telefono<-subset(Rep_Telefono,select=c("Identificacion","Telefono_01"
                                           ,"Telefono_02"
                                           ,"Telefono_03"
                                           ,"Telefono_04"
                                           ,"Telefono_05"))

####################Mails Disponibles###########################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "DispositivoUbicacion")

MailsUbicacion <- sqlExecute(odbcChannel, "SELECT * FROM DispositivoUbicacion
                             WHERE
                             TipoDispositivo = 'EMAIL'
                             ", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)

#load("d:/Users_info/ALBANPD/My Documents/Bases/Demografica.Rdata")

#Aux<-subset(Demografica,select=c("IdClienteNatural","Identificacion","EdadCliente"))
#MailsUbicacion<-left_join(MailsUbicacion,Aux,by="IdClienteNatural")
#MailsUbicacion<-subset(MailsUbicacion,select=c("Identificacion","Valor","EdadCliente"))
############################Cruses############################
Febrero<-left_join(Febrero,ControlAcceso,by="Identificacion")

Febrero<-left_join(Febrero,MailsUbicacion,by="IdClienteNatural")
Febrero<-subset(Febrero,!duplicated(Identificacion))

Febrero<-left_join(Febrero,Rep_Telefono,by="Identificacion")


########################Cruses para nombre del cliente##################

odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "InformacionDemografica")

InformacionDemografica <- sqlExecute(odbcChannel, "SELECT * FROM InformacionDemografica", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)

Demografica<-full_join(InformacionDemografica,ClienteNatural,by="IdClienteNatural")
Demografica<-full_join(Demografica,Cliente,by="IdCliente")
Demografica<-subset(Demografica,select=c("Identificacion","PrimerApellido","SegundoApellido",
                                         "PrimerNombre","SegundoNombre"))
Demografica$Identificacion<-as.character(Demografica$Identificacion)
##################################Poner Nombre################

Febrero<-left_join(Febrero,Demografica,by="Identificacion")
Febrero<-subset(Febrero,!duplicated(Identificacion))
aux<-subset(Febrero,Decil=="P10" | Decil=="P9")
aux<-subset(aux,FechaApertura > as.Date("2017-01-31") & !is.na(FechaApertura))
write.csv(aux,file="NuevosFebrero2017.csv")
