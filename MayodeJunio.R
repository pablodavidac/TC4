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

TDCMayo <- sqlExecute(odbcChannel, "SELECT * FROM SB_TDCActivas", fetch = TRUE)

odbcClose(odbcChannel)

Mayo<-subset(TDCMayo,CuentaTarjetaMonitor==1)
Mayo$FechaApertura <- as.Date(as.character(Mayo$FechaApertura))
#Mayo<-subset(Mayo,FechaApertura > as.Date("2016-12-31"))
table(Mayo$Segmento_TDCAbril2017)
#Mayo[which(Mayo$Segmento_TDCEnero2017=="0. Nuevos 6M"),]$Segmento_TDCEnero2017<-"0. NUEVOS 6M"
###################################################################################
Mayo<-subset(Mayo,select=c("Identificacion",
                             "IdCuentaTarjeta",
                             "Afinidad",
                             "CupoUtilizado",
                             "CupoUtilizadoAvance",
                             "NumeroEntidadesSF",
                             "Segmento_TDCAbril2017",
                             "FechaApertura"))

Mayo<-subset(Mayo,!duplicated(IdCuentaTarjeta))

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

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-06-01' 
                               and FechaConciliacion > '2016-12-01'", fetch = TRUE)




aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))



AutorizacionesMayo <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-06-01' 
                                  and FechaConciliacion > '2017-05-01'", fetch = TRUE)

SoloSeguro<-subset(AutorizacionesMayo,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesMayo,TipoConsumo=="TRJAVANCE" | 
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

##############################Base Mayo################################
Mayo<-left_join(Mayo,Demografica,by="Identificacion")
Mayo<-subset(Mayo,!duplicated(IdCuentaTarjeta))
Mayo<-left_join(Mayo,Autorizaciones,by="IdCuentaTarjeta")
##############################################################################


###################################Remplazar NA por ceros##################
Mayo[which(is.na(Mayo$Autorizaciones6M)),]$Autorizaciones6M<-0
Mayo[which(is.na(Mayo$ConsumosSeguro)),]$ConsumosSeguro<-0
Mayo[which(is.na(Mayo$Consumos)),]$Consumos<-0
#Mayo<-Mayo[,-13]
#Mayo<-Mayo[,-12]
###########################Cambiar codigos####################
Mayo$EstadoCivil<-as.character(Mayo$EstadoCivil)
Mayo[which(Mayo$EstadoCivil!="ESTCIVC" & 
              Mayo$EstadoCivil!="ESTCIVD" &
              Mayo$EstadoCivil!="ESTCIVS" &
              Mayo$EstadoCivil!="ESTCIVU" &
              Mayo$EstadoCivil!="ESTCIVV" ),]$EstadoCivil<-NA

Mayo[which(Mayo$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Mayo[which(Mayo$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorsiado"
Mayo[which(Mayo$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Mayo[which(Mayo$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Union Libre"
Mayo[which(Mayo$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"
#############################



##################Cupo Utilizado t-6################
#########################T6 Cupo Utilizado########################################
Noviembre<-read.table("Base20161130.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")

Noviembre<-subset(Noviembre,select=c("IdCuentaTarjeta","CupoUtilizado"))
colnames(Noviembre)[2]<-"CupoUtilizadoT6"
#Septiembre[which(is.na(Septiembre$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Mayo<-left_join(Mayo,Noviembre,by="IdCuentaTarjeta")


######################Codificacion variables#######################
Mayo["T6CupoUtilizado"]<-0
Mayo$T6CupoUtilizado<-(Mayo$CupoUtilizado-Mayo$CupoUtilizadoT6)/Mayo$CupoUtilizadoT6

NROW(Mayo[which(is.infinite(Mayo$T6CupoUtilizado)),])
NROW(Mayo[which(is.na(Mayo$T6CupoUtilizado)),])

Mayo[which(is.infinite(Mayo$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Mayo[which(is.na(Mayo$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
########################Cupo Utlizado Avances#############
Mayo["CupoUtilizadoAvanceIguala0"]<-0
Mayo[which(Mayo$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1

########################Estado Civil############################
Mayo["CodEstadoCivil"]<-1
Mayo[which(Mayo$EstadoCivil=="Casado"),]$CodEstadoCivil<-1
Mayo[which(Mayo$EstadoCivil=="Divorsiado"),]$CodEstadoCivil<-1


Mayo[which(Mayo$EstadoCivil=="Soltero"),]$CodEstadoCivil<-0
Mayo[which(Mayo$EstadoCivil=="Union Libre"),]$CodEstadoCivil<-0
Mayo[which(Mayo$EstadoCivil=="Viudo"),]$CodEstadoCivil<-0

Mayo[which(Mayo$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1

########################Afinidad#############################
Mayo$AfinidadTarjeta<-Mayo$Afinidad
Mayo["Afinidad"]<-"NA"
Mayo[which(Mayo$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-1
Mayo[which(Mayo$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Mayo[which(Mayo$AfinidadTarjeta=="GOLD     "),]$Afinidad<-1

Mayo[which(Mayo$AfinidadTarjeta=="BLACK"),]$Afinidad<-0
Mayo[which(Mayo$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-0
Mayo[which(Mayo$Afinidad=="NA"),]$Afinidad<-0

########################Variable provincia#############################
Mayo["ProvinciaDomicilio"]<-Mayo$Provincia
Mayo["Provincia"]<-"NA"
Mayo[which(Mayo$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Mayo[which(Mayo$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Mayo[which(Mayo$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Mayo[which(Mayo$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Mayo[which(Mayo$Provincia=="NA"),]$Provincia<-0
########################Otra tarjeta################################
Mayo["Otra_Tarjeta"]<-"SI"
Mayo[which(Mayo$NumeroEntidadesSF==0),]$Otra_Tarjeta<-"NO"
########################Solo Seguro#################################
Mayo["SoloSeguro"]<-"NO"
Mayo[which(Mayo$Consumos==0 & Mayo$ConsumosSeguro>0),]$SoloSeguro<-"SI"

#######################Autorizaciones 6 meses#############################
Mayo["Menos8Autorizaciones6M"]<-0
Mayo[which(Mayo$Autorizaciones6M<8.5),]$Menos8Autorizaciones6M<-1

##########################T6 Cupo Utilizado##################################

Mayo["CodT6CupoUtilizado"]<-1
Mayo[which(Mayo$T6CupoUtilizado>=(-0.31)),]$CodT6CupoUtilizado<-0

##############################Cupo Normalizado#############################
Mayo["NormCupoUtilizado"]<-(Mayo$CupoUtilizado-mean(Mayo$CupoUtilizado))/sd(Mayo$CupoUtilizado)

#Mayo<-left_join(Mayo,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
MayoCompleto<-Mayo
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")

Mayo<-subset(Mayo,Segmento_TDCAbril2017=="0. NUEVOS +6M" |
               Segmento_TDCAbril2017=="0. NUEVOS 6M" |
               Segmento_TDCAbril2017=="1. VIP" |
               Segmento_TDCAbril2017=="2. PREFERENTE" |
               Segmento_TDCAbril2017=="3. MOVILIZACION A")
####################Deciles###########################
fitted.results <- predict(modA2,newdata=Mayo,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Mayo["Score"]<-predict(modA2,newdata=Mayo,type='response')
Mayo["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Mayo[which(Mayo$Score <= quan[2]),]$Decil<-"P1"
Mayo[which(Mayo$Score <=quan[3] & Mayo$Score > quan[2]),]$Decil<-"P2"
Mayo[which(Mayo$Score <=quan[4] & Mayo$Score > quan[3]),]$Decil<-"P3"
Mayo[which(Mayo$Score <=quan[5] & Mayo$Score > quan[4]),]$Decil<-"P4"
Mayo[which(Mayo$Score <=quan[6] & Mayo$Score > quan[5]),]$Decil<-"P5"
Mayo[which(Mayo$Score <=quan[7] & Mayo$Score > quan[6]),]$Decil<-"P6"
Mayo[which(Mayo$Score <=quan[8] & Mayo$Score > quan[7]),]$Decil<-"P7"
Mayo[which(Mayo$Score <=quan[9] & Mayo$Score > quan[8]),]$Decil<-"P8"
Mayo[which(Mayo$Score <=quan[10] & Mayo$Score > quan[9]),]$Decil<-"P9"
Mayo[which(Mayo$Score >=quan[10]),]$Decil<-"P10"



View(table(Mayo$Decil))
#write.csv(Mayo,file="MayodeMayo2017.csv")
########################Cruses de Grupos################################
Mayo["SegmentoDesercion"]<-NA


Mayo[which(Mayo$CodEstadoCivil==1),]$SegmentoDesercion<-"5. Estado Civil"

Mayo[which(Mayo$CupoUtilizadoAvance==0),]$SegmentoDesercion<-"4. Avances 0"

Mayo[which(Mayo$Otra_Tarjeta=="SI"),]$SegmentoDesercion<-"3. Otra tarjeta"

Mayo[which(Mayo$SoloSeguro=="SI"),]$SegmentoDesercion<-"2. Solo seguro"

Mayo[which(Mayo$CodT6CupoUtilizado==1 | Mayo$Menos8Autorizaciones6M==1),]$SegmentoDesercion<-"1. Decrecio en el cupo Utilizado o Bajo Consumo"



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

Aux<-subset(Demografica,select=c("IdClienteNatural","Identificacion"))
MailsUbicacion<-left_join(MailsUbicacion,Aux,by="IdClienteNatural")
#MailsUbicacion<-subset(MailsUbicacion,select=c("Identificacion","Valor","EdadCliente"))
############################Cruses############################
Mayo<-left_join(Mayo,ControlAcceso,by="Identificacion")

Mayo<-left_join(Mayo,MailsUbicacion,by=c("Identificacion","IdClienteNatural"))
Mayo<-subset(Mayo,!duplicated(Identificacion))

Mayo<-left_join(Mayo,Rep_Telefono,by="Identificacion")


########################Cruses para nombre del cliente##################

odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "InformacionDemografica")

InformacionDemografica <- sqlExecute(odbcChannel, "SELECT * FROM InformacionDemografica", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)

Demografica<-full_join(InformacionDemografica,ClienteNatural,by="IdClienteNatural")
#Demografica<-full_join(Demografica,Cliente,by="IdCliente")
Demografica<-subset(Demografica,select=c("IdClienteNatural","PrimerApellido","SegundoApellido",
                                         "PrimerNombre","SegundoNombre"))
#Demografica$Identificacion<-as.character(Demografica$Identificacion)
##################################Poner Nombre################

Mayo<-left_join(Mayo,Demografica,by="IdClienteNatural")
Mayo<-subset(Mayo,!duplicated(Identificacion))

#aux<-subset(aux,FechaApertura > as.Date("2017-01-31") & !is.na(FechaApertura))
#############################################################################3

########################Escoger variables y depurar mails#########################
Mayo["CorreoElectronico"]<-Mayo$Valor
Mayo$CorreoElectronico<-as.character(Mayo$CorreoElectronico)
NROW(Mayo[which(nchar(Mayo$CorreoElectronico)>3),])  #6546
NROW(Mayo[which(is.na(Mayo$CorreoElectronico)),])  #6546
Mayo[which(is.na(Mayo$CorreoElectronico)),]$CorreoElectronico<-Mayo[which(is.na(Mayo$CorreoElectronico)),]$CorreoElectronicoRegistrado
Mayo[which(nchar(Mayo$CorreoElectronico)<3),]$CorreoElectronico<-Mayo[which(nchar(Mayo$CorreoElectronico)<2),]$CorreoElectronicoRegistrado
Mayo[which(!is.na(as.numeric(Mayo$CorreoElectronico))),]$CorreoElectronico<-NA
Mayo$CorreoElectronico<-tolower(Mayo$CorreoElectronico)
Aux<-subset(Mayo, select = c("Identificacion",
                              "EstadoCivil",
                              "AfinidadTarjeta",	
                              "ProvinciaDomicilio",
                              "Otra_Tarjeta",
                              "SoloSeguro",
                              "Segmento_TDCAbril2017",
                              "Score",
                              "Decil",
                              "SegmentoDesercion",
                              "CorreoElectronico",
                              "NumeroCelularRegistrado",
                              "Telefono_01",
                              "Telefono_02",
                              "Telefono_03",
                              "Telefono_04",
                              "Telefono_05",
                              "PrimerApellido",
                              "SegundoApellido",
                              "PrimerNombre",
                              "SegundoNombre"))
aux<-subset(Aux,Decil=="P9" | Decil=="P10")

##############################Nuevos desertores########################333
BaseAnterior<-read.table("CampañaRetencionTDCAbril2017.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA")
BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion<-paste0(0,BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion)
########################Aumentar columna de nuevos desertores##################
BaseAnterior["Nuevo"]<-"NO"
BaseAnterior<-subset(BaseAnterior,select=c("Identificacion","Nuevo"))

aux<-left_join(aux,BaseAnterior,by="Identificacion")
aux[which(is.na(aux$Nuevo)),]$Nuevo<-"SI"
table(aux$Nuevo)

write.csv(aux,file="CampañaRetencionTDCMayo2017.csv",row.names = FALSE)

#######################Envio de base y escritura####################
aux<-subset(Aux,Decil=="P10" | Decil=="P9")
aux<-aux[order(aux$Score, decreasing = T),]
write.csv(aux,file="CampañaRetencionTDCMayo2017.csv",row.names=FALSE)









########################Base de mails###################################
mails<-subset(Mayo,nchar(CorreoElectronico)>3) 
mails<-subset(mails,Decil=="P10" | Decil=="P9" | Decil=="P8")
Aux<-subset(mails, select = c("Identificacion",
                              "EstadoCivil",
                              "AfinidadTarjeta",	
                              "ProvinciaDomicilio",
                              "Otra_Tarjeta",
                              "SoloSeguro",
                              "Segmento_TDCEnero2017",
                              "Score",
                              "Decil",
                              "SegmentoDesercion",
                              "CorreoElectronico",
                              
                              "PrimerApellido",
                              "SegundoApellido",
                              "PrimerNombre",
                              "SegundoNombre"))
write.csv(Aux,file="MailsPosiblesDesertoresMarzo17.csv",row.names=FALSE)

###################Base de telefonos############################
Telefonos<-subset(Mayo,nchar(as.character(NumeroCelularRegistrado))>2 |
                    nchar(as.character(Telefono_01))>2|
                    nchar(as.character(Telefono_02))>2|
                    nchar(as.character(Telefono_03))>2|
                    nchar(as.character(Telefono_04))>2|
                    nchar(as.character(Telefono_05))>2)
Telefonos<-Telefonos[order(Telefonos$Score,decreasing = TRUE),]
Aux<-subset(Telefonos, select = c("Identificacion",
                                  "EstadoCivil",
                                  "AfinidadTarjeta",	
                                  "ProvinciaDomicilio",
                                  "Otra_Tarjeta",
                                  "SoloSeguro",
                                  "Segmento_TDCEnero2017",
                                  "Score",
                                  "Decil",
                                  "SegmentoDesercion",
                                  "NumeroCelularRegistrado",
                                  "Telefono_01",
                                  "Telefono_02",
                                  "Telefono_03",
                                  "Telefono_04",
                                  "Telefono_05",
                                  "PrimerApellido",
                                  "SegundoApellido",
                                  "PrimerNombre",
                                  "SegundoNombre"))
Aux<-Aux[1:7000,]
write.csv(Aux,file="TelefonosPosiblesDesertoresMarzo17.csv",row.names=FALSE)
###################################Clientes Nuevos######################
Aux<-subset(Mayo,Decil=="P10" | Decil=="P9")
Campana<-read.table("ClientesCampana.csv",
                    header=TRUE,
                    sep=",",
                    na.strings = "NA")


aux<-anti_join(Aux,Campana,by="Identificacion")
aux<-subset(aux,select = c("Identificacion",
                           "IdCuentaTarjeta",
                           "EstadoCivil",
                           "AfinidadTarjeta",	
                           "ProvinciaDomicilio",
                           "Otra_Tarjeta",
                           "SoloSeguro",
                           "Segmento_TDCEnero2017",
                           "Score",
                           "Decil",
                           "SegmentoDesercion",
                           "CorreoElectronico",
                           "NumeroCelularRegistrado",
                           "Telefono_01",
                           "Telefono_02",
                           "Telefono_03",
                           "Telefono_04",
                           "Telefono_05",
                           "PrimerApellido",
                           "SegundoApellido",
                           "PrimerNombre",
                           "SegundoNombre"))
aux<-aux[order(aux$Score,decreasing = TRUE),]
write.csv(aux,file = "NuevosPosiblesDesertoresMarzo17.csv",row.names=FALSE)
