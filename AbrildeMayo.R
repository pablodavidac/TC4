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

TDCAbril <- sqlExecute(odbcChannel, "SELECT * FROM SB_TDCActivas", fetch = TRUE)

odbcClose(odbcChannel)

Abril<-subset(TDCAbril,CuentaTarjetaMonitor==1)
Abril$FechaApertura <- as.Date(as.character(Abril$FechaApertura))
#Abril<-subset(Abril,FechaApertura > as.Date("2016-12-31"))
table(Abril$Segmento_TDCEnero2017)
Abril[which(Abril$Segmento_TDCEnero2017=="0. Nuevos 6M"),]$Segmento_TDCEnero2017<-"0. NUEVOS 6M"
###################################################################################
Abril<-subset(Abril,select=c("Identificacion",
                             "IdCuentaTarjeta",
                             "Afinidad",
                             "CupoUtilizado",
                             "CupoUtilizadoAvance",
                             "NumeroEntidadesSF",
                             "Segmento_TDCEnero2017",
                             "FechaApertura"))

Abril<-subset(Abril,!duplicated(IdCuentaTarjeta))

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

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-05-01' 
                               and FechaConciliacion > '2016-11-01'", fetch = TRUE)




aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))



AutorizacionesAbril <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2017-05-01' 
                                  and FechaConciliacion > '2017-04-01'", fetch = TRUE)

SoloSeguro<-subset(AutorizacionesAbril,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesAbril,TipoConsumo=="TRJAVANCE" | 
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

##############################Base Abril################################
Abril<-left_join(Abril,Demografica,by="Identificacion")
Abril<-subset(Abril,!duplicated(IdCuentaTarjeta))
Abril<-left_join(Abril,Autorizaciones,by="IdCuentaTarjeta")


##############################################################################


###################################Remplazar NA por ceros##################
Abril[which(is.na(Abril$Autorizaciones6M)),]$Autorizaciones6M<-0
Abril[which(is.na(Abril$ConsumosSeguro)),]$ConsumosSeguro<-0
Abril[which(is.na(Abril$Consumos)),]$Consumos<-0
#Abril<-Abril[,-13]
#Abril<-Abril[,-12]
###########################Cambiar codigos####################
Abril$EstadoCivil<-as.character(Abril$EstadoCivil)
Abril[which(Abril$EstadoCivil!="ESTCIVC" & 
              Abril$EstadoCivil!="ESTCIVD" &
              Abril$EstadoCivil!="ESTCIVS" &
              Abril$EstadoCivil!="ESTCIVU" &
              Abril$EstadoCivil!="ESTCIVV" ),]$EstadoCivil<-NA

Abril[which(Abril$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Abril[which(Abril$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorsiado"
Abril[which(Abril$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Abril[which(Abril$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Union Libre"
Abril[which(Abril$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"
#############################
#cHECK



##################Cupo Utilizado t-6################
#########################T6 Cupo Utilizado########################################
Octubre<-read.table("TDC 20161031.txt",
                       header=TRUE,
                       sep="\t",
                       na.strings = "NA")

Octubre<-subset(Octubre,select=c("IdCuentaTarjeta","CupoUtilizado"))
colnames(Octubre)[2]<-"CupoUtilizadoT6"
#Septiembre[which(is.na(Septiembre$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Abril<-left_join(Abril,Octubre,by="IdCuentaTarjeta")


######################Codificacion variables#######################
Abril["T6CupoUtilizado"]<-0
Abril$T6CupoUtilizado<-(Abril$CupoUtilizado-Abril$CupoUtilizadoT6)/Abril$CupoUtilizadoT6

NROW(Abril[which(is.infinite(Abril$T6CupoUtilizado)),])
NROW(Abril[which(is.na(Abril$T6CupoUtilizado)),])

Abril[which(is.infinite(Abril$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Abril[which(is.na(Abril$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
########################Cupo Utlizado Avances#############
Abril["CupoUtilizadoAvanceIguala0"]<-0
Abril[which(Abril$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1

########################Estado Civil############################
Abril["CodEstadoCivil"]<-1
Abril[which(Abril$EstadoCivil=="Casado"),]$CodEstadoCivil<-1
Abril[which(Abril$EstadoCivil=="Divorsiado"),]$CodEstadoCivil<-1


Abril[which(Abril$EstadoCivil=="Soltero"),]$CodEstadoCivil<-0
Abril[which(Abril$EstadoCivil=="Union Libre"),]$CodEstadoCivil<-0
Abril[which(Abril$EstadoCivil=="Viudo"),]$CodEstadoCivil<-0

Abril[which(Abril$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1

########################Afinidad#############################
Abril$AfinidadTarjeta<-Abril$Afinidad
Abril["Afinidad"]<-"NA"
Abril[which(Abril$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-1
Abril[which(Abril$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Abril[which(Abril$AfinidadTarjeta=="GOLD     "),]$Afinidad<-1

Abril[which(Abril$AfinidadTarjeta=="BLACK"),]$Afinidad<-0
Abril[which(Abril$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-0
Abril[which(Abril$Afinidad=="NA"),]$Afinidad<-0

########################Variable provincia#############################
Abril["ProvinciaDomicilio"]<-Abril$Provincia
Abril["Provincia"]<-"NA"
Abril[which(Abril$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Abril[which(Abril$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Abril[which(Abril$Provincia=="NA"),]$Provincia<-0
########################Otra tarjeta################################
Abril["Otra_Tarjeta"]<-"SI"
Abril[which(Abril$NumeroEntidadesSF==0),]$Otra_Tarjeta<-"NO"
########################Solo Seguro#################################
Abril["SoloSeguro"]<-"NO"
Abril[which(Abril$Consumos==0 & Abril$ConsumosSeguro>0),]$SoloSeguro<-"SI"

#######################Autorizaciones 6 meses#############################
Abril["Menos8Autorizaciones6M"]<-0
Abril[which(Abril$Autorizaciones6M<8.5),]$Menos8Autorizaciones6M<-1

##########################T6 Cupo Utilizado##################################

Abril["CodT6CupoUtilizado"]<-1
Abril[which(Abril$T6CupoUtilizado>=(-0.31)),]$CodT6CupoUtilizado<-0

##############################Cupo Normalizado#############################
Abril["NormCupoUtilizado"]<-(Abril$CupoUtilizado-mean(Abril$CupoUtilizado))/sd(Abril$CupoUtilizado)

#Abril<-left_join(Abril,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
AbrilCompleto<-Abril
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")

Abril<-subset(Abril,Segmento_TDCEnero2017=="0. NUEVOS +6M" |
                Segmento_TDCEnero2017=="0. NUEVOS 6M" |
                Segmento_TDCEnero2017=="1. VIP" |
                Segmento_TDCEnero2017=="2. PREFERENTE" |
                Segmento_TDCEnero2017=="3. MOVILIZACION A")
####################Deciles###########################
fitted.results <- predict(modA2,newdata=Abril,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Abril["Score"]<-predict(modA2,newdata=Abril,type='response')
Abril["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Abril[which(Abril$Score <= quan[2]),]$Decil<-"P1"
Abril[which(Abril$Score <=quan[3] & Abril$Score > quan[2]),]$Decil<-"P2"
Abril[which(Abril$Score <=quan[4] & Abril$Score > quan[3]),]$Decil<-"P3"
Abril[which(Abril$Score <=quan[5] & Abril$Score > quan[4]),]$Decil<-"P4"
Abril[which(Abril$Score <=quan[6] & Abril$Score > quan[5]),]$Decil<-"P5"
Abril[which(Abril$Score <=quan[7] & Abril$Score > quan[6]),]$Decil<-"P6"
Abril[which(Abril$Score <=quan[8] & Abril$Score > quan[7]),]$Decil<-"P7"
Abril[which(Abril$Score <=quan[9] & Abril$Score > quan[8]),]$Decil<-"P8"
Abril[which(Abril$Score <=quan[10] & Abril$Score > quan[9]),]$Decil<-"P9"
Abril[which(Abril$Score >=quan[10]),]$Decil<-"P10"



View(table(Abril$Decil))
#write.csv(Abril,file="AbrildeMayo2017.csv")
########################Cruses de Grupos################################
Abril["SegmentoDesercion"]<-NA


Abril[which(Abril$CodEstadoCivil==1),]$SegmentoDesercion<-"5. Estado Civil"

Abril[which(Abril$CupoUtilizadoAvance==0),]$SegmentoDesercion<-"4. Avances 0"

Abril[which(Abril$Otra_Tarjeta=="SI"),]$SegmentoDesercion<-"3. Otra tarjeta"

Abril[which(Abril$SoloSeguro=="SI"),]$SegmentoDesercion<-"2. Solo seguro"

Abril[which(Abril$CodT6CupoUtilizado==1 | Abril$Menos8Autorizaciones6M==1),]$SegmentoDesercion<-"1. Decrecio en el cupo Utilizado o Bajo Consumo"



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
Abril<-left_join(Abril,ControlAcceso,by="Identificacion")

Abril<-left_join(Abril,MailsUbicacion,by=c("Identificacion","IdClienteNatural"))
Abril<-subset(Abril,!duplicated(Identificacion))

Abril<-left_join(Abril,Rep_Telefono,by="Identificacion")


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

Abril<-left_join(Abril,Demografica,by="IdClienteNatural")
Abril<-subset(Abril,!duplicated(Identificacion))

#aux<-subset(aux,FechaApertura > as.Date("2017-01-31") & !is.na(FechaApertura))
#############################################################################3

########################Escoger variables y depurar mails#########################
Abril["CorreoElectronico"]<-Abril$Valor
Abril$CorreoElectronico<-as.character(Abril$CorreoElectronico)
NROW(Abril[which(nchar(Abril$CorreoElectronico)>3),])  #6546
NROW(Abril[which(is.na(Abril$CorreoElectronico)),])  #6546
Abril[which(is.na(Abril$CorreoElectronico)),]$CorreoElectronico<-Abril[which(is.na(Abril$CorreoElectronico)),]$CorreoElectronicoRegistrado
Abril[which(nchar(Abril$CorreoElectronico)<3),]$CorreoElectronico<-Abril[which(nchar(Abril$CorreoElectronico)<2),]$CorreoElectronicoRegistrado
Abril[which(!is.na(as.numeric(Abril$CorreoElectronico))),]$CorreoElectronico<-NA
Abril$CorreoElectronico<-tolower(Abril$CorreoElectronico)
Aux<-subset(Abril, select = c("Identificacion",
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

 #write.csv(aux,file="Abril2017.csv")
##############################Nuevos desertores########################333
BaseAnterior<-read.table("Abril2017.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA")
BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion<-paste0(0,BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion)

Nuevos<-semi_join(aux,BaseAnterior,by="Identificacion")

#######################Envio de base y escritura####################
aux<-subset(Aux,Decil=="P10" | Decil=="P9")
aux<-aux[order(aux$Score, decreasing = T),]
write.csv(aux,file="CampañaRetencionTDCAbril2017.csv",row.names=FALSE)

########################Base de mails###################################
mails<-subset(Abril,nchar(CorreoElectronico)>3) 
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
Telefonos<-subset(Abril,nchar(as.character(NumeroCelularRegistrado))>2 |
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
Aux<-subset(Abril,Decil=="P10" | Decil=="P9")
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
