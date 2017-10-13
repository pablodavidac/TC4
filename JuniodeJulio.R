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

library("DBI", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dbplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("odbc", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

setwd("d:/Users_info/ALBANPD/My Documents/Bases")


####################Tarjetas activas###########################################################
DWH_CRM <- dbConnect(odbc::odbc(),
                 Driver    = "SQL Server", 
                 Server    = "BSUIO-DWH02",
                 Database  = "DWH_CRM")

Mayo<-tbl(DWH_CRM, "SB_TDCActivas") %>% filter(CuentaTarjetaMonitor==1)%>% collect()

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
                             "FechaApertura"
                           ))

Mayo<-subset(Mayo,!duplicated(IdCuentaTarjeta))

##################################Demograficas##################################
################Informacion del cliente################
Cliente <- dbConnect(odbc::odbc(),
                     Driver    = "SQL Server", 
                     Server    = "BSUIO-DWH02",
                     Database  = "Cliente")

Demografica<- tbl(Cliente, "ClienteNatural") %>%
  full_join(tbl(Cliente, "Cliente")) %>% 
  full_join(tbl(Cliente, "InformacionDemografica"),by="IdClienteNatural") %>% 
  full_join(tbl(Cliente, "DireccionClienteNatural"),by="IdClienteNatural") %>% 
  dplyr::select(IdClienteNatural, 
                IdCliente, 
                Identificacion,
                EstadoCivil,
                PrimerApellido,
                SegundoApellido,
                PrimerNombre,
                SegundoNombre,
                UbicacionGeografica1,
                UbicacionGeografica2,
                UbicacionGeografica3,
                UbicacionGeografica4)%>% collect()

colnames(Demografica)[9]<-"CodigoUbicacionGeografica1"
colnames(Demografica)[10]<-"CodigoUbicacionGeografica2"
colnames(Demografica)[11]<-"CodigoUbicacionGeografica3"
colnames(Demografica)[12]<-"CodigoUbicacionGeografica4"
Demografica$CodigoUbicacionGeografica1<-as.integer(Demografica$CodigoUbicacionGeografica1)
Demografica$CodigoUbicacionGeografica2<-as.integer(Demografica$CodigoUbicacionGeografica2)
Demografica$CodigoUbicacionGeografica3<-as.integer(Demografica$CodigoUbicacionGeografica3)
Demografica$CodigoUbicacionGeografica4<-as.integer(Demografica$CodigoUbicacionGeografica4)
#Demografica<-subset(Demografica,!duplicated(Identificacion))
#################Codificacion de Provinca Domicilio#####################
odbcChannel <-odbcConnect("Configuracion") #Databases
#sqlTables(odbcChannel, tableType = "TABLE")
#sqlColumns(odbcChannel, "UbicacionGeografica1")
UG1 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica1", fetch = TRUE)
UG1["Provincia"]<-UG1$Nombre
UG1<- dplyr::select(UG1,CodigoUbicacionGeografica1,Provincia)
UG2 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica2", fetch = TRUE)
UG2["Canton"]<-UG2$Nombre
UG2<- dplyr::select(UG2,CodigoUbicacionGeografica2,Canton)
UG3 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica3", fetch = TRUE)
UG3["Parroquia"]<-UG3$Nombre
UG3<- dplyr::select(UG3,CodigoUbicacionGeografica3,Parroquia)
UG4 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica4", fetch = TRUE)
UG4["Barrio"]<-UG4$Nombre
UG4$CodigoUbicacionGeografica4<-as.character(UG4$CodigoUbicacionGeografica4)
UG4$CodigoUbicacionGeografica4<-as.integer(UG4$CodigoUbicacionGeografica4)
UG4<- dplyr::select(UG4,CodigoUbicacionGeografica4,Barrio)

odbcClose(odbcChannel)


Demografica<-Demografica %>% 
  left_join(UG1,by="CodigoUbicacionGeografica1") %>%
  left_join(UG2,by="CodigoUbicacionGeografica2")%>%
left_join(UG3,by="CodigoUbicacionGeografica3")%>%
left_join(UG4,by="CodigoUbicacionGeografica4")

Demografica<-subset(Demografica,!duplicated(Identificacion))
Demografica<-dplyr::select(Demografica,IdClienteNatural:SegundoNombre,Provincia:Barrio)
#############################Autorizaciones############################

Tarjeta <- dbConnect(odbc::odbc(),
                 Driver    = "SQL Server", 
                 Server    = "BSUIO-DWH02",
                 Database  = "Tarjeta")
Autorizaciones6M<- tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-07-01' 
                                                           & FechaConciliacion > '2016-12-31')%>% collect()

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

AutorizacionesMayo <-tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-07-01' 
                                                            & FechaConciliacion > '2017-06-01')%>% collect()


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
Diciembre<-read.table("Base 2016 12 31.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")

Diciembre<-subset(Diciembre,select=c("IdCuentaTarjeta","CupoUtilizado"))
colnames(Diciembre)[2]<-"CupoUtilizadoT6"
#Septiembre[which(is.na(Septiembre$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Mayo<-left_join(Mayo,Diciembre,by="IdCuentaTarjeta")


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


odbcChannel <-odbcConnect("DWH_Reporte") #Databases

Mails <- sqlExecute(odbcChannel, "SELECT * FROM Rep_ClienteCorreoElectronico", fetch = TRUE)
odbcClose(odbcChannel)
rm(odbcChannel)

Mails$ClienteBankPlus_1<-as.character(Mails$ClienteBankPlus_1)
Mails$ClienteBankPlus_2<-as.character(Mails$ClienteBankPlus_2)
Mails$ClienteBankPlus_3<-as.character(Mails$ClienteBankPlus_3)
Mails$PlanRecompensa<-as.character(Mails$PlanRecompensa)
Mails$MailCanales<-as.character(Mails$MailCanales)

Mails["CorreoElectronico"]<-Mails$ClienteBankPlus_1
Mails[which(nchar(Mails$CorreoElectronico)<3),]$CorreoElectronico<-Mails[which(nchar(Mails$CorreoElectronico)<3),]$PlanRecompensa
Mails[which(nchar(Mails$CorreoElectronico)<5),]$CorreoElectronico<-Mails[which(nchar(Mails$CorreoElectronico)<3),]$MailCanales

Mails[which(is.na(Mails$CorreoElectronico)),]$CorreoElectronico<-Mails[which(is.na(Mails$CorreoElectronico)),]$PlanRecompensa
Mails[which(is.na(Mails$CorreoElectronico)),]$CorreoElectronico<-Mails[which(is.na(Mails$CorreoElectronico)),]$MailCanales
Mails<-subset(Mails,select=c("Identificacion","CorreoElectronico"))

############################Cruses############################
Mayo<-left_join(Mayo,ControlAcceso,by="Identificacion")

Mayo<-left_join(Mayo,Mails,by=c("Identificacion"))
Mayo<-subset(Mayo,!duplicated(Identificacion))

Mayo<-left_join(Mayo,Rep_Telefono,by="Identificacion")



##################################Poner Nombre################

Mayo<-left_join(Mayo,Demografica,by="IdClienteNatural")
Mayo<-subset(Mayo,!duplicated(Identificacion))

#aux<-subset(aux,FechaApertura > as.Date("2017-01-31") & !is.na(FechaApertura))
#############################################################################3

########################Escoger variables y depurar mails#########################
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

Aux<-subset(Aux,Decil %in% c("P9","P10"))
#write.csv(aux,file="Mayo2017.csv")
##############################Nuevos desertores########################333
BaseAnterior<-read.table("CampañaRetencionTDCMayo2017.txt",
                         header=TRUE,
                         sep="\t",
                         na.strings = "NA")
BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion<-paste0(0,BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion)

Viejos<-semi_join(BaseAnterior,Aux,by="Identificacion")
Aux<-left_join(Aux,Viejos,by="Identificacion")
Aux$Nuevo<-as.character(Aux$Nuevo)
Aux[which(is.na(Aux$Nuevo)),]$Nuevo<-"Si"
write.csv(Aux,file="Junio2017.csv",row.names = FALSE)
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
