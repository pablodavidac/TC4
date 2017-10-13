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

Agosto<-tbl(DWH_CRM, "SB_TDCActivas") %>% filter(CuentaTarjetaMonitor==1)%>% collect()

Agosto$FechaApertura <- as.Date(as.character(Agosto$FechaApertura))
#Agosto<-subset(Agosto,FechaApertura > as.Date("2016-12-31"))
table(Agosto$SegmentoTDCAbril2017)
#Agosto[which(Agosto$Segmento_TDCFebrero2017=="0. Nuevos 6M"),]$Segmento_TDCFebrero2017<-"0. NUEVOS 6M"
###################################################################################
Agosto<-subset(Agosto,select=c("Identificacion",
                             "IdCuentaTarjeta",
                             "Afinidad",
                             "CupoUtilizado",
                             "CupoUtilizadoAvance",
                             "NumeroEntidadesSF",
                             "SegmentoTDCAbril2017",
                             "FechaApertura"
))

Agosto<-subset(Agosto,!duplicated(IdCuentaTarjeta))

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
Autorizaciones6M<- tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-09-01' 
                                                           & FechaConciliacion > '2017-02-28')%>% collect()

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

AutorizacionesAgosto <-tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-09-01' 
                                                              & FechaConciliacion > '2017-08-01')%>% collect()


SoloSeguro<-subset(AutorizacionesAgosto,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesAgosto,TipoConsumo=="TRJAVANCE" | 
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

##############################Base Agosto################################
Agosto<-left_join(Agosto,Demografica,by="Identificacion")
Agosto<-subset(Agosto,!duplicated(IdCuentaTarjeta))
Agosto<-left_join(Agosto,Autorizaciones,by="IdCuentaTarjeta")


##############################################################################


###################################Remplazar NA por ceros##################
Agosto[which(is.na(Agosto$Autorizaciones6M)),]$Autorizaciones6M<-0
Agosto[which(is.na(Agosto$ConsumosSeguro)),]$ConsumosSeguro<-0
Agosto[which(is.na(Agosto$Consumos)),]$Consumos<-0
#Agosto<-Agosto[,-13]
#Agosto<-Agosto[,-12]
###########################Cambiar codigos####################
Agosto$EstadoCivil<-as.character(Agosto$EstadoCivil)
Agosto[which(Agosto$EstadoCivil!="ESTCIVC" & 
              Agosto$EstadoCivil!="ESTCIVD" &
              Agosto$EstadoCivil!="ESTCIVS" &
              Agosto$EstadoCivil!="ESTCIVU" &
              Agosto$EstadoCivil!="ESTCIVV" ),]$EstadoCivil<-NA

Agosto[which(Agosto$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Agosto[which(Agosto$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorsiado"
Agosto[which(Agosto$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Agosto[which(Agosto$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Union Libre"
Agosto[which(Agosto$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"
#############################


##################Cupo Utilizado t-6################
#########################T6 Cupo Utilizado########################################
Febrero<-read.table("Base 2017 02 28.txt",
                  header=TRUE,
                  sep="\t",
                  na.strings = "NA")

Febrero<-subset(Febrero,select=c("Identificacion","CupoUtilizado"))
colnames(Febrero)[2]<-"CupoUtilizadoT6"
#Septiembre[which(is.na(Septiembre$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Febrero$Identificacion<-as.character(Febrero$Identificacion)
Agosto<-left_join(Agosto,Febrero,by="Identificacion")


######################Codificacion variables#######################
Agosto["T6CupoUtilizado"]<-0
Agosto$T6CupoUtilizado<-(Agosto$CupoUtilizado-Agosto$CupoUtilizadoT6)/Agosto$CupoUtilizadoT6

NROW(Agosto[which(is.infinite(Agosto$T6CupoUtilizado)),])
NROW(Agosto[which(is.na(Agosto$T6CupoUtilizado)),])

Agosto[which(is.infinite(Agosto$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Agosto[which(is.na(Agosto$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
########################Cupo Utlizado Avances#############
Agosto["CupoUtilizadoAvanceIguala0"]<-0
Agosto[which(Agosto$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1

########################Estado Civil############################
Agosto["CodEstadoCivil"]<-1
Agosto[which(Agosto$EstadoCivil=="Casado"),]$CodEstadoCivil<-1
Agosto[which(Agosto$EstadoCivil=="Divorsiado"),]$CodEstadoCivil<-1


Agosto[which(Agosto$EstadoCivil=="Soltero"),]$CodEstadoCivil<-0
Agosto[which(Agosto$EstadoCivil=="Union Libre"),]$CodEstadoCivil<-0
Agosto[which(Agosto$EstadoCivil=="Viudo"),]$CodEstadoCivil<-0

Agosto[which(Agosto$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1

########################Afinidad#############################
Agosto$AfinidadTarjeta<-Agosto$Afinidad
Agosto["Afinidad"]<-"NA"
Agosto[which(Agosto$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-1
Agosto[which(Agosto$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Agosto[which(Agosto$AfinidadTarjeta=="GOLD     "),]$Afinidad<-1

Agosto[which(Agosto$AfinidadTarjeta=="BLACK"),]$Afinidad<-0
Agosto[which(Agosto$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-0
Agosto[which(Agosto$Afinidad=="NA"),]$Afinidad<-0

########################Variable provincia#############################
Agosto["ProvinciaDomicilio"]<-Agosto$Provincia
Agosto["Provincia"]<-"NA"
Agosto[which(Agosto$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Agosto[which(Agosto$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Agosto[which(Agosto$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Agosto[which(Agosto$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Agosto[which(Agosto$Provincia=="NA"),]$Provincia<-0
########################Otra tarjeta################################
Agosto["Otra_Tarjeta"]<-"SI"
Agosto[which(Agosto$NumeroEntidadesSF==0),]$Otra_Tarjeta<-"NO"
########################Solo Seguro#################################
Agosto["SoloSeguro"]<-"NO"
Agosto[which(Agosto$Consumos==0 & Agosto$ConsumosSeguro>0),]$SoloSeguro<-"SI"

#######################Autorizaciones 6 meses#############################
Agosto["Menos8Autorizaciones6M"]<-0
Agosto[which(Agosto$Autorizaciones6M<8.5),]$Menos8Autorizaciones6M<-1

##########################T6 Cupo Utilizado##################################

Agosto["CodT6CupoUtilizado"]<-1
Agosto[which(Agosto$T6CupoUtilizado>=(-0.31)),]$CodT6CupoUtilizado<-0

##############################Cupo Normalizado#############################
Agosto["NormCupoUtilizado"]<-(Agosto$CupoUtilizado-mean(Agosto$CupoUtilizado))/sd(Agosto$CupoUtilizado)

#Agosto<-left_join(Agosto,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
AgostoCompleto<-Agosto
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")

Agosto<-subset(Agosto,SegmentoTDCAbril2017=="0. NUEVOS +6M" |
                SegmentoTDCAbril2017=="0. NUEVOS 6M" |
                SegmentoTDCAbril2017=="1. VIP" |
                SegmentoTDCAbril2017=="2. PREFERENTE" |
                SegmentoTDCAbril2017=="3. MOVILIZACION A")
####################Deciles###########################
fitted.results <- predict(modA2,newdata=Agosto,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Agosto["Score"]<-predict(modA2,newdata=Agosto,type='response')
Agosto["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Agosto[which(Agosto$Score <= quan[2]),]$Decil<-"P1"
Agosto[which(Agosto$Score <=quan[3] & Agosto$Score > quan[2]),]$Decil<-"P2"
Agosto[which(Agosto$Score <=quan[4] & Agosto$Score > quan[3]),]$Decil<-"P3"
Agosto[which(Agosto$Score <=quan[5] & Agosto$Score > quan[4]),]$Decil<-"P4"
Agosto[which(Agosto$Score <=quan[6] & Agosto$Score > quan[5]),]$Decil<-"P5"
Agosto[which(Agosto$Score <=quan[7] & Agosto$Score > quan[6]),]$Decil<-"P6"
Agosto[which(Agosto$Score <=quan[8] & Agosto$Score > quan[7]),]$Decil<-"P7"
Agosto[which(Agosto$Score <=quan[9] & Agosto$Score > quan[8]),]$Decil<-"P8"
Agosto[which(Agosto$Score <=quan[10] & Agosto$Score > quan[9]),]$Decil<-"P9"
Agosto[which(Agosto$Score >=quan[10]),]$Decil<-"P10"



View(table(Agosto$Decil))
#write.csv(Agosto,file="AgostodeAgosto2017.csv")
########################Cruses de Grupos################################
Agosto["SegmentoDesercion"]<-NA


Agosto[which(Agosto$CodEstadoCivil==1),]$SegmentoDesercion<-"5. Estado Civil"

Agosto[which(Agosto$CupoUtilizadoAvance==0),]$SegmentoDesercion<-"4. Avances 0"

Agosto[which(Agosto$Otra_Tarjeta=="SI"),]$SegmentoDesercion<-"3. Otra tarjeta"

Agosto[which(Agosto$SoloSeguro=="SI"),]$SegmentoDesercion<-"2. Solo seguro"

Agosto[which(Agosto$CodT6CupoUtilizado==1 | Agosto$Menos8Autorizaciones6M==1),]$SegmentoDesercion<-"1. Decrecio en el cupo Utilizado o Bajo Consumo"



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
Aux<-Agosto
Agosto<-left_join(Agosto,ControlAcceso,by="Identificacion")

Agosto<-left_join(Agosto,Mails,by=c("Identificacion"))
Agosto<-subset(Agosto,!duplicated(Identificacion))

Agosto<-left_join(Agosto,Rep_Telefono,by="Identificacion")



##################################Poner Nombre################

#Agosto<-left_join(Agosto,Demografica,by=c("IdClienteNatural","Identificacion"))
#Agosto<-subset(Agosto,!duplicated(Identificacion))

#aux<-subset(aux,FechaApertura > as.Date("2017-01-31") & !is.na(FechaApertura))
#############################################################################3

########################Escoger variables y depurar mails#########################
Agosto$CorreoElectronico<-as.character(Agosto$CorreoElectronico)
NROW(Agosto[which(nchar(Agosto$CorreoElectronico)>3),])  #6546
NROW(Agosto[which(is.na(Agosto$CorreoElectronico)),])  #6546
Agosto[which(is.na(Agosto$CorreoElectronico)),]$CorreoElectronico<-Agosto[which(is.na(Agosto$CorreoElectronico)),]$CorreoElectronicoRegistrado
Agosto[which(nchar(Agosto$CorreoElectronico)<3),]$CorreoElectronico<-Agosto[which(nchar(Agosto$CorreoElectronico)<2),]$CorreoElectronicoRegistrado
Agosto[which(!is.na(as.numeric(Agosto$CorreoElectronico))),]$CorreoElectronico<-NA
Agosto$CorreoElectronico<-tolower(Agosto$CorreoElectronico)
Aux<-subset(Agosto, select = c("Identificacion",
                              "EstadoCivil",
                              "AfinidadTarjeta",	
                              "ProvinciaDomicilio",
                              "Otra_Tarjeta",
                              "SoloSeguro",
                              "SegmentoTDCAbril2017",
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

##############Añadir Celulares y mails externos#############

CeluMailsExt<-read.table("CelularesMailsExternosAlia.txt",
                         header=TRUE,
                         sep="\t",
                         na.strings = "NA")
colnames(CeluMailsExt)[1]<-"Identificacion"
CeluMailsExt$Identificacion<-as.character(CeluMailsExt$Identificacion)
CeluMailsExt$Celular_01<-as.character(CeluMailsExt$Celular_01)
CeluMailsExt$CorreoElectronico_01<-as.character(CeluMailsExt$CorreoElectronico_01)
CeluMailsExt<-subset(CeluMailsExt,select=c("Identificacion",
                                           "Celular_01",
                                           "CorreoElectronico_01"))


CeluMailsExt$CorreoElectronico_01<-tolower(CeluMailsExt$CorreoElectronico_01)
CeluMailsExt[grep("tiene",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA
CeluMailsExt[grep("tiene",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA
CeluMailsExt[grep("no@no.com",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA
CeluMailsExt[grep("no@telefonica.com.ec",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_02<-NA

CeluMailsExt[grep("noposee",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA
CeluMailsExt[grep("nose",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA
CeluMailsExt[grep("nose",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA
CeluMailsExt[grep("null",CeluMailsExt$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-NA

CeluMailsExt[grep("NULL",CeluMailsExt$Celular_01,value = FALSE),]$Celular_01<-NA



aux<-left_join(Aux,CeluMailsExt, by="Identificacion")
aux[which(is.na(aux$CorreoElectronico)),]$CorreoElectronico<-aux[which(is.na(aux$CorreoElectronico)),]$CorreoElectronico_01
aux[which(is.na(aux$NumeroCelularRegistrado)),]$NumeroCelularRegistrado<-aux[which(is.na(aux$NumeroCelularRegistrado)),]$Celular_01
aux[which(nchar(aux$NumeroCelularRegistrado)<7),]$NumeroCelularRegistrado<-aux[which(nchar(aux$NumeroCelularRegistrado)<7),]$Celular_01
aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01<-aux[which(nchar(aux$CorreoElectronico_01)<2),]$Celular_01
aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01<-NA
################################3
NROW(subset(aux,!is.na(CorreoElectronico)))
#81295


NROW(subset(aux,!is.na(NumeroCelularRegistrado)))
#41260
NROW(subset(aux,nchar(CorreoElectronico)>2))

NROW(subset(aux,nchar(NumeroCelularRegistrado)>2))

Aux<-subset(aux,Decil %in% c("P9","P10"))
write.csv(Aux,file="Agosto2017.csv", row.names = FALSE)
#############################Campaña 2######################
Campania<-anti_join(aux,Aux,by="Identificacion")
a1<-subset(Agosto,Decil %in% c("P4","P5","P6","P7") & Afinidad==1 & Provincia==0)
a2<-subset(Agosto,Decil %in% c("P4","P5","P6") & Provincia==1)
a3<-subset(Agosto,Decil %in% c("P8") & Afinidad==0)
a4<-subset(Agosto,CupoUtilizado<120 & Decil %in% c("P1","P2","P3","P4","P5","P6","P7","P8"))
Campania<-rbind(a1,a2,a3,a4)
Campania<-subset(Campania,!duplicated(Identificacion))


Aux<-subset(Campania, select = c("Identificacion",
                               "EstadoCivil",
                               "AfinidadTarjeta",	
                               "ProvinciaDomicilio",
                               "Otra_Tarjeta",
                               "SoloSeguro",
                               "SegmentoTDCAbril2017",
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

aux<-left_join(Aux,CeluMailsExt, by="Identificacion")
aux[which(is.na(aux$CorreoElectronico)),]$CorreoElectronico<-aux[which(is.na(aux$CorreoElectronico)),]$CorreoElectronico_01
aux[which(is.na(aux$NumeroCelularRegistrado)),]$NumeroCelularRegistrado<-aux[which(is.na(aux$NumeroCelularRegistrado)),]$Celular_01
aux[which(nchar(aux$NumeroCelularRegistrado)<7),]$NumeroCelularRegistrado<-aux[which(nchar(aux$NumeroCelularRegistrado)<7),]$Celular_01
aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01<-aux[which(nchar(aux$CorreoElectronico_01)<2),]$Celular_01
aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01<-NA

write.csv(aux,file="Campaña2Agosto2017.csv", row.names = FALSE)
#library("xlsx", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
#library(xlsx) #load the package
#write.xlsx(Aux, file = "AgostoCampaña.xlsx", row.names = FALSE)
##############################Nuevos desertores########################333
BaseAnterior<-read.table("CampañaRetencionTDCJunio2017.txt",
                         header=TRUE,
                         sep="\t",
                         na.strings = "NA")
BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion<-paste0(0,BaseAnterior[which(nchar(BaseAnterior$Identificacion)==9),]$Identificacion)

Viejos<-semi_join(BaseAnterior,Aux,by="Identificacion")
Viejos<-subset(Viejos,select=c("Identificacion"))
Viejos["Nuevo"]<-"No"

Aux<-left_join(Aux,Viejos,by="Identificacion")
Aux$Nuevo<-as.character(Aux$Nuevo)
Aux[which(is.na(Aux$Nuevo)),]$Nuevo<-"Si"
write.csv(Aux,file="Agosto2017.csv",row.names = FALSE)
#######################Envio de base y escritura####################
aux<-subset(Aux,Decil=="P10" | Decil=="P9")
aux<-aux[order(aux$Score, decreasing = T),]
write.csv(aux,file="CampañaRetencionTDCAgosto2017.csv",row.names=FALSE)

########################Base de mails###################################
mails<-subset(Agosto,nchar(CorreoElectronico)>3) 
mails<-subset(mails,Decil=="P10" | Decil=="P9" | Decil=="P8")
Aux<-subset(mails, select = c("Identificacion",
                              "EstadoCivil",
                              "AfinidadTarjeta",	
                              "ProvinciaDomicilio",
                              "Otra_Tarjeta",
                              "SoloSeguro",
                              "Segmento_TDCFebrero2017",
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
Telefonos<-subset(Agosto,nchar(as.character(NumeroCelularRegistrado))>2 |
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
                                  "Segmento_TDCFebrero2017",
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
Aux<-subset(Agosto,Decil=="P10" | Decil=="P9")
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
                           "Segmento_TDCFebrero2017",
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
