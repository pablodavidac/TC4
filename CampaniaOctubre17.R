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

Septiembre<-tbl(DWH_CRM, "SB_TDCActivas") %>% filter(CuentaTarjetaMonitor==1)%>% collect()

Septiembre$FechaApertura <- as.Date(as.character(Agosto$FechaApertura))
#Agosto<-subset(Agosto,FechaApertura > as.Date("2016-12-31"))
table(Septiembre$Segmento_TDCAgosto2017)
#Agosto[which(Agosto$Segmento_TDCFeb




Marzo<-read.table("Base 2017 03 31.csv",
                     header=TRUE,
                     sep=",",
                     na.strings = "NA")

Marzo$Identificacion<-as.character(Marzo$Identificacion)
Marzo[which(nchar(Marzo$Identificacion)==9),]$Identificacion<-paste0(0,Marzo[which(nchar(Marzo$Identificacion)==9),]$Identificacion)
Marzo<-subset(Marzo,select=c("Identificacion","CupoUtilizado"))
colnames(Marzo)[2]<-"CupoUtilizadoT6"


###################################

Septiembre<-subset(Septiembre,select=c("Identificacion",
                                   "IdCuentaTarjeta",
                                   "Afinidad",
                                   "CupoUtilizado",
                                   "CupoUtilizadoAvance",
                                   "NumeroEntidadesSF",
                                   "Segmento_TDCAgosto2017",
                                   "FechaApertura"))

Septiembre<-subset(Septiembre,!duplicated(IdCuentaTarjeta))


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
Autorizaciones6M<- tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-10-01' 
                                                           & FechaConciliacion > '2017-03-31')%>% collect()

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

AutorizacionesSeptiembre <-tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-03-01' 
                                                               & FechaConciliacion > '2017-02-01')%>% collect()


SoloSeguro<-subset(AutorizacionesSeptiembre,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesSeptiembre,TipoConsumo=="TRJAVANCE" | 
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
Septiembre<-left_join(Septiembre,Demografica,by="Identificacion")
Septiembre<-subset(Septiembre,!duplicated(IdCuentaTarjeta))
Septiembre<-left_join(Septiembre,Autorizaciones,by="IdCuentaTarjeta")
Septiembre<-left_join(Septiembre,Marzo,by="Identificacion")



###################################Remplazar NA por ceros##################
Septiembre[which(is.na(Septiembre$Autorizaciones6M)),]$Autorizaciones6M<-0
Septiembre[which(is.na(Septiembre$ConsumosSeguro)),]$ConsumosSeguro<-0
Septiembre[which(is.na(Septiembre$Consumos)),]$Consumos<-0
Septiembre[which(is.na(Septiembre$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Septiembre$Autorizaciones6M<-as.integer(Septiembre$Autorizaciones6M)

#########################Creación de variables#####################3

Septiembre["SoloSeguro"]<-"NO"
Septiembre[which(Septiembre$Consumos==0 & Septiembre$ConsumosSeguro>0),]$SoloSeguro<-"SI"


Septiembre["DifCupoUtilizado"]<-Septiembre$CupoUtilizado-Septiembre$CupoUtilizadoT6
Septiembre[which(is.na(Septiembre$DifCupoUtilizado)),]$DifCupoUtilizado<-0


Septiembre["T6CupoUtilizado"]<-0
Septiembre$T6CupoUtilizado<-(Septiembre$CupoUtilizado-Septiembre$CupoUtilizadoT6)/Septiembre$CupoUtilizadoT6

NROW(Septiembre[which(is.infinite(Septiembre$T6CupoUtilizado)),])
NROW(Septiembre[which(is.na(Septiembre$T6CupoUtilizado)),])

Septiembre[which(is.infinite(Septiembre$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Septiembre[which(is.na(Septiembre$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
##############################################################################



##########################################################33

ClientesInteres<-subset(Septiembre,
                        Segmento_TDCAgosto2017%in%c("0. NUEVOS +6M",
                                                   "0. NUEVOS 6M",
                                                   "1. VIP",
                                                   "2. PREFERENTE",
                                                   "3. MOVILIZACION A"))

ClientesInteres["NormDifCupoUtilizado"]<-as.numeric(scale(ClientesInteres$DifCupoUtilizado))

################## Cupo Utilizado en Avances Normalizado###############3
ClientesInteres["NormCupoUtilizadoAvance"]<-as.numeric(scale(ClientesInteres$CupoUtilizadoAvance))

################## Cupo Utilizado Normalizado###############3
ClientesInteres["NormCupoUtilizado"]<-as.numeric(scale(ClientesInteres$CupoUtilizado))


Codificada<-ClientesInteres

######################Codificacion variables#######################
########################Afinidad#############################
Codificada$AfinidadTarjeta<-Codificada$Afinidad
Codificada["Afinidad"]<-"Otra afinidad"
Codificada[which(Codificada$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-"Afinidad Interes"
Codificada[which(Codificada$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-"Afinidad Interes"
Codificada[which(Codificada$AfinidadTarjeta=="BLACK"),]$Afinidad<-"Afinidad Interes"
Codificada[which(Codificada$AfinidadTarjeta=="GOLD     "),]$Afinidad<-"Afinidad Interes"


Codificada[which(Codificada$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-"Otra afinidad"
Codificada[which(Codificada$Afinidad=="NA"),]$Afinidad<-"Otra afinidad"

Codificada$Afinidad<-as.factor(Codificada$Afinidad)
########################Variable provincia#############################
Codificada["ProvinciaDomicilio"]<-Codificada$Provincia
Codificada["Provincia"]<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="AZUAY"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="EL ORO"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-"Provincia de Interes"


Codificada[which(Codificada$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="LOJA"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="CARCHI"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="MANABI"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="NAPO"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-"Otra Provincia"


Codificada[which(Codificada$Provincia=="NA"),]$Provincia<-"Otra Provincia"

Codificada$Provincia<-as.factor(Codificada$Provincia)
########################Otra tarjeta################################
Codificada["Otra_Tarjeta"]<-"NO"
Codificada[which(Codificada$NumeroEntidadesSF>0.5),]$Otra_Tarjeta<-"SI"
Codificada$Otra_Tarjeta<-as.factor(Codificada$Otra_Tarjeta)
#######################Autorizaciones 6 meses#############################
Codificada["SinAutorizaciones6M"]<-"Con autorizaciones"
Codificada[which(Codificada$Autorizaciones6M<0.5),]$SinAutorizaciones6M<-"Sin autorizaciones"
Codificada$SinAutorizaciones6M<-as.factor(Codificada$SinAutorizaciones6M)
##########################Cupo Utilizado##################################

Codificada["CodCupoUtilizado"]<-"Mayor a 1728"
Codificada[which(Codificada$CupoUtilizado<1728),]$CodCupoUtilizado<-"1522 a 1728"
Codificada[which(Codificada$CupoUtilizado<1522),]$CodCupoUtilizado<-"719 a 1522"
Codificada[which(Codificada$CupoUtilizado<719),]$CodCupoUtilizado<-"400 a 719"
Codificada[which(Codificada$CupoUtilizado<=400),]$CodCupoUtilizado<-"0 a 400"
Codificada$CodCupoUtilizado<-as.factor(Codificada$CodCupoUtilizado)

Codificada$CodCupoUtilizado<-factor(Codificada$CodCupoUtilizado, levels=c("0 a 400",
                                                                          "400 a 719",
                                                                          "719 a 1522",
                                                                          "1522 a 1728",
                                                                          "Mayor a 1728"), ordered=FALSE)
##########################Cupo Utilizado##################################

Codificada["CodDifCupoUtilizado"]<-"Mayor a Cero"
Codificada[which(Codificada$DifCupoUtilizado<(127)),]$CodDifCupoUtilizado<-"Menor a Cero"
Codificada$CodDifCupoUtilizado<-as.factor(Codificada$CodDifCupoUtilizado)
##########################Cupo Utilizado##################################

Codificada["CodCupoUtiAvance"]<-"Mayor a 204"
Codificada[which(Codificada$CupoUtilizadoAvance<204),]$CodCupoUtiAvance<-"Menor a 204"
Codificada$CodCupoUtiAvance<-as.factor(Codificada$CodCupoUtiAvance)


###########################SegmentoTDC####################
Codificada["CodSegmento"]<-"Otro Segmento"

Codificada[which(Codificada$Segmento_TDCAgosto2017%in%c("0. NUEVOS +6M",
                                                       "0. NUEVOS 6M"
                                                       #,"3. MOVILIZACION A"
)),]$CodSegmento<-"Segmento de Interes"

Codificada$CodSegmento<-as.factor(Codificada$CodSegmento)

############################CodT6CupoUtilizado###################
Codificada["CodT6CupoUtilizado"]<-"Decrece mas 100%"
Codificada[which(Codificada$T6CupoUtilizado>=(-0.99)),]$CodT6CupoUtilizado<-"Decrece menos 100%"


########################Estado Civil############################
Codificada["CodEstadoCivil"]<-1
Codificada[which(Codificada$EstadoCivil=="ESTCIVC"),]$CodEstadoCivil<-1
Codificada[which(Codificada$EstadoCivil=="ESTCIVD"),]$CodEstadoCivil<-1


Codificada[which(Codificada$EstadoCivil=="ESTCIVS"),]$CodEstadoCivil<-0
Codificada[which(Codificada$EstadoCivil=="ESTCIVU"),]$CodEstadoCivil<-0
Codificada[which(Codificada$EstadoCivil=="ESTCIVV"),]$CodEstadoCivil<-0

Codificada[which(Codificada$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1

Codificada$CodEstadoCivil<-as.factor(Codificada$CodEstadoCivil)
###########################################

#############Conjunto de entrenamiento##################
Codificada<-subset(Codificada,!duplicated(Identificacion))
#TrainD2 <- createDataPartition(Codificada$IdCuentaTarjeta, p=0.7, list=FALSE)



####################Deciles###########################
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloNuevoChurn.Rdata")

Codificada["Score"]<-predict(modA4,newdata=Codificada,type='response')
Codificada["Decil"]<-"NA"

Codificada<-subset(Codificada,!duplicated(Identificacion))

fitted.results <- predict(modA4,newdata=Codificada,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
################Codificaccion de P1 - P10 ###########################
Codificada[which(Codificada$Score <= quan[2]),]$Decil<-"P1"
Codificada[which(Codificada$Score <=quan[3] & Codificada$Score > quan[2]),]$Decil<-"P2"
Codificada[which(Codificada$Score <=quan[4] & Codificada$Score > quan[3]),]$Decil<-"P3"
Codificada[which(Codificada$Score <=quan[5] & Codificada$Score > quan[4]),]$Decil<-"P4"
Codificada[which(Codificada$Score <=quan[6] & Codificada$Score > quan[5]),]$Decil<-"P5"
Codificada[which(Codificada$Score <=quan[7] & Codificada$Score > quan[6]),]$Decil<-"P6"
Codificada[which(Codificada$Score <=quan[8] & Codificada$Score > quan[7]),]$Decil<-"P7"
Codificada[which(Codificada$Score <=quan[9] & Codificada$Score > quan[8]),]$Decil<-"P8"
Codificada[which(Codificada$Score <=quan[10] & Codificada$Score > quan[9]),]$Decil<-"P9"
Codificada[which(Codificada$Score >=quan[10]),]$Decil<-"P10"


View(table(Codificada$Decil))

Base<-subset(Codificada,Decil %in%c("P10","P9"))

##########################################################



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

Base<-left_join(Base,ControlAcceso,by="Identificacion")

Base<-left_join(Base,Mails,by=c("Identificacion"))
Base<-subset(Base,!duplicated(Identificacion))

Base<-left_join(Base,Rep_Telefono,by="Identificacion")



##################################Poner Nombre################

#Agosto<-left_join(Agosto,Demografica,by=c("IdClienteNatural","Identificacion"))
#Agosto<-subset(Agosto,!duplicated(Identificacion))

#aux<-subset(aux,FechaApertura > as.Date("2017-01-31") & !is.na(FechaApertura))
#############################################################################3

########################Escoger variables y depurar mails#########################
Base$CorreoElectronico<-as.character(Base$CorreoElectronico)
NROW(Base[which(nchar(Base$CorreoElectronico)>3),])  #6546
NROW(Base[which(is.na(Base$CorreoElectronico)),])  #6546
Base[which(is.na(Base$CorreoElectronico)),]$CorreoElectronico<-Base[which(is.na(Base$CorreoElectronico)),]$CorreoElectronicoRegistrado
Base[which(nchar(Base$CorreoElectronico)<3),]$CorreoElectronico<-Base[which(nchar(Base$CorreoElectronico)<2),]$CorreoElectronicoRegistrado
Base$CorreoElectronico<-tolower(Base$CorreoElectronico)
Aux<-subset(Base, select = c("Identificacion",
                               "EstadoCivil",
                               "AfinidadTarjeta",	
                               "ProvinciaDomicilio",
                               "Otra_Tarjeta",
                               "SoloSeguro",
                               "Segmento_TDCAgosto2017",
                               "Score",
                               "Decil",
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
aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01<-aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01
aux[which(nchar(aux$CorreoElectronico_01)<2),]$CorreoElectronico_01<-NA
################################3
NROW(subset(aux,!is.na(CorreoElectronico)))
#81295


NROW(subset(aux,!is.na(NumeroCelularRegistrado)))
#41260
NROW(subset(aux,nchar(CorreoElectronico)>2))

NROW(subset(aux,nchar(NumeroCelularRegistrado)>2))

Aux<-aux[,c(1:20)]
Aux <-Aux[order(-Aux$Score),] 

write.csv(Aux,file="Septiembre2017.csv", row.names = FALSE)
