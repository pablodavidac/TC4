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


Febrero17<-read.table("Base 2017 02 28.csv",
                      header=TRUE,
                      sep=",",
                      na.strings = "NA")

DesertoresAlia<-read.table("DesertoresAlia2.csv",
                           header=TRUE,
                           sep=",",
                           na.strings = "NA")


#DesertoresAlia$ObjetivoCharacter<-as.character(DesertoresAlia$ObjetivoCharacter)

Agosto16<-read.table("Base 2016 08 31.csv",
                     header=TRUE,
                     sep=",",
                     na.strings = "NA")


##############
DesertoresAlia$Identificacion<-as.character(DesertoresAlia$Identificacion)
DesertoresAlia[which(nchar(DesertoresAlia$Identificacion)==9),]$Identificacion<-paste0(0,DesertoresAlia[which(nchar(DesertoresAlia$Identificacion)==9),]$Identificacion)
DesertoresAlia["ObjetivoCharacter"]<-"Desertor"
DesertoresAlia["Objetivo"]<-1
DesertoresAlia<-subset(DesertoresAlia,EstadoTarjeta=="ACTIVA" & TieneReestructura==0)
DesertoresAlia<-subset(DesertoresAlia,select=c("Identificacion","Mes","Objetivo","ObjetivoCharacter"))
DesertoresAlia<-subset(DesertoresAlia,Mes>=3)
DesertoresAlia<-subset(DesertoresAlia,Mes<=8)

#MuestraDesertores<-DesertoresAlia %>% group_by(Mes) %>% sample_n(size = 517,replace=FALSE)
#MuestraDesertores<-subset(MuestraDesertores,!duplicated(Identificacion))

Agosto16$Identificacion<-as.character(Agosto16$Identificacion)
Agosto16[which(nchar(Agosto16$Identificacion)==9),]$Identificacion<-paste0(0,Agosto16[which(nchar(Agosto16$Identificacion)==9),]$Identificacion)
Agosto16<-subset(Agosto16,select=c("Identificacion","CupoUtilizado"))
colnames(Agosto16)[2]<-"CupoUtilizadoT6"

Febrero17$Identificacion<-as.character(Febrero17$Identificacion)
Febrero17<-subset(Febrero17,CuentaTarjetaMonitor==1)
table(Febrero17$Segmento_TDCEnero2017)
###################################

Febrero<-subset(Febrero17,select=c("Identificacion",
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
Autorizaciones6M<- tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-03-01' 
                                                           & FechaConciliacion > '2016-08-31')%>% collect()

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

AutorizacionesAgosto <-tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2017-03-01' 
                                                               & FechaConciliacion > '2017-02-01')%>% collect()


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
Febrero<-left_join(Febrero,Demografica,by="Identificacion")
Febrero<-subset(Febrero,!duplicated(IdCuentaTarjeta))
Febrero<-left_join(Febrero,Autorizaciones,by="IdCuentaTarjeta")
Febrero<-left_join(Febrero,Agosto16,by="Identificacion")


Febrero<-left_join(Febrero,DesertoresAlia,by="Identificacion")
#Agosto<-left_join(Agosto,MuestraDesertores,by="Identificacion")


###################################Remplazar NA por ceros##################
Febrero[which(is.na(Febrero$Autorizaciones6M)),]$Autorizaciones6M<-0
Febrero[which(is.na(Febrero$ConsumosSeguro)),]$ConsumosSeguro<-0
Febrero[which(is.na(Febrero$Consumos)),]$Consumos<-0
Febrero[which(is.na(Febrero$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Febrero[which(is.na(Febrero$Objetivo)),]$Objetivo<-0
Febrero[which(is.na(Febrero$ObjetivoCharacter)),]$ObjetivoCharacter<-"No desertor"
Febrero$ObjetivoCharacter<-as.factor(Febrero$ObjetivoCharacter)
Febrero$Autorizaciones6M<-as.integer(Febrero$Autorizaciones6M)

#########################Creación de variables#####################3

Febrero["SoloSeguro"]<-"NO"
Febrero[which(Febrero$Consumos==0 & Febrero$ConsumosSeguro>0),]$SoloSeguro<-"SI"


Febrero["DifCupoUtilizado"]<-Febrero$CupoUtilizado-Febrero$CupoUtilizadoT6
Febrero[which(is.na(Febrero$DifCupoUtilizado)),]$DifCupoUtilizado<-0


Febrero["T6CupoUtilizado"]<-0
Febrero$T6CupoUtilizado<-(Febrero$CupoUtilizado-Febrero$CupoUtilizadoT6)/Febrero$CupoUtilizadoT6

NROW(Febrero[which(is.infinite(Febrero$T6CupoUtilizado)),])
NROW(Febrero[which(is.na(Febrero$T6CupoUtilizado)),])

Febrero[which(is.infinite(Febrero$T6CupoUtilizado)),]$T6CupoUtilizado<-1
Febrero[which(is.na(Febrero$T6CupoUtilizado)),]$T6CupoUtilizado<-(-1)
##############################################################################



##########################################################33

ClientesInteres<-subset(Febrero,
                        Segmento_TDCEnero2017%in%c("0. NUEVOS +6M",
                                                   "0. NUEVOS 6M",
                                                   "1. VIP",
                                                   "2. PREFERENTE",
                                                   "3. MOVILIZACION A"))
Copia<-ClientesInteres
#Buenos<-subset(ClientesInteres,Objetivo==0)
#Malos<-subset(ClientesInteres,Objetivo==1) %>% group_by(Mes) %>% sample_n(size = 317,replace=FALSE)
#ClientesInteres<-rbind(as.data.frame(Buenos),as.data.frame(Malos))
##################Dif Cupo Utilizado Normalizado###############3
ClientesInteres["NormDifCupoUtilizado"]<-as.numeric(scale(ClientesInteres$DifCupoUtilizado))

################## Cupo Utilizado en Avances Normalizado###############3
ClientesInteres["NormCupoUtilizadoAvance"]<-as.numeric(scale(ClientesInteres$CupoUtilizadoAvance))

################## Cupo Utilizado Normalizado###############3
ClientesInteres["NormCupoUtilizado"]<-as.numeric(scale(ClientesInteres$CupoUtilizado))


#######################Arbol###########################
table(ClientesInteres$Objetivo)
#Desertor No desertor 
#2062      103354 

########################Malos####################
#Desertores<-subset(ClientesInteres,Objetivo==1)
n<-50
Malos<-do.call("rbind", replicate(n, subset(ClientesInteres, Objetivo==1), simplify = FALSE))
Buenos<-sample_n(subset(ClientesInteres,Objetivo==0), 103100,replace=FALSE)

BalanceadaD<-rbind(as.data.frame(Buenos),as.data.frame(Malos))
BalanceadaD %>% count(Objetivo)


tree2 <- rpart(ObjetivoCharacter ~ 
                 NormCupoUtilizado+
                 #CupoUtilizadoT6+
                 #NormCupoUtilizadoAvance+
                 Afinidad+
                 NumeroEntidadesSF+
                 Segmento_TDCEnero2017+
                 Provincia+
                 Autorizaciones6M+
                 #SoloSeguro+
                 T6CupoUtilizado+
                 CupoUtilizadoAvance+
                 EstadoCivil
               ,data = BalanceadaD, control = rpart.control(cp = 0.0010))



plot(tree2)
prp(tree2,  cex = 0.8, extra = 0)
prp(tree2, branch.type=5, yesno=FALSE, faclen=0)
rpart.plot(tree2,  cex = 0.4,extra=0)
rpart.plot(tree2,  cex = 0.5,extra=0)
###################Codificacion##################
Copia<-Codificada
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

Codificada[which(Codificada$Segmento_TDCEnero2017%in%c("0. NUEVOS +6M",
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
table(trainingD$Objetivo)
#Desertor No desertor 
#1399      71712 
#############Conjunto de entrenamiento##################
Codificada<-subset(Codificada,!duplicated(Identificacion))
#TrainD2 <- createDataPartition(Codificada$IdCuentaTarjeta, p=0.7, list=FALSE)




trainingD <- Codificada[ TrainD2, ]
testingD <- Codificada[ -TrainD2, ]
trainingD %>% count(Objetivo)
testingD %>% count(Objetivo)



#######################Balancear base training################3
#Desertores<-subset(ClientesInteres,Objetivo==1)
n<-50
Malos<-do.call("rbind", replicate(n, subset(trainingD, Objetivo==1), simplify = FALSE))
Buenos<-sample_n(subset(trainingD,Objetivo==0), 71450,replace=FALSE)

trainingD<-rbind(as.data.frame(Buenos),as.data.frame(Malos))
trainingD %>% count(Objetivo)

####################Corrida del modelo###############

modA4 <- glm(Objetivo~ 
               Afinidad+
               Provincia+
               #NumeroEntidadesSF+
               CodT6CupoUtilizado+
               Otra_Tarjeta+
               #Autorizaciones6M+
               #SinAutorizaciones6M*CodCupoUtilizado+
               #CodDifCupoUtilizado+
               SinAutorizaciones6M+
               #NormDifCupoUtilizado+
               #NormCupoUtilizadoAvance+
               NormCupoUtilizado+
               CodEstadoCivil
             +CodSegmento
             #+SoloSeguro
             #SinAutorizaciones6M+
             +CodCupoUtiAvance
             , data=trainingD,
             family="binomial"(link = "logit"))
summary(modA4)


####################Deciles###########################


testingD["Score"]<-predict(modA4,newdata=testingD,type='response')
testingD["Decil"]<-"NA"

testingD<-subset(testingD,!duplicated(Identificacion))

fitted.results <- predict(modA4,newdata=testingD,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
################Codificaccion de P1 - P10 ###########################
testingD[which(testingD$Score <= quan[2]),]$Decil<-"P1"
testingD[which(testingD$Score <=quan[3] & testingD$Score > quan[2]),]$Decil<-"P2"
testingD[which(testingD$Score <=quan[4] & testingD$Score > quan[3]),]$Decil<-"P3"
testingD[which(testingD$Score <=quan[5] & testingD$Score > quan[4]),]$Decil<-"P4"
testingD[which(testingD$Score <=quan[6] & testingD$Score > quan[5]),]$Decil<-"P5"
testingD[which(testingD$Score <=quan[7] & testingD$Score > quan[6]),]$Decil<-"P6"
testingD[which(testingD$Score <=quan[8] & testingD$Score > quan[7]),]$Decil<-"P7"
testingD[which(testingD$Score <=quan[9] & testingD$Score > quan[8]),]$Decil<-"P8"
testingD[which(testingD$Score <=quan[10] & testingD$Score > quan[9]),]$Decil<-"P9"
testingD[which(testingD$Score >=quan[10]),]$Decil<-"P10"



View(table(testingD$ObjetivoCharacter,testingD$Decil,testingD$Mes))
table(testingD$Decil,testingD$ObjetivoCharacter)

