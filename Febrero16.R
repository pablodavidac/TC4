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


Base20160229III<-read.table("Base 2016 02 29III.csv",
                           header=TRUE,
                           sep=",",
                           na.strings = "NA")

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
#Febrero<-subset(Base20160229,CuentaTarjetaMonitor==1 & ActivadaFebrero2016==0)

###################################################################################
FebreroI<-subset(Base20160229III,select=c(
                                        #"Identificacion",
                                         "IdCuentaTarjeta",
                                         "Afinidad"))
FebreroI$Afinidad<-as.character(FebreroI$Afinidad)

Febrero<-subset(Base20160229,select=c("Identificacion",
                                 "IdCuentaTarjeta",
                                 "CupoUtilizado",
                                 "CupoUtilizadoAvance",
                                 "NumeroEntidadesSF",
                                 "Segmento_TDCEnero2016"))
Febrero<-left_join(Febrero,FebreroI,by=c("IdCuentaTarjeta"))

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
#############################################################################

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
Febrero["AfinidadTarjeta"]<-"PLATINUM"
Febrero$AfinidadTarjeta<-Febrero$Afinidad

Febrero["Afinidad"]<-"NA"
Febrero[which(Febrero$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-1
Febrero[which(Febrero$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Febrero[which(Febrero$AfinidadTarjeta=="GOLD     "),]$Afinidad<-1

Febrero[which(Febrero$AfinidadTarjeta=="BLACK"),]$Afinidad<-0
Febrero[which(Febrero$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-0
Febrero[which(Febrero$Afinidad=="NA"),]$Afinidad<-0

##################Cupo Utilizado t-6################

Febrero[which(is.na(Febrero$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0




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
Febrero<-subset(Febrero,Segmento_TDCEnero2017=="0. NUEVOS +6M" |
                  Segmento_TDCEnero2017=="0. NUEVOS 6M" |
                  Segmento_TDCEnero2017=="1. VIP" |
                  Segmento_TDCEnero2017=="2. PREFERENTE" |
                  Segmento_TDCEnero2017=="3. MOVILIZACION A")


Febrero["NormCupoUtilizado"]<-(Febrero$CupoUtilizado-mean(Febrero$CupoUtilizado))/sd(Febrero$CupoUtilizado)

#Febrero<-left_join(Febrero,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
FebreroCompleto<-Febrero
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")


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

