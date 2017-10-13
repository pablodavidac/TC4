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
library("lubridate", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("xlsx", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
setwd("d:/Users_info/ALBANPD/My Documents/Bases")




###########################Cargar bases TDC###########################

TDCJunio<-read.table("Base 2017 06 30.txt",
                     header=TRUE,
                     sep="\t",
                     na.strings = "NA")

TDCMayo<-read.table("Base 2017 05 31.txt",
                     header=TRUE,
                     sep="\t",
                     na.strings = "NA")

TDCAbril<-read.table("Base 2017 04 30.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")

TDCMarzo<-read.table("Base 2017 03 31.txt",
                     header=TRUE,
                     sep="\t",
                     na.strings = "NA")

TDCFeb<-read.table("Base 2017 02 28.txt",
                     header=TRUE,
                     sep="\t",
                     na.strings = "NA")

TDCEne<-read.table("Base 2017 01 31.txt",
                   header=TRUE,
                   sep="\t",
                   na.strings = "NA")
###############Filtrar variables##################

TDCJunio<-subset(TDCJunio,CuentaTarjetaMonitor==1)
TDCJunio<-subset(TDCJunio,select=c("FechaCorte","Identificacion"))
TDCJunio$FechaCorte<-as.character(TDCJunio$FechaCorte)
TDCJunio$FechaCorte<-as.Date(TDCJunio$FechaCorte)
  
TDCMayo<-subset(TDCMayo,CuentaTarjetaMonitor==1)
TDCMayo<-subset(TDCMayo,select=c("FechaCorte","Identificacion"))
TDCMayo$FechaCorte<-as.character(TDCMayo$FechaCorte)
TDCMayo$FechaCorte<-as.Date(TDCMayo$FechaCorte)

TDCAbril<-subset(TDCAbril,CuentaTarjetaMonitor==1)
TDCAbril<-subset(TDCAbril,select=c("FechaCorte","Identificacion"))
TDCAbril$FechaCorte<-as.character(TDCAbril$FechaCorte)
TDCAbril$FechaCorte<-as.Date(TDCAbril$FechaCorte)


TDCMarzo<-subset(TDCMarzo,CuentaTarjetaMonitor==1)
TDCMarzo<-subset(TDCMarzo,select=c("FechaCorte","Identificacion"))
TDCMarzo$FechaCorte<-as.character(TDCMarzo$FechaCorte)
TDCMarzo$FechaCorte<-as.Date(TDCMarzo$FechaCorte)

TDCFeb<-subset(TDCFeb,CuentaTarjetaMonitor==1)
TDCFeb<-subset(TDCFeb,select=c("FechaCorte","Identificacion"))
TDCFeb$FechaCorte<-as.character(TDCFeb$FechaCorte)
TDCFeb$FechaCorte<-as.Date(TDCFeb$FechaCorte)

TDCEne<-subset(TDCEne,CuentaTarjetaMonitor==1)
TDCEne<-subset(TDCEne,select=c("FechaCorte","Identificacion"))
TDCEne$FechaCorte<-as.character(TDCEne$FechaCorte)
TDCEne$FechaCorte<-as.Date(TDCEne$FechaCorte)

TDC<-rbind(TDCEne,TDCFeb,TDCMarzo,TDCAbril,TDCMayo,TDCJunio)
TDC$FechaCorte<-as.Date(paste0(year(TDC$FechaCorte),"/",month(TDC$FechaCorte),"/",01),format="%Y/%m/%d")
TDC["Producto"]<-"Alia"
##############################Creditos############################
######################Cargar Saldos 231##########################
#################Saldo 231######################3

odbcChannel <-odbcConnect("DWH_TrabajoEstudios") #Databases

sqlTables(odbcChannel, tableType = "TABLE")
sqlColumns(odbcChannel, "HISTORICO.SaldosCartera231")

SaldosCartera231 <- sqlExecute(odbcChannel, "SELECT * FROM HISTORICO.SaldosCartera231 WHERE FechaCorte > '2017-01-01'" , fetch = TRUE)
odbcClose(odbcChannel)


SaldosCartera231$FechaCorte<-as.character(SaldosCartera231$FechaCorte)
SaldosCartera231$FechaCorte<-as.Date(SaldosCartera231$FechaCorte,"%Y-%m-%d")
SaldosCartera231$FechaCorte<-as.Date(paste0(year(SaldosCartera231$FechaCorte),"/",month(SaldosCartera231$FechaCorte),"/",01),format="%Y/%m/%d")

#################Poner Productos##########################
Creditos<-subset(SaldosCartera231,Estado!="CASTIG")

Creditos["Producto"]<-NA
Creditos[which(Creditos$CodigoProducto=="MICCAMTRANSF" |
                 Creditos$CodigoProducto=="MICNORMALIZ"  |
                 Creditos$CodigoProducto=="MICROCAMPANIA"  |
                 Creditos$CodigoProducto=="MICRORURAL"  |
                 Creditos$CodigoProducto=="MICROURBANO"  |
                 Creditos$CodigoProducto=="MICRURTRANSF"  |
                 Creditos$CodigoProducto=="MICURBCOMPRA"  |
                 Creditos$CodigoProducto=="MICURBTRANSF" ),]$Producto<-"Microcrédito"


Creditos[which(Creditos$CodigoProducto=="OLLACONPRE" |
                 Creditos$CodigoProducto=="OLLACONSEG" |
                 Creditos$CodigoProducto=="OLLACONSU" |
                 Creditos$CodigoProducto=="OLLAEMERFCUO" |
                 Creditos$CodigoProducto=="OLLAMICPRE"  |
                 Creditos$CodigoProducto=="OLLAMICRO" |
                 Creditos$CodigoProducto=="OLLAMICSEG"),]$Producto<-"Olla de Oro"



Creditos[which(
  Creditos$CodigoProducto=="POINT" |
    Creditos$CodigoProducto=="CINTICOMP" |
    Creditos$CodigoProducto=="CARTIMEX" |
    Creditos$CodigoProducto=="CHEPEREZ" |
    Creditos$CodigoProducto=="COMPTECO" |
    Creditos$CodigoProducto=="MOTOFACIL" |
    Creditos$CodigoProducto=="INDIANMOTOS" |
    Creditos$CodigoProducto=="FDGCOMPUTER" |
    Creditos$CodigoProducto=="HOGARESSALUDAB" |
    Creditos$CodigoProducto=="CONSTRUAHORA" |
    Creditos$CodigoProducto=="MADERASBOSQUE" |
    Creditos$CodigoProducto=="ELECTROFACIL" |
    Creditos$CodigoProducto=="BAYANGO" |
    Creditos$CodigoProducto=="EYSCORP" |
    Creditos$CodigoProducto=="TARDI" |
    Creditos$CodigoProducto=="CASAEXITO" |
    Creditos$CodigoProducto=="DISTRIBPACIFICO" |
    Creditos$CodigoProducto=="ELPENION" |
    Creditos$CodigoProducto=="CONSMAPE" |
    Creditos$CodigoProducto=="CHIMASA" |
    Creditos$CodigoProducto=="RAINBOW" |
    Creditos$CodigoProducto=="TECNOHOGAR" |
    Creditos$CodigoProducto=="ARTEYMUEBLE" |
    Creditos$CodigoProducto=="CARLETHY" |
    Creditos$CodigoProducto=="MASTERPC" |
    Creditos$CodigoProducto=="DISTRIBUIDORES" |
    Creditos$CodigoProducto=="ELECTROFACILDOS" |
    Creditos$CodigoProducto=="CINTICOMPDOS" |
    Creditos$CodigoProducto=="MOTORUNO" |
    Creditos$CodigoProducto=="CONSTRUMAJI" |
    Creditos$CodigoProducto=="TECNOHOGARDOS" |
    Creditos$CodigoProducto=="UNIFER" |
    Creditos$CodigoProducto=="PEDROYPABLO" |
    Creditos$CodigoProducto=="DISTRIBUIDORES" |
    Creditos$CodigoProducto=="HRNET" |
    Creditos$CodigoProducto=="LAURORA"),]$Producto<-"Casas Comerciales"

  
  
  Creditos[which(Creditos$CodigoProducto=="UNICREDITO" | 
                   Creditos$CodigoProducto=="REESTRTRJ"|
                   Creditos$CodigoProducto=="CONNORMALIZ" ),]$Producto<-"Unicrédito"

Creditos<-subset(Creditos,select=c("FechaCorte","Identificacion","Producto"))
#Aux<-subset(Creditos,!is.na(Producto))
#write.csv(Aux,file="Creditos6.csv",row.names = FALSE)
#############################Cuentas##################################

#####################Pasivo#################################
odbcChannel <-odbcConnect("Cuenta") #Databases

sqlTables(odbcChannel, tableType = "TABLE")
sqlColumns(odbcChannel, "CuentaCliente")


CuentaCliente <- sqlExecute(odbcChannel, "SELECT * FROM CuentaCliente", fetch = TRUE)
Producto<- sqlExecute(odbcChannel, "SELECT * FROM Producto", fetch = TRUE)
SubProducto<- sqlExecute(odbcChannel, "SELECT * FROM SubProducto", fetch = TRUE)
colnames(Producto)[2]<-"Producto"
colnames(Producto)[3]<-"NombreProducto"
colnames(SubProducto)[3]<-"SubProducto"
colnames(SubProducto)[4]<-"NombreSubProducto"
odbcClose(odbcChannel)

CuentaCliente<-left_join(CuentaCliente,Producto,by="Producto")
CuentaCliente<-left_join(CuentaCliente,SubProducto,by="SubProducto")

ClientesCuentas<-subset(CuentaCliente,IdTipoEstadoCuenta==1 & IdInstitucion==2)
#ClientesCuentas<-subset(ClientesCuentas,!duplicated(IdCliente))
ClientesCuentas<-subset(ClientesCuentas,select = c("IdCliente","NombreProducto",
                                                   "NombreSubProducto",
                                                   "TipoCuentaContable",
                                                   "TipoCuenta.x",
                                                   "FechaCreacionCuenta"))

ClientesCuentas$FechaCreacionCuenta<-as.character(ClientesCuentas$FechaCreacionCuenta)
ClientesCuentas$FechaCreacionCuenta<-as.Date(ClientesCuentas$FechaCreacionCuenta,"%Y-%m-%d")
ClientesCuentas$FechaCreacionCuenta<-as.Date(paste0(year(ClientesCuentas$FechaCreacionCuenta),"/",month(ClientesCuentas$FechaCreacionCuenta),"/",01),format="%Y/%m/%d")

ClientesCuentas["Producto"]<-NA
ClientesCuentas[which(ClientesCuentas$TipoCuenta.x=="CTACORRIENTE"),]$Producto<-"Cuentas Corrientes"
ClientesCuentas[which(ClientesCuentas$TipoCuenta.x=="CTAAHORRO"),]$Producto<-"Cuentas de Ahorros"

################Informacion del cliente################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Cliente")

ClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM ClienteNatural WHERE IdInstitucion = 2", fetch = TRUE)

Cliente <- sqlExecute(odbcChannel, "SELECT * FROM Cliente", fetch = TRUE)

InformacionDemografica<- sqlExecute(odbcChannel, "SELECT * FROM InformacionDemografica", fetch = TRUE)

odbcClose(odbcChannel)


Demografica<-left_join(ClienteNatural,Cliente,by="IdCliente")

Demografica<-subset(Demografica,select=c("Identificacion",
                                         "IdCliente"
))

ClientesCuentas<-left_join(ClientesCuentas,Demografica,by="IdCliente")
ClientesCuentas<-subset(ClientesCuentas,!is.na(Identificacion))
ClientesCuentas["FechaCorte"]<-ClientesCuentas$FechaCreacionCuenta
ClientesCuentas<-subset(ClientesCuentas,select=c("FechaCorte","Identificacion","Producto"))
CCEne<-subset(ClientesCuentas,FechaCorte<"2017-01-02")
CCEne$FechaCorte<-as.Date("2017-01-01")
CCFeb<-subset(ClientesCuentas,FechaCorte<"2017-02-02")
CCFeb$FechaCorte<-as.Date("2017-02-01")
CCMar<-subset(ClientesCuentas,FechaCorte<"2017-03-02")
CCMar$FechaCorte<-as.Date("2017-03-01")
CCAbr<-subset(ClientesCuentas,FechaCorte<"2017-04-02")
CCAbr$FechaCorte<-as.Date("2017-04-01")
CCMay<-subset(ClientesCuentas,FechaCorte<"2017-05-02")
CCMay$FechaCorte<-as.Date("2017-05-01")
CCJun<-subset(ClientesCuentas,FechaCorte<"2017-06-02")
CCJun$FechaCorte<-as.Date("2017-06-01")

Pasivo<-rbind(CCEne,CCFeb,CCMar,CCAbr,CCMay,CCJun)
######################Base de Clientes###########################

Solidario<-rbind(TDC,Creditos,Pasivo)


####################Tabla con  correos validados######################

DWH_Reporte <- dbConnect(odbc::odbc(),
                         Driver    = "SQL Server", 
                         Server    = "BSUIO-DWH02",
                         Database  = "DWH_Reporte")



ClienteCorreoElectronico<- tbl(DWH_Reporte, "ClienteCorreoElectronico") %>%
  filter(Resultado %in% c("Deliverable","Risky") & EsValido==1) %>% collect()

ClienteCorreoElectronico$FechaCreacion<-as.Date(ClienteCorreoElectronico$FechaCreacion)
ClienteCorreoElectronico$Identificacion<-as.factor(ClienteCorreoElectronico$Identificacion)
ClienteCorreoElectronico$FechaCorte<-ClienteCorreoElectronico$FechaCreacion
Correos<-subset(ClienteCorreoElectronico,select=c("FechaCorte","Tipo","Identificacion","EsValido","FechaCreacion"))
Correos$FechaCorte<-as.Date(paste0(year(Correos$FechaCorte),"/",month(Correos$FechaCorte),"/",01),format="%Y/%m/%d")
Correos$FechaCreacion<-as.Date(paste0(year(Correos$FechaCreacion),"/",month(Correos$FechaCreacion),"/",01),format="%Y/%m/%d")


Correos[which(Correos$Tipo=="CANALES"),]$Tipo<-"Notificación"
Correos[which(Correos$Tipo=="CLIENTES"),]$Tipo<-"BankPlus"
Correos[which(Correos$Tipo=="PLANRECOMPENSA"),]$Tipo<-"Plan Recompensas"
write.csv(Correos,file = "Correos.csv",row.names = FALSE)



CorreosEne<-subset(Correos,FechaCorte<="2017-01-31")
CorreosEne$FechaCorte<-as.Date("2017-01-01")
CorreosFeb<-subset(Correos,FechaCorte<="2017-02-28")
CorreosFeb$FechaCorte<-as.Date("2017-02-01")
CorreosMar<-subset(Correos,FechaCorte<="2017-03-31")
CorreosMar$FechaCorte<-as.Date("2017-03-01")
CorreosAbr<-subset(Correos,FechaCorte<="2017-04-30")
CorreosAbr$FechaCorte<-as.Date("2017-04-01")
CorreosMay<-subset(Correos,FechaCorte<="2017-05-31")
CorreosMay$FechaCorte<-as.Date("2017-05-01")
CorreosJun<-subset(Correos,FechaCorte<="2017-06-30")
CorreosJun$FechaCorte<-as.Date("2017-06-01")

Correos<-rbind(CorreosEne,CorreosFeb,CorreosMar,CorreosAbr,CorreosMay,CorreosJun)
Correos$Identificacion<-as.character(Correos$Identificacion)
aux<- Correos %>% group_by(FechaCorte,Tipo) %>% summarise(Correos =sum(EsValido)) 
write.csv(aux,file = "CorreosxTipo.csv",row.names = FALSE)
###########################Cruze de base##########################

Base<-left_join(Correos,Solidario,by=c("FechaCorte"="FechaCorte","Identificacion"="Identificacion"))
Base[which(is.na(Base$Producto)),]$Producto<-"Excliente"
Base[which(Base$Tipo=="CANALES"),]$Tipo<-"Notificación"
Base[which(Base$Tipo=="CLIENTES"),]$Tipo<-"BankPlus"
Base[which(Base$Tipo=="PLANRECOMPENSA"),]$Tipo<-"Plan Recompensas"
write.csv(Base,file = "Notificacion.csv",row.names = FALSE)

##########################Columna de Orden########################
Base["SortOrder"]<-NA
Base$SortOrder<-as.numeric(Base$SortOrder)
Base[which(Base$Producto=="Excliente"),]$SortOrder<-1
Base[which(Base$Producto=="Alia"),]$SortOrder<-2
Base[which(Base$Producto=="Cuentas de Ahorros"),]$SortOrder<-3
Base[which(Base$Producto=="Olla de Oro"),]$SortOrder<-4
Base[which(Base$Producto=="Microcrédito"),]$SortOrder<-5
Base[which(Base$Producto=="Unicrédito"),]$SortOrder<-6
Base[which(Base$Producto=="Casas Comerciales"),]$SortOrder<-7
Base[which(Base$Producto=="Cuentas Corrientes"),]$SortOrder<-8

#################################################
Solidario[which(is.na(Solidario$Producto)),]$Producto<-"Excliente"
write.csv(Solidario,file = "Clientes.csv",row.names = FALSE)

###################################################

C<-read.csv2("NumClientes.csv",sep = ",",header = TRUE,dec = ".")
C$Producto<-as.character(C$Producto)
C$FechaCorte<-as.character(C$FechaCorte)
C$FechaCorte<-as.Date(C$FechaCorte,"%d/%m/%Y")

Co<-read.csv2("Correos.csv",sep = ",",header = TRUE,dec = ".")
Co$Producto<-as.character(Co$Producto)
Co$Tipo<-as.character(Co$Tipo)
Co$FechaCorte<-as.character(Co$FechaCorte)
Co$FechaCorte<-as.Date(Co$FechaCorte,"%d/%m/%Y")

aux<-left_join(Co,C,by=c("Producto"="Producto","FechaCorte"="FechaCorte"))
aux["Porcentaje"]<-aux$Correos/aux$Clientes
write.csv(aux,file = "Aux.csv",row.names = FALSE)

####################Solo notificaciones#################
Negocio <- dbConnect(odbc::odbc(),
                         Driver    = "SQL Server", 
                         Server    = "BSUIO-DWH02",
                         Database  = "Negocio")



ControlAcceso<- tbl(Negocio, "ControlAcceso") %>%
  dplyr::select(NumeroIdentificacion,
                NumeroCelularRegistrado,
                CorreoElectronicoRegistrado,
                EsActivo,
                FechaCreacion,
                FechaRegistro) %>% collect()

ControlAcceso$FechaCreacion<-as.character(ControlAcceso$FechaCreacion)
ControlAcceso$FechaCreacion<-as.Date(ControlAcceso$FechaCreacion,"%Y-%m-%d")

ControlAcceso$FechaRegistro<-as.character(ControlAcceso$FechaRegistro)
ControlAcceso$FechaRegistro<-as.Date(ControlAcceso$FechaRegistro,"%Y-%m-%d")

ControlAcceso["FechaCorte"]<-NA
ControlAcceso[which(nchar(ControlAcceso$FechaRegistro)>4),]$FechaCorte<-ControlAcceso[which(nchar(ControlAcceso$FechaRegistro)>4),]$FechaRegistro
ControlAcceso[which(nchar(ControlAcceso$FechaCreacion)>4),]$FechaCorte<-ControlAcceso[which(nchar(ControlAcceso$FechaCreacion)>4),]$FechaCreacion

ControlAcceso$FechaCorte<-as.Date(ControlAcceso$FechaCorte,"%Y-%m-%d")

############################Base de Celulares de Notificacion#############
CelNot<-ControlAcceso[which(nchar(ControlAcceso$NumeroCelularRegistrado)>9),]
CelNot<-subset(CelNot,select=c("NumeroIdentificacion","NumeroCelularRegistrado","FechaCorte"))
colnames(CelNot)[1]<-"Identificacion"

CelNot$FechaCorte<-as.Date(paste0(year(CelNot$FechaCorte),"/",month(CelNot$FechaCorte),"/",01),format="%Y/%m/%d")


#######################Dispositivos de noficaciones por mes################
CelNotEne<-subset(CelNot,FechaCorte<="2017-01-31")
CelNotEne$FechaCorte<-as.Date("2017-01-01")
CelNotFeb<-subset(CelNot,FechaCorte<="2017-02-28")
CelNotFeb$FechaCorte<-as.Date("2017-02-01")
CelNotMar<-subset(CelNot,FechaCorte<="2017-03-31")
CelNotMar$FechaCorte<-as.Date("2017-03-01")
CelNotAbr<-subset(CelNot,FechaCorte<="2017-04-30")
CelNotAbr$FechaCorte<-as.Date("2017-04-01")
CelNotMay<-subset(CelNot,FechaCorte<="2017-05-31")
CelNotMay$FechaCorte<-as.Date("2017-05-01")
CelNotJun<-subset(CelNot,FechaCorte<="2017-06-30")
CelNotJun$FechaCorte<-as.Date("2017-06-01")

CelNotMes<-rbind(CelNotEne,CelNotFeb,CelNotMar,CelNotAbr,CelNotMay,CelNotJun)


#table(CelNotMes$FechaCorte)

ClientesCelNot<-left_join(CelNot,Solidario,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))

