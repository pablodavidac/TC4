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
library("xlsx", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("reshape", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
setwd("d:/Users_info/ALBANPD/My Documents/Bases")


######################Base de clientes 6 meses############################


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
#TDC$FechaCorte<-as.Date(paste0(year(TDC$FechaCorte),"/",month(TDC$FechaCorte),"/",01),format="%Y/%m/%d")
TDC["Producto"]<-"Alia"
table(TDC$FechaCorte)
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
#SaldosCartera231$FechaCorte<-as.Date(paste0(year(SaldosCartera231$FechaCorte),"/",month(SaldosCartera231$FechaCorte),"/",01),format="%Y/%m/%d")

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
#ClientesCuentas$FechaCreacionCuenta<-as.Date(paste0(year(ClientesCuentas$FechaCreacionCuenta),"/",month(ClientesCuentas$FechaCreacionCuenta),"/",01),format="%Y/%m/%d")

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
CCEne<-subset(ClientesCuentas,FechaCorte<"2017-01-31")
CCEne$FechaCorte<-as.Date("2017-01-31")
CCFeb<-subset(ClientesCuentas,FechaCorte<"2017-02-28")
CCFeb$FechaCorte<-as.Date("2017-02-28")
CCMar<-subset(ClientesCuentas,FechaCorte<"2017-03-31")
CCMar$FechaCorte<-as.Date("2017-03-31")
CCAbr<-subset(ClientesCuentas,FechaCorte<"2017-04-30")
CCAbr$FechaCorte<-as.Date("2017-04-30")
CCMay<-subset(ClientesCuentas,FechaCorte<"2017-05-31")
CCMay$FechaCorte<-as.Date("2017-05-31")
CCJun<-subset(ClientesCuentas,FechaCorte<"2017-06-30")
CCJun$FechaCorte<-as.Date("2017-06-30")

Pasivo<-rbind(CCEne,CCFeb,CCMar,CCAbr,CCMay,CCJun)
######################Base de Clientes###########################

Solidario<-rbind(TDC,Creditos,Pasivo)
#Solidario<-subset(Solidario,!duplicated("FechaCorte","Identificación"))
Solidario$Producto<-"Todos"
SEne<-Solidario[which(Solidario$FechaCorte=="2017-01-31"),]
SEne<-subset(SEne,!duplicated(Identificacion))

SFeb<-Solidario[which(Solidario$FechaCorte=="2017-02-28"),]
SFeb<-subset(SFeb,!duplicated(Identificacion))

SMar<-Solidario[which(Solidario$FechaCorte=="2017-03-31"),]
SMar<-subset(SMar,!duplicated(Identificacion))

SAbr<-Solidario[which(Solidario$FechaCorte=="2017-04-30"),]
SAbr<-subset(SAbr,!duplicated(Identificacion))

SMay<-Solidario[which(Solidario$FechaCorte=="2017-05-31"),]
SMay<-subset(SMay,!duplicated(Identificacion))

SJun<-Solidario[which(Solidario$FechaCorte=="2017-06-30"),]
SJun<-subset(SJun,!duplicated(Identificacion))

Solidario<-rbind(SEne,SFeb,SMar,SAbr,SMay,SJun)

Clientes<-rbind(TDC,Creditos,Pasivo,Solidario)

####################Carga de correos validados######################

DWH_Reporte <- dbConnect(odbc::odbc(),
                         Driver    = "SQL Server", 
                         Server    = "BSUIO-DWH02",
                         Database  = "DWH_Reporte")



CorreoElectronico<- tbl(DWH_Reporte, "ClienteCorreoElectronico") %>%
  filter(Resultado %in% c("Deliverable","Risky") & EsValido==1) %>% collect()

CorreoElectronico$FechaCreacion<-as.Date(CorreoElectronico$FechaCreacion)
CorreoElectronico$Identificacion<-as.factor(CorreoElectronico$Identificacion)
CorreoElectronico$FechaCorte<-CorreoElectronico$FechaCreacion
Correos<-subset(CorreoElectronico,select=c("FechaCorte","Tipo","Identificacion","FechaCreacion"))
#Correos$FechaCorte<-as.Date(paste0(year(Correos$FechaCorte),"/",month(Correos$FechaCorte),"/",01),format="%Y/%m/%d")
#Correos$FechaCreacion<-as.Date(paste0(year(Correos$FechaCreacion),"/",month(Correos$FechaCreacion),"/",01),format="%Y/%m/%d")
Correos<-subset(Correos,Tipo=="CANALES")
Correos<-subset(Correos,select=c("Identificacion","FechaCorte","FechaCreacion"))
##########################################################################################

######################Adquisición de correos#######################################
AdCorreos<-Correos
AdCorreos["Mensual"]<-NA
AdCorreos$Mensual<-as.Date(paste0(year(AdCorreos$FechaCreacion),"-",month(AdCorreos$FechaCreacion),"-",01),format="%Y-%m-%d")
AdCorreos$Mensual<-format(as.Date(AdCorreos$Mensual), "%Y-%m")


AdCorreos["Trimestre"]<-NA
AdCorreos[which(month(AdCorreos$FechaCreacion)<=3),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=3),]$FechaCreacion) ,".T1")
AdCorreos[which(month(AdCorreos$FechaCreacion)>3 & month(AdCorreos$FechaCreacion)<=6),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=6 & month(AdCorreos$FechaCreacion)>3),]$FechaCreacion) ,".T2")
AdCorreos[which(month(AdCorreos$FechaCreacion)>6 & month(AdCorreos$FechaCreacion)<=9),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=9 & month(AdCorreos$FechaCreacion)>6),]$FechaCreacion) ,".T3")
AdCorreos[which(month(AdCorreos$FechaCreacion)>9 & month(AdCorreos$FechaCreacion)<=12),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=12 & month(AdCorreos$FechaCreacion)>9),]$FechaCreacion) ,".T4")


#Colocacion$FechaDesembolso<-as.character(as.Date(Colocacion$FechaDesembolso), "%Y-%m")
AdCorreos["Mensual"]<-NA
AdCorreos$Mensual<-as.character(as.Date(AdCorreos$FechaCreacion), "%Y-%m")



AdCorreos["MTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
AdCorreos[which(day(AdCorreos$FechaCreacion)<=25),]$MTD<-as.character(as.Date(AdCorreos[which(day(AdCorreos$FechaCreacion)<=25),]$FechaCreacion), "%Y-%m")

AdCorreos0<-AdCorreos
AdCorreos<-subset(AdCorreos,FechaCreacion>="2015-01-01")
AdCorreos <- as.data.frame(AdCorreos)
mAdCorreos <- reshape::melt(AdCorreos, id=c(1:3),na.rm = TRUE,variable_name = "Filtro") 

#tabla de 
aux<- mAdCorreos %>% group_by(Filtro,value) %>% 
  summarise(Correos = length(unique(Identificacion))) 


aux<-ddply(aux,"Filtro",transform,
      Tasa=c(NA,exp(diff(log(Correos)))-1))

write.csv(aux,file="JAdquisicionCorreos.csv",row.names = FALSE)
##########################################################################
##################Evolucion de porcentaje por cliente de Correos#####################


Correos["Filtro"]<-NA
#############Mensual#########
CorreosEne<-subset(Correos,FechaCorte<="2017-01-31")
CorreosEne$Filtro<-as.character("Mensual")
CorreosEne$value<-as.character("2017-01")
CorreosFeb<-subset(Correos,FechaCorte<="2017-02-28")
CorreosFeb$Filtro<-as.character("Mensual")
CorreosFeb$value<-as.character("2017-02")
CorreosMar<-subset(Correos,FechaCorte<="2017-03-31")
CorreosMar$Filtro<-as.character("Mensual")
CorreosMar$value<-as.character("2017-03")
CorreosAbr<-subset(Correos,FechaCorte<="2017-04-30")
CorreosAbr$Filtro<-as.character("Mensual")
CorreosAbr$value<-as.character("2017-04")
CorreosMay<-subset(Correos,FechaCorte<="2017-05-31")
CorreosMay$Filtro<-as.character("Mensual")
CorreosMay$value<-as.character("2017-05")
CorreosJun<-subset(Correos,FechaCorte<="2017-06-30")
CorreosJun$Filtro<-as.character("Mensual")
CorreosJun$value<-as.character("2017-06")

auxCorreos<-rbind(CorreosEne,CorreosFeb,CorreosMar,CorreosAbr,CorreosMay,CorreosJun)

CorreosT1<-Correos[which(month(Correos$FechaCreacion)<=3),]
CorreosT1$Filtro<-as.character("Trimestre")
CorreosT1$value<-paste0(year(CorreosT1[which(month(CorreosT1$FechaCreacion)<=3),]$FechaCreacion) ,".T1")

CorreosT2<-Correos[which(month(Correos$FechaCreacion)>3 & month(Correos$FechaCreacion)<=6),]
CorreosT2$Filtro<-as.character("Trimestre")
CorreosT2$value<-paste0(year(CorreosT2[which(month(CorreosT2$FechaCreacion)<=6 & month(CorreosT2$FechaCreacion)>3),]$FechaCreacion) ,".T2")

CorreosFeb<-subset(Correos,FechaCorte<="2017-02-28")
CorreosFeb$Filtro<-as.character("Mensual")
CorreosFeb$value<-as.character("2017-02")
CorreosMar<-subset(Correos,FechaCorte<="2017-03-31")
CorreosMar$Filtro<-as.character("Mensual")
CorreosMar$value<-as.character("2017-03")

AdCorreos[which(month(AdCorreos$FechaCreacion)<=3),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=3),]$FechaCreacion) ,".T1")
AdCorreos[which(month(AdCorreos$FechaCreacion)>3 & month(AdCorreos$FechaCreacion)<=6),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=6 & month(AdCorreos$FechaCreacion)>3),]$FechaCreacion) ,".T2")
AdCorreos[which(month(AdCorreos$FechaCreacion)>6 & month(AdCorreos$FechaCreacion)<=9),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=9 & month(AdCorreos$FechaCreacion)>6),]$FechaCreacion) ,".T3")
AdCorreos[which(month(AdCorreos$FechaCreacion)>9 & month(AdCorreos$FechaCreacion)<=12),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=12 & month(AdCorreos$FechaCreacion)>9),]$FechaCreacion) ,".T4")



AdCorreos$Mensual<-as.Date(paste0(year(AdCorreos$FechaCreacion),"-",month(AdCorreos$FechaCreacion),"-",01),format="%Y-%m-%d")
AdCorreos$Mensual<-format(as.Date(AdCorreos$Mensual), "%Y-%m")


AdCorreos["Trimestre"]<-NA
AdCorreos[which(month(AdCorreos$FechaCreacion)<=3),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=3),]$FechaCreacion) ,".T1")
AdCorreos[which(month(AdCorreos$FechaCreacion)>3 & month(AdCorreos$FechaCreacion)<=6),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=6 & month(AdCorreos$FechaCreacion)>3),]$FechaCreacion) ,".T2")
AdCorreos[which(month(AdCorreos$FechaCreacion)>6 & month(AdCorreos$FechaCreacion)<=9),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=9 & month(AdCorreos$FechaCreacion)>6),]$FechaCreacion) ,".T3")
AdCorreos[which(month(AdCorreos$FechaCreacion)>9 & month(AdCorreos$FechaCreacion)<=12),]$Trimestre<-paste0(year(AdCorreos[which(month(AdCorreos$FechaCreacion)<=12 & month(AdCorreos$FechaCreacion)>9),]$FechaCreacion) ,".T4")


#Colocacion$FechaDesembolso<-as.character(as.Date(Colocacion$FechaDesembolso), "%Y-%m")
AdCorreos["Mensual"]<-NA
AdCorreos$Mensual<-as.character(as.Date(AdCorreos$FechaCreacion), "%Y-%m")



AdCorreos["MTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
AdCorreos[which(day(AdCorreos$FechaCreacion)<=25),]$MTD<-as.character(as.Date(AdCorreos[which(day(AdCorreos$FechaCreacion)<=25),]$FechaCreacion), "%Y-%m")






auxCorreos<-rbind(CorreosEne,CorreosFeb,CorreosMar,CorreosAbr,CorreosMay,CorreosJun)
auxCorreos["Mail"]<-1

##################Cruzar correos con clientes##################
ClientesMail<-left_join(auxCorreos,Clientes,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
ClientesMail[which(is.na(ClientesMail$Producto)),]$Producto<-"Excliente"

ClientesMail <- as.data.frame(ClientesMail)
mClientesMail <- reshape::melt(ClientesMail, id=c("Identificacion",
                                                  "FechaCorte",
                                                  "FechaCreacion",
                                                  "Producto"),na.rm = TRUE,variable_name = "Filtro") 

#tabla de 
aux<- mClientesMail %>% group_by(Filtro,value) %>% 
  summarise(Correos = length(unique(Identificacion))) 


aux<-ddply(aux,"Filtro",transform,
           Tasa=c(NA,exp(diff(log(Correos)))-1))

write.csv(aux,file="JAdquisicionCorreos.csv",row.names = FALSE)

###########################Carga Celulares de Notificación###########################
Negocio <- dbConnect(odbc::odbc(),
                         Driver    = "SQL Server", 
                         Server    = "BSUIO-DWH02",
                         Database  = "Negocio")



ControlAcceso<- tbl(Negocio, "ControlAcceso") %>% collect()

Celulares<-subset(ControlAcceso,select=c("NumeroIdentificacion",
                                         "FechaRegistro",
                                         "FechaCreacion",
                                         "NumeroCelularRegistrado"))
Celulares["FechaCorte"]<-as.character(NA)
Celulares$FechaRegistro<-as.Date(Celulares$FechaRegistro,"%Y-%m-%d")
Celulares$FechaCreacion<-as.Date(Celulares$FechaCreacion,"%Y-%m-%d")
Celulares$FechaRegistro<-as.character(Celulares$FechaRegistro)
Celulares$FechaCreacion<-as.character(Celulares$FechaCreacion)

Celulares<-subset(Celulares,nchar(Celulares$NumeroCelularRegistrado)>8)
Celulares[which(nchar(Celulares$FechaRegistro)>5),]$FechaCorte<-Celulares[which(nchar(Celulares$FechaRegistro)>5),]$FechaRegistro
Celulares[which(nchar(Celulares$FechaCreacion)>5),]$FechaCorte<-Celulares[which(nchar(Celulares$FechaCreacion)>5),]$FechaCreacion
Celulares$FechaCorte<-as.Date(Celulares$FechaCorte,"%Y-%m-%d")
Celulares$FechaCreacion<-Celulares$FechaCorte
Celulares$FechaCorte<-as.Date(paste0(year(Celulares$FechaCreacion),"/",month(Celulares$FechaCreacion),"/",01),format="%Y/%m/%d")
colnames(Celulares)[1]<-"Identificacion"
Celulares<-subset(Celulares,select=c("Identificacion","NumeroCelularRegistrado","FechaCorte","FechaCreacion"))
#############################################################################################
######################Adquisición de Celulares#######################################
AdCelulares<-Celulares
AdCelulares["Mensual"]<-NA
AdCelulares$Mensual<-as.Date(paste0(year(AdCelulares$FechaCreacion),"-",month(AdCelulares$FechaCreacion),"-",01),format="%Y-%m-%d")
AdCelulares$Mensual<-format(as.Date(AdCelulares$Mensual), "%Y-%m")


AdCelulares["Trimestre"]<-NA
AdCelulares[which(month(AdCelulares$FechaCreacion)<=3),]$Trimestre<-paste0(year(AdCelulares[which(month(AdCelulares$FechaCreacion)<=3),]$FechaCreacion) ,".T1")
AdCelulares[which(month(AdCelulares$FechaCreacion)>3 & month(AdCelulares$FechaCreacion)<=6),]$Trimestre<-paste0(year(AdCelulares[which(month(AdCelulares$FechaCreacion)<=6 & month(AdCelulares$FechaCreacion)>3),]$FechaCreacion) ,".T2")
AdCelulares[which(month(AdCelulares$FechaCreacion)>6 & month(AdCelulares$FechaCreacion)<=9),]$Trimestre<-paste0(year(AdCelulares[which(month(AdCelulares$FechaCreacion)<=9 & month(AdCelulares$FechaCreacion)>6),]$FechaCreacion) ,".T3")
AdCelulares[which(month(AdCelulares$FechaCreacion)>9 & month(AdCelulares$FechaCreacion)<=12),]$Trimestre<-paste0(year(AdCelulares[which(month(AdCelulares$FechaCreacion)<=12 & month(AdCelulares$FechaCreacion)>9),]$FechaCreacion) ,".T4")


#Colocacion$FechaDesembolso<-as.character(as.Date(Colocacion$FechaDesembolso), "%Y-%m")
AdCelulares["Mensual"]<-NA
AdCelulares$Mensual<-as.character(as.Date(AdCelulares$FechaCreacion), "%Y-%m")



AdCelulares["MTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
AdCelulares[which(day(AdCelulares$FechaCreacion)<=25),]$MTD<-as.character(as.Date(AdCelulares[which(day(AdCelulares$FechaCreacion)<=25),]$FechaCreacion), "%Y-%m")

AdCelulares0<-AdCelulares
AdCelulares<-subset(AdCelulares,FechaCreacion>="2015-01-01")
AdCelulares <- as.data.frame(AdCelulares)
mAdCelulares <- reshape::melt(AdCelulares, id=c(1:4),na.rm = TRUE,variable_name = "Filtro") 

#tabla de 
aux<- mAdCelulares %>% group_by(Filtro,value) %>% 
  summarise(Celular = length(unique(Identificacion))) 


aux<-ddply(aux,"Filtro",transform,
           Tasa=c(NA,exp(diff(log(Celular)))-1))

write.csv(aux,file="JAdquisicionCelulares.csv",row.names = FALSE)

####################################################################




CelularesEne<-subset(Celulares,FechaCreacion<="2017-01-31")
CelularesEne$FechaCorte<-as.Date("2017-01-31")
CelularesFeb<-subset(Celulares,FechaCreacion<="2017-02-28")
CelularesFeb$FechaCorte<-as.Date("2017-02-28")
CelularesMar<-subset(Celulares,FechaCreacion<="2017-03-31")
CelularesMar$FechaCorte<-as.Date("2017-03-31")
CelularesAbr<-subset(Celulares,FechaCreacion<="2017-04-30")
CelularesAbr$FechaCorte<-as.Date("2017-04-30")
CelularesMay<-subset(Celulares,FechaCreacion<="2017-05-31")
CelularesMay$FechaCorte<-as.Date("2017-05-31")
CelularesJun<-subset(Celulares,FechaCreacion<="2017-06-30")
CelularesJun$FechaCorte<-as.Date("2017-06-30")

auxCelular<-rbind(CelularesEne,CelularesFeb,CelularesMar,CelularesAbr,CelularesMay,CelularesJun)
auxCelular<-subset(auxCelular,select=c("Identificacion","FechaCorte","FechaCreacion"))
auxCelular["Celular"]<-1
auxCelular<-left_join(auxCelular,Clientes,select=c("Identificacion","FechaCorte","FechaCreacion"))
auxCelular[which(is.na(auxCelular$Producto)),]$Producto<-"Excliente"
write.csv(auxCelular,file = "AuxCelular.csv",row.names = FALSE)
########################Adquisicion de mail mensual############################
CorreosMes<-Correos
CorreosMes<-subset(CorreosMes,FechaCreacion>"2015-01-01")
CorreosMes$FechaCreacion<-as.Date(paste0(year(CorreosMes$FechaCreacion),"/",month(CorreosMes$FechaCreacion),"/",01),format="%Y/%m/%d")

write.csv(CorreosMes,file = "AdquisicionCorreos.csv",row.names = FALSE)

aux<-subset(Celulares,FechaCreacion>"2015-01-01")
write.csv(aux,file = "AdquisicionCelulares.csv",row.names = FALSE)

##################Cruzar correos con clientes##################
ClientesMail2<-left_join(auxCorreos,Clientes,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
ClientesMail[which(is.na(ClientesMail$Producto)),]$Producto<-"Excliente"


aux<- B1 %>% group_by(FechaCorte,Producto) %>% 
  summarise(p = length(unique(Mail)), q = length(unique(Celular))) 




auxCelular2<-subset(auxCelular,select=c("Identificacion","FechaCorte","Celular"))

B1<-full_join(auxCelular2, auxCorreos,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
B1[which(B1$Mail==1),]$Mail<-B1[which(B1$Mail==1),]$Identificacion
B1[which(B1$Celular==1),]$Celular<-B1[which(B1$Celular==1),]$Identificacion
B1<-left_join(B1,Clientes,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
B1[which(is.na(B1$Producto)),]$Producto<-"Excliente"





B2
B1["MTD"]<-"NA"
B1[which(B1$)]
#B1["Trimestre"]
B1["Mensual"]

write.csv(B1,file = "DN2.csv",row.names = FALSE)

auxCelular2<-left_join(auxCelular,Clientes,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
auxCelular2[which(is.na(auxCelular2$Producto)),]$Producto<-"Excliente"
auxCelular2$FechaCorte<-as.Date(paste0(year(auxCelular2$FechaCreacion),"/",month(auxCelular2$FechaCreacion),"/",01),format="%Y/%m/%d")


auxCorreos2<-left_join(auxCorreos,Clientes,by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
auxCorreos2[which(is.na(auxCorreos2$Producto)),]$Producto<-"Excliente"
auxCorreos2$FechaCorte<-as.Date(paste0(year(auxCorreos2$FechaCreacion),"/",month(auxCorreos2$FechaCreacion),"/",01),format="%Y/%m/%d")

write.csv(auxCelular2,file = "auxCelular.csv",row.names = FALSE)

write.csv(auxCorreos2,file = "auxCorreos.csv",row.names = FALSE)

#Correos$Identificacion<-as.character(Correos$Identificacion)
#aux<- Correos %>% group_by(FechaCorte,Tipo) %>% summarise(Correos =sum(EsValido)) 
write.csv(aux,file = "CorreosxTipo.csv",row.names = FALSE)
