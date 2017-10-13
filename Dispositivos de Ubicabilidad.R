library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("RODBCext", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

setwd("d:/Users_info/ALBANPD/My Documents/Bases")
#c<-(sapply(ClientesNov16, function(x) class(x)[[1]]))       #lista de las clases de las variables
#ClientesNov16<-read.table("ClientesTDC 30-11-2016.csv",header=TRUE,sep=",",na.strings = "NA")
##############Correos electronicos#######################
odbcChannel <-odbcConnect("DWH_Reporte") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Rep_ClienteCorreoElectronico")

CorreoElectronico <- sqlExecute(odbcChannel, "SELECT * FROM Rep_ClienteCorreoElectronico", fetch = TRUE)




####################Tarjetas activas###########################################################
odbcChannel <-odbcConnect("DWH_CRM") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "SB_TDCActivas")



TDCActivas <- sqlExecute(odbcChannel, "SELECT * FROM SB_TDCActivas", fetch = TRUE)
TDCActivas<-subset(TDCActivas,CuentaTarjetaMonitor==1)
TDCActivas<-subset(TDCActivas,select = c("Identificacion"))
odbcClose(odbcChannel)
########################Celulares y mails de Notificacion######################
odbcChannel <-odbcConnect("Negocio") #Databases

View(sqlTables(odbcChannel, tableType = "TABLE"))

View(sqlColumns(odbcChannel, "ControlAcceso"))
ControlAcceso <- sqlExecute(odbcChannel, "SELECT * FROM ControlAcceso", fetch = TRUE)
odbcClose(odbcChannel)

colnames(ControlAcceso)[4]<-"Identificacion"

ControlAcceso$NumeroCelularRegistrado<-as.character(ControlAcceso$NumeroCelularRegistrado)
ControlAcceso$CorreoElectronicoRegistrado<-as.character(ControlAcceso$CorreoElectronicoRegistrado)

ControlAcceso<-cbind(ControlAcceso,
  nchar(as.character(ControlAcceso$NumeroCelularRegistrado)),
                 nchar(as.character(ControlAcceso$CorreoElectronicoRegistrado)))
colnames(ControlAcceso)[20:21]<-c("nCelular","nCorreo")

CorreosNot<-subset(ControlAcceso,nCorreo!=0)
CelularesNot<-subset(ControlAcceso,nCelular!=0)
CorreoyCel<-subset(ControlAcceso,nCelular!=0 & nCorreo!=0)
CorreosNot<-subset(CorreosNot,!duplicated(Identificacion))
CelularesNot<-subset(CelularesNot,!duplicated(Identificacion))

summary(aux$NumeroCelularRegistrado)


######################Celulares Ubicacion#######################
library(RODBC)
#DWH_Externa..RegistroLaboral
odbcChannel <-odbcConnect("DWH_Reporte") #Databases
Correos<-sqlFetch(odbcChannel,"Rep_ClienteCorreoElectronico") #Tables
odbcClose(odbcChannel)

aux<-inner_join(Correos,ClientesNov16,by="Identificacion")

View(table(aux$CuentaTarjetaMonitor))

View(ClientesNov16[which(ClientesNov16$Identificacion=="1307294924"),])
(Correos[which(Correos$Identificacion=="1307294924"),])


nchar(ClientesNov16[which(ClientesNov16$Identificacion=="1307294924"),]$IdCuentaTarjeta)


#DWH_Externa..RegistroLaboral
odbcChannel <-odbcConnect("DWH_Reporte") #Databases
Telefonos<-sqlFetch(odbcChannel,"Rep_TelefonoClienteTotal") #Tables
odbcClose(odbcChannel)


ntelefono<-cbind(nchar(as.character(Telefonos$Telefono_01)),
                 nchar(as.character(Telefonos$Telefono_02)),
                 nchar(as.character(Telefonos$Telefono_03)),
                 nchar(as.character(Telefonos$Telefono_04)),
                 nchar(as.character(Telefonos$Telefono_05)),
                 nchar(as.character(Telefonos$Telefono_06)),
                 nchar(as.character(Telefonos$Telefono_07))
                 )
ntelefono<-as.data.frame(ntelefono)
ntelefono<-cbind(Telefonos$Identificacion,ntelefono)

aux<-subset(ntelefono,
              V1==10 |
              V2==10 |
              V3==10 | 
              V4==10 |
              V5==10 |
              V6==10 |
              V7==10)
colnames(aux)[1]<-"Identificacion"

Celulares<-semi_join(Telefonos,aux,by="Identificacion")


aux<-semi_join(ClientesNov16,Celulares,by="Identificacion")
View(table(aux$CuentaTarjetaMonitor))


#################Dispositivos de Ubicación 2#############################

odbcChannel <-odbcConnect("Cliente") #Databases

View(sqlTables(odbcChannel, tableType = "TABLE"))

View(sqlColumns(odbcChannel, "DispositivoUbicacion"))

DispositivoUbicacion <- sqlExecute(odbcChannel, "SELECT * FROM DispositivoUbicacion", fetch = TRUE)
odbcClose(odbcChannel)



odbcChannel <-odbcConnect("Cliente") #Databases
sqlTables(odbcChannel, tableType = "TABLE")
sqlColumns(odbcChannel, "Cliente")
ClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM ClienteNatural WHERE IdInstitucion = 2", fetch = TRUE)
Cliente <- sqlExecute(odbcChannel, "SELECT * FROM Cliente", fetch = TRUE)
odbcClose(odbcChannel)


Cliente<-subset(Cliente,select = c("IdCliente","TipoIdentificacion","Identificacion"))

aux<-inner_join(Cliente,ClienteNatural)
DispositivoUbicacion<-inner_join(DispositivoUbicacion,aux,by="IdClienteNatural")
DispositivoUbicacion<-subset(DispositivoUbicacion,select = c(
  "TipoDispositivo",
  "Identificacion",
  "Valor" ,
  "IdInstitucion",
  "TipoIdentificacion",
  "EsPersonal"
))

Correos<-subset(DispositivoUbicacion,TipoDispositivo=="EMAIL")
Correos<-subset(Correos,!duplicated(Identificacion))
NROW(Correos)
Celulares<-subset(DispositivoUbicacion,TipoDispositivo=="CEL")
Celulares<-subset(Celulares,!duplicated(Identificacion))
NROW(Celulares)
#################3Saldo 231######################3

odbcChannel <-odbcConnect("DWH_TrabajoEstudios") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "HISTORICO.SaldosCartera231")



SaldosCartera231 <- sqlExecute(odbcChannel, "SELECT * FROM HISTORICO.SaldosCartera231 WHERE FechaCorte = ?", '2017-05-31', fetch = TRUE)


SaldosCartera231$Identificacion<-as.character(SaldosCartera231$Identificacion)
SaldosCartera231[which(nchar(SaldosCartera231$Identificacion)==9),]$Identificacion<-as.character(paste0(0,SaldosCartera231[which(nchar(SaldosCartera231$Identificacion)==9),]$Identificacion))
SaldosCartera231[which(nchar(SaldosCartera231$Identificacion)==12),]$Identificacion<-as.character(paste0(0,SaldosCartera231[which(nchar(SaldosCartera231$Identificacion)==12),]$Identificacion))
#SaldosCartera231$Identificacion<-as.factor(SaldosCartera231$Identificacion)
odbcClose(odbcChannel)

#############Productos de activo#####################

###################Tarjeta#######################
#ClientesTDC<-subset(TDCActivas,CuentaTarjetaMonitor==1)
ClientesTDC<-TDCActivas
Correos<-CorreoElectronico
ClientesTDC<-subset(ClientesTDC,!duplicated(Identificacion))

TDCMail<-semi_join(ClientesTDC,Correos,by="Identificacion")
TDCMail<-subset(TDCMail,!duplicated(Identificacion))
NROW(TDCMail)

 
TDCCel<-semi_join(ClientesTDC,Celulares,by="Identificacion")
TDCCel<-subset(TDCCel,!duplicated(Identificacion))
NROW(TDCCel)  


NROW(ClientesTDC)

#########################Unicredito##########################
ClientesUnicredito<-subset(SaldosCartera231,CodigoProducto=="UNICREDITO" | 
                             CodigoProducto=="REESTRTRJ"|
                             CodigoProducto=="CONNORMALIZ")
ClientesUnicredito<-subset(ClientesUnicredito,!duplicated(Identificacion))

ClientesUnicredito<-subset(ClientesUnicredito,Estado!="CASTIG")
View(table(ClientesUnicredito$Estado))

UniCel<-semi_join(ClientesUnicredito,Celulares,by="Identificacion")
UniCel<-subset(UniCel,!duplicated(Identificacion))
View(table(UniCel$Estado))
NROW(UniCel)

UniMail<-semi_join(ClientesUnicredito,Correos,by="Identificacion")
UniMail<-subset(UniMail,!duplicated(Identificacion))
View(table(UniMail$Estado))
NROW(UniMail)


aux<-subset(ClientesUnicredito,Estado!="CASTIG")
sum(aux$Saldo)
################Microcredito########################
ClientesMicro<-subset(SaldosCartera231,
            CodigoProducto=="MICCAMTRANSF" |
            CodigoProducto=="MICNORMALIZ"  |
              CodigoProducto=="MICROCAMPANIA"  |
              CodigoProducto=="MICRORURAL"  |
              CodigoProducto=="MICROURBANO"  |
              CodigoProducto=="MICRURTRANSF"  |
              CodigoProducto=="MICURBCOMPRA"  |
              CodigoProducto=="MICURBTRANSF"  
            )
ClientesMicro<-subset(ClientesMicro,!duplicated(Identificacion))
ClientesMicro<-subset(ClientesMicro,Estado!="CASTIG")

View(table(ClientesMicro$Estado))


MicCel<-semi_join(ClientesMicro,Celulares,by="Identificacion")
MicMail<-semi_join(ClientesMicro,Correos,by="Identificacion")
View(table(MicCel$Estado))
View(table(MicMail$Estado))
NROW(MicCel)
NROW(MicMail)
NROW(ClientesMicro)

#################Clientes Olla##################33
ClientesOlla<-subset(SaldosCartera231,
                     CodigoProducto=="OLLACONPRE" |
                       CodigoProducto=="OLLACONSEG" |
                       CodigoProducto=="OLLACONSU" |
                       CodigoProducto=="OLLAEMERFCUO" |
                       CodigoProducto=="OLLAMICPRE"  |
                       CodigoProducto=="OLLAMICRO" |
                       CodigoProducto=="OLLAMICSEG"  )

ClientesOlla<-subset(ClientesOlla,!duplicated(Identificacion))
View(table(ClientesOlla$Estado))
View(table(ClientesOlla$CodigoProducto))

OllaCel<-semi_join(ClientesOlla,Celulares,by="Identificacion")
NROW(subset(OllaCel,duplicated(Identificacion)))
OllaMail<-semi_join(ClientesOlla,Correos,by="Identificacion")
View(table(OllaCel$Estado))
View(table(OllaMail$Estado))
NROW(OllaCel)
NROW(OllaMail)

sum(ClientesOlla$Saldo)

NROW(ClientesOlla)
View(table(ClientesOlla$CodigoProducto))
###############Casas Conmerciales#######################
ClientesCC<-subset(SaldosCartera231,
                   CodigoProducto=="POINT" |
                     CodigoProducto=="CINTICOMP" |
                     CodigoProducto=="CARTIMEX" |
                     CodigoProducto=="CHEPEREZ" |
                     CodigoProducto=="COMPTECO" |
                     CodigoProducto=="MOTOFACIL" |
                     CodigoProducto=="INDIANMOTOS" |
                     CodigoProducto=="FDGCOMPUTER" |
                     CodigoProducto=="HOGARESSALUDAB" |
                     CodigoProducto=="CONSTRUAHORA" |
                     CodigoProducto=="MADERASBOSQUE" |
                     CodigoProducto=="ELECTROFACIL" |
                     CodigoProducto=="BAYANGO" |
                     CodigoProducto=="EYSCORP" |
                     CodigoProducto=="TARDI" |
                     CodigoProducto=="CASAEXITO" |
                     CodigoProducto=="DISTRIBPACIFICO" |
                     CodigoProducto=="ELPENION" |
                     CodigoProducto=="CONSMAPE" |
                     CodigoProducto=="CHIMASA" |
                     CodigoProducto=="RAINBOW" |
                     CodigoProducto=="TECNOHOGAR" |
                     CodigoProducto=="ARTEYMUEBLE" |
                     CodigoProducto=="CARLETHY" |
                     CodigoProducto=="MASTERPC" |
                     CodigoProducto=="DISTRIBUIDORES" |
                     CodigoProducto=="ELECTROFACILDOS" |
                     CodigoProducto=="CINTICOMPDOS" |
                     CodigoProducto=="MOTORUNO" |
                     CodigoProducto=="CONSTRUMAJI" |
                     CodigoProducto=="TECNOHOGARDOS" |
                     CodigoProducto=="UNIFER" |
                     CodigoProducto=="PEDROYPABLO" |
                     CodigoProducto=="DISTRIBUIDORES" |
                     CodigoProducto=="HRNET" |
                     CodigoProducto=="LAURORA" |
                     CodigoProducto=="TECNOHOGARDOS" |
                     CodigoProducto=="UNIFER" |
CodigoProducto=="POINT"|
                   CodigoProducto=="LAURORA" |
                   CodigoProducto=="MASTERPC" |
CodigoProducto=="MADERASBOSQUE" |
                   CodigoProducto=="MOTOFACIL" |
CodigoProducto=="MOTORUNO" |
                   CodigoProducto=="POINT" |
CodigoProducto=="TARDI" |
                   CodigoProducto=="FDGCOMPUTER" |
CodigoProducto=="ELPENION" |
CodigoProducto=="FIRMAS" |
                   CodigoProducto=="FDGCOMPUTER" |
CodigoProducto=="ESPECIAL" |
                   CodigoProducto=="ELPENION" |
CodigoProducto=="ELECTROFACILDOS" |
                   CodigoProducto=="DISTRIBUIDORES" |
CodigoProducto=="DISTRIBPACIFICO" |
                   CodigoProducto=="CONTINGENTE" |
CodigoProducto=="CONSTRUAHORA" |
                   CodigoProducto=="CHIMASA" |
CodigoProducto=="CINTICOMP" |
                   CodigoProducto=="CINTICOMPDOS" |
CodigoProducto=="CHEPEREZ" |
                   CodigoProducto=="CARLETHY" |
CodigoProducto=="ARTEYMUEBLE"
                     )

ClientesCC<-subset(ClientesCC,Estado!="CASTIG")
sum(ClientesCC$Saldo)

ClientesCC<-subset(ClientesCC,!duplicated(Identificacion))
View(table(ClientesCC$Estado))
View(table(ClientesCC$CodigoProducto))

CCCel<-semi_join(ClientesCC,Celulares,by="Identificacion")
NROW(subset(CCCel,duplicated(Identificacion)))
CCMail<-semi_join(ClientesCC,Correos,by="Identificacion")
View(table(CCCel$Estado))
View(table(CCMail$Estado))
NROW(CCCel)
NROW(CCMail)

#####################Producto Icesa#######################
ClientesIcesa<-subset(SaldosCartera231,
                   CodigoProducto=="ICESACCO" |
                     CodigoProducto=="ICESA")

ClientesIcesa<-subset(ClientesIcesa,Estado!="CASTIG")
sum(ClientesIcesa$Saldo)

ClientesIcesa<-subset(ClientesIcesa,!duplicated(Identificacion))
View(table(ClientesIcesa$Estado))
View(table(ClientesIcesa$CodigoProducto))

IcesaCel<-semi_join(ClientesIcesa,Celulares,by="Identificacion")
NROW(subset(IcesaCel,duplicated(Identificacion)))
IcesaMail<-semi_join(ClientesIcesa,Correos,by="Identificacion")
View(table(IcesaCel$Estado))
View(table(IcesaMail$Estado))

NROW(IcesaCel)
NROW(IcesaMail)

#####################Pasivo#################################
odbcChannel <-odbcConnect("Cuenta") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "CuentaCliente")



CuentaCliente <- sqlExecute(odbcChannel, "SELECT * FROM CuentaCliente", fetch = TRUE)

table(CuentaCliente$IdTipoEstadoCuenta, CuentaCliente$TipoCuenta,CuentaCliente$TipoCuentaContable)

table(CuentaCliente$TipoCuenta,CuentaCliente$TipoCuentaContable)

odbcClose(odbcChannel)

Cuentas<-subset(CuentaCliente,IdTipoEstadoCuenta==1)

########################################


#################Productos del pasivo####################
odbcChannel <-odbcConnect("Cuenta") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "TipoEstadoCuenta")



TipoEstadoCuenta <- sqlExecute(odbcChannel, "SELECT * FROM TipoEstadoCuenta", fetch = TRUE)

table(data$IdTipoEstadoCuenta, data$TipoCuenta,data$TipoCuentaContable)

table(data$TipoCuenta,data$TipoCuentaContable)

odbcClose(odbcChannel)
###################################################
################Informacion del cliente################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Cliente")

ClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM ClienteNatural WHERE IdInstitucion = 2", fetch = TRUE)

Cliente <- sqlExecute(odbcChannel, "SELECT * FROM Cliente", fetch = TRUE)

InformacionDemografica<- sqlExecute(odbcChannel, "SELECT * FROM InformacionDemografica", fetch = TRUE)

odbcClose(odbcChannel)


auxClienteNatural<-subset(ClienteNatural,select = c("IdClienteNatural","IdCliente"))
auxCliente<-subset(Cliente,select = c("IdCliente","Identificacion","TipoIdentificacion"))
auxInformacionDemografica<-subset(InformacionDemografica,select = c("IdClienteNatural",
                                                                    "FechaNacimiento",
                                                                    "Sexo",
                                                                    "NivelInstruccion",
                                                                    "EsFallecido",
                                                                    "UbicacionGeografica1LugarNac",
                                                                    "UbicacionGeografica2LugarNac",
                                                                    "UbicacionGeografica3LugarNac"
                                                                    ))



Demografica<-left_join(auxCliente,auxClienteNatural)
Demografica<-left_join(Demografica,auxInformacionDemografica)
table(Demografica$TipoIdentificacion)

#Demografica<-left_join(Demografica,RegistroLaboral)



#####################Cruze entre Demografica y Cuentas#################333
auxCuentas<-subset(Cuentas,select = c("IdCuentaCliente",
                                      "IdInstitucion",
                                      "IdCliente",
                                      "TipoCuenta",
                                      "IdTipoEstadoCuenta",
                                      "FechaUltimaTransaccion",
                                      "FechaCierre"
                                      ))
ProductoPasivo<-left_join(auxCuentas,Demografica,by="IdCliente")
#PasivoMail<-semi_join(ProductoPasivo,Correos,by="Identificacion")
aux1<-subset(ProductoPasivo,IdTipoEstadoCuenta==1)
aux1<-subset(aux1,!duplicated(Identificacion))
aux2<-subset(ProductoPasivo,IdTipoEstadoCuenta>1)
aux2<-subset(aux2,!duplicated(Identificacion))
aux2<-anti_join(aux2,aux1,by="Identificacion")
Pasivo<-rbind(aux1,aux2)



Pasivo$FechaNacimiento<-as.Date(Pasivo$FechaNacimiento)
Pasivo$FechaUltimaTransaccion<-as.Date(Pasivo$FechaUltimaTransaccion)
Pasivo$FechaCierre<-as.Date(Pasivo$FechaCierre)


Pasivo$FechaNacimiento <- replace(Pasivo$FechaNacimiento,which(is.na(Pasivo$FechaNacimiento)),as.Date(ISOdate(1900,01,01)))
Pasivo$FechaUltimaTransaccion <- replace(Pasivo$FechaUltimaTransaccion,which(is.na(Pasivo$FechaUltimaTransaccion)),as.Date(ISOdate(1998,01,01)))
Pasivo$FechaCierre <- replace(Pasivo$FechaCierre,which(is.na(Pasivo$FechaCierre)),as.Date(ISOdate(2017,01,04)))


##################Calculo de Edades############################
d2<-Pasivo$FechaNacimiento
d3<- substr(d2, 0, 10)
#yy<-substr(d3,7,10)
yy<-substr(d3,0,4)
#mm<-substr(d3,4,5)
mm<-substr(d3,6,7)
#dd<-substr(d3,0,2)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2016", "04","30"))
n<-length(Pasivo$FechaNacimiento)
Pasivo["EdadCliente"] <- NA
Pasivo$EdadCliente<-age_years(d5,rep(as.Date(ISOdate("2016", "12","31")),n))
age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age}
Pasivo$EdadCliente<-as.numeric(Pasivo$EdadCliente)


################Dias desde la ultima transaccion######################
d2<-Pasivo$FechaUltimaTransaccion
d3<- substr(d2, 0, 10)
#yy<-substr(d3,7,10)
yy<-substr(d3,0,4)
#mm<-substr(d3,4,5)
mm<-substr(d3,6,7)
#dd<-substr(d3,0,2)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2016", "04","30"))


Pasivo["DiasUltimaTransaccion"]<-NA
Pasivo$DiasUltimaTransaccion<-round((as.Date(ISOdate(2017,01,04))-as.Date(d5)), digits = 0)
Pasivo$DiasUltimaTransaccion<-as.numeric(Pasivo$DiasUltimaTransaccion)



################Dias desde la Fecha Cierre######################
d2<-Pasivo$FechaCierre
d3<- substr(d2, 0, 10)
#yy<-substr(d3,7,10)
yy<-substr(d3,0,4)
#mm<-substr(d3,4,5)
mm<-substr(d3,6,7)
#dd<-substr(d3,0,2)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2016", "04","30"))


Pasivo["DiasFechaCierre"]<-NA
Pasivo$DiasFechaCierre<-round((as.Date(ISOdate(2017,01,04))-as.Date(d5)), digits = 0)
Pasivo$DiasFechaCierre<-as.numeric(Pasivo$DiasFechaCierre)

#######################Clientes Cuentas#############################
NROW(subset(Pasivo,duplicated(Identificacion)))
ClientesCuenta<-subset(Pasivo,IdTipoEstadoCuenta==1)
View(table(ClientesCuenta$TipoCuenta))
NROW(subset(ClientesCuenta,duplicated(Identificacion)))
ClientesCuenta<-subset(ClientesCuenta,TipoCuenta!="CTACONTABLE")
NROW(ClientesCuenta)

CuentaCel<-semi_join(ClientesCuenta,Celulares,by="Identificacion")
NROW(subset(CuentaCel,duplicated(Identificacion)))
CuentaMail<-semi_join(ClientesCuenta,Correos,by="Identificacion")
View(table(CuentaCel$TipoCuenta))
View(table(CuentaMail$TipoCuenta))
NROW(CuentaCel)
NROW(CuentaMail)


########################Clientes Exclusivos############################
aux<-anti_join(ClientesIcesa,ClientesTDC,by="Identificacion")
aux<-anti_join(aux,ClientesUnicredito,by="Identificacion")

aux<-anti_join(aux,ClientesMicro,by="Identificacion")

aux<-anti_join(aux,ClientesOlla,by="Identificacion")

aux<-anti_join(aux,ClientesCuenta,by="Identificacion")

aux<-anti_join(aux,ClientesCC,by="Identificacion")

NROW(aux)
####################Clientes con productos compartidos####################
NROW(aux)
NROW(auxm)
NROW(auxc)


aux<-semi_join(ClientesTDC,ClientesUnicredito,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesTDC,ClientesMicro,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesTDC,ClientesOlla,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")




aux<-semi_join(ClientesTDC,ClientesCuenta,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesUnicredito,ClientesMicro,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesUnicredito,ClientesOlla,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesUnicredito,ClientesCuenta,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")



aux<-semi_join(ClientesMicro,ClientesOlla,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")



aux<-semi_join(ClientesMicro,ClientesCuenta,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")




aux<-semi_join(ClientesOlla,ClientesCuenta,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesCuenta,ClientesCC,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesCuenta,ClientesIcesa,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesCC,ClientesIcesa,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesTDC,ClientesCC,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")

aux<-semi_join(ClientesUnicredito,ClientesCC,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")

aux<-semi_join(ClientesMicro,ClientesCC,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesOlla,ClientesCC,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")

aux<-semi_join(ClientesTDC,ClientesIcesa,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesUnicredito,ClientesIcesa,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesMicro,ClientesIcesa,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")


aux<-semi_join(ClientesOlla,ClientesIcesa,by="Identificacion")
auxm<-semi_join(aux,Correos,by="Identificacion")
auxc<-semi_join(aux,Celulares,by="Identificacion")

NROW(aux)
NROW(auxm)
NROW(auxc)
####################Número de clientes del banco solidario#######################
ClientesSolidario<-rbind(subset(ClientesCuenta,select = "Identificacion"),
         subset(ClientesIcesa,select = "Identificacion"),
         subset(ClientesCC,select = "Identificacion"),
         subset(ClientesOlla,select = "Identificacion"),
         subset(ClientesMicro,select = "Identificacion"),
         subset(ClientesTDC,select = "Identificacion"),
         subset(ClientesUnicredito,select = "Identificacion")
)
ClientesSolidario<-subset(ClientesSolidario,!duplicated(Identificacion))
auxm<-semi_join(ClientesSolidario,Correos,by="Identificacion")
auxc<-semi_join(ClientesSolidario,Celulares,by="Identificacion")
aux<-semi_join(auxc,auxm,by="Identificacion")
  
NROW(auxm)
NROW(auxc)
########################Número de clientes con mail y celulares a la vez#####################
CorreoyCel<-semi_join(Celulares,Correos,by="Identificacion")


aux<-semi_join(ClientesTDC,CorreoyCel,by="Identificacion")
NROW(aux)

aux<-semi_join(ClientesUnicredito,CorreoyCel,by="Identificacion")
NROW(aux)

aux<-semi_join(ClientesMicro,CorreoyCel,by="Identificacion")
NROW(aux)

aux<-semi_join(ClientesOlla,CorreoyCel,by="Identificacion")
NROW(aux)

aux<-semi_join(ClientesCC,CorreoyCel,by="Identificacion")
NROW(aux)

aux<-semi_join(ClientesIcesa,CorreoyCel,by="Identificacion")
NROW(aux)


aux<-semi_join(ClientesCuenta,CorreoyCel,by="Identificacion")
NROW(aux)



aux<-semi_join(TDCCel,TDCMail,by="Identificacion")
NROW(aux)

aux<-semi_join(UniCel,UniMail,by="Identificacion")
NROW(aux)

aux<-semi_join(MicCel,MicMail,by="Identificacion")
NROW(aux)

aux<-semi_join(OllaCel,OllaMail,by="Identificacion")
NROW(aux)

aux<-semi_join(CCCel,CCMail,by="Identificacion")
NROW(aux)

aux<-semi_join(IcesaCel,IcesaMail,by="Identificacion")
NROW(aux)

aux<-semi_join(CuentaCel,CuentaMail,by="Identificacion")
NROW(aux)
#####################
##########################Numero de celules y mails######################
ncelulares<-cbind(nchar(as.character(Celulares$Telefono_01)),
                 nchar(as.character(Celulares$Telefono_02)),
                 nchar(as.character(Celulares$Telefono_03)),
                 nchar(as.character(Celulares$Telefono_04)),
                 nchar(as.character(Celulares$Telefono_05)),
                 nchar(as.character(Celulares$Telefono_06)),
                 nchar(as.character(Celulares$Telefono_07))
)
ncelulares<-as.data.frame(ncelulares)
ncelulares<-cbind(Celulares$Identificacion,ncelulares)

ncelulares[which(ncelulares$V1!=10),]$V1<-0
ncelulares[which(ncelulares$V2!=10),]$V2<-0
ncelulares[which(ncelulares$V3!=10),]$V3<-0
ncelulares[which(ncelulares$V4!=10),]$V4<-0
ncelulares[which(ncelulares$V5!=10),]$V5<-0
ncelulares[which(ncelulares$V6!=10),]$V6<-0
ncelulares[which(ncelulares$V7!=10),]$V7<-0


aux<-subset(ncelulares, V1+
              V2+
              V3+
              V4+
              V5+
              V6+
              V7>40)

NROW(aux)




aux<-subset(Correos, !is.na(ClienteBankPlus_1)& 
              is.na(ClienteBankPlus_2) &
              is.na(ClienteBankPlus_3) &
             # is.na(PlanRecompensa) &
              is.na(MailCanales)
            )

####################Edades Clientes Olla######################
aux<-subset(Demografica,select=c("Identificacion",
                               "FechaNacimiento"))
Aux<-left_join(ClientesUnicredito,aux,by=c("Identificacion"))
Aux<-subset(Aux,select=c(
  "FechaCorte",
  "NumeroOperacion",
  "FechaOperacion",
  "TipoIdentificacion",
  "Identificacion",
  "RazonSocial",
  "ClienteVinculado",
  "ClaseCredito",
  "TipoOperacion",
  "Saldo",
  "ProvisionOriginal",
  "CalificacionOriginal",
  "MontoGarantia",
  "CodigoGarantia",
  "Segmento",
  "Agente",
"Plazo",
  "SaldoVigente",
  "SaldoContaminado",
  "SaldoVencido",
  "Producto",
  "Clase",
  "DiasMora",
  "CodigoProducto",
  "CodigoClase",
  "Origen",
  "Estado",
  "NombreOficina",
  "RangoMora",
  "CalificacionFinal",
  "CodigoCategoria",
  "SaldoTotal",
  "FechaNacimiento"
))

c<-(sapply(Aux, function(x) class(x)[[1]]))


Aux$FechaNacimiento <- replace(Aux$FechaNacimiento,which(is.na(Aux$FechaNacimiento)),as.Date(ISOdate(1900,01,01)))

d2<-Aux$FechaNacimiento
d3<- substr(d2, 0, 10)
#yy<-substr(d3,7,10)
yy<-substr(d3,0,4)
#mm<-substr(d3,4,5)
mm<-substr(d3,6,7)
#dd<-substr(d3,0,2)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2016", "04","30"))
n<-length(Aux$FechaNacimiento)
Aux["EdadCliente"] <- NA
Aux$EdadCliente<-age_years(d5,rep(as.Date(ISOdate("2017", "01","13")),n))
age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age}
Aux$EdadCliente<-as.numeric(Aux$EdadCliente)
Aux$Identificacion<-as.factor(Aux$Identificacion)
Aux["RangoEdad"]<-NA

Aux[which(Aux$EdadCliente<=25),]$RangoEdad<-"20-25"
Aux[which(Aux$EdadCliente<=30 & Aux$EdadCliente>25),]$RangoEdad<-"26-30"
Aux[which(Aux$EdadCliente<=35 & Aux$EdadCliente>30),]$RangoEdad<-"31-35"
  Aux[which(Aux$EdadCliente<=40 & Aux$EdadCliente>35),]$RangoEdad<-"36-40"
  Aux[which(Aux$EdadCliente<=45 & Aux$EdadCliente>40),]$RangoEdad<-"41-45"  
  Aux[which(Aux$EdadCliente<=50 & Aux$EdadCliente>45),]$RangoEdad<-"46-50"
  Aux[which(Aux$EdadCliente<=55 & Aux$EdadCliente>50),]$RangoEdad<-"51-55"  
  Aux[which(Aux$EdadCliente<=60 & Aux$EdadCliente>55),]$RangoEdad<-"56-60"
  Aux[which(Aux$EdadCliente<=65 & Aux$EdadCliente>60),]$RangoEdad<-"61-65"  
  Aux[which(Aux$EdadCliente<=70 & Aux$EdadCliente>65),]$RangoEdad<-"66-70"
  Aux[which(Aux$EdadCliente<=75 & Aux$EdadCliente>70),]$RangoEdad<-"71-75"
    Aux[which(Aux$EdadCliente<=80 & Aux$EdadCliente>75),]$RangoEdad<-"76-80" 
    Aux[which(Aux$EdadCliente<=85 & Aux$EdadCliente>80),]$RangoEdad<-"81-85"  
    Aux[which(Aux$EdadCliente<=90 & Aux$EdadCliente>85),]$RangoEdad<-"86-90" 

write.csv(Aux,file="ClientesUnicredito.csv")


c2<-cut(Aux$EdadCliente,breaks = seq(from = 20, to = 100, by =5,right=FALSE))
c2<-as.data.frame(table(c2))
c2$Freq
x<-seq(from=20,to=100,by=5)
c<-as.data.frame(cbind(x,c2$Freq))

#########################################################################

aux<-semi_join(Correos,ClientesSolidario,by="Identificacion")
aux<-semi_join(Celulares,ClientesSolidario,by="Identificacion")
aux<-semi_join(Celulares,Correos,by="Identificacion")
aux<-semi_join(aux,ClientesSolidario,by="Identificacion")
aux<-semi_join(CelularesNot,ClientesSolidario,by="Identificacion")
NROW(aux)



aux<-ControlAcceso
aux$FechaRegistro<-as.Date(aux$FechaRegistro)
aux$FechaRegistro<-as.Date(aux$FechaRegistro, "%y-%m-%d")

#aux<-subset(aux,FechaRegistro >= as.Date("2016-12-01"))
aux$NumeroCelularRegistrado<-as.character(aux$NumeroCelularRegistrado)
aux["ff"]<-nchar(aux$NumeroCelularRegistrado)
aux<-subset(aux,ff>0)
table(aux$CanalUltimoAcceso)


aux<-CorreosNot
aux<-subset(aux,!duplicated(Identificacion))

aux<-semi_join(Correos,CorreosNot,by="Identificacion")
aux<-semi_join(ClientesSolidario,aux,by="Identificacion")
NROW(aux)
NROW(subset(Correos,!is.na(MailCanales)))

Correos<-subset(DispositivoUbicacion,TipoDispositivo=="EMAIL")
Celulares<-subset(DispositivoUbicacion,TipoDispositivo=="CEL")

Correos<-CorreosNot
Celulares<-CelularesNot

PasivoCel<-semi_join(Pasivo,auxCel,by="Identificacion")
#View(table(PasivoCel$TipoCuenta))
PasivoCel["TieneCel"]<-1

PasivoMail<-semi_join(Pasivo,auxMail,by="Identificacion")
PasivoMail["TieneMail"]<-1

Pasivo<-left_join(Pasivo,
                          subset(PasivoCel,select = c("Identificacion","TieneCel")),
                          by="Identificacion")
Pasivo<-left_join(Pasivo,
                          subset(PasivoMail,select = c("Identificacion","TieneMail")),
                          by="Identificacion")
Pasivo$TieneCel <- replace(Pasivo$TieneCel,which(is.na(Pasivo$TieneCel)),0)

Pasivo$TieneMail <- replace(Pasivo$TieneMail,which(is.na(Pasivo$TieneMail)),0)

nchar(as.numeric(auxCel$Telefono_01))


Aux[which(nchar(Aux$Identificacion)==9),]$Identificacion<-as.character(paste0(0,Aux[which(nchar(Aux$Identificacion)==9),]$Identificacion))



ggplot() +
  geom_density(aes(x=EdadCliente), colour="red", data=ProductoPasivo) + 
  geom_density(aes(x=EdadCliente), colour="blue", data=PasivoCel)+
  xlab("Edad del Cliente")+
  ylab("Densidad")+ 
  ggtitle("Densidad de Clientes con Celular")+
  scale_x_continuous(breaks=seq(0,90,by=10),limits =c(0,90))+
  theme_bw()

ggplot() +
  geom_density(aes(x=EdadCliente), colour="red", data=ProductoPasivo) + 
  geom_density(aes(x=EdadCliente), colour="blue", data=PasivoMail)+
  xlab("Edad del Cliente")+
  ylab("Densidad")+ 
  ggtitle("Densidad de Clientes con Mails")+
  scale_x_continuous(breaks=seq(0,90,by=10),limits =c(0,90))+
  theme_bw()

#######################Clientes del Solidario########################
aux<-rbind(ClientesIcesa,ClientesCC,ClientesOlla,ClientesMicro)
aux<-subset(aux,!duplicated(Identificacion))
aux["Intrumento"]<-NA
aux<-subset(aux,select = c("Identificacion","Intrumento"))
ClientesTDC["Intrumento"]<-NA
Pasivo["Intrumento"]<-NA
auxPasivo<-subset(Pasivo,select = c("Identificacion","Intrumento"))
aux3<-rbind(aux,auxPasivo,ClientesTDC)
aux3<-subset(aux3,!duplicated(Identificacion))

MailSol<-semi_join(aux3,Correos,by="Identificacion")

