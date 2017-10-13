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
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("RODBCext", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

setwd("d:/Users_info/ALBANPD/My Documents/Bases")

####################Tarjetas activas###########################################################
odbcChannel <-odbcConnect("DWH_CRM") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "SB_TDCActivas")



TDCActivas <- sqlExecute(odbcChannel, "SELECT * FROM SB_TDCActivas", fetch = TRUE)
ClientesTDC<-subset(TDCActivas,CuentaTarjetaMonitor==1)

ClientesTDC<-subset(ClientesTDC,select = c("Identificacion",
                                          "IdCuentaTarjeta"
                                          ))

odbcClose(odbcChannel)

#################Saldo 231######################3

odbcChannel <-odbcConnect("DWH_TrabajoEstudios") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "HISTORICO.SaldosCartera231")



SaldosCartera231 <- sqlExecute(odbcChannel, "SELECT * FROM HISTORICO.SaldosCartera231 WHERE FechaCorte = ?", '2017-06-30', fetch = TRUE)



odbcClose(odbcChannel)



SaldosCartera231$Identificacion<-as.character(SaldosCartera231$Identificacion)
SaldosCartera231[which(nchar(SaldosCartera231$Identificacion)==9),]$Identificacion<-as.character(paste0(0,SaldosCartera231[which(nchar(SaldosCartera231$Identificacion)==9),]$Identificacion))
SaldosCartera231$FechaCorte<-as.Date(SaldosCartera231$FechaCorte)
SaldosCartera231$FechaCorte<-format(as.Date(SaldosCartera231$FechaCorte), "%Y-%m")


#############Productos de activo#####################

###################Tarjeta#######################
ClientesTDC<-subset(ClientesTDC,!duplicated(Identificacion))

NROW(ClientesTDC)

#########################Unicredito##########################
ClientesUnicredito<-subset(SaldosCartera231,CodigoProducto=="UNICREDITO" | 
                             CodigoProducto=="REESTRTRJ"|
                             CodigoProducto=="CONNORMALIZ")
ClientesUnicredito<-subset(ClientesUnicredito,!duplicated(Identificacion))

ClientesUnicredito<-subset(ClientesUnicredito,Estado!="CASTIG")

ClientesUnicredito<-subset(ClientesUnicredito,select = c("Identificacion","Estado"))
View(table(ClientesUnicredito$Estado))


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
ClientesMicro<-subset(ClientesMicro,select = c("Identificacion","CodigoProducto"))
View(table(ClientesMicro$CodigoProducto))

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
ClientesOlla<-subset(ClientesOlla,select = c("Identificacion"))

sum(ClientesOlla$Saldo)

NROW(ClientesOlla)
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
                     CodigoProducto=="LAURORA")

ClientesCC<-subset(ClientesCC,Estado!="CASTIG")
ClientesCC<-subset(ClientesCC,!duplicated(Identificacion))
ClientesCC<-subset(ClientesCC,select = c("Identificacion"))

sum(ClientesCC$Saldo)


#####################Producto Icesa#######################
ClientesIcesa<-subset(SaldosCartera231,
                      CodigoProducto=="ICESACCO" |
                        CodigoProducto=="ICESA")

ClientesIcesa<-subset(ClientesIcesa,Estado!="CASTIG")
sum(ClientesIcesa$Saldo)

ClientesIcesa<-subset(ClientesIcesa,!duplicated(Identificacion))
ClientesIcesa<-subset(ClientesIcesa,!duplicated(Identificacion))
ClientesIcesa<-subset(ClientesIcesa,select = c("Identificacion"))

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

table(CuentaCliente$IdTipoEstadoCuenta, CuentaCliente$TipoCuenta,CuentaCliente$TipoCuentaContable)

table(CuentaCliente$TipoCuenta,CuentaCliente$TipoCuentaContable)

odbcClose(odbcChannel)

CuentaCliente<-left_join(CuentaCliente,Producto,by="Producto")
CuentaCliente<-left_join(CuentaCliente,SubProducto,by="SubProducto")

ClientesCuentas<-subset(CuentaCliente,IdTipoEstadoCuenta==1 & IdInstitucion==2)
ClientesCuentas<-subset(ClientesCuentas,!duplicated(IdCliente))
ClientesCuentas<-subset(ClientesCuentas,select = c("IdCliente","NombreProducto",
                                                   "NombreSubProducto",
                                                   "TipoCuentaContable",
                                                   "TipoCuenta.x"))


################Informacion del cliente################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Cliente")

ClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM ClienteNatural WHERE IdInstitucion = 2", fetch = TRUE)

Cliente <- sqlExecute(odbcChannel, "SELECT * FROM Cliente", fetch = TRUE)

InformacionDemografica<- sqlExecute(odbcChannel, "SELECT * FROM InformacionDemografica", fetch = TRUE)

odbcClose(odbcChannel)


Demografica<-left_join(ClienteNatural,Cliente,by="IdCliente")
Demografica<-subset(Demografica,select = c("Identificacion","IdCliente","IdClienteNatural"))

Demografica<-left_join(InformacionDemografica,Demografica,by="IdClienteNatural")
Demografica<-subset(Demografica,select=c("Identificacion",
                                         "IdCliente",
                                         "Sexo",
                                         "EstadoCivil",
                                         "FechaNacimiento"
                                         ))


##################Calculo de Edades############################
d2<-Demografica$FechaNacimiento
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
n<-length(Demografica$FechaNacimiento)
Demografica["EdadCliente"] <- NA
Demografica$EdadCliente<-age_years(d5,rep(as.Date(ISOdate("2016", "12","31")),n))
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
Demografica$EdadCliente<-as.numeric(Demografica$EdadCliente)
####################Estados Civiles##############
table(Demografica$EstadoCivil)
Demografica$EstadoCivil<-as.character(Demografica$EstadoCivil)
Demografica[which(Demografica$EstadoCivil=="ESTCIVC"),]$EstadoCivil<-"Casado"
Demografica[which(Demografica$EstadoCivil=="ESTCIVD"),]$EstadoCivil<-"Divorciado"
Demografica[which(Demografica$EstadoCivil=="ESTCIVS"),]$EstadoCivil<-"Soltero"
Demografica[which(Demografica$EstadoCivil=="ESTCIVU"),]$EstadoCivil<-"Unión Libre"
Demografica[which(Demografica$EstadoCivil=="ESTCIVV"),]$EstadoCivil<-"Viudo"

##############Cruze con los productos#####################
ClientesCC<-left_join(ClientesCC,Demografica,by="Identificacion")
ClientesCuentas<-left_join(ClientesCuentas,Demografica,by="IdCliente")
ClientesCuentas<-subset(ClientesCuentas,!duplicated(Identificacion))

ClientesIcesa<-left_join(ClientesIcesa,Demografica,by="Identificacion")
ClientesMicro<-left_join(ClientesMicro,Demografica,by="Identificacion")
ClientesOlla<-left_join(ClientesOlla,Demografica,by="Identificacion")

ClientesTDC<-left_join(ClientesTDC,Demografica,by="Identificacion")
ClientesUnicredito<-left_join(ClientesUnicredito,Demografica,by="Identificacion")

write.csv(ClientesCC,file="ClientesCC.csv")

write.csv(ClientesTDC,file="ClientesTDC.csv")


write.csv(ClientesCuentas,file="ClientesCuentas.csv")

write.csv(ClientesIcesa,file="ClientesIcesa.csv")

write.csv(ClientesMicro,file="ClientesMicro.csv")

write.csv(ClientesOlla,file="ClientesOlla.csv")

write.csv(ClientesUnicredito,file="ClientesUnicredito.csv")


#####################Correos Disponibles###############


odbcChannel <-odbcConnect("DWH_Reporte") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Rep_ClienteCorreoElectronico")



ClienteCorreoElectronico <- sqlExecute(odbcChannel, "SELECT * FROM Rep_ClienteCorreoElectronico",  fetch = TRUE)

odbcClose(odbcChannel)

######################Cruze con correos##################
ClienteCorreoElectronico$ClienteBankPlus_1<-as.character(ClienteCorreoElectronico$ClienteBankPlus_1)
ClienteCorreoElectronico$ClienteBankPlus_2<-as.character(ClienteCorreoElectronico$ClienteBankPlus_2)
ClienteCorreoElectronico$ClienteBankPlus_3<-as.character(ClienteCorreoElectronico$ClienteBankPlus_3)
ClienteCorreoElectronico$PlanRecompensa<-as.character(ClienteCorreoElectronico$PlanRecompensa)
ClienteCorreoElectronico$MailCanales<-as.character(ClienteCorreoElectronico$MailCanales)

BP1<-subset(ClienteCorreoElectronico,select=c("Identificacion","ClienteBankPlus_1"))
BP1<-BP1[which(nchar(BP1$ClienteBankPlus_1)>3),]
BP2<-subset(ClienteCorreoElectronico,select=c("Identificacion","ClienteBankPlus_2"))
BP2<-BP2[which(nchar(BP2$ClienteBankPlus_2)>3),]
BP3<-subset(ClienteCorreoElectronico,select=c("Identificacion","ClienteBankPlus_3"))
BP3<-BP3[which(nchar(BP3$ClienteBankPlus_3)>3),]

PlanRecompensa<-subset(ClienteCorreoElectronico,select=c("Identificacion","PlanRecompensa"))
PlanRecompensa<-PlanRecompensa[which(nchar(PlanRecompensa$PlanRecompensa)>3),]

MailCanales<-subset(ClienteCorreoElectronico,select=c("Identificacion","MailCanales"))
MailCanales<-MailCanales[which(nchar(MailCanales$MailCanales)>3),]

###################Cruze con clientes################3
CorreosTDCBP1<-semi_join(ClientesTDC,BP1,by="Identificacion")
CorreosTDCBP1<-subset(CorreosTDCBP1,!duplicated(CorreosTDCBP1$Identificacion))
NROW(CorreosTDCBP1)

CorreosTDCBP2<-semi_join(ClientesTDC,BP2,by="Identificacion")
CorreosTDCBP2<-subset(CorreosTDCBP2,!duplicated(CorreosTDCBP2$Identificacion))
NROW(CorreosTDCBP2)


CorreosTDCBP3<-semi_join(ClientesTDC,BP3,by="Identificacion")
CorreosTDCBP3<-subset(CorreosTDCBP3,!duplicated(CorreosTDCBP3$Identificacion))
NROW(CorreosTDCBP3)

CorreosTDCPlanRecompensa<-semi_join(ClientesTDC,PlanRecompensa,by="Identificacion")
CorreosTDCPlanRecompensa<-subset(CorreosTDCPlanRecompensa,!duplicated(CorreosTDCPlanRecompensa$Identificacion))
NROW(CorreosTDCPlanRecompensa)


aux<-semi_join(ClientesTDC,PlanRecompensa,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)



aux<-semi_join(ClientesTDC,MailCanales,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)

aux<-subset(ClienteCorreoElectronico,!is.na(MailCanales))


aux<-semi_join(ClientesMicro,MailCanales,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)

aux<-semi_join(ClientesMicro,BP3,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)



aux<-semi_join(ClientesUnicredito,MailCanales,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)


aux<-semi_join(ClientesUnicredito,BP3,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)


aux<-semi_join(ClientesOlla,MailCanales,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)


aux<-semi_join(ClientesOlla,BP3,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)

aux<-semi_join(ClientesCuentas,MailCanales,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)


aux<-semi_join(ClientesCuentas,BP3,by="Identificacion")
aux<-subset(aux,!duplicated(Identificacion))
NROW(aux)

