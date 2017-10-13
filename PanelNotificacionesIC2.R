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
load("d:/Users_info/ALBANPD/My Documents/Bases/Clientes 27 07 2017.RData")
aux3<-Clientes %>% group_by(FechaCorte,Producto) %>%
  summarise(Clientes = length(unique(Identificacion))) 
aux3[which(is.na(aux3$Producto)),]$Producto<-"Excliente"
write.csv(aux3,file="Clientes.csv",row.names = FALSE)

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
Celulares<-subset(Celulares,select=c("Identificacion","FechaCorte","FechaCreacion"))
#############################################################################################

#####################Tabla de ambos dispositivos#############################################
Ambos<-semi_join(Correos,Celulares,by="Identificacion")
AlmenosUno<-rbind(Correos,Celulares)
AlmenosUno<-subset(AlmenosUno,!duplicated(Identificacion))
Correos["Dispositivo"]<-"Correo"
Celulares["Dispositivo"]<-"Celular"
Ambos["Dispositivo"]<-"Ambos"
AlmenosUno["Dispositivo"]<-"Al menos uno"

DN<-rbind(Correos,Celulares,Ambos,AlmenosUno)

######################Adquisición #######################################
AdDN<-DN
AdDN["Mensual"]<-NA
AdDN$Mensual<-as.Date(paste0(year(AdDN$FechaCreacion),"-",month(AdDN$FechaCreacion),"-",01),format="%Y-%m-%d")
AdDN$Mensual<-format(as.Date(AdDN$Mensual), "%Y-%m")


#AdDN["Trimestre"]<-NA
#AdDN[which(month(AdDN$FechaCreacion)<=3),]$Trimestre<-paste0(year(AdDN[which(month(AdDN$FechaCreacion)<=3),]$FechaCreacion) ,".T1")
#AdDN[which(month(AdDN$FechaCreacion)>3 & month(AdDN$FechaCreacion)<=6),]$Trimestre<-paste0(year(AdDN[which(month(AdDN$FechaCreacion)<=6 & month(AdDN$FechaCreacion)>3),]$FechaCreacion) ,".T2")
#AdDN[which(month(AdDN$FechaCreacion)>6 & month(AdDN$FechaCreacion)<=9),]$Trimestre<-paste0(year(AdDN[which(month(AdDN$FechaCreacion)<=9 & month(AdDN$FechaCreacion)>6),]$FechaCreacion) ,".T3")
#AdDN[which(month(AdDN$FechaCreacion)>9 & month(AdDN$FechaCreacion)<=12),]$Trimestre<-paste0(year(AdDN[which(month(AdDN$FechaCreacion)<=12 & month(AdDN$FechaCreacion)>9),]$FechaCreacion) ,".T4")


#Colocacion$FechaDesembolso<-as.character(as.Date(Colocacion$FechaDesembolso), "%Y-%m")


AdDN["MTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
AdDN[which(day(AdDN$FechaCreacion)<=25),]$MTD<-as.character(as.Date(AdDN[which(day(AdDN$FechaCreacion)<=25),]$FechaCreacion), "%Y-%m")

AdDN0<-AdDN
AdDN<-subset(AdDN,FechaCreacion>="2015-01-01")
AdDN <- as.data.frame(AdDN)
mAdDN <- reshape::melt(AdDN, id=c(1:4),na.rm = TRUE,variable_name = "Filtro") 

#tabla de 
aux<- mAdDN %>% group_by(Dispositivo,Filtro,value) %>% 
  summarise(Dispositivos = length(unique(Identificacion))) 


aux2<-ddply(aux,c("Dispositivo","Filtro"),transform,
           Tasa=c(NA,exp(diff(log(Dispositivos)))-1))

write.csv(aux2,file="Adquisicion.csv",sep=",",row.names = FALSE)

####################################################################


#####################Clientes con Dispositivos por mes####################

DNEne<-subset(DN,FechaCreacion<="2017-01-31")
DNEne$FechaCorte<-as.Date("2017-01-31")
DNEne$Filtro<-as.character("Mensual")
DNEne$value<-as.character("2017-01")

DNFeb<-subset(DN,FechaCreacion<="2017-02-28")
DNFeb$FechaCorte<-as.Date("2017-02-28")
DNFeb$Filtro<-as.character("Mensual")
DNFeb$value<-as.character("2017-02")

DNMar<-subset(DN,FechaCreacion<="2017-03-31")
DNMar$FechaCorte<-as.Date("2017-03-31")
DNMar$Filtro<-as.character("Mensual")
DNMar$value<-as.character("2017-03")

DNAbr<-subset(DN,FechaCreacion<="2017-04-30")
DNAbr$FechaCorte<-as.Date("2017-04-30")
DNAbr$Filtro<-as.character("Mensual")
DNAbr$value<-as.character("2017-04")

DNMay<-subset(DN,FechaCreacion<="2017-05-31")
DNMay$FechaCorte<-as.Date("2017-05-31")
DNMay$Filtro<-as.character("Mensual")
DNMay$value<-as.character("2017-05")

DNJun<-subset(DN,FechaCreacion<="2017-06-30")
DNJun$FechaCorte<-as.Date("2017-06-30")
DNJun$Filtro<-as.character("Mensual")
DNJun$value<-as.character("2017-06")

auxDN1<-rbind(DNEne,DNFeb,DNMar,DNAbr,DNMay,DNJun)

DNEne<-subset(DN,FechaCreacion<="2017-01-25")
DNEne$FechaCorte<-as.Date("2017-01-31")
DNEne$Filtro<-as.character("MTD")
DNEne$value<-as.character("2017-01")

DNFeb<-subset(DN,FechaCreacion<="2017-02-25")
DNFeb$FechaCorte<-as.Date("2017-02-28")
DNFeb$Filtro<-as.character("MTD")
DNFeb$value<-as.character("2017-02")

DNMar<-subset(DN,FechaCreacion<="2017-03-25")
DNMar$FechaCorte<-as.Date("2017-03-31")
DNMar$Filtro<-as.character("MTD")
DNMar$value<-as.character("2017-03")

DNAbr<-subset(DN,FechaCreacion<="2017-04-25")
DNAbr$FechaCorte<-as.Date("2017-04-30")
DNAbr$Filtro<-as.character("MTD")
DNAbr$value<-as.character("2017-04")

DNMay<-subset(DN,FechaCreacion<="2017-05-25")
DNMay$FechaCorte<-as.Date("2017-05-31")
DNMay$Filtro<-as.character("MTD")
DNMay$value<-as.character("2017-05")

DNJun<-subset(DN,FechaCreacion<="2017-06-25")
DNJun$FechaCorte<-as.Date("2017-06-30")
DNJun$Filtro<-as.character("MTD")
DNJun$value<-as.character("2017-06")

DNJul<-subset(DN,FechaCreacion<="2017-07-25")
DNJul$FechaCorte<-as.Date("2017-07-31")
DNJul$Filtro<-as.character("MTD")
DNJul$value<-as.character("2017-07")

auxDN2<-rbind(DNEne,DNFeb,DNMar,DNAbr,DNMay,DNJun,DNJul)

auxDN<-rbind(auxDN1,auxDN2)

auxClienteDN<-left_join(auxDN,Clientes, by=c("Identificacion"="Identificacion","FechaCorte"="FechaCorte"))
auxClienteDN[which(is.na(auxClienteDN$Producto)),]$Producto<-"Excliente"




auxClienteDN <- as.data.frame(auxClienteDN)
mClienteDN <- reshape::melt(auxClienteDN, id=c("Identificacion",
                                               "FechaCorte",
                                               "FechaCreacion",
                                               "Dispositivo",
                                               "Producto"),na.rm = TRUE,variable_name = "Filtro") 

#tabla de 
aux<- auxClienteDN %>% group_by(Dispositivo,Filtro,value,Producto) %>% 
  summarise(Dispositivos = length(unique(Identificacion))) 


aux2<-ddply(aux,c("Dispositivo","Filtro","Producto"),transform,
            Tasa=c(NA,exp(diff(log(Dispositivos)))-1))

left_join(aux2,aux3,by=c("FechaCorte"="FechaCorte","Producto"="Producto"))

write.csv(aux2,file = "DispositivosProducto.csv",row.names = FALSE)

############################Clientes Cruzados###########################
ClienteCorte<-subset(Clientes,FechaCorte=="2017-06-30")

Alia<-subset(ClienteCorte,Producto=="Alia")
Unicredito<-subset(ClienteCorte,Producto=="Unicrédito")
Microcredito<-subset(ClienteCorte,Producto=="Microcrédito")
Microcredito<-subset(Microcredito,!duplicated(Identificacion))
Olla<-subset(ClienteCorte,Producto=="Olla de Oro")
Olla<-subset(Olla,!duplicated(Identificacion))
Cuentas<-subset(ClienteCorte,Producto=="Cuentas de Ahorros" | Producto=="Cuentas Corrientes")
Cuentas<-subset(Cuentas,!duplicated(Identificacion))
Casas<-subset(ClienteCorte,Producto=="Casas Comerciales")
Casas<-subset(Casas,!duplicated(Identificacion))
##########Clientes Unicos################
NROW(aux<-anti_join(Alia,Unicredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Microcredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Olla,by="Identificacion"))
NROW(aux<-anti_join(aux,Cuentas,by="Identificacion"))
NROW(aux<-anti_join(aux,Casas,by="Identificacion"))



NROW(aux<-anti_join(Unicredito,Microcredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Alia,by="Identificacion"))
NROW(aux<-anti_join(aux,Olla,by="Identificacion"))
NROW(aux<-anti_join(aux,Cuentas,by="Identificacion"))
NROW(aux<-anti_join(aux,Casas,by="Identificacion"))


NROW(aux<-anti_join(Olla,Microcredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Alia,by="Identificacion"))
NROW(aux<-anti_join(aux,Unicredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Cuentas,by="Identificacion"))
NROW(aux<-anti_join(aux,Casas,by="Identificacion"))



NROW(aux<-anti_join(Cuentas,Microcredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Alia,by="Identificacion"))
NROW(aux<-anti_join(aux,Olla,by="Identificacion"))
NROW(aux<-anti_join(aux,Unicredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Casas,by="Identificacion"))


NROW(aux<-anti_join(Casas,Microcredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Alia,by="Identificacion"))
NROW(aux<-anti_join(aux,Olla,by="Identificacion"))
NROW(aux<-anti_join(aux,Unicredito,by="Identificacion"))
NROW(aux<-anti_join(aux,Cuentas,by="Identificacion"))
##########Alia cruzados################
NROW(aux<-semi_join(Alia,Unicredito,by="Identificacion"))
NROW(aux<-semi_join(Alia,Microcredito,by="Identificacion"))
NROW(aux<-semi_join(Alia,Olla,by="Identificacion"))
NROW(aux<-semi_join(Alia,Cuentas,by="Identificacion"))
NROW(aux<-semi_join(Alia,Casas,by="Identificacion"))


NROW(aux<-semi_join(Unicredito,Microcredito,by="Identificacion"))
NROW(aux<-semi_join(Unicredito,Olla,by="Identificacion"))
NROW(aux<-semi_join(Unicredito,Cuentas,by="Identificacion"))
NROW(aux<-semi_join(Unicredito,Casas,by="Identificacion"))


NROW(aux<-semi_join(Microcredito,Olla,by="Identificacion"))
NROW(aux<-semi_join(Microcredito,Cuentas,by="Identificacion"))
NROW(aux<-semi_join(Microcredito,Casas,by="Identificacion"))


NROW(aux<-semi_join(Olla,Cuentas,by="Identificacion"))
NROW(aux<-semi_join(Olla,Casas,by="Identificacion"))


NROW(aux<-semi_join(Casas,Cuentas,by="Identificacion"))
#######################Clientes con DN####################
DNCorte<-semi_join(DN,ClienteCorte,by="Identificacion")
aux<-DNCorte %>% group_by(Dispositivo) %>%
  summarise(Dispositivos = length (Identificacion))
aux["Clientes"]<-390164
aux["Porcentaje"]<-aux$Dispositivos/aux$Clientes


write.csv(aux,file = "TipoDNCorte.csv",row.names = FALSE)
