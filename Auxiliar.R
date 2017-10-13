library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("xtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("arules", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggrepel", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("caret", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library(ROCR)
library("stargazer", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("scales", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("gtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("randomForest", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

setwd("d:/Users_info/ALBANPD/My Documents/Bases")

Reporte43A<-read.table("Reporte-RIEG-INCO-00043A.txt",header=TRUE,sep="\t",na.strings = "NA")

Reporte42A<-read.table("Reporte-RIEG-INCO-00042A.txt",header=TRUE,sep="\t",na.strings = "NA")

c<-(sapply(Reporte43A, function(x) class(x)[[1]]))

write.csv(c,file = "c43A.csv")



Reporte42A<-read.table("Reporte-RIEG-INCO-00042A.txt",header=TRUE,sep="\t",na.strings = "NA", colClasses =c(
  "factor",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
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
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
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
  "integer",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "integer",
  "integer",
  "factor",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "factor",
  "numeric",
  "integer",
  "numeric",
  "factor",
  "integer",
  "factor",
  "factor",
  "factor"
))




Reporte43A<-read.table("Reporte-RIEG-INCO-00043A.txt",header=TRUE,sep="\t",na.strings = "NA", colClasses =c(
  "factor",
  "integer",
  "factor",
  "factor",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "integer",
  "factor",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "factor",
  "numeric",
  "integer",
  "factor",
  "integer",
  "numeric",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "factor",
  "factor",
  "factor"
  ))


R42<-subset(Reporte42A, select=c(
"IdCuentaTarjeta",
"Identificacion",
"EstadoCuentaTarjeta",
"EstadoTarjeta",
"PropietariaCartera",
"FechaConciliacionUltimoConsumo",
"FechaActivacionCuentaTarjeta",
"Autorizacion12M",
"Max_Dias_sin_consumos_12M",
"Max_Dias_sin_consumos_6M",
"Max_Dias_sin_consumos_3M",
"Max_Dias_sin_avances_12M",
"Max_Dias_sin_avances_6M",
"Max_Dias_sin_avances_3M",
"NumeroConsumos3M",
"NumeroConsumos6M",
"NumeroConsumos12M",
"NumeroConsumosAvance3M",
"NumeroConsumosAvance6M",
"NumeroConsumosAvance12M",
"NumeroConsumosSuperAvance3M",
"NumeroConsumosSuperAvance6M",
"NumeroConsumosSuperAvance12M",
"NumeroConsumosAire3M",
"NumeroConsumosAire6M",
"NumeroConsumosAire12M",
"NumeroConsumosDebito3M",
"NumeroConsumosDebito6M",
"NumeroConsumosDebito12M",
"MedioPagoPOS12M",
"MedioPagoVoucher12M",
"NumeroConsumoCorriente3M",
"NumeroConsumoCorriente6M",
"NumeroConsumoCorriente12M",
"NumeroConsumoSinInteres3M",
"NumeroConsumoSinInteres6M",
"NumeroConsumoSinInteres12M",
"NumeroConsumoConInteres3M",
"NumeroConsumoConInteres6M",
"NumeroConsumoConInteres12M",
"SVidaDesgravamen",
"SAsistencia",
"SExequial",
"STotalFamiliar",
"SDental",
"STotalDif",
"Seguro20160430",
"AfinidadTarjeta",
"FechaNacimiento",
"EstadoCliente",
"Genero",
"EstadoCivil",
"NivelInstruccion",
"ProvinciaNacimiento",
"CiudadNacimiento",
"ParroquiaNacimiento",
"ProvinciaDomicilio",
"CiudadDomicilio",
"ParroquiaDomicilio",
"TelCel",
"CargasFamiliares",
"Sector",
"ActividadEconomica",
"FuenteIngreso",
"LineaNegocio",
"Profesion",
"SegmentoTDC_20160430",
"Numero_tarjetasAbril2016",
"Numero_tarjetasEne16",
"Numero_tarjetasOct15",
"Otra_Tarjeta20160430",
"ValorSaldo12M",
"ValorSaldo6M",
"ValorSaldo3M",
"SaldoCompetencia12M",
"CupoAprobadoCompetencia12M",
"SaldoVencidoCompetencia12M",
"MaximoDiasMoraCompetencia12M",
"SaldoCompetencia6M",
"CupoAprobadoCompetencia6M",
"SaldoVencidoCompetencia6M",
"MaximoDiasMoraCompetencia6M",
"SaldoCompetencia3M",
"CupoAprobadoCompetencia3M",
"SaldoVencidoCompetencia3M",
"MaximoDiasMoraCompetencia3M",
"Pres_Micro_Abr16",
"Pres_Micro_12M",
"Pres_Uni_Abr16",
"Pres_Uni_12M",
"Olla_Oro_Abr16",
"Olla_Oro_12M",
"Sueldo20160430",
"sueldo20160131",
"TieneEmail",
"MesesRecencia",
"D",
"Saldo231",
"AtrazoTrimestral",
"TipoCliente",
"MontoSinSeguro",
"MontoConSeguro",
"MontoSeguro",
"TienePlan",
"SoloSeguro",
"SoloPlan",
"SoloPlanSeguro"))




R43<-subset(Reporte43A, select=c(
  "IdCuentaTarjeta",
  "Identificacion",
  "EstadoCuentaTarjeta",
  "EstadoTarjeta",
  "PropietariaCartera",
  "FechaActivacionCuentaTarjeta",
  "FechaConciliacionUltimoConsumo",
  "Autorizacion12M",
  "Max_Dias_sin_consumos_12M",
  "Max_Dias_sin_consumos_6M",
  "Max_Dias_sin_consumos_3M",
  "Max_Dias_sin_avances_12M",
  "Max_Dias_sin_avances_6M",
  "Max_Dias_sin_avances_3M",
  "NumeroConsumos3M",
  "NumeroConsumos6M",
  "NumeroConsumos12M",
  "NumeroConsumosAvance3M",
  "NumeroConsumosAvance6M",
  "NumeroConsumosAvance12M",
  "NumeroConsumosSuperAvance3M",
  "NumeroConsumosSuperAvance6M",
  "NumeroConsumosSuperAvance12M",
  "NumeroConsumosAire3M",
  "NumeroConsumosAire6M",
  "NumeroConsumosAire12M",
  "NumeroConsumosDebito3M",
  "NumeroConsumosDebito6M",
  "NumeroConsumosDebito12M",
  "MedioPagoPOS12M",
  "MedioPagoVoucher12M",
  "NumeroConsumoCorriente3M",
  "NumeroConsumoCorriente6M",
  "NumeroConsumoCorriente12M",
  "NumeroConsumoSinInteres3M",
  "NumeroConsumoSinInteres6M",
  "NumeroConsumoSinInteres12M",
  "NumeroConsumoConInteres3M",
  "NumeroConsumoConInteres6M",
  "NumeroConsumoConInteres12M",
  "SVidaDesgravamen",
  "SAsistencia",
  "SExequial",
  "STotalFamiliar",
  "SDental",
  "STotalDif",
  "Seguro20151231",
  "AfinidadTarjeta",
  "FechaNacimiento",
  "EstadoCliente",
  "Genero",
  "EstadoCivil",
  "NivelInstruccion",
  "ProvinciaNacimiento",
  "CiudadNacimiento",
  "ParroquiaNacimiento",
  "ProvinciaDomicilio",
  "CiudadDomicilio",
  "ParroquiaDomicilio",
  "TelCel",
  "CargasFamiliares",
  "Sector",
  "ActividadEconomica",
  "FuenteIngreso",
  "LineaNegocio",
  "Profesion",
  "SegmentoTDC_20151231",
  "Numero_tarjetasDic15",
  "Numero_tarjetasSep15",
  "Numero_tarjetasJun15",
  "Otra_Tarjeta20151231",
  "ValorSaldo12M",
  "ValorSaldo6M",
  "ValorSaldo3M",
  "SaldoCompetencia12M",
  "CupoAprobadoCompetencia12M",
  "SaldoVencidoCompetencia12M",
  "MaximoDiasMoraCompetencia12M",
  "SaldoCompetencia6M",
  "CupoAprobadoCompetencia6M",
  "SaldoVencidoCompetencia6M",
  "MaximoDiasMoraCompetencia6M",
  "SaldoCompetencia3M",
  "CupoAprobadoCompetencia3M",
  "SaldoVencidoCompetencia3M",
  "MaximoDiasMoraCompetencia3M",
  "Pres_Micro_Dic15",
  "Pres_Micro_12M",
  "Pres_Uni_Dic15",
  "Pres_Uni_12M",
  "Olla_Oro_Dic15",
  "Olla_Oro_12M",
  "Sueldo20160131",
  "sueldo20150930",
  "TieneEmail",
  "MesesRecencia",
  "D",
  "Saldo231",
  "AtrazoTrimestral",
  "TipoCliente",
  "MontoSinSeguro",
  "MontoConSeguro",
  "MontoSeguro",
  "TienePlan",
  "SoloSeguro",
  "SoloPlan",
  "SoloPlanSeguro"))



colnames(R43)<-c(
  "IdCuentaTarjeta",
  "Identificacion",
  "EstadoCuentaTarjeta",
  "EstadoTarjeta",
  "PropietariaCartera",
  "FechaActivacionCuentaTarjeta",
  "FechaConciliacionUltimoConsumo",
  "Autorizacion12M",
  "Max_Dias_sin_consumos_12M",
  "Max_Dias_sin_consumos_6M",
  "Max_Dias_sin_consumos_3M",
  "Max_Dias_sin_avances_12M",
  "Max_Dias_sin_avances_6M",
  "Max_Dias_sin_avances_3M",
  "NumeroConsumos3M",
  "NumeroConsumos6M",
  "NumeroConsumos12M",
  "NumeroConsumosAvance3M",
  "NumeroConsumosAvance6M",
  "NumeroConsumosAvance12M",
  "NumeroConsumosSuperAvance3M",
  "NumeroConsumosSuperAvance6M",
  "NumeroConsumosSuperAvance12M",
  "NumeroConsumosAire3M",
  "NumeroConsumosAire6M",
  "NumeroConsumosAire12M",
  "NumeroConsumosDebito3M",
  "NumeroConsumosDebito6M",
  "NumeroConsumosDebito12M",
  "MedioPagoPOS12M",
  "MedioPagoVoucher12M",
  "NumeroConsumoCorriente3M",
  "NumeroConsumoCorriente6M",
  "NumeroConsumoCorriente12M",
  "NumeroConsumoSinInteres3M",
  "NumeroConsumoSinInteres6M",
  "NumeroConsumoSinInteres12M",
  "NumeroConsumoConInteres3M",
  "NumeroConsumoConInteres6M",
  "NumeroConsumoConInteres12M",
  "SVidaDesgravamen",
  "SAsistencia",
  "SExequial",
  "STotalFamiliar",
  "SDental",
  "STotalDif",
  "Seguro",
  "AfinidadTarjeta",
  "FechaNacimiento",
  "EstadoCliente",
  "Genero",
  "EstadoCivil",
  "NivelInstruccion",
  "ProvinciaNacimiento",
  "CiudadNacimiento",
  "ParroquiaNacimiento",
  "ProvinciaDomicilio",
  "CiudadDomicilio",
  "ParroquiaDomicilio",
  "TelCel",
  "CargasFamiliares",
  "Sector",
  "ActividadEconomica",
  "FuenteIngreso",
  "LineaNegocio",
  "Profesion",
  "SegmentoTDC",
  "Numero_tarjetas",
  "Numero_tarjetasT3",
  "Numero_tarjetasT6",
  "Otra_Tarjeta",
  "ValorSaldo12M",
  "ValorSaldo6M",
  "ValorSaldo3M",
  "SaldoCompetencia12M",
  "CupoAprobadoCompetencia12M",
  "SaldoVencidoCompetencia12M",
  "MaximoDiasMoraCompetencia12M",
  "SaldoCompetencia6M",
  "CupoAprobadoCompetencia6M",
  "SaldoVencidoCompetencia6M",
  "MaximoDiasMoraCompetencia6M",
  "SaldoCompetencia3M",
  "CupoAprobadoCompetencia3M",
  "SaldoVencidoCompetencia3M",
  "MaximoDiasMoraCompetencia3M",
  "Pres_Micro",
  "Pres_Micro_12M",
  "Pres_Uni",
  "Pres_Uni_12M",
  "Olla_Oro",
  "Olla_Oro_12M",
  "Sueldo",
  "sueldoT3",
  "TieneEmail",
  "MesesRecencia",
  "D",
  "Saldo231",
  "AtrazoTrimestral",
  "TipoCliente",
  "MontoSinSeguro",
  "MontoConSeguro",
  "MontoSeguro",
  "TienePlan",
  "SoloSeguro",
  "SoloPlan",
  "SoloPlanSeguro")

#Filtro clientes activos - activos  
#R42AC<-subset(R42,EstadoTarjeta=="ACTIVA" & EstadoCuentaTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")

#R43AC<-subset(R43,EstadoTarjeta=="ACTIVA" & EstadoCuentaTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")
#############################################################################
#########################Unificadas a Diciembre y Abril######################
R42<-R42[,-c(3,4,5,6)]
R43<-R43[,-c(3,4,5,6)]
SaldosDic<-subset(SaldosDic, EstadoTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")
SaldosAbr<-subset(SaldosAbr, EstadoTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")

UnficadaDic<-left_join(SaldosDic,R43,by =c("Identificacion","IdCuentaTarjeta"))
UnficadaAbr<-left_join(SaldosAbr,R42,by =c("Identificacion","IdCuentaTarjeta"))

###############Calculo de edades###########################
Base3<-UnficadaAbr
########################Edad cliente######################
d2<-Base3$FechaNacimiento
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
n<-length(Base3$FechaNacimiento)
UnficadaAbr["Edad.Cliente"] <- NA
UnficadaAbr$Edad.Cliente<-age_years(d5,rep(as.Date(ISOdate("2016", "04","30")),n))
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

#######################################Edad de la cuenta tarjeta###########################
d2<-Base3$FechaActivacionCuentaTarjeta
d3<- substr(d2, 0, 10)
yy<-substr(d3,0,4)
mm<-substr(d3,6,7)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2015", "12","31"))
n<-length(Base3$FechaActivacionCuentaTarjeta)
UnficadaAbr["Edad.Cuenta.Tc"] <- "NA"
UnficadaAbr$Edad.Cuenta.Tc<-round((as.Date(ISOdate("2016", "04","30"))-as.Date(d5))/30, digits = 0)
#####################################Tiempo del último consumo#######################
d2<-Base3$FechaConciliacionUltimoConsumo
d3<- substr(d2, 0, 10)
yy<-substr(d3,0,4)
mm<-substr(d3,6,7)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2015", "12","31"))
n<-length(Base3$FechaConciliacionUltimoConsumo)
UnficadaAbr["Dias.sin.consumo"] <- NA
UnficadaAbr$Dias.sin.consumo<-round((as.Date(ISOdate("2016", "04","30"))-as.Date(d5)), digits = 0)
UnficadaAbr$Meses.sin.consumo<-round((as.Date(ISOdate("2016", "04","30"))-as.Date(d5))/30, digits = 0)
UnficadaAbr$Meses.sin.consumo<-as.numeric(UnficadaAbr$Meses.sin.consumo)
UnficadaAbr$Edad.Cuenta.Tc<-as.numeric(UnficadaAbr$Edad.Cuenta.Tc)
UnficadaAbr$Dias.sin.consumo<-as.numeric(UnficadaAbr$Dias.sin.consumo)


########################################Asalariado########################
#View(subset(Base3,select = c("FuenteIngreso","Sueldo20160131")))
UnficadaAbr["Asalariado"]<-"0"
UnficadaAbr$Asalariado[UnficadaAbr$FuenteIngreso=="SALARIO"]<-"1"
########################Numero_tarjetasDic15##################
UnficadaAbr$Numero_tarjetas[is.na(UnficadaAbr$Numero_tarjetas)]<-0
########################Numero_tarjetasSep15##################
UnficadaAbr$Numero_tarjetasT3[is.na(UnficadaAbr$Numero_tarjetasT3)]<-0
########################Numero_tarjetasJun15##################
UnficadaAbr$Numero_tarjetasT6[is.na(UnficadaAbr$Numero_tarjetasT6)]<-0
##########################NuevaTC##############################33
UnficadaAbr["NuevaTc3M"]<-UnficadaAbr$Numero_tarjetas-UnficadaAbr$Numero_tarjetasT3
UnficadaAbr["NuevaTc6M"]<-UnficadaAbr$Numero_tarjetas-UnficadaAbr$Numero_tarjetasT6
####################################################################
ND<-anti_join(UnficadaDic,UnficadaAbr,by ="Identificacion")

NAb<-anti_join(UnficadaAbr,UnficadaDic,by="Identificacion")
NN<-anti_join(UnficadaDic,ND,by="Identificacion")
NN %>% count(Objetivo)
NoDesertores %>% count()

summary(NoDesertores$T6.CupoUtilizado)
NROW(subset(NoDesertores,!is.finite(T6.CupoUtilizado)))
########################Muetreo 50/50 Nodesertores##################

Particion <- createDataPartition(NoDesertores$IdCuentaTarjeta, p=0.5, list=FALSE)
Grupo1 <- NoDesertores[ Particion, ]
Grupo2 <- NoDesertores[ -Particion, ]

Grupo1<-subset(Grupo1,select=c("Identificacion"))
Grupo2<-subset(Grupo2,select=c("Identificacion"))

Desertores1<-subset(UnficadaDic,Objetivo==1 & Mesdesercion<=4)
Desertores1<-subset(Desertores1,select=c("Identificacion"))
Desertores2<-subset(UnficadaAbr,Objetivo==1)
Desertores2<-subset(Desertores2,select=c("Identificacion"))  

##############Concatenar Base#######################

Grupo1<-left_join(Grupo1,UnficadaDic,by="Identificacion")
Desertores1<-left_join(Desertores1,UnficadaDic,by="Identificacion")

Grupo2<-left_join(Grupo2,UnficadaAbr,by="Identificacion")
Desertores2<-left_join(Desertores2,UnficadaAbr,by="Identificacion")

Base<-rbind(Grupo1,Desertores1,Grupo2,Desertores2)

Base %>% count(TipoCliente)
#Activos<-subset(Base,TipoCliente=="ACTIVO")
Activos<-subset(UnficadaDic,TipoCliente=="ACTIVO")
Activos$Objetivo <- ifelse(Activos$Mesdesercion<=4,1,0)
Activos[is.na(Activos$Objetivo),]$Objetivo<-0
table(Activos$Objetivo)
#Base %>% count(Objetivo)
##################    Poner ceros a los NA e Infinitos##############

Activos[!is.finite(Activos$T3.CupoUtilizado),]$T3.CupoUtilizado<-0
Activos[!is.finite(Activos$T6.CupoUtilizado),]$T6.CupoUtilizado<-0
Activos[!is.finite(Activos$T36.CupoUtilizado),]$T36.CupoUtilizado<-0
Activos[!is.finite(Activos$`CupoUtilizado/Cupo.Apro.Normal`),]$`CupoUtilizado/Cupo.Apro.Normal`<-0
Activos[!is.finite(Activos$`Cupo.Ut.Avances/Cupo.Aprobado`),]$`Cupo.Ut.Avances/Cupo.Aprobado`<-0

Activos[!is.finite(Activos$Meses.sin.consumo),]$Meses.sin.consumo<-mean(Activos$Meses.sin.consumo,na.rm=TRUE)
Activos[!is.finite(Activos$Edad.Cliente),]$Edad.Cliente<-mean(Activos$Edad.Cliente,na.rm=TRUE)
Activos[!is.finite(Activos$CargasFamiliares),]$CargasFamiliares<-mean(Activos$CargasFamiliares,na.rm=TRUE)
Activos[!is.finite(Activos$Edad.Cuenta.Tc),]$Edad.Cuenta.Tc<-mean(Activos$Edad.Cuenta.Tc,na.rm=TRUE)



ActivosDic<-subset(BaseDic,TipoCliente=="ACTIVO")


ActivosDic$T3.CupoUtilizado[which(!is.finite(ActivosDic$T3.CupoUtilizado))] <- 0

ActivosDic$T6.CupoUtilizado[which(!is.finite(ActivosDic$T6.CupoUtilizado))] <- 0

ActivosDic$T36.CupoUtilizado[which(!is.finite(ActivosDic$T36.CupoUtilizado))] <- 0

ActivosDic$`CupoUtilizado/Cupo.Apro.Normal`[which(!is.finite(ActivosDic$`CupoUtilizado/Cupo.Apro.Normal`))] <- 0

ActivosDic$`Cupo.Ut.Avances/Cupo.Aprobado`[which(!is.finite(ActivosDic$`Cupo.Ut.Avances/Cupo.Aprobado`))] <- 0

ActivosDic$Meses.sin.consumo[which(!is.finite(ActivosDic$Meses.sin.consumo))]<-mean(ActivosDic$Meses.sin.consumo,na.rm=TRUE)

ActivosDic$Edad.Cliente[which(!is.finite(ActivosDic$Edad.Cliente))]<-mean(ActivosDic$Edad.Cliente,na.rm=TRUE)

ActivosDic$CargasFamiliares[which(!is.finite(ActivosDic$CargasFamiliares))]<-mean(ActivosDic$CargasFamiliares,na.rm=TRUE)

ActivosDic$Edad.Cuenta.Tc[which(!is.finite(ActivosDic$Edad.Cuenta.Tc))]<-mean(ActivosDic$Edad.Cuenta.Tc,na.rm=TRUE)



######################Sancar NA e Inf##############
Aux<-subset(Activos,
            is.finite(Meses.sin.consumo) &
              is.finite(Edad.Cliente) &
              is.finite(CargasFamiliares) &
              is.finite(Edad.Cuenta.Tc)&
              is.finite(Activos$T3.CupoUtilizado)&
              is.finite(Activos$T6.CupoUtilizado)&
              is.finite(Activos$T36.CupoUtilizado)&
              is.finite(Activos$`CupoUtilizado/Cupo.Apro.Normal`)&
              is.finite(Activos$`Cupo.Ut.Avances/Cupo.Aprobado`)
           
           )

Aux %>% count(TipoCliente)
Aux<-subset(Aux,!is.na(Identificacion) & TipoCliente=="ACTIVO")

#############Conjunto entrenamiento y prueba##############
Train <- createDataPartition(ActivosDic$IdCuentaTarjeta, p=0.5, list=FALSE)
training <- ActivosDic[ Train, ]
testing <- ActivosDic[ -Train, ]

training %>% count(Objetivo)
testing %>% count(Objetivo)
#######################Balaceo  de base Buenos scalados####################
muestra<-sample_n(subset(training, Objetivo==0),552,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(training, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-205]
########################Malos####################
n<-64
Malos<-do.call("rbind", replicate(n, subset(training, Objetivo==1), simplify = FALSE))

Balanceada<-rbind(Buenos,Malos)
Balanceada %>% count(Objetivo)


#####################################################

table(training$Objetivo)
trainingD<-subset(training,Objetivo==1)
trainingND<-subset(training,Objetivo==0)

Train <- createDataPartition(trainingND$IdCuentaTarjeta, p=0.5, list=FALSE)
trainingND <- trainingND[ Train, ]

n<-4
trainingD<-do.call("rbind", replicate(n, subset(trainingD, Objetivo==1), simplify = FALSE))

training2<-rbind(trainingND,trainingD)



#######################################################


Aux<-subset(Aux, select=c(
  "CupoUtilizado",
  "CupoUtilizadoNormal",
  "Prom.CupoUtilizado3",
  "Cupo.Utilizado/Cupo.Aprobado",
  "Cupo.Ut.Avances/Cupo.Aprobado",
  "NumeroConsumosAvance12M",
  "T3.CupoUtilizado",
  "T6.CupoUtilizado",
  "T36.CupoUtilizado",
  "Meses.sin.consumo",
  "Prom.DiasMora6",
  "Prom.DiasMora3",
  "NumeroConsumoCorriente3M",
  "NumeroConsumoSinInteres6M",
  "NumeroConsumoConInteres3M",
  "NumeroConsumosSuperAvance12M",
  "Max_Dias_sin_consumos_3M",
  "Numero_tarjetas",
  "Edad.Cliente",
  "CargasFamiliares",
  "Edad.Cuenta.Tc"))
#######################Modelo Balanceado################################
library("arm", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

mod1 <- glm(as.factor(Objetivo) ~ 
                     #CupoUtilizado+
                  #CupoUtilizadoNormal+
                   Prom.CupoUtilizado3+
             #`Cupo.Utilizado/Cupo.Aprobado`+
             `Cupo.Ut.Avances/Cupo.Aprobado`+
                   NumeroConsumosAvance12M+
                  #T3.CupoUtilizado+
                   #T6.CupoUtilizado+
                   #T36.CupoUtilizado+
              Dias.sin.consumo+
                   Prom.DiasMora3+
                  # NumeroConsumoCorriente3M+
                   NumeroConsumoSinInteres6M+
                   NumeroConsumoConInteres3M+
                   #NumeroConsumosSuperAvance12M+
                   Max_Dias_sin_consumos_3M+
                   Numero_tarjetas+
                   Edad.Cliente+
                   #CargasFamiliares+
                   Edad.Cuenta.Tc-1
              #MontoSeguro+
            # MontoConSeguro+
            
              #Numero_tarjetasT6+
             #EstadoCivil+
              #ProvinciaDomicilio+
             #NivelInstruccion+
            #Sector+
              #TienePlan+
             #SoloPlan+
             # SoloSeguro+
             #SoloPlanSeguro
            , data=Balanceada, family="poisson")
                   #, data=BalanceadaDic, family="binomial"(link = "probit"))
#, data=Balanceada, family = binomial(link = "logit"))

mod1 <- glm(as.factor(Objetivo) ~ 
              NormPromCupoUtilizado6M+
             # `Cupo.Utilizado/Cupo.Aprobado`+
              `Cupo.Ut.Avances/Cupo.Aprobado`+
              DiferenciaTC3M+
            #Prom.CupoUtilizado6+
            #CupoAprobado+
            NumeroConsumosAvance12M+
           # Prom.CupoUtilizadoAvance6+
            Max_Dias_sin_consumos_12M+
            STotalFamiliar+
            Edad.Cliente+
            Prom.DiasMora+
            SVidaDesgravamen+
              #CupoUtilizadoAvance+
            #T36.CupoUtilizado+
            NumeroConsumoCorriente12M+
            Edad.Cuenta.Tc+
            Numero_tarjetas+
            #NuevaTc6M+
            #Prom.CupoUtilizadoAvance+
            MedioPagoVoucher12M+
            Prom.DiasMora3+
              #Numero_tarjetasT6+
            NumeroConsumoSinInteres12M+
           # CupoUtilizadoAvance+
            #NumeroConsumos12M+
             SoloPlan+
             SoloSeguro+
             SoloPlanSeguro
            #Prom.DiasMora6+
           # EstadoCivil+
            #Asalariado
            ,data=BalanceadaDic, family = binomial(link = "logit"))


mod1 <- glm(as.factor(Objetivo) ~ 
CupoUtilizado+
  `Cupo.Utilizado/Cupo.Aprobado`+
  `Cupo.Ut.Avances/Cupo.Aprobado`+
#Edad.Cuenta.Tc+
#Dias.sin.consumo+
#Sector+
#CargasFamiliares+
Edad.Cliente+
T6.CupoUtilizado+
Prom.DiasMora3+
#TienePlan+
T36.CupoUtilizado+
#SoloSeguro+
Max_Dias_sin_consumos_3M+
Numero_tarjetas
, data=Balanceada, family="binomial")





mod1 <- glm(as.factor(Objetivo) ~ 
              Max_Dias_sin_consumos_3M+
              CupoUtilizado+
              Dias.sin.consumo+
              MesesRecencia+
              Prom.DiasMora6+
              Numero_tarjetas+
              Otra_Tarjeta+
              NumeroConsumos6M+
              NumeroConsumoSinInteres6M+
              NumeroConsumos12M+
              NumeroConsumoCorriente12M+
              SVidaDesgravamen+
              STotalFamiliar+
              MedioPagoVoucher12M+
              Edad.Cuenta.Tc+
              Max_Dias_sin_avances_3M+
              Sueldo
 , data=BalanceadaDic, family="binomial"(link = "probit"))
#, data=Balanceada, family = binomial(link = "logit"))


BalanceadaDic$Max_Dias_sin_avances_6M


#training$EstadoCivil<-as.character(training$EstadoCivil)
#table(training$EstadoCivil)
#training$EstadoCivil<-as.factor(training$EstadoCivil)
summary(mod1)

View(varImp(mod1))
#varImpPlot(mod_fit_one,type=2)
#mod1<-mod_fit_one


fitted.results <- predict(mod1,newdata=testingD,type='response')

Aux2<-cbind(testing,fitted.results)

mean(subset(Aux2,Objetivo==1 & Mesdesercion==4)$fitted.results,na.rm=TRUE)
Aux2$Objetivo<-0
Aux2$Objetivo<-ifelse(Aux2$Mesdesercion==1,1,0)
table(Aux2$Objetivo)
Aux2[is.na(Aux2$Objetivo),]$Objetivo<-0
Aux2<-Aux2[,-205]
fitted.results <- predict(mod1,newdata=Aux2,type='response')

fitted.results <- ifelse(fitted.results> 0.5,1,0)


#testingb$Objetivo<-gsub(0,2,testingb$Objetivo)
#testingb$Objetivo<-gsub(1,0,testingb$Objetivo)
#testingb$Objetivo<-gsub(2,1,testingb$Objetivo)


pr <- prediction(fitted.results, testingD$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###############Matriz de confución##############################

confusionMatrix(testingD$Objetivo, fitted.results)

table(testing$Objetivo, fitted.results)















#####################Segmentacion TDC######################
str(subset(Reporte43b,TieneScore6Meses==0 | is.na(AtrazoTrimestral)))

Aux3<-Reporte43b
Aux3["Inactivo"]<-"NA"
Aux3["IdCuentaTarjeta"]<-Aux3$Identificacion.x
Aux3[Aux3$TieneScore6Meses==0 | is.na(Aux3$AtrazoTrimestral),]$Inactivo<-1
Aux3[Aux3$Inactivo!=1,]$Inactivo<-0


table(Aux3$Inactivo)

Ax4<-left_join(UnificadaDic,subset(Aux3,select=c("IdCuentaTarjeta","Inactivo")),by="IdCuentaTarjeta")

table(Ax4$Inactivo,Ax4$Cliente.Activo)
table(Aux3$Inactivo)




table(Reporte43b$TieneScore6Meses)
summary(Reporte43b$AtrazoTrimestral)




#################################Saldos########################################
SaldosDic$FechaCorte<-"2015-12-31"
SaldosAbr$FechaCorte<-"2016-04-30"

c<-rbind(SaldosDic,SaldosAbr)
Aux<-subset(c,!duplicated(Identificacion) & EstadoTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")
Aux<-Aux[,-c(95,96)]


SaldosDic<-subset(SaldosDic, EstadoTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")
SaldosAbr<-subset(SaldosAbr, EstadoTarjeta=="ACTIVA" & PropietariaCartera=="BANCO SOLIDARIO")


NDSaldosDic<-subset(SaldosDic, Objetivo==0)
NDSaldosAbr<-subset(SaldosAbr,Objetivo==0)

  Ndic<-anti_join(NDSaldosDic,NDSaldosAbr, by="Identificacion")
  Nabr<-anti_join(NDSaldosAbr,NDSaldosDic, by="Identificacion")
NN<-anti_join(NDSaldosAbr,Nabr,by="Identificacion")
NN<-anti_join(NDSaldosDic,Ndic,by="Identificacion")
################Cruzar con la variable objetivo#################################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Desertores<-read.table("Desercion.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Desertores<-subset(Desertores,Reestructura==0 & EstadoTarjetaAntesCancelación=="ACTIVA")
#Desertores %>% count(Mesdesercion)
Desertores["Objetivo"]<-1
Desertores<-subset(Desertores,EstadoTarjetaAntesCancelación=="ACTIVA" & Reestructura==0)
Auxiliar<-subset(Desertores,select=c("IdCuentaTarjeta","Objetivo","Mesdesercion"))
Aux<-Aux %>% left_join(Auxiliar, by = c("IdCuentaTarjeta" = "IdCuentaTarjeta"))  #8250 desertorees de 8370 xq 120 abrieron en 2016
Aux$Objetivo[is.na(Aux$Objetivo)]<-0



Dic<-UnficadaDic[,-which(names(UnficadaDic) %in% c("Objetivo"))]

Desertores<-read.table(file = "DesertoresEne-Sep.csv")
Desertores<-read.csv("DesertoresEne-Sep.csv",header=TRUE,sep=",",na.strings = "NA", colClasses =c(
  "integer",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "integer",
  "integer",
  "factor"))

UnficadaDic$Identificacion<-as.factor(UnficadaDic$Identificacion)
table(Desertores$EstadoTarjetaAntesCancelacion)
Aux<-subset(Desertores,Reestructura==0 & EstadoTarjetaAntesCancelacion=="ACTIVA")
Aux<-subset(Aux,select=c("Identificacion","IdCuentaTarjeta","Mesdesercion","Objetivo"))
Aux2<-UnficadaDic[,-which(names(UnficadaDic) %in% c("Objetivo", "Mesdesercion"))]
BaseDic<-left_join(Aux2,Aux,by=c("IdCuentaTarjeta"))
BaseDic %>% count(Objetivo)
BaseDic$Objetivo[which(BaseDic$Mesdesercion>4)]<-0

ActivosDic<-subset(BaseDic,TipoCliente=="ACTIVO")
Train <- createDataPartition(ActivosDic$IdCuentaTarjeta, p=0.7, list=FALSE)
trainingD <- ActivosDic[ Train, ]
testingD <- ActivosDic[ -Train, ]

trainingD %>% count(Objetivo)


#######################Balaceo A Diciembre####################
muestra<-sample_n(subset(trainingD, Objetivo==0),1075,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(trainingD, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-206]
########################Malos
n<-62
Malos<-do.call("rbind", replicate(n, subset(trainingD, Objetivo==1), simplify = FALSE))

BalanceadaDic<-rbind(Malos,Buenos)
BalanceadaDic %>% count(Objetivo)

save(BaseDic,trainingD,testingD,BalanceadaDic,Desertores,file = "BaseDiciembre.Rdata")

summary(BalanceadaDic$Numero_tarjetasT3)
BalanceadaDic["DiferenciaTC3M"]<-BalanceadaDic$Numero_tarjetas-BalanceadaDic$Numero_tarjetasT3
BalanceadaDic["DiferenciaTC6M"]<-BalanceadaDic$Numero_tarjetas-BalanceadaDic$Numero_tarjetasT6
BalanceadaDic["NormCupoUtilizado"]<-(BalanceadaDic$CupoUtilizado-mean(BalanceadaDic$CupoUtilizado))/sd(BalanceadaDic$CupoUtilizado)
BalanceadaDic["NormPromCupoUtilizado6M"]<-(BalanceadaDic$Prom.CupoUtilizado6-mean(BalanceadaDic$Prom.CupoUtilizado6))/sd(BalanceadaDic$Prom.CupoUtilizado6)


testingD["DiferenciaTC3M"]<-testingD$Numero_tarjetas-testingD$Numero_tarjetasT3
testingD["DiferenciaTC6M"]<-testingD$Numero_tarjetas-testingD$Numero_tarjetasT6
testingD["NormCupoUtilizado"]<-(testingD$CupoUtilizado-mean(testingD$CupoUtilizado))/sd(testingD$CupoUtilizado)
testingD["NormPromCupoUtilizado6M"]<-(testingD$Prom.CupoUtilizado6-mean(testingD$Prom.CupoUtilizado6))/sd(testingD$Prom.CupoUtilizado6)

trainingD["DiferenciaTC3M"]<-trainingD$Numero_tarjetas-trainingD$Numero_tarjetasT3
trainingD["DiferenciaTC6M"]<-trainingD$Numero_tarjetas-trainingD$Numero_tarjetasT6
trainingD["NormCupoUtilizado"]<-(trainingD$CupoUtilizado-mean(trainingD$CupoUtilizado))/sd(trainingD$CupoUtilizado)
trainingD["NormPromCupoUtilizado6M"]<-(trainingD$Prom.CupoUtilizado6-mean(trainingD$Prom.CupoUtilizado6))/sd(trainingD$Prom.CupoUtilizado6)

save(BaseDic,BalanceadaDic,testingD,trainingD,file="BaseRegDicSinNa.Rdata")

table(BalanceadaDic$Objetivo)
BalanceadaDic$Objetivo[which(BalanceadaDic$Mesdesercion>4))] <- 0

testingD

library(foreign)
write.table(mydata, "c:/mydata.txt", sep="\t") 

write.foreign(BalanceadaDic, "mydata.sps",   package="SPSS")
