library(data.table)
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
library(plyr)
#system.time(fread('Empresas2014.csv', header = T, sep = ',')) 

Reporte13<-read.table("Reporte-RIEG-INCO-00013.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Reporte14<-read.table("Reporte-RIEG-INCO-00014.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
TCHab<-read.table("TarjetaHabiente.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)

######################Adicionales###################
tc<-subset(TCHab,EsPrincipal=="0")
s<-subset(tc,!duplicated(tc$IdCuentaTarjeta)=="TRUE")
Ad<-subset(Reporte14,EsPrincipal=="ADICIONAL")
D<-left_join(Ad,s,by="IdCuentaTarjeta")
#write.csv(D,file = "D.csv")
table(D$Parentesco,D$EsPrincipal.y)

R13<-subset(Reporte13,!duplicated(Reporte13$IdCuentaTarjeta)=="TRUE")
DD<-left_join(D,R13, by= "IdCuentaTarjeta")

table(DD$Parentesco, DD$EstadoTarjetaCorte20160531)

F<-subset(DD,!(DD$EstadoTarjetaCorte20160531=="CANCELADA CLIENTE"))
F<-subset(F,!(F$EstadoTarjetaCorte20160531=="CANCELADA EMISOR"))
F<-subset(F,!(F$EstadoTarjetaCorte20160531=="ANULADA"))
F<-subset(F,!(F$EstadoTarjetaCorte20160531=="SINIESTRO"))
F<-subset(F,!(F$EstadoTarjetaCorte20160531=="CASTIGO"))
table(F$Parentesco, F$EstadoTarjetaCorte20160531)

View(F %>% count(Parentesco))
#############################Edades TC Mayo 2016########################333+
d2<-R13$FechaActivacion
d3<- substr(d2, 0, 10)
yy<-substr(d3,7,10)
mm<-substr(d3,4,5)
dd<-substr(d3,0,2)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
n<-length(R13$FechaActivacion)
R13["EdadTC"] <- NA
R13$EdadTC<-age_years(d5,rep(Sys.Date(),n))
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

View(R13 %>% count(EdadTC))
View(table(R13$EstadoTarjetaCorte20160531))


###########################Edad Cliente Mayo 2016##########################
R14<-subset(Reporte14,EsPrincipal=="PRINCIPAL")
R14<-subset(R14,!duplicated(R14$IdCuentaTarjeta)=="TRUE")
R140<-left_join(R14,R13, by="IdCuentaTarjeta")
R14b<-subset(R140,!duplicated(R140$IdCuentaTarjeta)=="TRUE")
table(R14A$EstadoTarjetaCorte20160531)

R14A<-subset(R14b,!(R14b$EstadoTarjetaCorte20160531=="CANCELADA CLIENTE"))
R14A<-subset(R14A,!(R14A$EstadoTarjetaCorte20160531=="CANCELADA EMISOR"))
R14A<-subset(R14A,!(R14A$EstadoTarjetaCorte20160531=="ANULADA"))
R14A<-subset(R14A,!(R14A$EstadoTarjetaCorte20160531=="SINIESTRO"))
R14A<-subset(R14A,!(R14A$EstadoTarjetaCorte20160531=="CASTIGO"))
head(R14A)


d2<-R14A$FechaNacimiento
yy<-substr(d2,7,10)
mm<-substr(d2,4,5)
dd<-substr(d2,0,2)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
n<-length(R14A$FechaNacimiento)
R14A["EdadCliente"] <- NA
R14A$EdadCliente<-age_years(d5,rep(Sys.Date(),n))
View(R14A %>% count(EdadCliente))

###########################Edad Cliente Mayo 2014##########################
R14<-subset(Reporte14,EsPrincipal=="PRINCIPAL")
R14<-subset(R14,!duplicated(R14$IdCuentaTarjeta)=="TRUE")
R140<-left_join(R14,R13, by="IdCuentaTarjeta")
R14b<-subset(R140,!duplicated(R140$IdCuentaTarjeta)=="TRUE")
R14b<-subset(R14b,!(EstadoTarjetaCorte20140531=="SIN ESTADO"))

table(R14b$EstadoTarjetaCorte20140531)

d2<-R14b$FechaNacimiento
yy<-substr(d2,7,10)
mm<-substr(d2,4,5)
dd<-substr(d2,0,2)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
n<-length(R14b$FechaNacimiento)
R14b["EdadCliente14"] <- NA
R14b$EdadCliente14<-age_years(d5,rep(as.Date("2014-08-30"),n))
View(R14b %>% count(EdadCliente14))

View(h<-hist(R14b$EdadCliente14,breaks =30 ))
View(cbind(h$breaks,h$counts))


EdadEC<-read.table("EdadesEc.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
EdadEC$Poblacion<-as.numeric(EdadEC$Poblacion)
(h<-hist(EdadEC,breaks = 30))

############################Cargas##########################

View(R14A %>% count(CargasFamiliares20150531))

############################Estado Civil##########################
View(R14A %>% count(EstadoCivil20150531))
View(subset(R14A,Genero=="FEMENINO") %>% count(EstadoCivil20150531))
View(subset(R14A,Genero=="MASCULINO") %>% count(EstadoCivil20150531))
############################Genero##########################
View(R14A %>% count(Genero))




##########################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
RR13<-read.table("RIEG13.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
RR13<-subset(RR13,!duplicated(RR13$IdCuentaTarjeta)=="TRUE")
RR13["CicloM"] <- NA
d2<-RR13$FechaActivacion
yy<-substr(d2,7,10)
mm<-substr(d2,4,5)
dd<-substr(d2,0,2)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
RR13$FechaAct<-d5

RR13$CicloM[RR13$EstadoTarjetaCorte20140731=="CANCELADA CLIENTE"]<-as.Date("2014/07/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20140831=="CANCELADA CLIENTE"]<-as.Date("2014/08/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20140930=="CANCELADA CLIENTE"]<-as.Date("2014/09/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20141031=="CANCELADA CLIENTE"]<-as.Date("2014/10/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20141130=="CANCELADA CLIENTE"]<-as.Date("2014/11/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20141231=="CANCELADA CLIENTE"]<-as.Date("2014/12/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20150131=="CANCELADA CLIENTE"]<-as.Date("2015/01/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20150228=="CANCELADA CLIENTE"]<-as.Date("2015/02/28")
RR13$CicloM[RR13$EstadoTarjetaCorte20150331=="CANCELADA CLIENTE"]<-as.Date("2015/03/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20150430=="CANCELADA CLIENTE"]<-as.Date("2015/04/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20150531=="CANCELADA CLIENTE"]<-as.Date("2015/05/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20150630=="CANCELADA CLIENTE"]<-as.Date("2015/06/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20150731=="CANCELADA CLIENTE"]<-as.Date("2015/07/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20150831=="CANCELADA CLIENTE"]<-as.Date("2015/08/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20150930=="CANCELADA CLIENTE"]<-as.Date("2015/09/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20151031=="CANCELADA CLIENTE"]<-as.Date("2015/10/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20151130=="CANCELADA CLIENTE"]<-as.Date("2015/11/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20151231=="CANCELADA CLIENTE"]<-as.Date("2015/12/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20160131=="CANCELADA CLIENTE"]<-as.Date("2016/01/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20160229=="CANCELADA CLIENTE"]<-as.Date("2016/02/29")
RR13$CicloM[RR13$EstadoTarjetaCorte20160331=="CANCELADA CLIENTE"]<-as.Date("2016/03/31")
RR13$CicloM[RR13$EstadoTarjetaCorte20160430=="CANCELADA CLIENTE"]<-as.Date("2016/04/30")
RR13$CicloM[RR13$EstadoTarjetaCorte20160531=="CANCELADA CLIENTE"]<-as.Date("2016/05/31")

RR13["MesesVidaCl"] <- NA
RR13$MesesVidaCl<-round((as.Date(RR13$CicloM)-RR13$FechaAct)/30, digits = 0)
#write.csv(RR13 %>% count(MesesVidaCl),file = "Meses de Vida.csv")
(h<-hist(as.numeric((RR13$MesesVidaCl)), breaks = 20))
View(cbind(h$breaks, h$counts))
write.csv(cbind(h$breaks,h$counts),file = "Hvidaclientes.csv")
View(RR13 %>% count(MesesVidaCl))
table(RR13$MesesVidaCl)

RR13 %>% count(Segmento20160131)

(RR13$CicloM[RR13$EstadoTarjetaCorte20160131=="CANCELADA CLIENTE" & RR13$Segmento20160131=="7. INACTIVOS"])
Inactivos<-
table(RR13$Segmento20160531,RR13$EstadoTarjetaCorte2016013)
#################Ingresos####################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Ingresos<-read.table("Reporte22.csv",header=TRUE,sep="," ,na.strings = "NA", colClasses = NA)
Inn<-subset(Ingresos,Tipo_Ingreso=="A")
Inn["Segmento"] <- NA
Inn$Segmento[Inn$Ingreso<286]<-"D"
Inn$Segmento[Inn$Ingreso>286 & Inn$Ingreso<893]<-"C-"
Inn$Segmento[Inn$Ingreso>893 & Inn$Ingreso<1789]<-"C+"
Inn$Segmento[Inn$Ingreso>1789 & Inn$Ingreso<4132]<-"B"
Inn$Segmento[Inn$Ingreso>4132]<-"A"

Inn[Inn$Segmento=="NA",]
View(Inn %>% count(Segmento))

##################Número de Clientes#############################3
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Segmento<-read.table("Base.Segmentacion.csv",header=TRUE,sep="," ,na.strings = "NA", colClasses = NA)

str(Segmento)
Segmento %>% count(EstadoCuentaTarjeta)
table(Segmento$PropietariaCartera,Segmento$SegmentoTDCAbril2016)
Seg<-subset(Segmento, PropietariaCartera=="BANCO SOLIDARIO")
table(Seg$Segmento_TDCJulio2016)
View(Seg %>% count(Segmento_TDCJulio2016))

Seg<-subset(Seg,!duplicated(Seg$Identificacion)=="TRUE")

table(Seg2$EstadoCuentaTarjeta,Seg2$Estadotarjeta)
Seg2<-subset(Seg,EstadoCuentaTarjeta=="ACTIVA" & Estadotarjeta=="ACTIVA")
Seg3<-(subset(Seg2,!PrimerConsumo=="NULL"))
####################Establecimientos Solidario#########################
data <- read.delim("CENEC_2010.dat", sep="\t") 
View(data %>% count(CIIU4_P ))
data1<-subset(data,!CIIU4_P=="1" & !CIIU4_P=="2" & !CIIU4_P=="3" & !CIIU4_P=="12" & !CIIU4_P=="15" & !CIIU4_P=="20" & !CIIU4_P=="21")
View(data1 %>% count(CIIU4_P ))
sum(data1$CIIU4_P)
write.csv(table(data1$S1P2,(data1$CIIU4_P)),file = "EstablecimientosEc.csv")

##########################Segmento Tc####################################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Segmento<-read.table("Base.Segmentacion.csv",header=TRUE,sep="," ,na.strings = "NA", colClasses = NA)

#####################Tiempo entre el primer consumo y la apertura##################33
Primerconsumo<-subset(Segmento,!(PrimerConsumo=="NULL"))

Primerconsumo["1erConsumo"] <- NA
a<-Primerconsumo$PrimerConsumo
ayy<-substr(a,7,10)
amm<-substr(a,4,5)
add<-substr(a,0,2)
a2<-ISOdate(ayy, amm, add)
a2<-as.Date(a2)
Primerconsumo["1erConsumo"] <-a2
  
  
Primerconsumo["Activacion"] <- NA
b<-Primerconsumo$FechaApertura
byy<-substr(b,7,10)
bmm<-substr(b,4,5)
bdd<-substr(b,0,2)
b2<-ISOdate(byy, bmm, bdd)
b2<-as.Date(b2)
Primerconsumo["Activacion"] <- b2
Primerconsumo["Tiempohasta1erconsumo"] <- NA
Primerconsumo["Tiempohasta1erconsumo"] <- round((as.Date(Primerconsumo$`1erConsumo`)-Primerconsumo$Activacion)/30.41667, digits = 1)
Primerconsumo<-subset(Primerconsumo,EstadoCuentaTarjeta=="ACTIVA")

table(Primerconsumo$EstadoCuentaTarjeta)
#str(Primerconsumo[Primerconsumo$Tiempohasta1erconsumo<0,])
Primerconsumo<-subset(Primerconsumo,!(Tiempohasta1erconsumo<0))
Primerconsumo[Primerconsumo$Tiempohasta1erconsumo>=200,]


Primerconsumo$Tiempohasta1erconsumo<-round(Primerconsumo$Tiempohasta1erconsumo,digits = 0)
Primerconsumo %>% count(Tiempohasta1erconsumo)
h<-hist(as.numeric(round(Primerconsumo$Tiempohasta1erconsumo,digits = 0)), breaks = 216)
write.csv(cbind(h$breaks,h$counts),file = "Primerconsumo2.csv")
#Primerconsumo[Primerconsumo$Identificacion==703026047,]
##############################################################
cbind(R14A %>% count(EstadoTarjetaCorte20140531),
      R14A %>% count(EstadoTarjetaCorte20140630)
      EstadoTarjetaCorte20140731
      EstadoTarjetaCorte20140831
      EstadoTarjetaCorte20140930
      EstadoTarjetaCorte20141031
      EstadoTarjetaCorte20141130
      EstadoTarjetaCorte20141231
      EstadoTarjetaCorte20150131
      EstadoTarjetaCorte20150228
      EstadoTarjetaCorte20150331
      EstadoTarjetaCorte20150430
      EstadoTarjetaCorte20150531
      EstadoTarjetaCorte20150630
      EstadoTarjetaCorte20150731
      EstadoTarjetaCorte20150831
      EstadoTarjetaCorte20150930
      EstadoTarjetaCorte20151031
      EstadoTarjetaCorte20151130
      EstadoTarjetaCorte20151231
      EstadoTarjetaCorte20160131
      EstadoTarjetaCorte20160229
      EstadoTarjetaCorte20160331
      EstadoTarjetaCorte20160430
      EstadoTarjetaCorte20160531
      )

#####################Ingresos por cliente compartido#######################
R14b2<-R14b
R14b2["Identificacion"]<-"NA"
Segmento["Ingresos"]<-"NA"
R14b2$Identificacion<-R14b2$Identificacion.x
Segmento$Identificacion<-as.numeric(Segmento$Identificacion)


Compartido<-left_join(Segmento,Ingresos,by="IdCuentaTarjeta")

CC<-subset(Compartido,!(Ingreso=="NA"))
CC<-subset(Compartido, EstadoCuentaTarjeta=="ACTIVA" & ActivadaFebrero2016==0)
CC2<-subset(CC,Tipo_Ingreso=="A" & !(Ingreso=="NA"))


CC2["SegIn"] <- NA
CC2$SegIn[CC2$Ingreso<286]<-"D"
CC2$SegIn[CC2$Ingreso>286 & CC2$Ingreso<893]<-"C-"
CC2$SegIn[CC2$Ingreso>893 & CC2$Ingreso<1789]<-"C+"
CC2$SegIn[CC2$Ingreso>1789 & CC2$Ingreso<4132]<-"B"
CC2$SegIn[CC2$Ingreso>4132]<-"A"
CCsol<-subset(CC2,ClienteCompartido==0)
CCcom<-subset(CC2,ClienteCompartido==1)

View(CCsol %>% count(SegIn))

View(CCcom %>% count(SegIn))
########################Cliente#####################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Segmento<-read.table("Base.Segmentacion.csv",header=TRUE,sep="," ,na.strings = "NA", colClasses = NA)
load("d:/Users_info/ALBANPD/My Documents/Bases/Base clientes TC.RData")
Segmento$Saldo231<-as.numeric(Segmento$Saldo231)
Segmento2<-subset(Segmento,0<Saldo231 & EstadoCuentaTarjeta=="ACTIVA" & Estadotarjeta=="ACTIVA" & ActivadaFebrero2016==0)

clientes<-left_join(Segmento2,Reporte14,by="IdCuentaTarjeta")
clientes<-subset(clientes,EsPrincipal=="PRINCIPAL")
##########################################
d2<-clientes$FechaApertura
d3<- substr(d2, 0, 10)
yy<-substr(d3,7,10)
mm<-substr(d3,4,5)
dd<-substr(d3,0,2)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)

n<-length(clientes$FechaApertura)
clientes["EdadTC"] <- NA
#clientes$EdadTC<-age_years(d5,rep(Sys.Date(),n))
clientes$EdadTC<-round((Sys.Date()-as.Date(d5))/30, digits = 0)
Edadtc<-hist(as.numeric(clientes$EdadTC),breaks = 220)

cc<-cbind(Edadtc$breaks,Edadtc$counts)
write.csv(cc,file = "Edadtc.csv")
cc<-cc[-c(1,2,3),]

Ed<-array(0,dim=c(35,2))
for (i in 1:34) {
  Ed[i,]=(cc[,2][i*6]+cc[,2][i*6-1]+cc[,2][i*6-2]+cc[,2][i*6-3]+cc[,2][i*6-4]+cc[,2][i*6-5])
  }

######################Clientes Quito##################
clientesq<-subset(clientes, CantonDomicilio=="QUITO")
clientesq<-subset(clientes, CantonDomicilio=="")
write.csv(clientes%>% count(ParroquiaDomicilio),file = "Clientesparr.csv")

##########################Segmento Tc####################################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
Segmento<-read.table("Base.Segmentacion.csv",header=TRUE,sep="," ,na.strings = "NA", colClasses = NA)
Seg<-(Segmento[Segmento$EstadoCuentaTarjeta=="ACTIVA" & Segmento$ActivadaFebrero2016==0 & Segmento$Estadotarjeta=="ACTIVA",])
Seg<-(Segmento[Segmento$ActivadaFebrero2016==0,])
library("tigerstats", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
xtabs(~Segmento_TDC+EstadoCuentaTarjeta,Seg)


Segmento %>% group_by(EstadoCuentaTarjeta) %>% summarise(Order.Amount=sum(CupoAprobadoNormalCorte))

Segmento %>% count(EstadoCuentaTarjeta) %>% summarise(Order.Amount=sum(CupoAprobadoNormalCorte))



du<-Segmento[duplicated(Segmento$Identificacion),]

View(Segmento[Segmento$Identificacion==1719299776,])

du[du$Identificacion==1719213868,]

head(du[du$EstadoCuentaTarjeta=="ACTIVA",])
