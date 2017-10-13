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

F1<-read.table("FebreroNuevos.csv",
            header=TRUE,
            sep=",",
            na.strings = "NA")
F1[which(nchar(F1$Identificacion)==9),]$Identificacion<-paste0(0,F1[which(nchar(F1$Identificacion)==9),]$Identificacion)
F2<-read.table("FebreroClientesPersistentesDiciembre.csv",
                 header=TRUE,
                 sep=",",
                 na.strings = "NA")
F2$Identificacion<-as.character(F2$Identificacion)

Febrero<-rbind(F1,F2)


Desertores<-read.table("Tarjetas_Canceladas_2017_03_16_04_15.csv",
                       header=TRUE,
                       sep=",",
                       na.strings = "NA")

Desertores[which(nchar(Desertores$Identificacion)==9),]$Identificacion<-paste0(0,Desertores[which(nchar(Desertores$Identificacion)==9),]$Identificacion)
Desertores<-subset(Desertores,EstadoTarjeta=="ACTIVA" & TieneReestructura==0)
DesertoresInt<-subset(Desertores,SegmentoTDC=="0. NUEVOS +6M" |
                        SegmentoTDC=="0. NUEVOS 6M" |
                        SegmentoTDC==" 1. VIP" |
                        SegmentoTDC=="2. PREFERENTE" |
                        SegmentoTDC=="3. MOVILIZACION A" )

GestionTel<-read.table("GestionTelefonosMarAbr17.csv",header=TRUE,
                       sep=",",
                       na.strings = "NA")
GestionTel[which(nchar(GestionTel$Identificacion)==9),]$Identificacion<-paste0(0,GestionTel[which(nchar(GestionTel$Identificacion)==9),]$Identificacion)
##################Ordenar y rankiar Febrero##########################
Febrero<-Febrero[order(Febrero$Score, decreasing=T),]
Febrero$X<-seq(1,NROW(Febrero),by=1)

######################Desertores en base de gestion###################
DesertoresTel<-semi_join(GestionTel,Desertores,by="Identificacion")
Aux<-semi_join(Febrero,GestionTel,by="Identificacion")
aux<-subset(GestionTel,!duplicated(Identificacion))
Tel<-semi_join(GestionTel,Febrero,by="Identificacion")
Tel2<-semi_join(Febrero,GestionTel,by="Identificacion")
Tel3<-subset(Tel2,X<10000)
Tel3<-semi_join(Tel3,Desertores,by="Identificacion")
Tel3<-semi_join(GestionTel,Tel3,by="Identificacion")


F1000<-sample_n(subset(Febrero,X<10000))

DesertoresTel<-semi_join(Tel,Desertores,by="Identificacion")
Aux<-semi_join(Febrero,DesertoresTel,by="Identificacion")
Aux<-semi_join(DesertoresTel,Febrero,by="Identificacion")

load("d:/Users_info/ALBANPD/My Documents/Bases/Diciembremod16.Rdata")
Diciembre<-Diciembre[order(Diciembre$Score,decreasing=TRUE),]
Diciembre["X"]<-seq(1,NROW(Diciembre),1)
A<-sample_n(Diciembre,2765, replace = FALSE)
 table(A$Decil,A$Mesdesercion) 
 
 table(Diciembre[1:2765,]$Decil,Diciembre[1:2765,]$Mesdesercion)
 
 table(Diciembre[1:3500,]$Decil,Diciembre[1:3500,]$Mesdesercion)
 table(Diciembre$Decil,Diciembre$Mesdesercion)
 
 DB<-semi_join(Febrero,Desertores,by="Identificacion")
 
 
 #################Correos Aperturados##############################
CorreoAper<-read.table("CorreosAperturadosMarAbr.csv",header=TRUE,
                         sep=",",
                         na.strings = "NA")
 colnames(CorreoAper)[1]<-"Identificacion"
 
CorreoAper[which(nchar(CorreoAper$Identificacion)==9),]<-paste0(0,CorreoAper[which(nchar(CorreoAper$Identificacion)==9),])

DerAper<-semi_join(Desertores,CorreoAper,by="Identificacion")
FebAper<-semi_join(Febrero,CorreoAper,by="Identificacion")
hist(FebAper$X,breaks = 50)


tabla1<-as.table(array(c(182,20479,363,20349),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(0,759,180,579),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(49,20,2716,2745),dim = c(2,2)))
summary(tabla1)


tabla1<-as.table(array(c(182,167,	20479,83867),dim = c(2,2)))
summary(tabla1)
#################################################
DesertoresTel<-semi_join(GestionTel,Desertores,by="Identificacion")

Tel<-semi_join(GestionTel,Febrero,by="Identificacion")
DerTel<-semi_join(DesertoresTel,Tel,by="Identificacion")
table(DerTel$Gestión)
table(Tel$Gestión)
tabla1<-as.table(array(c(2201,2745,	29,20),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(2729,2745,	36,20),dim = c(2,2)))
summary(tabla1)

#################Sin Telefonos######################




##################Mails#######################
DesertoresCor<-semi_join(CorreoAper,Desertores,by="Identificacion")

Cor<-semi_join(Febrero,CorreoAper,by="Identificacion")
DerCor<-semi_join(DesertoresCor,Cor,by="Identificacion")
DerCor<-semi_join(Febrero,DesertoresCor,by="Identificacion")

table(DerTel$Gestión)
table(Tel$Gestión)
tabla1<-as.table(array(c(2201,2745,	29,20),dim = c(2,2)))
summary(tabla1)

emails<-subset(Febrero,nchar(as.character(Febrero$Valor))>2)
NoConMail<-anti_join(emails,CorreoAper,by="Identificacion")
DMail<-semi_join(Desertores,NoConMail,by="Identificacion")
NoDMail<-anti_join(emails,Desertores,by="Identificacion")

###############Con mails#####################
SiM<-semi_join(DesertoresInt,CorreoAper,by="Identificacion")
SiTSiM<-semi_join(SiM,Contacto,by="Identificacion")



##############Sin mails####################
SinEmails<-subset(Febrero,nchar(as.character(Febrero$Valor))<2)
DSinEmail<-semi_join(Tel,SinEmails,by="Identificacion")
table(DSinEmail$Gestión)


#####################Cruze Telefonos vs Mails#######################
Contacto<-subset(Tel,Gestión=="CONTACTADO")
NoContacto<-subset(Tel,Gestión=="NO CONTACTADO")


CorreoAper
DesertoresInt
SiTSiD<-semi_join(Contacto,CorreoAper,by="Identificacion")
SiTSiD<-semi_join(SiTSiD,DesertoresInt,by="Identificacion")

NoTSiD<-semi_join(NoContacto,CorreoAper,by="Identificacion")
NoTSiD<-semi_join(NoTSiD,Desertores,by="Identificacion")

NoM<-anti_join(DesertoresInt,CorreoAper,by="Identificacion")
SiTNoM<-semi_join(NoM,Contacto,by="Identificacion")

SiTNoM<-subset(SiTNoM,nchar(as.character(SiTNoM$Valor))<2)

NoTNoM<-semi_join(Febrero,NoContacto,by="Identificacion")
NoTNoM<-semi_join(NoTNoM,DesertoresInt,by="Identificacion")

tabla1<-as.table(array(c(279,3,	2,0),dim = c(2,2)))
mcnemar.test(tabla1)

################################



SiASiD<-semi_join(CorreoAper,DesertoresInt,by="Identificacion")

tabla1<-as.table(array(c(12733,421,	130,2),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(2729,2745,	36,20),dim = c(2,2)))
summary(tabla1)

DesertoresConMail<-semi_join(DesertoresInt,emails,by="Identificacion")
NoAper<-anti_join(emails,CorreoAper,by="Identificacion")

NoASiD<-semi_join(NoAper,DesertoresConMail,by="Identificacion")

#####################Deserores de Interes con mail##############

DeserIntMail<-semi_join(DesertoresInt,emails,by="Identificacion")
DeserIntMail<-semi_join(Febrero,DeserIntMail,by="Identificacion")

tabla1<-as.table(array(c(20,29,	33,28),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(2,130,7,233),dim = c(2,2)))
summary(tabla1)
##########################Desertores con Telefono sin mail###############

DerIntSinMail<-anti_join(DesertoresInt,emails,by="Identificacion")
DerIntSinMail<-semi_join(Febrero,DerIntSinMail,by="Identificacion")


DerIntBase<-semi_join(Febrero,DesertoresInt,by="Identificacion")

DerIntSinMail<-DerIntBase[which(nchar(as.character(DerIntBase$Valor))>3),]
FeCampana<-Febrero


PobMailBase<-Febrero[which(nchar(as.character(Febrero$Valor))>3),]
PobSinMailBase<-anti_join(Febrero,PobMailBase,by="Identificacion")

#####################Campaña por sms########################

SmsOt<-read.table("SmsOtMarAbr.csv",header=TRUE,
                       sep=",",
                       na.strings = "NA")
colnames(SmsOt)<-"Identificacion"
SmsOt<-subset(SmsOt,!duplicated(Identificacion))

SmsOt[which(nchar(SmsOt$Identificacion)==9),]<-paste0(0,SmsOt[which(nchar(SmsOt$Identificacion)==9),])

SmsDesertores<-semi_join(SmsOt,DesertoresInt,by="Identificacion")
SmsFebrero<-semi_join(Febrero,SmsOt,by="Identificacion")
hist(SmsFebrero$X,breaks = 50)
NoSms<-anti_join(Febrero,SmsFebrero,by="Identificacion")
NoSmsDer<-semi_join(NoSms,DesertoresInt,by="Identificacion")
#######################Mailling################################3
MaillingFebOt<-read.table("MaillingFebOt.csv",header=TRUE,
                  sep=",",
                  na.strings = "NA")
MaillingFebOt$Identificacion<-as.character(MaillingFebOt$Identificacion)
MaillingFebOt<-subset(MaillingFebOt,!duplicated(Identificacion))

MaillingDesertores<-semi_join(MaillingFebOt,DesertoresInt,by="Identificacion")

MaillingFebrero<-semi_join(Febrero,MaillingFebOt,by="Identificacion")
hist(MaillingFebrero$X,breaks = 50)

NoMailling<-anti_join(Febrero,MaillingFebOt,by="Identificacion")
NoMaillingDer<-semi_join(NoMailling,Desertores,by="Identificacion")
##########################SMS vs Mailling################
SmsVsMail<-semi_join(MaillingFebrero,SmsFebrero,by="Identificacion")

SmsVsMailDer<-semi_join(MaillingDesertores,SmsDesertores,by="Identificacion")


SmsYMail<-semi_join(SmsFebrero,MaillingFebrero,by="Identificacion")
SmsSinMail<-anti_join(SmsFebrero,MaillingFebrero,by="Identificacion")
MailSinSms<-anti_join(MaillingFebrero,SmsFebrero,by="Identificacion")

SmsYMailDer<-semi_join(SmsYMail,DesertoresInt,by="Identificacion")
SmsSinMailDer<-semi_join(SmsSinMail,DesertoresInt,by="Identificacion")
MailSinSmsDer<-semi_join(MailSinSms,DesertoresInt,by="Identificacion")

NoSmsSinMail<-anti_join(Febrero,SmsFebrero,by="Identificacion")
NoSmsSinMail<-anti_join(NoSmsSinMail,SmsYMail,by="Identificacion")


NoMailSinSms<-anti_join(Febrero,MaillingFebrero,by="Identificacion")
NoMailSinSms<-anti_join(NoMaiSinSms,SmsYMail,by="Identificacion")

NoMailSinSmsDer<-semi_join(NoMailSinSms,DesertoresInt,by="Identificacion")
  

SinSmsYSinMail<-anti_join(Febrero,SmsFebrero,by="Identificacion")
SinSmsYSinMail<-anti_join(SinSmsYSinMail,MaillingFebrero,by="Identificacion")
SinSmsYSinMailDer<-semi_join(SinSmsYSinMail,DesertoresInt,by="Identificacion")

SinSms<-anti_join(Febrero,SmsOt,by="Identificacion")
aux<-semi_join(SinSms,MaillingFebOt,by="Identificacion")

SinSms<-anti_join(SinSms,MaillingFebOt,by="Identificacion")

SinMail<-anti_join(Febrero,MaillingFebOt,by="Identificacion")
SinSmsYSinMail<-semi_join(SinSms,SinMail,by="Identificacion")
SinSmsYSinMailDer<-semi_join(SinSmsYSinMail,DesertoresInt,by="Identificacion")


#####################Tamaño grupos#################
SoloSms<-anti_join(SmsOt,MaillingFebOt,by="Identificacion")
SoloSmsDer<-semi_join(SoloSms,Desertores,by="Identificacion")

SoloMail<-anti_join(MaillingDesertores,SmsOt,by="Identificacion")
SoloMailDer<-semi_join(SoloMail,SmsDesertores,by="Identificacion")


aux<-semi_join(SoloMailDer,SmsVsMailDer,by="Identificacion")



#####################
tabla1<-as.table(array(c(34,44,138,128),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(20,2,29,130),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(10238,10251,138,34),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(8266,12223,128,44),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(5471,7456,121,27),dim = c(2,2)))
summary(tabla1)



tabla1<-as.table(array(c(5471,2795,121,7),dim = c(2,2)))
summary(tabla1)

tabla1<-as.table(array(c(5471,4767,121,17),dim = c(2,2)))
summary(tabla1)


tabla1<-as.table(array(c(5471,7456,121,27),dim = c(2,2)))
summary(tabla1)
