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

Cupones<-read.table("CuponesVirtuales0517.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")
Cupones<-as.data.frame(Cupones)
Cupones[which(nchar(Cupones$Identificacion)==9),]<-paste0(0,Cupones[which(nchar(Cupones$Identificacion)==9),])

SanaSana<-read.table("SanaSana0517.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")
SanaSana[which(nchar(SanaSana$SMS)==9),]$SMS<-paste0(0,SanaSana[which(nchar(SanaSana$SMS)==9),]$SMS)
SanaSana[which(nchar(SanaSana$Mail)==9),]$Mail<-paste0(0,SanaSana[which(nchar(SanaSana$Mail)==9),]$Mail)


SuperExito<-read.table("SuperEx0517.txt",
                     header=TRUE,
                     sep="\t",
                     na.strings = "NA")
SuperExito[which(nchar(SuperExito$SMS)==9),]$SMS<-paste0(0,SuperExito[which(nchar(SuperExito$SMS)==9),]$SMS) 
SuperExito[which(nchar(SuperExito$Mail)==9),]$Mail<-paste0(0,SuperExito[which(nchar(SuperExito$Mail)==9),]$Mail) 


Payless<-read.table("Payless0517.txt",
                       header=TRUE,
                       sep="\t",
                       na.strings = "NA")
Payless[which(nchar(Payless$SMS)==9),]<-paste0(0,Payless[which(nchar(Payless$SMS)==9),])


####################Tarjetas activas###########################################################
odbcChannel <-odbcConnect("DWH_CRM") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "SB_TDCActivas")

TDC <- sqlExecute(odbcChannel, "SELECT * FROM SB_TDCActivas", fetch = TRUE)

odbcClose(odbcChannel)
#############################Autorizaciones############################

odbcChannel <-odbcConnect("Tarjeta") #Databases
sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Autorizacion")

Autorizaciones <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConsumo < '2017-06-08' 
                               and FechaConsumo > '2017-05-11'", fetch = TRUE)

Comercio<-sqlExecute(odbcChannel, "SELECT * FROM ComercioAfiliado", fetch = TRUE)

Establecimiento<-sqlExecute(odbcChannel, "SELECT * FROM Establecimiento", fetch = TRUE)
odbcClose(odbcChannel)
################Filtrar establecimientos y comercios###############
colnames(Establecimiento)[3]<-"Establecimiento"
colnames(Comercio)[2]<-"Comercio"

Establecimiento<-subset(Establecimiento,select=c("IdEstablecimiento","Establecimiento"))
Comercio<-subset(Comercio, select=c("IdComercioAfiliado","Comercio"))
###############Cruzar IdEstablecimiento e IdCoemrcios##################
Autorizaciones<-left_join(Autorizaciones,Comercio, by="IdComercioAfiliado")
Autorizaciones<-left_join(Autorizaciones,Establecimiento, by="IdEstablecimiento")
######################Cruze numero de cedula autorizacion#############3
TDC<-subset(TDC,select=c("Identificacion","IdCuentaTarjeta"))
Autorizaciones<-left_join(Autorizaciones,TDC, by="IdCuentaTarjeta")

################Solo consumos establecimiento#################

SoloConsumo<-subset(Autorizaciones,  TipoConsumo=="TRJCONSUMO" & IdEstadoAutorizacion==3 & !(Comercio=="SOLIDARIO"))
SoloConsumo<-subset(SoloConsumo,select=c("IdAutorizacion",
                                         "Identificacion",
                                         "FechaConsumo",
                                         "ValorConsumo",
                                         "Comercio",
                                         "Establecimiento"))
SoloConsumo$Identificacion<-as.character(SoloConsumo$Identificacion)
##################Cupones virtuales##################

aux<-semi_join(Cupones,SoloConsumo,by="Identificacion")
aux<-left_join(aux,SoloConsumo,by="Identificacion")
aux<-aux %>% group_by(Identificacion) %>% mutate(NumeroConsumos = n())
ConsumoCupones<-aux %>% group_by(Comercio) %>% mutate(ConsumoTotal = sum(ValorConsumo))

write.csv(aux,file = "RendCupones.csv", row.names = FALSE)

#######################
SoloConsumo["SMS"]<-SoloConsumo$Identificacion
SoloConsumo["Mail"]<-SoloConsumo$Identificacion
#############################Sana Sana##################
SMS<-semi_join(SanaSana,SoloConsumo,by="SMS")
SMS<-subset(SMS,select=c("SMS"))
SMS<-left_join(SMS,SoloConsumo,by="SMS")
SMS<-subset(SMS,Comercio=="SANA SANA" |
              Comercio=="SANA SANA 2" |
              Comercio=="SANA SANA 4" |
              Comercio=="SANA SANA 3" |
              Comercio=="FARMACIA SANA SANA")
write.csv(SMS,file = "SanaSanaSMS.csv")

Mail<-semi_join(SanaSana,SoloConsumo,by="Mail")
Mail<-subset(Mail,select=c("Mail"))
Mail<-left_join(Mail,SoloConsumo,by="Mail")
Mail<-subset(Mail,Comercio=="SANA SANA" |
              Comercio=="SANA SANA 2" |
              Comercio=="SANA SANA 4" |
              Comercio=="SANA SANA 3" |
              Comercio=="FARMACIA SANA SANA")
write.csv(Mail,file = "SanaSanaMail.csv")


#####################SuperExito##################
SMS<-semi_join(SuperExito,SoloConsumo,by="SMS")
SMS<-subset(SMS,select=c("SMS"))
SMS<-left_join(SMS,SoloConsumo,by="SMS")
SMS<-subset(SMS,Comercio=="SUPER EXITO")
write.csv(SMS,file = "SuperExitoSMS.csv",row.names = FALSE)

Mail<-semi_join(SanaSana,SoloConsumo,by="Mail")
Mail<-subset(Mail,select=c("Mail"))
Mail<-left_join(Mail,SoloConsumo,by="Mail")
Mail<-subset(Mail,Comercio=="SUPER EXITO")
write.csv(Mail,file = "SuperExitoMail.csv",row.names = FALSE)


####################Payless##################
SMS<-semi_join(Payless,SoloConsumo,by="SMS")
SMS<-left_join(SMS,SoloConsumo,by="SMS")
SMS<-subset(SMS,Comercio=="PAYLESS SHOES")
write.csv(SMS,file = "PaylessSMS.csv",row.names = FALSE)


#################Cargar desertores##################
Canceladas<-read.table("CanceladasAbr-Jun.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")
Canceladas[which(nchar(Canceladas$Identificacion)==9),]$Identificacion<-paste0(0,Canceladas[which(nchar(Canceladas$Identificacion)==9),]$Identificacion)
Canceladas$fechacancelacion<-as.character(Canceladas$fechacancelacion)
Canceladas$fechacancelacion<-as.Date(Canceladas$fechacancelacion, "%d/%m/%Y") 
Canceladas["SMS"]<-Canceladas$Identificacion
Canceladas["Mail"]<-Canceladas$Identificacion

#####################Cargar campañas de Abril##############
CampanaAbril<-read.table("CampanaTDCAbril2017.txt",
                       header=TRUE,
                       sep="\t",
                       na.strings = "NA")
CampanaAbril[which(nchar(CampanaAbril$Identificacion)==9),]$Identificacion<-paste0(0,CampanaAbril[which(nchar(CampanaAbril$Identificacion)==9),]$Identificacion)

#####################Cargar campañas de Marzo##############
CampanaMarzo<-read.table("CampanaTDCMarzo2017.txt",
                         header=TRUE,
                         sep="\t",
                         na.strings = "NA")
CampanaMarzo[which(nchar(CampanaMarzo$Identificacion)==9),]$Identificacion<-paste0(0,CampanaMarzo[which(nchar(CampanaMarzo$Identificacion)==9),]$Identificacion)
####################Campañas de Abril y Marzo#################
aux<-subset(CampanaMarzo,select=c("Identificacion",
                                  "Mail",
                                  "SMS"))
aux2<-subset(CampanaAbril,select=c("Identificacion",
                                  "Mail",
                                  "SMS"))
CampanaMarAbr<-rbind(aux,aux2)
CampanaMarAbr<-subset(CampanaMarAbr,!duplicated(Identificacion))


###############Deserotores en campana De abril##############
Canceladas15Mar<-subset(Canceladas,fechacancelacion<"2017-05-16")
Canceladas15Mar<-subset(Canceladas15Mar,fechacancelacion>"2017-04-14")

Canceladas16Mayo<-subset(Canceladas,fechacancelacion>="2017-05-16")
Canceladas16Mayo["SMS"]<-Canceladas16Mayo$Identificacion
Canceladas16Mayo["Mail"]<-Canceladas16Mayo$Identificacion
DeserCampanaAbril<-semi_join(Canceladas16Mayo,CampanaAbril,by="Identificacion")
DeserCampanaMar<-semi_join(Canceladas15Mar,CampanaMarzo,by="Identificacion")


CampanaAbril["SMS"]<-CampanaAbril$Identificacion
CampanaAbril["Mail"]<-CampanaAbril$Identificacion

CampanaMarzo["SMS"]<-CampanaMarzo$Identificacion
CampanaMarzo["Mail"]<-CampanaMarzo$Identificacion

###############################Efectividad de los cupones##########################3
CampanaCupones<-semi_join(CampanaMarAbr,Cupones,by="Identificacion")
Canceladas11Mayo<-subset(Canceladas,fechacancelacion>="2017-05-11" & fechacancelacion<"2017-06-12")
DeserCampanaCupones<-semi_join(Cupones,Canceladas,by="Identificacion")

##################################Efectividad avances#########################


Avances<-read.table("Avances.txt",
                         header=TRUE,
                         sep="\t",
                         na.strings = "NA")
Avances[which(nchar(Avances$SMS)==9),]$SMS<-paste0(0,Avances[which(nchar(Avances$SMS)==9),]$SMS)
Avances[which(nchar(Avances$Mail)==9),]$Mail<-paste0(0,Avances[which(nchar(Avances$Mail)==9),]$Mail)
DeserAvancesSMS<-semi_join(Canceladas,Avances,by="SMS")
DeserAvancesMail<-semi_join(Canceladas,Avances,by="Mail")


aux<-semi_join(Avances,CampanaMarAbr,by="SMS")
#aux<-semi_join(Avances,CampanaMarzo,by="Mail")
########################Extra cupo############################
ExtraCupo<-read.table("ExtraCupo.txt",
                    header=TRUE,
                    sep="\t",
                    na.strings = "NA")
ExtraCupo[which(nchar(ExtraCupo$SMS)==9),]$SMS<-paste0(0,ExtraCupo[which(nchar(ExtraCupo$SMS)==9),]$SMS)
ExtraCupo[which(nchar(ExtraCupo$Mail)==9),]$Mail<-paste0(0,ExtraCupo[which(nchar(ExtraCupo$Mail)==9),]$Mail)

DeserExtraCupoSMS<-semi_join(Canceladas,ExtraCupo,by="SMS")
DeserExtraCupoMail<-semi_join(Canceladas,ExtraCupo,by="Mail")

aux<-semi_join(ExtraCupo,CampanaMarAbr,by="SMS")
aux<-semi_join(ExtraCupo,CampanaMarAbr,by="Mail")
#######################Call Center#######################

CallCenter<-read.table("CallCenter.txt",
           header=TRUE,
           sep="\t",
           na.strings = "NA")
CallCenter[which(nchar(CallCenter$Identificacion)==9),]$Identificacion<-paste0(0,CallCenter[which(nchar(CallCenter$Identificacion)==9),]$Identificacion)
table(CallCenter$Gestion)
DeserCallCenter<-semi_join(CallCenter,Canceladas,by="Identificacion")

aux<-semi_join(CallCenter,CampanaMarAbr,by="Identificacion")
####################Sana Sana######################


CampanaSanaSanaSMS<-semi_join(CampanaAbril,SanaSana,by="SMS")
DeserSanaSanaSMS<-semi_join(Canceladas,CampanaSanaSanaSMS,by="Identificacion")

CampanaSanaSanaMail<-semi_join(CampanaAbril,SanaSana,by="Mail")
DeserSanaSanaMail<-semi_join(Canceladas,CampanaSanaSanaMail,by="Identificacion")


aux<-semi_join(SanaSana,CampanaMarAbr,by="SMS")
aux<-semi_join(SanaSana,CampanaMarAbr,by="Mail")

######################Super Exito#################
CampanaSuperExitoSMS<-semi_join(CampanaAbril,SuperExito,by="SMS")
DeserSuperExitoSMS<-semi_join(Canceladas,CampanaSuperExitoSMS,by="Identificacion")

CampanaSuperExitoMail<-semi_join(CampanaAbril,SuperExito,by="Mail")
DeserSuperExitoMail<-semi_join(Canceladas,CampanaSuperExitoMail,by="Identificacion")

#################Payless############################
CampanaPaylessSMS<-semi_join(CampanaAbril,Payless,by="SMS")
DeserPaylessSMS<-semi_join(Canceladas,CampanaPaylessSMS,by="Identificacion")

################Rendimiento campaña################3
Campana<-read.table("Campana15May.txt",
                       header=TRUE,
                       sep="\t",
                       na.strings = "NA")
Campana[which(nchar(Campana$Identificacion)==9),]<-paste0(0,Campana[which(nchar(Campana$Identificacion)==9),])
Campana["Var"]<-NA


Campana<-Campana[1823:dim(Campana)[1],]
Campana<-subset(Campana,!duplicated(Identificacion))
aux2<-anti_join(CampanaMarAbr,Campana,by="Identificacion")
aux2<-subset(aux2,!duplicated(Identificacion))

NoCampana<-anti_join(CampanaMarAbr,Campana,by="Identificacion")
NoCampana<-subset(NoCampana,!duplicated("Identificacion"))
DesertoresNoCampana<-semi_join(Canceladas,NoCampana,by="Identificacion")

DesertoresCampana<-semi_join(Campana,Canceladas,by="Identificacion")
DesertoresCampana<-subset(DesertoresCampana,!duplicated(Identificacion))

CampanaMarAbr

aux3<-semi_join(Campana,CampanaMarzo,by="Identificacion")
aux3<-subset(aux3,!duplicated(Identificacion))
