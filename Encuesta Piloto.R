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
setwd("d:/Users_info/ALBANPD/My Documents/Bases")


BasePiloto <- read.csv(file="BasePiloto.csv", header=TRUE, sep=",",colClasses =c("character",
                                                                                  "factor",
                                                                                  "character",
                                                                                  "character"))
BasePiloto<-subset(BasePiloto,!duplicated(Identificacion))
########################Muestreo sin reposición##############3

SMSAldeamo<-sample_n(BasePiloto, 300,replace = FALSE)  #Segmentos de interes

Resto<-anti_join(BasePiloto,SMSAldeamo,by="Identificacion")
SMSMind<-sample_n(Resto, 300,replace = FALSE)
Resto<-anti_join(Resto,SMSMind,by="Identificacion")

WAppMind<-sample_n(Resto, 300,replace = FALSE)
Resto<-anti_join(Resto,WAppMind,by="Identificacion")
######################Cargar Correos Electronicos Validados##############
DWH_Reporte <- dbConnect(odbc::odbc(),
                         Driver    = "SQL Server", 
                         Server    = "BSUIO-DWH02",
                         Database  = "DWH_Reporte")



ClienteCorreoElectronico<- tbl(DWH_Reporte, "ClienteCorreoElectronico") %>%
  filter(Resultado %in% c("Deliverable","Risky") & EsValido==1) %>% collect()

Correo<-subset(ClienteCorreoElectronico,select=c("Identificacion","Valor"))
Correo$Valor<-tolower(Correo$Valor)
colnames(Correo)[2]<-"Email"
############################Cruzar correos###############
MailsMind<-left_join(Resto,Correo,by="Identificacion")
MailsMind<-subset(MailsMind,!duplicated(Identificacion))
MailsMind<-subset(MailsMind,!is.na(Email))
MailsMind<-sample_n(MailsMind, 300,replace = FALSE)

###########################Guardar bases####################################

library(xlsx)
write.xlsx(SMSAldeamo, "SMSAldeamo.xlsx",row.names = FALSE)

write.xlsx(SMSMind, "SMSMind.xlsx",row.names = FALSE)
write.xlsx(WAppMind, "WAppMind.xlsx",row.names = FALSE)
write.xlsx(MailsMind, "MailsMind.xlsx",row.names = FALSE)

