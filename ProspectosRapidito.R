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
setwd("d:/Users_info/ALBANPD/My Documents/Bases")

Base<-read.table("BaseRapidito2.txt",
                        header=TRUE,
                        sep="\t",
                        na.strings = "NA")

Base<-read.table("BaseRap.csv",
                 header=TRUE,
                 sep=",",
                 na.strings = "NA")


odbcChannel <-odbcConnect("temporal") #Databases

Base <- sqlExecute(odbcChannel, "SELECT * FROM ProspectosRapidito", fetch = TRUE)



colnames(Base)[1]<-"Identificacion"

colnames(Base)[15]<-"CelularNotificaciones"
colnames(Base)[16]<-"CorreoNotificaciones"

Base$Identificacion<-as.character(Base$Identificacion)
Base[which(nchar(Base$Identificacion)==9),]$Identificacion<-paste0(0,Base[which(nchar(Base$Identificacion)==9),]$Identificacion)

Base$CorreoElectronico_01<-as.character(Base$CorreoElectronico_01)
Base$CorreoElectronico_02<-as.character(Base$CorreoElectronico_02)

Base$Celular_01<-as.character(Base$Celular_01)
Base$Celular_02<-as.character(Base$Celular_02)
Base$Celular_03<-as.character(Base$Celular_03)
Base$Celular_04<-as.character(Base$Celular_04)
Base$Celular_05<-as.character(Base$Celular_05)
Base$Celular_06<-as.character(Base$Celular_06)
Base$Celular_07<-as.character(Base$Celular_07)
Base$Celular_08<-as.character(Base$Celular_08)
Base$Celular_09<-as.character(Base$Celular_09)
Base$Celular_10<-as.character(Base$Celular_10)

Base$CelularNotificaciones<-as.character(Base$CelularNotificaciones)
Base$CorreoNotificaciones<-as.character(Base$CorreoNotificaciones)

Base$CorreoElectronico_01<-tolower(Base$CorreoElectronico_01)
Base$CorreoElectronico_02<-tolower(Base$CorreoElectronico_02)
Base$CorreoNotificaciones<-tolower(Base$CorreoNotificaciones)

#######################Quitar no tiene#############
Base[grep("tiene",Base$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-"null"
Base[grep("tiene",Base$CorreoElectronico_02,value = FALSE),]$CorreoElectronico_02<-"null"
Base[grep("no@no.com",Base$CorreoElectronico_02,value = FALSE),]$CorreoElectronico_02<-"null"
Base[grep("no@telefonica.com.ec",Base$CorreoElectronico_02,value = FALSE),]$CorreoElectronico_02<-"null"

Base[grep("noposee",Base$CorreoElectronico_02,value = FALSE),]$CorreoElectronico_02<-"null"
Base[grep("nose",Base$CorreoElectronico_02,value = FALSE),]$CorreoElectronico_02<-"null"
Base[grep("nose",Base$CorreoElectronico_01,value = FALSE),]$CorreoElectronico_01<-"null"



Base["TieneMail"]<-0
Base[which(nchar(Base$CorreoElectronico_01)>6 | nchar(Base$CorreoElectronico_02)>6),]$TieneMail<-1
table(Base$TieneMail)


Base["TieneCelu"]<-0
Base[which(nchar(Base$Celular_01)>8 |
             nchar(Base$Celular_02)>8 |
             nchar(Base$Celular_03)>8 |
             nchar(Base$Celular_04)>8 |
             nchar(Base$Celular_05)>8 |
             nchar(Base$Celular_06)>8 |
             nchar(Base$Celular_07)>8 |
             nchar(Base$Celular_08)>8 |
             nchar(Base$Celular_09)>8 |
             nchar(Base$Celular_10)>8 ),]$TieneCelu<-1


Base["TieneCelularNotificaciones"]<-0
Base[which(nchar(Base$CelularNotificaciones)>6),]$TieneCelularNotificaciones<-1
table(Base$TieneCelularNotificaciones)


Base["TieneCorreoNotificaciones"]<-0
Base[which(nchar(Base$CorreoNotificaciones)>6),]$TieneCorreoNotificaciones<-1
table(Base$TieneCorreoNotificaciones)



B1<-Base[1:400000,]
B2<-Base[400001:877638,]

write.csv(B1,file="ProspectosRapidito1.csv",row.names = FALSE)

write.csv(Base,file="ProspectosRapidito.csv",row.names = FALSE)
#######################################


