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
setwd("d:/Users_info/ALBANPD/My Documents/Bases")


########################Cargar BAse################
odbcChannel <-odbcConnect("DWH_Externa") #Databases
#sqlTables(odbcChannel, tableType = "TABLE")
#sqlColumns(odbcChannel, "UbicacionGeografica1")
RegistroLaboral <- sqlExecute(odbcChannel, "SELECT * FROM RegistroLaboral", fetch = TRUE)
#######################
odbcChannel <-odbcConnect("DWH_Externa") #Databases
#sqlTables(odbcChannel, tableType = "TABLE")
#sqlColumns(odbcChannel, "UbicacionGeografica1")
UbicabilidadExterna <- sqlExecute(odbcChannel, "SELECT * FROM UbicabilidadExterna", fetch = TRUE)

UbicabilidadExterna$Identificacion<-as.character(UbicabilidadExterna$Identificacion)
UbicabilidadExterna<-subset(UbicabilidadExterna,select=c("Identificacion",
                                                         "Zonificacion_Provincia",
                                                         "Celular_01",
                                                         "Celular_02",
                                                         "Celular_03",
                                                         "Celular_04",
                                                         "Celular_05",
                                                         "Celular_06",
                                                         "Celular_07",
                                                         "Celular_08",
                                                         "Celular_09",
                                                         "Celular_10",
                                                         "CorreoElectronico_01",
                                                         "CorreoElectronico_02"
                                                         ))

#####################
RegistroLaboral<-subset(RegistroLaboral,TipoIngreso=="A")
RegistroLaboral$Identificacion<-as.character(RegistroLaboral$Identificacion)
RegistroLaboral[which(nchar(RegistroLaboral$Identificacion)==9),]$Identificacion<-paste0(0,RegistroLaboral[which(nchar(RegistroLaboral$Identificacion)==9),]$Identificacion)

aux<-subset(RegistroLaboral,select=c("Identificacion","Sueldo"))

Base<-left_join(aux,UbicabilidadExterna,by="Identificacion")
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
Base$CorreoElectronico_01<-as.character(Base$CorreoElectronico_01)
Base$CorreoElectronico_02<-as.character(Base$CorreoElectronico_02)

#################################
Base["Cel1"]<-nchar(Base$Celular_01)
Base["Cel2"]<-nchar(Base$Celular_02)
Base["Cel3"]<-nchar(Base$Celular_03)
Base["Cel4"]<-nchar(Base$Celular_04)
Base["Cel5"]<-nchar(Base$Celular_05)
Base["Cel6"]<-nchar(Base$Celular_06)
Base["Cel7"]<-nchar(Base$Celular_07)
Base["Cel8"]<-nchar(Base$Celular_08)
Base["Cel9"]<-nchar(Base$Celular_09)
Base["Cel10"]<-nchar(Base$Celular_10)
Base["Mail1"]<-nchar(Base$CorreoElectronico_01)
Base["Mail2"]<-nchar(Base$CorreoElectronico_02)
#################################
Base["TieneCelu"]<-0
Base[which(Base$Cel1>7 | 
             Base$Cel2>7 |
             Base$Cel3>7 |
             Base$Cel4>7 |
             Base$Cel5>7 |
             Base$Cel6>7 |
             Base$Cel7>7 |
             Base$Cel8>7 |
             Base$Cel9>7 |
             Base$Cel10>7 
           ),]$TieneCelu<-1

#################################
Base["TieneMail"]<-0
Base[which(Base$Mail1>5 | 
             Base$Mail2>5
),]$TieneMail<-1

#####################
UbicabilidadExterna$CorreoElectronico_01<-as.character(UbicabilidadExterna$CorreoElectronico_01)
UbicabilidadExterna$CorreoElectronico_02<-as.character(UbicabilidadExterna$CorreoElectronico_02)

aux<-subset(UbicabilidadExterna,nchar(CorreoElectronico_01)>4 | nchar(CorreoElectronico_02)>4)
str(UbicabilidadExterna)
