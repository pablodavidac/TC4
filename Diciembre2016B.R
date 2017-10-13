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
load("d:/Users_info/ALBANPD/My Documents/Bases/Diciembre.Rdata")





########################Cupo Utlizado Avances#############
Diciembre["CupoUtilizadoAvanceIguala0"]<-0
Diciembre[which(Diciembre$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1


#############################Autorizaciones############################

odbcChannel <-odbcConnect("Tarjeta") #Databases
sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Autorizacion")

Autorizaciones6M <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion < '2016-01-01' 
                               and FechaConciliacion > '2015-07-01'", fetch = TRUE)

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))

Diciembre<-left_join(Diciembre,aux,by=c("IdCuentaTarjeta"))



odbcChannel <-odbcConnect("DWH_CRM") #Databases
sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "SB_SegemtoDic2015")

SegemtoDic2015 <- sqlExecute(odbcChannel, "SELECT * FROM SB_SegemtoDic2015", fetch = TRUE)

SegemtoDic2015["Segmento_TDC2016"]<-SegemtoDic2015$Segmento_TDC

SegemtoDic2015<-subset(SegemtoDic2015,select=c("IdCuentaTarjeta","Segmento_TDC2016"))

Diciembre<-left_join(Diciembre,SegemtoDic2015,by="IdCuentaTarjeta")




#######################Autorizaciones 6 meses#############################
Diciembre["Menos8Autorizaciones6M"]<-0
Diciembre[which(Diciembre$Autorizaciones6M<8.5),]$Menos8Autorizaciones6M<-1


##############################Cupo Normalizado#############################
D<-subset(Diciembre,Segmento_TDC2016=="0. Nuevos +6M" |
                    Segmento_TDC2016=="0. Nuevos 6M" |
                    Segmento_TDC2016=="1. VIP" |
                    Segmento_TDC2016=="2. Preferente" |
                    Segmento_TDC2016=="3. Movilización A"|
                    Segmento_TDC2016=="3. Movilización B")


Diciembre["NormCupoUtilizado"]<-(Diciembre$CupoUtilizado-mean(Diciembre$CupoUtilizado))/sd(Diciembre$CupoUtilizado)

#Diciembre<-left_join(Diciembre,Aux,by="IdCuentaTarjeta")

#load("d:/Users_info/ALBANPD/My Documents/Bases/Ultimo 13 01 17.RData")
DiciembreCompleto<-Diciembre
load("d:/Users_info/ALBANPD/My Documents/Bases/ModeloChurn.Rdata")


####################Deciles###########################
fitted.results <- predict(modA2,newdata=Diciembre,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Diciembre["Score"]<-predict(modA2,newdata=Diciembre,type='response')
Diciembre["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Diciembre[which(Diciembre$Score <= quan[2]),]$Decil<-"P1"
Diciembre[which(Diciembre$Score <=quan[3] & Diciembre$Score > quan[2]),]$Decil<-"P2"
Diciembre[which(Diciembre$Score <=quan[4] & Diciembre$Score > quan[3]),]$Decil<-"P3"
Diciembre[which(Diciembre$Score <=quan[5] & Diciembre$Score > quan[4]),]$Decil<-"P4"
Diciembre[which(Diciembre$Score <=quan[6] & Diciembre$Score > quan[5]),]$Decil<-"P5"
Diciembre[which(Diciembre$Score <=quan[7] & Diciembre$Score > quan[6]),]$Decil<-"P6"
Diciembre[which(Diciembre$Score <=quan[8] & Diciembre$Score > quan[7]),]$Decil<-"P7"
Diciembre[which(Diciembre$Score <=quan[9] & Diciembre$Score > quan[8]),]$Decil<-"P8"
Diciembre[which(Diciembre$Score <=quan[10] & Diciembre$Score > quan[9]),]$Decil<-"P9"
Diciembre[which(Diciembre$Score >=quan[10]),]$Decil<-"P10"


###########Dispositivos de Ubicabilidad#################

odbcChannel <-odbcConnect("DWH_Reporte") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Rep_TelefonoClienteTotal")

Rep_Telefono <- sqlExecute(odbcChannel, "SELECT * FROM Rep_TelefonoClienteTotal", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)

Rep_Telefono$Identificacion<-as.character(Rep_Telefono$Identificacion)
Rep_Telefono<-subset(Rep_Telefono,select=c("Identificacion","Telefono_01"
                                           ,"Telefono_02"
                                           ,"Telefono_03"
                                           ,"Telefono_04"
                                           ,"Telefono_05"))
Rep_Telefono<-cbind(Rep_Telefono,
                    nchar(as.character(Rep_Telefono$Telefono_01)),
                    nchar(as.character(Rep_Telefono$Telefono_02)),
                    nchar(as.character(Rep_Telefono$Telefono_03)),
                    nchar(as.character(Rep_Telefono$Telefono_04)),
                    nchar(as.character(Rep_Telefono$Telefono_05)))


Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_01))`==0),]$Telefono_01<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_02))`==0),]$Telefono_02<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_03))`==0),]$Telefono_03<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_04))`==0),]$Telefono_04<-NA
Rep_Telefono[which(Rep_Telefono$`nchar(as.character(Rep_Telefono$Telefono_05))`==0),]$Telefono_05<-NA

Rep_Telefono<-subset(Rep_Telefono,select=c("Identificacion","Telefono_01"
                                           ,"Telefono_02"
                                           ,"Telefono_03"
                                           ,"Telefono_04"
                                           ,"Telefono_05"))

####################Mails Disponibles###########################
odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "DispositivoUbicacion")

MailsUbicacion <- sqlExecute(odbcChannel, "SELECT * FROM DispositivoUbicacion
                             WHERE
                             TipoDispositivo = 'EMAIL'
                             ", fetch = TRUE)

odbcClose(odbcChannel)

rm(odbcChannel)




odbcChannel <-odbcConnect("Cliente") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "InformacionDemografica")

ClienteNatural <- sqlExecute(odbcChannel, "SELECT * FROM ClienteNatural", fetch = TRUE)

Cliente <- sqlExecute(odbcChannel, "SELECT * FROM Cliente", fetch = TRUE)


odbcClose(odbcChannel)

rm(odbcChannel)

Demografica<-full_join(Cliente,ClienteNatural,by="IdCliente")
Demografica<-subset(Demografica,select=c("Identificacion","IdCliente","IdClienteNatural"))
MailsUbicacion<-left_join(MailsUbicacion,Demografica,by="IdClienteNatural")

############################Cruses############################
Diciembre<-left_join(Diciembre,MailsUbicacion,by="Identificacion")
Diciembre<-subset(Diciembre,!duplicated(Identificacion))

Diciembre<-left_join(Diciembre,Rep_Telefono,by="Identificacion")

save(Diciembre,file = "D2016.Rdata")


########################################################
load("d:/Users_info/ALBANPD/My Documents/Bases/D2016.Rdata")

Diciembre["Mail"]<-Diciembre$Valor

D16<-subset(Diciembre,select=c("Identificacion","IdCuentaTarjeta","Valor","Segmento_TDC2016","Score","Telefono_01",
                               "Telefono_01","Telefono_02","Telefono_03","Telefono_04"))
D16<-subset(D16,Segmento_TDC2016=="0. Nuevos +6M"
            | Segmento_TDC2016=="0. Nuevos 6M"
            | Segmento_TDC2016=="1. VIP"
            | Segmento_TDC2016=="2. Preferente"
            | Segmento_TDC2016=="3. Movilización A"
            | Segmento_TDC2016=="4. Movilización B" )
D16<-D16[order(-D16$Score),]
D<-D16[1:21170,]
D16Mail<-subset(D16,!is.na(Valor))



Dmail<-subset(D,!is.na(Valor))
DTel<-subset(D,nchar(as.character(D$Telefono_01))>0 |
             nchar(as.character(D$Telefono_02))>0 |
             nchar(as.character(D$Telefono_03))>0 |
             nchar(as.character(D$Telefono_04))>0 )

Desertores16<-read.table("BaseFebMar16.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA")
Desertores16[which(nchar(Desertores16$Identificacion)==9),]$Identificacion<-paste0("0",Desertores16[which(nchar(Desertores16$Identificacion)==9),]$Identificacion)


Aux<-semi_join(Desertores16,D16,by="Identificacion")
Aux<-semi_join(Aux,D,by="Identificacion")

DesMail<-semi_join(Desertores16,Dmail,by="Identificacion")

AUX<-semi_join(Desertores16,Dmail,by="Identificacion")

