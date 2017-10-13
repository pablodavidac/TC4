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
library("scales", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")


setwd("d:/Users_info/ALBANPD/My Documents/Bases")
c<-(sapply(Desertores, function(x) class(x)[[1]]))

Desertores<-read.table("Desertores25-01-17-14-03-17.csv",
                      header=TRUE,
                      sep=",",
                      na.strings = "NA", colClasses =c(
                        "factor",
                        "integer",
                        "factor",
                        "factor",
                        "factor",
                        "factor",
                        "factor",
                        "factor",
                        "integer",
                        "factor"))
Desertores$Identificacion<-as.character(Desertores$Identificacion)
Desertores[which(nchar(Desertores$Identificacion)==9),]$Identificacion<-paste0("0",Desertores[which(nchar(Desertores$Identificacion)==9),]$Identificacion)


View(nchar(as.character(Desertores$Identificacion)))

Diciembre<-read.table("D16.csv",
                      header=TRUE,
                      sep=",",
                      na.strings = "NA")

Enero<-read.table("Enero2017.csv",
                  header=TRUE,
                  sep=",",
                  na.strings = "NA", colClasses =c(
                    "factor",
                    "integer",
                    "factor"))
Diciembre<-subset(Diciembre,!Decil=="P8")
Enviado<-rbind(Diciembre,Enero)


Contactados<-read.table("BaseContactos.csv",
                        header=TRUE,
                        sep=",",
                        na.strings = "NA", colClasses =c(
                          "factor",
                          "factor",
                          "factor"))

                        
BaseGestion<-read.table("BaseGestionDicEne.csv",header=TRUE,
                                                sep=",",
                                                na.strings = "NA")     


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

load("d:/Users_info/ALBANPD/My Documents/Bases/Demografica.Rdata")

Aux<-subset(Demografica,select=c("IdClienteNatural","Identificacion","EdadCliente"))
MailsUbicacion<-left_join(MailsUbicacion,Aux,by="IdClienteNatural")
MailsUbicacion<-subset(MailsUbicacion,select=c("Identificacion","Valor","EdadCliente"))
########################################################################################################
Desertores<-subset(Desertores,select=c("Identificacion","IdCuentaTarjeta","SegmentoTDC"))
Desertores<-left_join(Desertores,MailsUbicacion,by="Identificacion")
Desertores<-subset(Desertores,!duplicated(Identificacion))
Desertores<-left_join(Desertores,Rep_Telefono,by="Identificacion")

Desertores[which(Desertores$Valor==" "),]$Valor<-"NA"

DTel<-subset(Desertores,!is.na(Telefono_01) 
             | !is.na(Telefono_02)
             | !is.na(Telefono_03)
             | !is.na(Telefono_04)
             | !is.na(Telefono_05))

DMail<-subset(Desertores,!is.na(Valor))

DMailoTel<-subset(Desertores,!is.na(Valor) | !is.na(Telefono_01) 
             | !is.na(Telefono_02)
             | !is.na(Telefono_03)
             | !is.na(Telefono_04)
             | !is.na(Telefono_05))

DMailyTel<-subset(Desertores,!is.na(Valor) & (!is.na(Telefono_01) 
                  | !is.na(Telefono_02)
                  | !is.na(Telefono_03)
                  | !is.na(Telefono_04)
                  | !is.na(Telefono_05)))


BGTel<-subset(BaseGestion,!is.na(NumeroCelularRegistrado)|!is.na(Telefono_01) 
           | !is.na(Telefono_02)
           | !is.na(Telefono_03)
           | !is.na(Telefono_04)
           | !is.na(Telefono_05))

BGMail<-subset(BaseGestion,!is.na(CorreoElectronicoRegistrado))

BGMailyTel<-subset(BaseGestion,!is.na(CorreoElectronicoRegistrado) & (!is.na(NumeroCelularRegistrado)
                                              | !is.na(Telefono_01) 
                                              | !is.na(Telefono_02)
                                              | !is.na(Telefono_03)
                                              | !is.na(Telefono_04)
                                              | !is.na(Telefono_05)))


aux1<-semi_join(Contactados,BGMail,by="Identificacion")
aux1<-subset(aux1,!duplicated(Identificacion))
table(aux1$CONTACTO)

a<-subset(Contactados,CONTACTO=="CONTACTADO")
a<-subset(a,!duplicated(Identificacion))
b<-subset(Contactados,CONTACTO=="NO CONTACTADO")
b<-subset(b,!duplicated(Identificacion))
b<-anti_join(b,a,by="Identificacion")
Contactados<-rbind(a,b)


Aux<-left_join(Contactados,Enviado,by="Identificacion")
Aux<-subset(Aux,!duplicated(Identificacion))


aux<-anti_join(Desertores,Enviado,by="Identificacion")
aux1<-anti_join(Enviado,Desertores,by="Identificacion")
aux2<-semi_join(Enviado,Desertores,by="Identificacion")
mail<-left_join(Contactados,BaseGestion,by="Identificacion")

Contactados<-subset(Contactados,CONTACTO=="CONTACTADO")

au<-anti_join(Desertores,Contactados,by="Identificacion")
au1<-anti_join(Contactados,Desertores,by="Identificacion")
au2<-semi_join(Contactados,Desertores,by="Identificacion")
Contactados<-subset(Contactados,CONTACTO=="CONTACTADO")


mail<-subset(BaseGestion,!is.na(CorreoElectronicoRegistrado) 
            & CorreoElectronicoRegistrado!="032601396" 
            & CorreoElectronicoRegistrado!="032604063"
            & CorreoElectronicoRegistrado!="032604630"
            & CorreoElectronicoRegistrado!="032605684"
            & CorreoElectronicoRegistrado!="032614251"
            & CorreoElectronicoRegistrado!="032904522"
            & CorreoElectronicoRegistrado!="032924117"
            & CorreoElectronicoRegistrado!="032926392"
            & CorreoElectronicoRegistrado!="032612300"
            & CorreoElectronicoRegistrado!="032969780"
            & CorreoElectronicoRegistrado!="032969780"
            & CorreoElectronicoRegistrado!=" ")

nomail<-subset(BaseGestion,is.na(CorreoElectronicoRegistrado) 
               | CorreoElectronicoRegistrado=="032601396" 
               | CorreoElectronicoRegistrado=="032604063"
               | CorreoElectronicoRegistrado=="032604630"
               | CorreoElectronicoRegistrado=="032605684"
               | CorreoElectronicoRegistrado=="032614251"
               | CorreoElectronicoRegistrado=="032904522"
               | CorreoElectronicoRegistrado=="032924117"
               | CorreoElectronicoRegistrado=="032926392"
               | CorreoElectronicoRegistrado=="032612300"
               | CorreoElectronicoRegistrado=="032969780"
               | CorreoElectronicoRegistrado=="032969780"
               | CorreoElectronicoRegistrado==" ")

aux<-anti_join(Desertores,mail,by="Identificacion")
aux1<-anti_join(mail,Desertores,by="Identificacion")
aux2<-semi_join(mail,Desertores,by="Identificacion")


aux<-anti_join(Desertores,nomail,by="Identificacion")
aux1<-anti_join(nomail,Desertores,by="Identificacion")
aux2<-semi_join(nomail,Desertores,by="Identificacion")




A<-subset(DiciembreI, Segmento_TDC!="5. Normalización A" & 
            Segmento_TDC!="6. Normalización B" &
            Segmento_TDC!="7. Inactivos")

DiciembreI$Segmento_TDC<-DiciembreI$`Segmento_TDC Nuevo`
A<-subset(DiciembreI,select=c("IdCuentaTarjeta","Mesdesercion","Objetivo"))
D15<-left_join(Diciembre,A,by="IdCuentaTarjeta")
