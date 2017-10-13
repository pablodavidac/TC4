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


#mydata <- read.xlsx("Balances.xlsx", sheetName = "mysheet") 

Balances<-read.table("Balances.txt",
                      header=TRUE,
                      sep="\t",
                      na.strings = "NA")

Balances$Año.Mes<-as.character(Balances$Año.Mes)

Balances$Año.Mes<-as.Date(Balances$Año.Mes,"%d/%m/%Y")


Balances["Tamanio"]<-NA
Balances[which(Balances$Banco=="PICHINCHA"),]$Tamanio<-"BANCOS GRANDES"
Balances[which(Balances$Banco=="PACIFICO"),]$Tamanio<-"BANCOS GRANDES"
Balances[which(Balances$Banco=="GUAYAQUIL"),]$Tamanio<-"BANCOS GRANDES"
Balances[which(Balances$Banco=="PRODUBANCO"),]$Tamanio<-"BANCOS GRANDES"
Balances[which(Balances$Banco=="PROMERICA"),]$Tamanio<-"BANCOS GRANDES"

Balances[which(Balances$Banco=="AUSTRO"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="BOLIVARIANO"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="CITIBANK"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="GENERAL RUMIÑAHUI"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="INTERNACIONAL"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="LOJA"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="MACHALA"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="SOLIDARIO"),]$Tamanio<-"BANCOS MEDIANOS"
Balances[which(Balances$Banco=="PROCREDIT"),]$Tamanio<-"BANCOS MEDIANOS"

Balances[which(Balances$Banco=="AMAZONAS"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="COMERCIAL DE MANABI"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="LITORAL"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="COOPNACIONAL"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="CAPITAL"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="FINCA"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="DELBANK"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="D-MIRO"),]$Tamanio<-"BANCOS PEQUEÑOS"
Balances[which(Balances$Banco=="CODESARROLLO"),]$Tamanio<-"BANCOS PEQUEÑOS"

#############cAMBIO Nombre producbanco#############3
Balances[which(Balances$Banco=="PRODUBANCO"),]$Banco<-"PROMERICA"

Balances$Banco<-as.character(Balances$Banco)


Balances$Nivel.3<-as.character(Balances$Nivel.3)
###########################Calculo de las carteras################
Balances$Valor<-Balances$Valor*1000


aux<-Balances[which(Balances$Nivel.3=="1407-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO POR VENCER" |
                 Balances$Nivel.3=="1415-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO REFINANCIADA POR VENCER" |
                 Balances$Nivel.3=="1423-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO REESTRUCTURADA POR VENCER" |
                 Balances$Nivel.3=="1431-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO QUE NO DEVENGA INTERESES" |
                 Balances$Nivel.3=="1439-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO REFINANCIADA QUE NO DEVENGA INTERESES" |
                 Balances$Nivel.3=="1447-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO REESTRUCTURADA QUE NO DEVENGA INTERESES" |
                 Balances$Nivel.3=="1455-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO VENCIDA" |
                 Balances$Nivel.3=="1463-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO REFINANCIADA VENCIDA" |
                 Balances$Nivel.3=="1471-CARTERA DE CRÉDITOS DE CONSUMO ORDINARIO REESTRUCTURADA VENCIDA"),] 

ConsumoOrdinario<- aux %>% group_by(Banco,Año.Mes) %>% summarise(ConsumoOrdinario =sum(Valor))         


#############################
aux<-Balances[which(Balances$Nivel.3=="1405-CARTERA DE CRÉDITO PRODUCTIVO POR VENCER" |
                      Balances$Nivel.3=="1413-CARTERA DE CRÉDITO PRODUCTIVO REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1421-CARTERA DE CRÉDITO PRODUCTIVO REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1429-CARTERA DE CRÉDITO PRODUCTIVO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1437-CARTERA DE CRÉDITO PRODUCTIVO REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1445-CARTERA DE CRÉDITO PRODUCTIVO REESTRUCTURADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1453-CARTERA DE CRÉDITO PRODUCTIVO VENCIDA" |
                      Balances$Nivel.3=="1461-CARTERA DE CRÉDITO PRODUCTIVO REFINANCIADA VENCIDA" |
                      Balances$Nivel.3=="1469-CARTERA DE CRÉDITO PRODUCTIVO REESTRUCTURADA VENCIDA" 
                     ),] 

Productivo<- aux %>% group_by(Banco,Año.Mes) %>% summarise(Productivo =sum(Valor))    

###########################

aux<-Balances[which(Balances$Nivel.3=="1401-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO POR VENCER" |
                      Balances$Nivel.3=="1409-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1417-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1425-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1433-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1441-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO REESTRUCTURADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1449-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO VENCIDA" |
                      Balances$Nivel.3=="1457-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO REFINANCIADA VENCIDA" |
                      Balances$Nivel.3=="1465-CARTERA DE CRÉDITOS COMERCIAL PRIORITARIO REESTRUCTURADA VENCIDA"
),]

ComercialPrioritario<- aux %>% group_by(Banco,Año.Mes) %>% summarise(ComercialPrioritario =sum(Valor))    


########################
aux<-Balances[which(Balances$Nivel.3=="1406-CARTERA DE CRÉDITO COMERCIAL ORDINARIO POR VENCER" |
                      Balances$Nivel.3=="1414-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1422-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1430-CARTERA DE CRÉDITO COMERCIAL ORDINARIO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1438-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1454-CARTERA DE CRÉDITO COMERCIAL ORDINARIO VENCIDA" |
                      Balances$Nivel.3=="1462-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REFINANCIADA VENCIDA" ),]

ComercialOrdinario<- aux %>% group_by(Banco,Año.Mes) %>% summarise(ComercialOrdinario =sum(Valor)) 

########################
aux<-Balances[which(Balances$Nivel.3=="1406-CARTERA DE CRÉDITO COMERCIAL ORDINARIO POR VENCER" |
                      Balances$Nivel.3=="1414-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1422-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1430-CARTERA DE CRÉDITO COMERCIAL ORDINARIO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1438-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1454-CARTERA DE CRÉDITO COMERCIAL ORDINARIO VENCIDA" |
                      Balances$Nivel.3=="1462-CARTERA DE CRÉDITO COMERCIAL ORDINARIO REFINANCIADA VENCIDA" ),]

ComercialOrdinario<- aux %>% group_by(Banco,Año.Mes) %>% summarise(ComercialOrdinario =sum(Valor))   


########################
aux<-Balances[which(Balances$Nivel.3=="1403-CARTERA DE CRÉDITO INMOBILIARIO POR VENCER" |
                      Balances$Nivel.3=="1411-CARTERA DE CRÉDITO INMOBILIARIO REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1419-CARTERA DE CRÉDITO INMOBILIARIO REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1427-CARTERA DE CRÉDITO INMOBILIARIO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1435-CARTERA DE CRÉDITO INMOBILIARIO REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1443-CARTERA DE CRÉDITO INMOBILIARIO REESTRUCTURADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1451-CARTERA DE CRÉDITO INMOBILIARIO VENCIDA" |
                      Balances$Nivel.3=="1459-CARTERA DE CRÉDITO INMOBILIARIO REFINANCIADA VENCIDA" |
                      Balances$Nivel.3=="1467-CARTERA DE CRÉDITO INMOBILIARIO REESTRUCTURADA VENCIDA"
                     ),]

Inmobiliaria<- aux %>% group_by(Banco,Año.Mes) %>% summarise(Inmobiliaria =sum(Valor))   

########################
aux<-Balances[which(Balances$Nivel.3=="1473-CARTERA DE CRÉDITO EDUCATIVO POR VENCER" |
                      Balances$Nivel.3=="1479-CARTERA DE CRÉDITO EDUCATIVO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1485-CARTERA DE CRÉDITO EDUCATIVO VENCIDA"),]

Educativo<- aux %>% group_by(Banco,Año.Mes) %>% summarise(Educativo =sum(Valor))   

########################
aux<-Balances[which(Balances$Nivel.3=="1404-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA POR VENCER" |
                      Balances$Nivel.3=="1412-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1420-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1428-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1436-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1444-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA REESTRUCTURADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1452-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA VENCIDA" |
                      Balances$Nivel.3=="1460-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA REFINANCIADA VENCIDA" |
                      Balances$Nivel.3=="1468-CARTERA DE CRÉDITOS PARA LA MICROEMPRESA REESTRUCTURADA VENCIDA" 
                      ),]

Microcredito<- aux %>% group_by(Banco,Año.Mes) %>% summarise(Microcredito =sum(Valor))  

########################
aux<-Balances[which(Balances$Nivel.3=="1402-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO POR VENCER" |
                      Balances$Nivel.3=="1410-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO REFINANCIADA POR VENCER" |
                      Balances$Nivel.3=="1418-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO REESTRUCTURADA POR VENCER" |
                      Balances$Nivel.3=="1426-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1434-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO REFINANCIADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1442-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO REESTRUCTURADA QUE NO DEVENGA INTERESES" |
                      Balances$Nivel.3=="1450-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO VENCIDA" |
                      Balances$Nivel.3=="1458-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO REFINANCIADA VENCIDA" |
                      Balances$Nivel.3=="1466-CARTERA DE CRÉDITOS DE CONSUMO PRIORITARIO REESTRUCTURADA VENCIDA" 
                      ),]

ConsumoPrioritario<- aux %>% group_by(Banco,Año.Mes) %>% summarise(ConsumoPrioritario =sum(Valor)) 
