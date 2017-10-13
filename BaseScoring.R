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

setwd("d:/Users_info/ALBANPD/My Documents/Bases")


Agosto16<-read.table("Base 2016 08 31.csv",
                    header=TRUE,
                    sep=",",
                    na.strings = "NA")

DesertoresAlia<-read.table("DesertoresAlia.csv",
                     header=TRUE,
                     sep=",",
                     na.strings = "NA")

#DesertoresAlia$ObjetivoCharacter<-as.character(DesertoresAlia$ObjetivoCharacter)

Febrero16<-read.table("Base 2016 02 29.csv",
                     header=TRUE,
                     sep=",",
                     na.strings = "NA")


##############
DesertoresAlia$Identificacion<-as.character(DesertoresAlia$Identificacion)
DesertoresAlia[which(nchar(DesertoresAlia$Identificacion)==9),]$Identificacion<-paste0(0,DesertoresAlia[which(nchar(DesertoresAlia$Identificacion)==9),]$Identificacion)
DesertoresAlia["ObjetivoCharacter"]<-"Desertor"
DesertoresAlia["Objetivo"]<-1
DesertoresAlia<-subset(DesertoresAlia,EstadoTarjeta=="ACTIVA" & TieneReestructura==0)
DesertoresAlia<-subset(DesertoresAlia,select=c("Identificacion","Mes","Objetivo","ObjetivoCharacter"))

Febrero16$Identificacion<-as.character(Febrero16$Identificacion)
Febrero16[which(nchar(Febrero16$Identificacion)==9),]$Identificacion<-paste0(0,Febrero16[which(nchar(Febrero16$Identificacion)==9),]$Identificacion)
Febrero16<-subset(Febrero16,select=c("Identificacion","CupoUtilizado"))
colnames(Febrero16)[2]<-"CupoUtilizadoT6"

Agosto16$Identificacion<-as.character(Agosto16$Identificacion)
Agosto16<-subset(Agosto16,CuentaTarjetaMonitor==1)
table(Agosto16$Segmento_TDCJulio2016)
###################################

Agosto<-subset(Agosto16,select=c("Identificacion",
                               "IdCuentaTarjeta",
                               "Afinidad",
                               "CupoUtilizado",
                               "CupoUtilizadoAvance",
                               "NumeroEntidadesSF",
                               "Segmento_TDCJulio2016",
                               "FechaApertura"))

Agosto<-subset(Agosto,!duplicated(IdCuentaTarjeta))


##################################Demograficas##################################
################Informacion del cliente################
Cliente <- dbConnect(odbc::odbc(),
                     Driver    = "SQL Server", 
                     Server    = "BSUIO-DWH02",
                     Database  = "Cliente")

Demografica<- tbl(Cliente, "ClienteNatural") %>%
  full_join(tbl(Cliente, "Cliente")) %>% 
  full_join(tbl(Cliente, "InformacionDemografica"),by="IdClienteNatural") %>% 
  full_join(tbl(Cliente, "DireccionClienteNatural"),by="IdClienteNatural") %>% 
  dplyr::select(IdClienteNatural, 
                IdCliente, 
                Identificacion,
                EstadoCivil,
                PrimerApellido,
                SegundoApellido,
                PrimerNombre,
                SegundoNombre,
                UbicacionGeografica1,
                UbicacionGeografica2,
                UbicacionGeografica3,
                UbicacionGeografica4)%>% collect()

colnames(Demografica)[9]<-"CodigoUbicacionGeografica1"
colnames(Demografica)[10]<-"CodigoUbicacionGeografica2"
colnames(Demografica)[11]<-"CodigoUbicacionGeografica3"
colnames(Demografica)[12]<-"CodigoUbicacionGeografica4"
Demografica$CodigoUbicacionGeografica1<-as.integer(Demografica$CodigoUbicacionGeografica1)
Demografica$CodigoUbicacionGeografica2<-as.integer(Demografica$CodigoUbicacionGeografica2)
Demografica$CodigoUbicacionGeografica3<-as.integer(Demografica$CodigoUbicacionGeografica3)
Demografica$CodigoUbicacionGeografica4<-as.integer(Demografica$CodigoUbicacionGeografica4)
#Demografica<-subset(Demografica,!duplicated(Identificacion))
#################Codificacion de Provinca Domicilio#####################
odbcChannel <-odbcConnect("Configuracion") #Databases
#sqlTables(odbcChannel, tableType = "TABLE")
#sqlColumns(odbcChannel, "UbicacionGeografica1")
UG1 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica1", fetch = TRUE)
UG1["Provincia"]<-UG1$Nombre
UG1<- dplyr::select(UG1,CodigoUbicacionGeografica1,Provincia)
UG2 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica2", fetch = TRUE)
UG2["Canton"]<-UG2$Nombre
UG2<- dplyr::select(UG2,CodigoUbicacionGeografica2,Canton)
UG3 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica3", fetch = TRUE)
UG3["Parroquia"]<-UG3$Nombre
UG3<- dplyr::select(UG3,CodigoUbicacionGeografica3,Parroquia)
UG4 <- sqlExecute(odbcChannel, "SELECT * FROM UbicacionGeografica4", fetch = TRUE)
UG4["Barrio"]<-UG4$Nombre
UG4$CodigoUbicacionGeografica4<-as.character(UG4$CodigoUbicacionGeografica4)
UG4$CodigoUbicacionGeografica4<-as.integer(UG4$CodigoUbicacionGeografica4)
UG4<- dplyr::select(UG4,CodigoUbicacionGeografica4,Barrio)

odbcClose(odbcChannel)


Demografica<-Demografica %>% 
  left_join(UG1,by="CodigoUbicacionGeografica1") %>%
  left_join(UG2,by="CodigoUbicacionGeografica2")%>%
  left_join(UG3,by="CodigoUbicacionGeografica3")%>%
  left_join(UG4,by="CodigoUbicacionGeografica4")

Demografica<-subset(Demografica,!duplicated(Identificacion))
Demografica<-dplyr::select(Demografica,IdClienteNatural:SegundoNombre,Provincia:Barrio)
#############################Autorizaciones############################

Tarjeta <- dbConnect(odbc::odbc(),
                     Driver    = "SQL Server", 
                     Server    = "BSUIO-DWH02",
                     Database  = "Tarjeta")
Autorizaciones6M<- tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2016-09-01' 
                                                           & FechaConciliacion > '2016-03-01')%>% collect()

aux<-Autorizaciones6M %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())
aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

AutorizacionesAgosto <-tbl(Tarjeta, "Autorizacion") %>% filter(FechaConciliacion < '2016-09-01' 
                                                               & FechaConciliacion > '2016-08-01')%>% collect()


SoloSeguro<-subset(AutorizacionesAgosto,TipoConsumo=="TRJSEGURO")
aux2<-SoloSeguro %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro = n())
aux2<-subset(aux2,select=c("IdCuentaTarjeta","ConsumosSeguro"))
aux2<-subset(aux2,!duplicated(IdCuentaTarjeta))




SoloConsumo<-subset(AutorizacionesAgosto,TipoConsumo=="TRJAVANCE" | 
                      TipoConsumo=="TRJCONSUMO" | 
                      TipoConsumo=="TRJDEBCON" |
                      TipoConsumo=="TRJSUPERAVCE" |
                      TipoConsumo=="TRJTAIRE"  )
aux3<-SoloConsumo %>% group_by(IdCuentaTarjeta) %>% mutate(Consumos = n())
aux3<-subset(aux3,select=c("IdCuentaTarjeta","Consumos"))
aux3<-subset(aux3,!duplicated(IdCuentaTarjeta))

Autorizaciones<-full_join(aux,aux2,by="IdCuentaTarjeta")
Autorizaciones<-full_join(Autorizaciones,aux3,by="IdCuentaTarjeta")
#########################################################################################

##############################Base Agosto################################
Agosto<-left_join(Agosto,Demografica,by="Identificacion")
Agosto<-subset(Agosto,!duplicated(IdCuentaTarjeta))
Agosto<-left_join(Agosto,Autorizaciones,by="IdCuentaTarjeta")
Agosto<-left_join(Agosto,Febrero16,by="Identificacion")
Agosto<-left_join(Agosto,DesertoresAlia,by="Identificacion")
Agosto["SoloSeguro"]<-"NO"
Agosto[which(Agosto$Consumos==0 & Agosto$ConsumosSeguro>0),]$SoloSeguro<-"SI"
Agosto$SoloSeguro<-as.factor(Agosto$SoloSeguro)
Agosto["DifCupoUtilizado"]<-Agosto$CupoUtilizado-Agosto$CupoUtilizadoT6
##############################################################################


###################################Remplazar NA por ceros##################
Agosto[which(is.na(Agosto$Autorizaciones6M)),]$Autorizaciones6M<-0
Agosto[which(is.na(Agosto$ConsumosSeguro)),]$ConsumosSeguro<-0
Agosto[which(is.na(Agosto$Consumos)),]$Consumos<-0
Agosto[which(is.na(Agosto$CupoUtilizadoT6)),]$CupoUtilizadoT6<-0
Agosto[which(is.na(Agosto$Objetivo)),]$Objetivo<-0
Agosto[which(is.na(Agosto$ObjetivoCharacter)),]$ObjetivoCharacter<-"No desertor"
Agosto$ObjetivoCharacter<-as.factor(Agosto$ObjetivoCharacter)
Agosto$Autorizaciones6M<-as.integer(Agosto$Autorizaciones6M)

##########################################################33

ClientesInteres<-subset(Agosto,
                        Segmento_TDCJulio2016%in%c("0. NUEVOS +6M",
                                                   "0. NUEVOS 6M",
                                                   "1. VIP",
                                                   "2. PREFERENTE",
                                                   "3. MOVILIZACION A"))

#######################Arbol###########################333

###########################################
#Desertor No desertor 
#4106      102442 
########################Malos####################
Desertores<-subset(ClientesInteres,Objetivo==1)
n<-25
Malos<-do.call("rbind", replicate(n, subset(Desertores, Objetivo==1), simplify = FALSE))

BalanceadaD<-rbind(subset(ClientesInteres, Objetivo==0),as.data.frame(Malos))
BalanceadaD %>% count(Objetivo)


tree2 <- rpart(ObjetivoCharacter ~ 
                 CupoUtilizado+
                 #CupoUtilizadoT6+
                 CupoUtilizadoAvance+
                 Afinidad+
                 NumeroEntidadesSF+
                 Segmento_TDCJulio2016+
                 Provincia+
                 Provincia+
                 Autorizaciones6M+
                 #SoloSeguro+
                 DifCupoUtilizado+
                 EstadoCivil
                ,data = BalanceadaD, control = rpart.control(cp = 0.001))



plot(tree2)
prp(tree2,  cex = 0.8, extra = 0)
prp(tree2, branch.type=5, yesno=FALSE, faclen=0)
rpart.plot(tree2,  cex = 0.5,extra=1)

###################Codificacion##################
Codificada<-BalanceadaD

######################Codificacion variables#######################
########################Afinidad#############################
Codificada$AfinidadTarjeta<-Codificada$Afinidad
Codificada["Afinidad"]<-"Otra afinidad"
Codificada[which(Codificada$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-"Afinidad Interes"
Codificada[which(Codificada$AfinidadTarjeta=="PLATINUM"),]$Afinidad<-"Afinidad Interes"

Codificada[which(Codificada$AfinidadTarjeta=="TCF PLATINUM-NORMAL"),]$Afinidad<-"Otra afinidad"
Codificada[which(Codificada$AfinidadTarjeta=="GOLD     "),]$Afinidad<-"Otra afinidad"
Codificada[which(Codificada$AfinidadTarjeta=="BLACK"),]$Afinidad<-"Otra afinidad"
Codificada[which(Codificada$Afinidad=="NA"),]$Afinidad<-"Otra afinidad"

Codificada$Afinidad<-as.factor(Codificada$Afinidad)
########################Variable provincia#############################
Codificada["ProvinciaDomicilio"]<-Codificada$Provincia
Codificada["Provincia"]<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="AZUAY"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="EL ORO"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="LOJA"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-"Provincia de Interes"
Codificada[which(Codificada$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-"Provincia de Interes"

Codificada[which(Codificada$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="CARCHI"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="MANABI"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="NAPO"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-"Otra Provincia"
Codificada[which(Codificada$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-"Otra Provincia"

Codificada[which(Codificada$Provincia=="NA"),]$Provincia<-"Otra Provincia"

Codificada$Provincia<-as.factor(Codificada$Provincia)
########################Otra tarjeta################################
Codificada["Otra_Tarjeta"]<-"SI"
Codificada[which(Codificada$NumeroEntidadesSF==0),]$Otra_Tarjeta<-"NO"
Codificada$Otra_Tarjeta<-as.factor(Codificada$Otra_Tarjeta)
#######################Autorizaciones 6 meses#############################
Codificada["SinAutorizaciones6M"]<-"Con autorizaciones"
Codificada[which(Codificada$Autorizaciones6M<0.5),]$SinAutorizaciones6M<-"Sin autorizaciones"
Codificada$SinAutorizaciones6M<-as.factor(Codificada$SinAutorizaciones6M)
##########################Cupo Utilizado##################################

Codificada["CodCupoUtilizado"]<-"Mayor a 3555"
Codificada[which(Codificada$CupoUtilizado<3555),]$CodCupoUtilizado<-"607 a 3555"
Codificada[which(Codificada$CupoUtilizado<607),]$CodCupoUtilizado<-"307 a 606.9"
Codificada[which(Codificada$CupoUtilizado<307),]$CodCupoUtilizado<-"238 a 306.9"
Codificada[which(Codificada$CupoUtilizado<=238),]$CodCupoUtilizado<-"0 a 237.9"
Codificada$CodCupoUtilizado<-as.factor(Codificada$CodCupoUtilizado)
##########################Cupo Utilizado##################################

Codificada["CodDifCupoUtilizado"]<-"Mayor a Cero"
Codificada[which(Codificada$DifCupoUtilizado<(-0.46)),]$CodDifCupoUtilizado<-"Menor a Cero"
Codificada$CodDifCupoUtilizado<-as.factor(Codificada$CodDifCupoUtilizado)
###########################SegmentoTDC####################
Codificada["CodSegmento"]<-"Otro Segmento"

Codificada[which(Codificada$Segmento_TDCJulio2016%in%c("0. NUEVOS +6M",
                                                       "3. MOVILIZACION A")),]$CodSegmento<-"Segmento de Interes"

Codificada$CodSegmento<-as.factor(Codificada$CodSegmento)

####################Corrida del modelo###############

modA3 <- glm(Objetivo~ 
               Afinidad+
               Provincia+
               Otra_Tarjeta+
               SinAutorizaciones6M+
               CodCupoUtilizado+
               CodDifCupoUtilizado+
               CodSegmento
             , data=Codificada,
             family="binomial"(link = "logit"))
summary(modA3)


####################Deciles###########################


Codificada["Score"]<-predict(modA3,newdata=Codificada,type='response')
Codificada["Decil"]<-"NA"

Codificada<-subset(Codificada,!duplicated(Identificacion))

fitted.results <- predict(modA3,newdata=Codificada,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
################Codificaccion de P1 - P10 ###########################
Codificada[which(Codificada$Score <= quan[2]),]$Decil<-"P1"
Codificada[which(Codificada$Score <=quan[3] & Codificada$Score > quan[2]),]$Decil<-"P2"
Codificada[which(Codificada$Score <=quan[4] & Codificada$Score > quan[3]),]$Decil<-"P3"
Codificada[which(Codificada$Score <=quan[5] & Codificada$Score > quan[4]),]$Decil<-"P4"
Codificada[which(Codificada$Score <=quan[6] & Codificada$Score > quan[5]),]$Decil<-"P5"
Codificada[which(Codificada$Score <=quan[7] & Codificada$Score > quan[6]),]$Decil<-"P6"
Codificada[which(Codificada$Score <=quan[8] & Codificada$Score > quan[7]),]$Decil<-"P7"
Codificada[which(Codificada$Score <=quan[9] & Codificada$Score > quan[8]),]$Decil<-"P8"
Codificada[which(Codificada$Score <=quan[10] & Codificada$Score > quan[9]),]$Decil<-"P9"
Codificada[which(Codificada$Score >=quan[10]),]$Decil<-"P10"



View(table(Codificada$ObjetivoCharacter,Codificada$Decil,Codificada$Mes))




