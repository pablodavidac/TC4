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


################Microcredito########################

odbcChannel <-odbcConnect("DWH_TrabajoEstudios") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "HISTORICO.SaldosCartera231")



SaldosCartera231 <- sqlExecute(odbcChannel, "SELECT FechaCorte,Identificacion, Saldo, Oficina
                                FROM HISTORICO.SaldosCartera231 WHERE Estado in ('VENCID','VIGENT')                            
and CodigoProducto in ('MICCAMTRANSF','MICNORMALIZ','MICROCAMPANIA','MICRORURAL', 'MICROURBANO',
                               'MICRURTRANSF','MICURBCOMPRA','MICURBTRANSF','MICROAEI','IMPULSO')
                               and FechaCorte >'2015-01-01'", 
                               fetch = TRUE)
aux<-SaldosCartera231
SaldosCartera231$FechaCorte<-as.Date(SaldosCartera231$FechaCorte)
SaldosCartera231$FechaCorte<-format(as.Date(SaldosCartera231$FechaCorte), "%Y-%m")

SaldosCartera231["YTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=6),]$YTD<-paste0(year(SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=6),]$FechaCorte))



SaldosCartera231["Trimestre"]<-NA
SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=3),]$Trimestre<-paste0(year(SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=3),]$FechaCorte) ,".T1")
SaldosCartera231[which(month(SaldosCartera231$FechaCorte)>3 & month(SaldosCartera231$FechaCorte)<=6),]$Trimestre<-paste0(year(SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=6 & month(SaldosCartera231$FechaCorte)>3),]$FechaCorte) ,".T2")
SaldosCartera231[which(month(SaldosCartera231$FechaCorte)>6 & month(SaldosCartera231$FechaCorte)<=9),]$Trimestre<-paste0(year(SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=9 & month(SaldosCartera231$FechaCorte)>6),]$FechaCorte) ,".T3")
SaldosCartera231[which(month(SaldosCartera231$FechaCorte)>9 & month(SaldosCartera231$FechaCorte)<=12),]$Trimestre<-paste0(year(SaldosCartera231[which(month(SaldosCartera231$FechaCorte)<=12 & month(SaldosCartera231$FechaCorte)>9),]$FechaCorte) ,".T4")

SaldosCartera231["Mensual"]<-NA
SaldosCartera231$Mensual<-SaldosCartera231$FechaCorte



write.table(SaldosCartera231, "cartera2.txt", sep="\t",row.names = FALSE) 


# example of melt function
library(reshape)
mdata <- melt(SaldosCartera231, id=c("FechaCorte","Identificacion","Saldo","Oficina")) 
mdata<-subset(mdata,!is.na(value))
write.table(mdata, "cartera2.txt", sep="\t",row.names = FALSE) 


aux<-subset(SaldosCartera231,!duplicated(Identificacion))
sum(SaldosCartera231$Saldo)

################################################3
DWH_TrabajoEstudios <- dbConnect(odbc::odbc(),
                     Driver    = "SQL Server", 
                     Server    = "BSUIO-DWH02",
                     Database  = "DWH_TrabajoEstudios")



Saldo231<- tbl(DWH_TrabajoEstudios, "HISTORICO.SaldosCartera231") %>%

  filter(CodigoProducto=="MICCAMTRANSF" |
           CodigoProducto=="MICNORMALIZ"  |
           CodigoProducto=="MICROCAMPANIA"  |
           CodigoProducto=="MICRORURAL"  |
           CodigoProducto=="MICROURBANO"  |
           CodigoProducto=="MICRURTRANSF"  |
           CodigoProducto=="MICURBCOMPRA"  |
           CodigoProducto=="MICURBTRANSF"  ) %>% collect()


ClientesMicro<-subset(SaldosCartera231,
                      CodigoProducto=="MICCAMTRANSF" |
                        CodigoProducto=="MICNORMALIZ"  |
                        CodigoProducto=="MICROCAMPANIA"  |
                        CodigoProducto=="MICRORURAL"  |
                        CodigoProducto=="MICROURBANO"  |
                        CodigoProducto=="MICRURTRANSF"  |
                        CodigoProducto=="MICURBCOMPRA"  |
                        CodigoProducto=="MICURBTRANSF"  
)
ClientesMicro<-subset(ClientesMicro,!duplicated(Identificacion))
ClientesMicro<-subset(ClientesMicro,Estado!="CASTIG")
ClientesMicro<-subset(ClientesMicro,select = c("Identificacion"))
View(table(ClientesMicro$Estado))

NROW(ClientesMicro)



#############################################
odbcChannel <-odbcConnect("Credito") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Credito")

Colocaciones <- sqlExecute(odbcChannel, "SELECT NumeroCredito,IdInstitucion,IdCliente,
Monto, PlazoReal,FechaDesembolso, IdOficina, IdOficial, IdCredito,IdProducto
                                FROM Credito WHERE IdProducto in (1,2,3,129,130,132,133,139,144,13,14) and FechaDesembolso >'2015-01-01'", 
                               fetch = TRUE)
odbcChannel <-odbcConnect("DWH_TrabajoEstudios")
Oficina<- sqlExecute(odbcChannel,"SELECT  * FROM Oficina",fetch = TRUE)
odbcClose(odbcChannel)


Oficina["IdOficina"]<-Oficina$Oficina
Oficina<-subset(Oficina,select=c("IdOficina","Nombre"))
colnames(Oficina)[2]<-"Oficina"


Colocaciones<-left_join(Colocaciones,Oficina,by="IdOficina")
Colocaciones["SubRegional"]<-NA
Colocaciones$Oficina<-as.character(Colocaciones$Oficina)

Colocaciones[which(Colocaciones$IdOficina %in% c(7396,7397,6862)),]$Oficina<-"MANTA"

Colocaciones[which(Colocaciones$IdOficina==7156),]$Oficina<-"GUAYAQUIL"
Colocaciones[which(Colocaciones$Oficina %in% c("CARAPUNGO",
                                               "CENTRO GARCIA MORENO ",
                                               "ESMERALDAS ",
                                               "IÑAQUITO ",
                                               "MATRIZ",
                                               "LA PRENSA",
                                               "QUININDE",
                                               "TUMBACO ",
                                               "SICOBRA QUITO",
                                               "LA MERCED")),]$SubRegional<-"Pichincha 1"


Colocaciones[which(Colocaciones$Oficina %in% c("ATAHUALPA",
                                               "EQ EL CARMEN",
                                               "GUAMANI",
                                               "AG ES GUAMANI",
                                               "MAYORISTA ",
                                               "RECREO ",
                                               "SANGOLQUI ",
                                               "SALCEDO",
                                               "SANTO DOMINGO",
                                               "EQ STO DOMING 2",
                                               "EQ STO DOMING 1",
                                               "RODRIGO DE CHAVEZ")),]$SubRegional<-"Pichincha 2"


Colocaciones[which(Colocaciones$Oficina %in% c("CAYAMBE ",
                                               "IBARRA ",
                                               "EQ IBARRA 1",
                                               "EQ IBARRA 2",
                                               "OTAVALO ")),]$SubRegional<-"Sierra Norte"


Colocaciones[which(Colocaciones$Oficina %in% c("EQ AMBATO CEVALLOS",
                                               "EQ AMBATO CASTILLO",
                                               "AMBATO CASTILLO",
                                               "AMBATO CEVALLOS ",
                                               "EQ HUACHI",
                                               "EQ LATACUNGA 1",
                                               "EQ LATACUNGA 2",
                                               "LATACUNGA",
                                               "MACHACHI ",
                                               "PELILEO ",
                                               "EQ RIOBAMBA 1",
                                               "EQ RIOBAMBA 2",
                                               "AG ES SALCEDO",
                                               "RIOBAMBA ")),]$SubRegional<-"Sierra Centro"

Colocaciones[which(Colocaciones$Oficina %in% c("LIBERTAD ",
                                               "LOJA",
                                               "MACHALA",
                                               "EQ. MALL EL FORTIN 1",
                                               "EQ. MALL EL FORTIN 2",
                                               "MALL EL FORTIN",
                                               "AG ES MILAGRO",
                                               "MILAGRO",
                                               "QUEVEDO",
                                               "GUAYAQUIL"
                                               )),]$SubRegional<-"Guayas Austro 1"



Colocaciones[which(Colocaciones$Oficina %in% c( "25 DE JULIO",
                                                "EQ 25 DE JULIO 1", 
                                                "EQ 25 DE JULIO 2",
                                                "CUENCA",
                                                "EQ JUNIN",
                                                "JUNIN ",
                                                "EQ PARQUE CALIFORNIA 1",
                                                "EQ PARQUE CALIFORNIA 2",
                                                "PARQUE CALIFORNIA",
                                                "PARQUE CALIFORNIA I - GYE",
                                                "C.R. SICOBRA LA PROSPERINA GYE",
                                               "EQ DURAN",
                                               "PORTETE"
                                               )),]$SubRegional<-"Guayas Austro 2"

Colocaciones[which(Colocaciones$Oficina %in% c( "CHONE",
                                                "MANTA",
                                                "MANTA CENTRO",
                                                "MANTA TARQUI ",
                                                "EQ PORTOVIEJO 1",
                                                "EQ PORTOVIEJO 2",
                                                "PRIMERO DE MAYO ",
                                                "PORTOVIEJO "
                                                )),]$SubRegional<-"Manabi"

Colocaciones["Regional"]<-NA
Colocaciones[which(Colocaciones$SubRegional %in% c( "Pichincha 1",
                                                    "Pichincha 2",
                                                    "Sierra Norte",
                                                    "Sierra Centro" 
                                                     
)),]$Regional<-"SIERRA"

Colocaciones[which(Colocaciones$SubRegional %in% c( "Guayas Austro 1",
                                                    "Guayas Austro 2",
                                                    "PRIMERO DE MAYO ",
                                                    "Manabi")),]$Regional<-"COSTA"


# example of melt function
library(reshape)

Colocaciones

Colocaciones["YTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
Colocaciones[which(month(Colocaciones$FechaDesembolso)<=6),]$YTD<-paste0(year(Colocaciones[which(month(Colocaciones$FechaDesembolso)<=6),]$FechaDesembolso))



Colocaciones["Trimestre"]<-NA
Colocaciones[which(month(Colocaciones$FechaDesembolso)<=3),]$Trimestre<-paste0(year(Colocaciones[which(month(Colocaciones$FechaDesembolso)<=3),]$FechaDesembolso) ,".T1")
Colocaciones[which(month(Colocaciones$FechaDesembolso)>3 & month(Colocaciones$FechaDesembolso)<=6),]$Trimestre<-paste0(year(Colocaciones[which(month(Colocaciones$FechaDesembolso)<=6 & month(Colocaciones$FechaDesembolso)>3),]$FechaDesembolso) ,".T2")
Colocaciones[which(month(Colocaciones$FechaDesembolso)>6 & month(Colocaciones$FechaDesembolso)<=9),]$Trimestre<-paste0(year(Colocaciones[which(month(Colocaciones$FechaDesembolso)<=9 & month(Colocaciones$FechaDesembolso)>6),]$FechaDesembolso) ,".T3")
Colocaciones[which(month(Colocaciones$FechaDesembolso)>9 & month(Colocaciones$FechaDesembolso)<=12),]$Trimestre<-paste0(year(Colocaciones[which(month(Colocaciones$FechaDesembolso)<=12 & month(Colocaciones$FechaDesembolso)>9),]$FechaDesembolso) ,".T4")

Colocaciones["Mensual"]<-NA
Colocaciones$Mensual<-Colocaciones$FechaDesembolso
Colocaciones["Plazo"]<-Colocaciones$PlazoReal/30

Colocaciones$FechaDesembolso<-format(as.Date(Colocaciones$FechaDesembolso), "%Y-%m")

mdata <- melt(Colocaciones, id=c("NumeroCredito",
                                     "IdInstitucion",
                                     "IdCliente",
                                     "Monto",
                                     "PlazoReal",
                                     "Plazo",
                                     "FechaDesembolso",
                                     "IdOficina",
                                     "IdOficial",
                                     "IdCredito",
                                     "IdProducto",
                                     "Oficina",
                                     "SubRegional",
                                     "Regional"
                                     )) 
mdata<-subset(mdata,!is.na(value))
write.table(mdata, "Colocaciones.txt", sep="\t",row.names = FALSE) 






View(table(Colocaciones$Oficina))
aux<-subset(Colocaciones,is.na(SubRegional))
aux<-subset(aux,!duplicated(IdOficina))
aux$Oficina
View(aux)

aux2<-subset(Colocaciones,!is.na(SubRegional))
aux3<-aux2 %>% group_by(FechaDesembolso) %>% mutate(Colocado = sum(Monto))
aux3<-subset(aux3,!duplicated(FechaDesembolso))

aux3<-Colocaciones %>% group_by(IdProducto) %>% mutate(ColocadoR = sum(Monto))
aux3<-subset(aux3,!duplicated(IdProducto))

aux3<-Colocaciones %>% group_by(FechaDesembolso,Oficina) %>% mutate(ColocadoR = sum(Monto))
aux3<-subset(aux3,FechaDesembolso=="2017-03")
aux3<-subset(aux3,!duplicated(Oficina))
###########################################################33




################Base con seguros################
DWH_Reporte <- dbConnect(odbc::odbc(),
                     Driver    = "SQL Server", 
                     Server    = "BSUIO-DWH02",
                     Database  = "DWH_Reporte")

Credito <- dbConnect(odbc::odbc(),
                                 Driver    = "SQL Server", 
                                 Server    = "BSUIO-DWH02",
                                 Database  = "Credito")

Colocacion<- tbl(DWH_Reporte, "vwJPR_DetalleOperacion") %>%
   left_join(tbl(DWH_Reporte, "vwJPR_SeguroCredito"),by="NumeroCredito") %>%
    filter(FechaDesembolso > '2015-01-01') %>%
  dplyr::select(FechaDesembolso,
                NumeroCredito,
                IdCredito,
                IdCliente,
                Monto,
                Producto,
                SubProducto,
                AsisHospital,
                SegDesgravamen,
                SegCredito,
                SegIncendio,
                SegFamiliaSegura,
                NumeroIdentificacion         # SegVoluntario,
                              )%>% collect()

Credito<- tbl(Credito, "Credito") %>%
  filter(FechaDesembolso > '2015-01-01') %>%
  dplyr::select(
                IdCredito,
                IdOficial,
                EsVip,
                IdTipoCliente,
                PlazoReal)%>% collect()

odbcChannel <-odbcConnect("DWH_Reporte") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "DWH_Reporte")

Operacion <- sqlExecute(odbcChannel, "SELECT IdCredito, Oficina, Region
                                FROM vwJPR_DetalleOperacion" , 
                           fetch = TRUE)

odbcClose(odbcChannel)

CuotasOperacion<-tbl(DWH_Reporte, "vwJPR_CuotasOperacion")%>%
  dplyr::select(IdCredito,segvoluntario)%>% collect()


Colocacion<-left_join(Colocacion,Operacion,by="IdCredito")
Colocacion<-left_join(Colocacion,Credito,by="IdCredito")
Colocacion<-left_join(Colocacion,CuotasOperacion,by="IdCredito")
Colocacion<-subset(Colocacion,!duplicated(IdCredito))

Colocacion$FechaDesembolso<-as.Date(Colocacion$FechaDesembolso)
#Colocacion$FechaDesembolso<-as.Date(paste0(year(Colocacion$FechaDesembolso),"-",month(Colocacion$FechaDesembolso),"-",01))
#Colocacion$FechaDesembolso<-as.character(Colocacion$FechaDesembolso, format="%b-%y")


Colocacion<-subset(Colocacion,Producto=="MICROEMPRESA" | Producto=="MICROAEI")

        


Colocacion$Oficina<-as.character(Colocacion$Oficina)

Colocacion["SubRegional"]<-NA
Colocacion[which(Colocacion$Oficina %in% c("CARAPUNGO",
                                               "CENTRO GARCIA MORENO ",
                                               "ESMERALDAS ",
                                               "IÑAQUITO ",
                                               "MATRIZ",
                                               "LA PRENSA",
                                               "QUININDE",
                                               "TUMBACO ",
                                               "SICOBRA QUITO",
                                               "LA MERCED")),]$SubRegional<-"Pichincha 1"


Colocacion[which(Colocacion$Oficina %in% c("ATAHUALPA",
                                               "EQ EL CARMEN",
                                               "GUAMANI",
                                               "AG ES GUAMANI",
                                               "MAYORISTA ",
                                               "RECREO ",
                                               "SANGOLQUI ",
                                               "SALCEDO",
                                               "SANTO DOMINGO",
                                               "EQ STO DOMING 2",
                                               "EQ STO DOMING 1",
                                               "RODRIGO DE CHAVEZ")),]$SubRegional<-"Pichincha 2"


Colocacion[which(Colocacion$Oficina %in% c("CAYAMBE ",
                                               "IBARRA ",
                                               "EQ IBARRA 1",
                                               "EQ IBARRA 2",
                                               "OTAVALO ")),]$SubRegional<-"Sierra Norte"


Colocacion[which(Colocacion$Oficina %in% c("EQ AMBATO CEVALLOS",
                                               "EQ AMBATO CASTILLO",
                                               "AMBATO CASTILLO",
                                               "AMBATO CEVALLOS ",
                                               "EQ HUACHI",
                                               "EQ LATACUNGA 1",
                                               "EQ LATACUNGA 2",
                                               "LATACUNGA",
                                               "MACHACHI ",
                                           "PELILEO ",
                                               "EQ RIOBAMBA 1",
                                               "EQ RIOBAMBA 2",
                                               "AG ES SALCEDO",
                                               "RIOBAMBA ")),]$SubRegional<-"Sierra Centro"

Colocacion[which(Colocacion$Oficina %in% c("LIBERTAD ",
                                               "LOJA",
                                               "MACHALA",
                                               "EQ. MALL EL FORTIN 1",
                                               "EQ. MALL EL FORTIN 2",
                                               "MALL EL FORTIN",
                                               "AG ES MILAGRO",
                                               "MILAGRO",
                                               "QUEVEDO",
                                               "GUAYAQUIL")),]$SubRegional<-"Guayas Austro 1"


Colocacion[which(Colocacion$Oficina %in% c( "25 DE JULIO",
                                                "EQ 25 DE JULIO 1", 
                                                "EQ 25 DE JULIO 2",
                                                "CUENCA",
                                                "EQ JUNIN",
                                                "JUNIN ",
                                                "EQ PARQUE CALIFORNIA 1",
                                                "EQ PARQUE CALIFORNIA 2",
                                                "PARQUE CALIFORNIA",
                                                "PARQUE CALIFORNIA I - GYE",
                                                "C.R. SICOBRA LA PROSPERINA GYE",
                                                "EQ DURAN",
                                            "SHOPPING DURAN",
                                                "PORTETE")),]$SubRegional<-"Guayas Austro 2"

Colocacion[which(Colocacion$Oficina %in% c( "CHONE",
                                                "MANTA",
                                                "MANTA CENTRO",
                                                "MANTA TARQUI ",
                                            "EQ MANTA 2",
                                            "EQ MANTA 1",
                                            "EL CARMEN",
                                                "EQ PORTOVIEJO 1",
                                                "EQ PORTOVIEJO 2",
                                                "PRIMERO DE MAYO ",
                                                "PORTOVIEJO ")),]$SubRegional<-"Manabí"



Colocacion["Regional"]<-NA
Colocacion[which(Colocacion$SubRegional %in% c( "Pichincha 1",
                                                    "Pichincha 2",
                                                    "Sierra Norte",
                                                    "Sierra Centro" 
                                                    
)),]$Regional<-"SIERRA"

Colocacion[which(Colocacion$SubRegional %in% c( "Guayas Austro 1",
                                                    "Guayas Austro 2",
                                                    "PRIMERO DE MAYO ",
                                                    "Manabí")),]$Regional<-"COSTA"

##############Cod Seguros#############
Colocacion["TieneSeguroAsisHospital"]<-0
Colocacion[which(Colocacion$AsisHospital>0),]$TieneSeguroAsisHospital<-1

Colocacion["TieneSeguroSegDesgravamen"]<-0
Colocacion[which(Colocacion$SegDesgravamen>0),]$TieneSeguroSegDesgravamen<-1

Colocacion["TieneSeguroSegCredito"]<-0
Colocacion[which(Colocacion$SegCredito>0),]$TieneSeguroSegCredito<-1

Colocacion["TieneSeguroSegIncendio"]<-0
Colocacion[which(Colocacion$SegIncendio>0),]$TieneSeguroSegIncendio<-1

Colocacion["TieneSegFamiliaSegura"]<-0
Colocacion[which(Colocacion$SegFamiliaSegura>0),]$TieneSegFamiliaSegura<-1

Colocacion["Tienesegvoluntario"]<-0
Colocacion[which(Colocacion$segvoluntario>0),]$Tienesegvoluntario<-1


##################Agregar Variables####################33
Colocacion$PlazoReal<-Colocacion$PlazoReal/30

Colocacion["Créditos Campañas"]<-0
Colocacion[which(Colocacion$SubProducto=="MICROCAMPANIA"),]$`Créditos Campañas`<-1
Colocacion["Créditos Normales"]<-0
Colocacion[which(Colocacion$`Créditos Campañas`==0),]$`Créditos Normales`<-1

####################Agregar Variables filtro#######################
Colocacion["YTD"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
Colocacion[which(month(Colocacion$FechaDesembolso)<=6),]$YTD<-paste0(year(Colocacion[which(month(Colocacion$FechaDesembolso)<=6),]$FechaDesembolso))


Colocacion["Trimestre"]<-NA
Colocacion[which(month(Colocacion$FechaDesembolso)<=3),]$Trimestre<-paste0(year(Colocacion[which(month(Colocacion$FechaDesembolso)<=3),]$FechaDesembolso) ,".T1")
Colocacion[which(month(Colocacion$FechaDesembolso)>3 & month(Colocacion$FechaDesembolso)<=6),]$Trimestre<-paste0(year(Colocacion[which(month(Colocacion$FechaDesembolso)<=6 & month(Colocacion$FechaDesembolso)>3),]$FechaDesembolso) ,".T2")
Colocacion[which(month(Colocacion$FechaDesembolso)>6 & month(Colocacion$FechaDesembolso)<=9),]$Trimestre<-paste0(year(Colocacion[which(month(Colocacion$FechaDesembolso)<=9 & month(Colocacion$FechaDesembolso)>6),]$FechaDesembolso) ,".T3")
Colocacion[which(month(Colocacion$FechaDesembolso)>9 & month(Colocacion$FechaDesembolso)<=12),]$Trimestre<-paste0(year(Colocacion[which(month(Colocacion$FechaDesembolso)<=12 & month(Colocacion$FechaDesembolso)>9),]$FechaDesembolso) ,".T4")


#Colocacion$FechaDesembolso<-as.character(as.Date(Colocacion$FechaDesembolso), "%Y-%m")
Colocacion["Mensual"]<-NA
Colocacion$Mensual<-as.character(as.Date(Colocacion$FechaDesembolso), "%Y-%m")



Colocacion["MTY"]<-NA
#SaldosCartera231$YTD<-as.Date(SaldosCartera231$YTD)
Colocacion[which(day(Colocacion$FechaDesembolso)<=13),]$MTY<-as.character(as.Date(Colocacion[which(day(Colocacion$FechaDesembolso)<=13),]$FechaDesembolso), "%Y-%m")

##################unpivot######################
library(reshape)

mdata <- reshape::melt(Colocacion, id=c("FechaDesembolso",
                               "NumeroCredito",
                               "IdCredito",
                               "IdCliente",
                               "Monto",
                               "Producto",
                               "SubProducto",
                               "AsisHospital",
                               "SegDesgravamen",
                               "SegCredito",
                               "SegIncendio",
                               "SegFamiliaSegura",
                               "NumeroIdentificacion",
                               "Oficina",
                               "Region",
                               "IdOficial",
                               "EsVip",
                               "IdTipoCliente",
                               "PlazoReal",
                               "segvoluntario",
                               "SubRegional",
                               "Regional",
                               "TieneSeguroAsisHospital",
                               "TieneSeguroSegDesgravamen",
                               "TieneSeguroSegCredito",
                               "TieneSeguroSegIncendio",
                               "TieneSegFamiliaSegura",
                               "Tienesegvoluntario")) 
Aux<-Colocacion
class(Colocacion)
Aux <- as.data.frame(Aux)

mdata <- reshape::melt(Aux, id=c(1:30),na.rm = TRUE,variable_name = "Filtro") 
mdata<-subset(mdata,!is.na(value))
write.table(mdata, "Colocaciones4.txt", sep="\t",row.names = FALSE) 
write.csv(mdata, file="Colocaciones4.csv",row.names = FALSE) 

######################Asignar pesos para filtros#######################3





