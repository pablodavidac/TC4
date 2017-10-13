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




Correos <- read.xlsx("CampaniaChurnJulio.xlsx",sheetName="Correos")
Correos$Identificacion<-as.character(Correos$Identificacion)

SMS <- read.xlsx("CampaniaChurnJulio.xlsx",sheetName="SMS")
SMS
Aplicativo <- read.xlsx("CampaniaChurnJulio.xlsx",sheetName="Aplicativo")

CallCenter <- read.xlsx("CampaniaChurnJulio.xlsx",sheetName="Call Center")

DesertoresJunJul17 <- read.xlsx("DesertoresJunJul17.xlsx",sheetName="Hoja1")


###############Caragas todas las campañas###########
Campanias<-read.table("CampañasRetencionAJulio17.txt",
                      header=TRUE,
                      sep="\t",
                      dec = ",",
                      na.strings = "NA")

aux<-Campanias %>% group_by(Identificacion) %>% mutate(Apariciones = n())
#aux<-subset(aux,Mes==7)
Apariciones<-subset(aux,!duplicated(Identificacion))
Apariciones<-subset(Apariciones,select=c("Identificacion","Apariciones"))

aux<-left_join(DesertoresJunJul17,Apariciones,by="Identificacion")
table(aux$Apariciones)

#########################Campañas###########################
CampaniaMayo<-subset(Campanias,Mes==6)
CampaniaJunio<-subset(Campanias,Mes==7)


##########################################

DesertoresJunJul17$fechacancelacion<-as.character(DesertoresJunJul17$fechacancelacion)
DesertoresJunJul17$fechacancelacion<-substring(DesertoresJunJul17$fechacancelacion, 1, 10)
DesertoresJunJul17$fechacancelacion<-as.Date(DesertoresJunJul17$fechacancelacion, "%Y-%m-%d")
DesertoresJunJul17$Identificacion<-as.character(DesertoresJunJul17$Identificacion)


CampaniaMayo<-subset(CampaniaMayoJunio,Campania=="Mayo")
CampaniaJunio<-subset(CampaniaMayoJunio,Campania=="Junio")
#####################Desertores por campania################
DesertoresCampaniaMayoJunio<-semi_join(CampaniaMayoJunio,DesertoresJunJul17,by="Identificacion")
table(DesertoresCampaniaMayoJunio$Mes,DesertoresCampaniaMayoJunio$Nuevo)

DesertoresCampaniaMayo<-semi_join(DesertoresJunJul17,CampaniaMayo,by="Identificacion")
table(DesertoresCampaniaMayo$Mes)

auxMayo<-subset(CampaniaMayo,select=c("Identificacion", "Nuevo"))
DesertoresCampaniaMayo<-left_join(DesertoresCampaniaMayo,auxMayo,by="Identificacion")
table(DesertoresCampaniaMayo$Mes,DesertoresCampaniaMayo$Nuevo)


DesertoresCampaniaJunio<-semi_join(DesertoresJunJul17,CampaniaJunio,by="Identificacion")
table(DesertoresCampaniaJunio$Mes)
auxJunio<-subset(CampaniaJunio,select=c("Identificacion", "Nuevo"))
DesertoresCampaniaJunio<-left_join(DesertoresCampaniaJunio,auxJunio,by="Identificacion")

table(DesertoresCampaniaJunio$Mes,DesertoresCampaniaJunio$Nuevo)


########################Correos#####################
aux<-semi_join(Correos,CampaniaJunio, by="Identificacion")
DesertoresCorreos<-semi_join(Correos,DesertoresJunJul17,by="Identificacion")
#aux<-subset(Correos,!duplicated(Identificacion))
aux<-subset(DesertoresCorreos,!duplicated(Identificacion))
write.xlsx(DesertoresCorreos,file = "DesertoresCorreos.xlsx",row.names = FALSE)

View(table(DesertoresCorreos$Beneficio))

########################SMS####################
DesertoresSMS<-semi_join(SMS,DesertoresCampaniaJunio,by="Identificacion")
aux<-subset(DesertoresSMS,!duplicated(Identificacion))
aux<-semi_join(CampaniaJunio,aux,by="Identificacion")
#write.xlsx(DesertoresSMS,file="DesertoresSMS.xlsx",row.names = FALSE)
#####################Call Center######################
colnames(CallCenter)[1]<-"Identificacion"
DesertoresCallCenter<-semi_join(CallCenter,DesertoresJunJul17,by="Identificacion")
aux<-subset(CallCenter,Base=="CHURN JUNIO")
DesertoresCallCenterMayo<-semi_join(aux,DersertoresJunio,by="Identificacion")

DesertoresCallCenterJunio<-semi_join(aux,DersertoresJulio,by="Identificacion")

table(CallCenter$Base)
View(table(CallCenter$Campania))
###############################Aplicativo####################
axuAplicativo<-semi_join(CampaniaMayoJunio,Aplicativo,by="Identificacion")
axuAplicativo<-subset(axuAplicativo,!duplicated(Identificacion))
auxDesertores<-semi_join(axuAplicativo,DesertoresJunJul17,by="Identificacion")


DM<-anti_join(DesertoresJunJul17,ScoreMayo,by="Identificacion")


#############Desertores###########
DersertoresJunio<-subset(DesertoresJunJul17,Mes=="Junio")
DersertoresJulio<-subset(DesertoresJunJul17,Mes=="Julio")


NoDetectadosJunio<-anti_join(DersertoresJunio,CampaniaMayo,by="Identificacion")
NoDetectadosJunio<-semi_join(ScoreMayo,NoDetectadosJunio,by="Identificacion")
table(NoDetectadosJunio$Decil)
NoDetectadosJunio<-left_join(NoDetectadosJunio,Apariciones,by="Identificacion")
##################No dectectados##################
NoDetectados<-read.table("NoDetectados.txt",
                      header=TRUE,
                      sep="\t",
                      na.strings = "NA")
NoDetectados$Identificacion<-as.character(NoDetectados$Identificacion)
NoDetectados[which(nchar(NoDetectados$Identificacion)==9),]$Identificacion<-paste0(0,NoDetectados[which(nchar(NoDetectados$Identificacion)==9),]$Identificacion)

aux<-left_join(NoDetectados,Apariciones,by="Identificacion")
aux<-semi_join(ScoreJunio,aux,by="Identificacion")
aux<-left_join(aux,Apariciones,by="Identificacion")
aux[which(is.na(aux$Apariciones)),]$Apariciones<-0
table(aux$Decil)table(aux$Decil)
############################nO Campaña##########
NoCampania<-anti_join(ScoreJunio,Apariciones,by="Identificacion")
table(NoCampania$Decil)
aux<-semi_join(NoCampania,NoDetectados,by="Identificacion")


aux<-subset(BaseB,Decil %in% c("P4","P5","P6","P7","P8"))
aux1<-subset(aux,Afinidad==0)
table(aux1$Odjetivo,aux1$Segmento_TDCAbril2017)
###########################################

####################No se hizo campañas################
SinCampania<-anti_join(CampaniaJunio,Correos,by="Identificacion")
SinCampania<-anti_join(SinCampania,SMS,by="Identificacion")
aux<-subset(CallCenter,Base=="CHURN JUNIO")
SinCampania<-anti_join(SinCampania,aux,by="Identificacion")


#SinCampania<-anti_join(SinCampania,subset(CallCenter,Base=="CHURN JUNIO"),by="Identificacion")

DesertoresSinCampania<-semi_join(SinCampania,DesertoresJunJul17,by="Identificacion")
DesertoresSinCampania<-subset(DesertoresSinCampania,!duplicated(Identificacion))

AparicionesSinCampania<-left_join(SinCampania,Apariciones,by="Identificacion")
table(AparicionesSinCampania$Apariciones)
#################preparar base###############
NoDetectados["Odjetivo"]<-1
NoDetectados<-subset(NoDetectados,select=c("Identificacion","Odjetivo"))

Base<-left_join(ScoreJunio,NoDetectados,by="Identificacion")
Base[which(is.na(Base$Odjetivo)),]$Odjetivo<-0
Base$Odjetivo<-as.factor(Base$Odjetivo)
Base$Afinidad<-as.factor(Base$Afinidad)
Base$NumeroEntidadesSF<-as.numeric(Base$NumeroEntidadesSF)
Base$Segmento_TDCAbril2017<-as.factor(Base$Segmento_TDCAbril2017)
Base$Provincia<-as.factor(Base$Provincia)
Base$Otra_Tarjeta<-as.factor(Base$Otra_Tarjeta)
Base$SoloSeguro<-as.factor(Base$SoloSeguro)
Base$Decil<-as.factor(Base$Decil)

###########################################
set.seed(123)
BaseB<-subset(Base,Decil!="P10" & Decil!="P9")
BaseB<-left_join(BaseB,Apa)
tree <- rpart(Odjetivo ~ ., data = Base, control = rpart.control(cp = 0.0001))

tree2 <- rpart(Odjetivo ~ Afinidad+
                 #NumeroEntidadesSF+
                 Segmento_TDCAbril2017+
                 Provincia+
                 #SoloSeguro+
                 #Otra_Tarjeta+
                 Decil
               , data = BaseB, control = rpart.control(cp = 0.00001))



plot(tree2)
prp(tree2,  cex = 0.8, extra = 1)

library("party")
iris_ctree <- ctree(Odjetivo ~ Afinidad+
                      #NumeroEntidadesSF+
                      Segmento_TDCAbril2017+
                      #Provincia+
                      #SoloSeguro+
                      #Otra_Tarjeta+
                      Decil, data = BaseB)

print(iris_ctree)
plot(iris_ctree)




##############

########################Malos####################
n<-624
#n<-21  #Activos
Malos<-do.call("rbind", replicate(n, subset(BaseB, Odjetivo==1), simplify = FALSE))

BalanceadaD<-rbind(as.data.frame(BaseB),as.data.frame(Malos))
BalanceadaD %>% count(Odjetivo)
BalanceadaD<-left_join(BalanceadaD,Apariciones,by="Identificacion")
BalanceadaD[which(is.na(BalanceadaD$Apariciones)),]$Apariciones<-0
tree2 <- rpart(Odjetivo ~ Afinidad+
                 #NumeroEntidadesSF+
                 Segmento_TDCAbril2017+
                 Provincia+
                # SoloSeguro+
                # Otra_Tarjeta+
                 Decil
                 #CupoUtilizado
                 #+Apariciones
               , data = BalanceadaD, control = rpart.control(cp = 0.01))



plot(tree2)
prp(tree2,  cex = 0.8, extra = 0)
prp(tree2, branch.type=5, yesno=FALSE, faclen=0)
rpart.plot(tree2,  cex = 0.5,extra=1)


################Arboles####################

Arbol<-subset(ScoreJunio,Decil!="P9" & Decil!="P10" )
table(Arbol$Decil)

R1<-subset(Arbol, Decil %in% c("P4","P5","P6","P7") & Provincia==1 & Afinidad==1)
R2<-subset(Arbol, Decil %in% c("P4","P5","P6") & Provincia==1)
R3<-subset(Arbol, Decil %in% c("P8") & Afinidad==0)

R4<-subset(Arbol,CupoUtilizado<119)

R<-rbind(R1,R2,R3,R4)
R<-subset(R,!duplicated(Identificacion))
aux<-semi_join(R,NoDetectados,by="Identificacion")
aux<-semi_join(DesertoresJunJul17,NoDetectados,by="Identificacion")
#####################histogramas##############
SinCampania$Score<-as.numeric(SinCampania$Score)
CampaniaJunio$Score<-as.numeric(CampaniaJunio$Score)
hist(as.numeric(SinCampania$Score),breaks = 50)
hist(as.numeric(CampaniaJunio$Score),breaks = 50)
d <- density(CampaniaJunio$Score)
plot(d)
d <- density(SinCampania$Score)
plot(d)
