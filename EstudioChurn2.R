library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
setwd("d:/Users_info/ALBANPD/My Documents/Bases")

library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
#DWH_Externa..RegistroLaboral
odbcChannel <-odbcConnect("temporal") #Databases

Reporte25<-sqlFetch(odbcChannel,"lTab_Clientes_DRIEG_INCO_00025_TDC") #Tables
odbcClose(odbcChannel)

unique(Reporte25$FechaCorte)

Reporte5<-read.table("Reporte-RIEG-INCO-00005.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Marzo<-read.table("Marzo.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)

Desertores<-read.table("Desertores2.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Desertores<-subset(Desertores, Reestructurada==0 & EstadoTarjetaAntesCancelación=="ACTIVA")
nrow(Desertores)


Base<-left_join(Marzo,Desertores, by="IdCuentaTarjeta")
sum(Base$Objetivo, na.rm = TRUE)

Base$Objetivo[is.na(Base$Objetivo)] <- 0

Base2<-left_join(Base,Reporte14,by="IdCuentaTarjeta")
Base3<-Base2[!duplicated(Base2$Identificacion.x),]


d2<-Base3$FechaNacimiento
d3<- substr(d2, 0, 10)
yy<-substr(d3,7,10)
mm<-substr(d3,4,5)
dd<-substr(d3,0,2)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
n<-length(Base3$FechaNacimiento)
Base3["Edad.Cliente"] <- NA
Base3$Edad.Cliente<-age_years(d5,rep(Sys.Date(),n))
age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age}



sum(Base3$Objetivo)

Buenos<-subset(Base3,Objetivo==0)
Malos<-subset(Base3,Objetivo==1)
Buenos["Identificacion"]<-as.integer(Buenos$Identificacion.x)
Buenos15<-left_join(Buenos,Reporte5, by="Identificacion")

mean(Buenos15,na.rm = T)
Malos15<-left_join(Desertores,Reporte5)



Cuamalos<-subset(Malos15,!is.na(Malos15$NumeroTarjetasAbril2016))
Cuabuenos<-sample_n(subset(Buenos15,!is.na(Buenos15$NumeroTarjetasAbril2016)),4192)
table(cua$Objetivo)
mm1<-cbind(Cuabuenos$Objetivo,Cuabuenos$NumeroTarjetasAbril2016)
mm2<-cbind(Cuamalos$Objetivo,Cuamalos$NumeroTarjetasAbril2016)
mm<-rbind(mm1,mm2)
mm<-as.data.frame(mm)
colnames(mm)<-c("Objetivo","NumeroTarjetas" )

table(subset(Buenos,!is.na(PorcentajeAvance))$PorcentajeAvance)


duration.cut = cut(Malos$Consumo3UltMeses, c(0,1,10,20,13), right=FALSE) 
View(table(duration.cut) )
range(Buenos$PorcentajeAvance,na.rm = TRUE)


duration.cut = cut(Buenos$MesesRecencia, c(0,2.3,13), right=FALSE) 
View(table(duration.cut) )

range(Malos$MesesRecencia,na.rm = TRUE)


ec<-plot.ecdf(Malos$CupoUtilizadoSuperAvance)
duration.cut = cut(Buenos$CupoUtilizadoSuperAvance, c(0,335,7604), right=FALSE) 

ec<-ecdf(Malos$CupoUtilizadoSuperAvance)
View(cbind(ec$x,ec$y))

ecdf(Buenos$CupoDisponibleTotal,xlim = c(0,6000))

##########################Grafico de la densidad#######################
plot(density(subset(Bd,Objetivo==1)$MesesRecencia),xlim=c(0,12),lty=2,col="blue", xlab='Meses', ylab='Densidad',main = "Meses Recencia")
lines(density(subset(Bd,Objetivo==0)$MesesRecencia),xlim=c(0,12),lty=2,col="chocolate3")
legend("topright", legend = c("Desertores","No desertores"), lty = c(2,2),col=c("blue","chocolate3")) # optional legend

plot(density(subset(Buenos15,Objetivo==0 & !is.na(NumeroTarjetasAbril2016))$NumeroTarjetasAbril2016),xlim=c(0,12),lty=2,col="chocolate3", xlab='Tarjetas de crédito', ylab='Densidad',main = "Tarjetas de crédito en la competencia")
lines(density(subset(mm,Objetivo==1 & !is.na(NumeroTarjetas))$NumeroTarjetas),xlim=c(0,12),lty=2,col="blue")
legend("topright", legend = c("Desertores","No desertores"), lty = c(2,2),col=c("blue","chocolate3")) # optional legend




########################################3Arboles##################################

Buenos["Consumo3UltMeses"]<-"NA"
Buenos$Consumo3UltMeses[Bd$NoConsumo3UltMeses==1]<-0
Buenos$Consumo3UltMeses[Bd$NoConsumo3UltMeses==0]<-1

Malos["Consumo3UltMeses"]<-"NA"
Malos$Consumo3UltMeses[Malos$NoConsumo3UltMeses==1]<-0
Malos$Consumo3UltMeses[Malos$NoConsumo3UltMeses==0]<-1



library(rpart)
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
# grow tree


fit <- rpart(Objetivo ~ NumeroTarjetas, 
             data=mm)
#rpart.plot(fit)

rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           #cex=0.87,
           tweak=1)



fit <- rpart(Objetivo ~ CupoUtilizadoSuperAvance, 
             data=mm)


####################################Otros Arboles#################
library("party", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
iris_ctree <- ctree(as.factor(Objetivo) ~ (PagosVencidos),
                    data=Balanceada)

plot(iris_ctree)
plot(iris_ctree, type="simple")
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
plot(iris_ctree)

require(C50)
data(churn)
myTree = C5.0(as.factor(Objetivo) ~ `Cupo.Ut.SuperAvances/Cupo.Apro.SuperAvance`,
              data=Balanceada)
summary(myTree)
plot(myTree)
library("partykit")
myTree2 <- C50:::as.party.C5.0(myTree)
plot(myTree2)
plot(myTree2[33])


