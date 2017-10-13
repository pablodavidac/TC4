library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
library(plyr)
#system.time(fread('Empresas2014.csv', header = T, sep = ',')) 

Desertores<-read.table("Desertores2.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Desertores<-subset(Desertores, Reestructurada==0)
Dst<-read.table("Desertores2.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Ds<-Dst[rep(seq_len(nrow(Dst)),each=2),]


df <- data.frame(a=1:2, b=letters[1:2]) 
df[rep(seq_len(nrow(df)), each=2),]





Septiembre<-read.table("Septiembre.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Diciembre<-read.table("Diciembre.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Marzo<-read.table("Marzo.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)


Reporte13<-read.table("Reporte-RIEG-INCO-00013.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)
Reporte14<-read.table("Reporte-RIEG-INCO-00014.csv",header=TRUE,sep=",",na.strings = "NA", colClasses = NA)


Base<-left_join(Marzo,Desertores, by="IdCuentaTarjeta")
#sum(Base$Objetivo,na.rm = NA)
Base2<-left_join(Base,Reporte13, by="IdCuentaTarjeta")
nrow(Base2[Base2$EstadoTarjetaCorte20160331=="ACTIVA",])
Base3<-anti_join(Marzo,Desertores, by="IdCuentaTarjeta")
Base3["Obserbacion"]<-0
Var.y<-rbind(Base3$Obserbacion,Desertores$Objetivo)
##################################################

nrow(Reporte13[Reporte13$EstadoTarjetaCorte20150331=="ACTIVA",])

bb<-anti_join(subset(Desertores,Mes_Cancelacion==4 &EstadoTarjetaAntesCancelación=="ACTIVA" & Reestructurada==0 ,),Marzo)
Desertores[Desertores$Identificacion==104459086,]
Marzo[Marzo$Identificacion==104459086,]

###############################################Scrip correcto#####################3
Base<-left_join(Marzo,subset(Desertores,EstadoTarjetaAntesCancelación=="ACTIVA" & Reestructurada==0)
                , by="IdCuentaTarjeta")
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
B<-sample_n(Buenos,4746,replace=T)
Bd<-rbind(B,Malos)

hist(Malos$CupoUtilizado)
plot(density(Malos$CupoUtilizado))
d<-density(Malos$CupoUtilizado)
h<-hist(Malos$CupoUtilizado,breaks = 300 , xlim=c(0,2000))
h<-hist(Malos$CupoUtilizado,breaks = c(0,1.5,395,1000,3000,7000,7604))
write.csv(cbind(h$breaks,h$counts),file = "CupoUtilizado.csv")

ggplot() + aes(Malos$CupoUtilizado)+ geom_histogram(binwidth=1, colour="black", fill="white")


JK<-left_join(Desertores,Reporte14)

View(prop.table(table(JK$EstadoCivil20150531)))
View(prop.table(table(Reporte14$EstadoCivil20150531)))

View(Reporte14 %>% select(Profesion20150531) %>% distinct)
View(Reporte14 %>% select(ActividadEconimica) %>% distinct)
View(Reporte14 %>% select(NivelInstruccion20150531) %>% distinct)
density(Malos$CupoAprobadoSuperAvance[!is.na(Malos$CupoUtilizadoSuperAvance)])
cupoavance<-subset(Malos,!is.na(cupoavance))





h<-hist(subset(Bd,Objetivo==1)$CupoUtilizadoAvance,breaks = c(0,32,100,1000,4000))
View(cbind(h$breaks,h$counts))
View(subset(Bd,Objetivo==1)$CupoUtilizadoAvance[subset(Bd,Objetivo==1)$CupoUtilizadoAvance>0 & subset(Bd,Objetivo==1)$CupoUtilizadoAvance<=32])
range(Bd$CupoUtilizadoSuperAvance,na.rm = TRUE)

Bd2<<-subset(Bd,!is.na(CupoAprobadoNorma) & Objetivo==1)
range(Bd2$CupoUtilizadoSuperAvance)
duration.cut = cut(Bd2$CupoUtilizadoSuperAvance, c(0,335,2000), right=FALSE) 



range(Buenos$CupoUtilizado,na.rm = TRUE)
duration.cut = cut(Buenos$CupoUtilizadoSuperAvance, c(0,335,2000), right=FALSE) 

View( table(duration.cut) )




plot.ecdf(Bd2$CupoUtilizadoSuperAvance, xlim=c(0,2000))
plot.ecdf(Buenos$CupoUtilizadoSuperAvance)
plot.ecdf(Malos$CupoUtilizadoSuperAvance)

plot.ecdf(Bd2$CupoUtilizadoSuperAvance)
d<-ecdf(Bd2$CupoUtilizadoAvance)

#############################################################################3
NROW(Malos$CupoUtilizado[!is.na(Malos$CupoUtilizado)])



cua<-sample_n(subset(Buenos,!is.na(Buenos$Consumo3UltMeses)),4746)
table(cua$Objetivo)
mm<-rbind(cua,Malos)
duration.cut = cut(Malos$Consumo3UltMeses, c(0,2.3,13), right=FALSE) 
View(table(duration.cut) )
range(Buenos$Consumo3UltMeses,na.rm = TRUE)


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

plot(density(subset(Bd,Objetivo==0 & !is.na(Consumo3UltMeses))$Consumo3UltMeses),xlim=c(0,12),lty=2,col="chocolate3", xlab='Meses', ylab='Densidad',main = "Meses Recencia")
lines(density(subset(Bd,Objetivo==1 & !is.na(Consumo3UltMeses))$Consumo3UltMeses),xlim=c(0,12),lty=2,col="blue")
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


fit <- rpart(Objetivo ~ Consumo3UltMeses, 
               data=Bd)
#rpart.plot(fit)

rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           #cex=0.87,
           tweak=1)



fit <- rpart(Objetivo ~ CupoUtilizadoSuperAvance, 
             data=mm)



prp(fit)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits


BBB<-subset(Bd,Edad.Cliente>=67)
sum(BBB$Objetivo,na.rm =TRUE)/sum(Bd$Objetivo,na.rm = TRUE)
library(party)
fit <- ctree(as.factor(Objetivo) ~ as.factor(CargasFamiliares20150531),data=Bd)
plot(fit)
summary(fit)

################################################
Bd["PorCupoUsado"]<-"Na"
Bd$PorCupoUsado<-1-Bd$CupoDisponibleTotal/Bd$CupoAprobadoNormal
head(Bd$PorCupoUsado)
range(Bd$PorCupoUsado,na.rm = TRUE)
Bd<-subset(Bd,PorCupoUsado>0 & Objetivo==1)
duration.cut = cut(Bd$PorCupoUsado, c(0,0.22,0.71,1), right=FALSE) 
View(table(duration.cut) )


Buenos["PorCupoUsado"]<-"Na"
Buenos$PorCupoUsado<-1-Buenos$CupoDisponibleTotal/Buenos$CupoAprobadoNormal

Buenos<-subset(Buenos,PorCupoUsado>0 & Objetivo==0)
duration.cut = cut(Buenos$PorCupoUsado, c(0,0.22,0.72,1), right=FALSE) 
View(table(duration.cut) )


fit <- rpart(Objetivo ~ PorCupoUsado, 
             data=subset(Bd,PorCupoUsado>0))
#rpart.plot(fit)

rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           #cex=0.87,
           tweak=1)
