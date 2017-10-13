#####################Encuesta Conjoint##############################
library(data.table)
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library(plyr)
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
encuesta<- read.csv("en.csv",header=TRUE,sep=",")

encuesta["Clasificacion_Social"]<-"NA"
table(encuesta$Nivel_Edu, encuesta$Nivel_Ocup)
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==2 & encuesta$Nivel_Ocup==2]<-"D"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==2 & encuesta$Nivel_Ocup==3]<-"D"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==2 & encuesta$Nivel_Ocup==4]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==2 & encuesta$Nivel_Ocup==5]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==2 & encuesta$Nivel_Ocup==5]<-"CA"

encuesta$Clasificacion_Social[encuesta$Nivel_Edu==3 & encuesta$Nivel_Ocup==2]<-"D"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==3 & encuesta$Nivel_Ocup==3]<-"D"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==3 & encuesta$Nivel_Ocup==4]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==3 & encuesta$Nivel_Ocup==5]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==3 & encuesta$Nivel_Ocup==6]<-"CA"

encuesta$Clasificacion_Social[encuesta$Nivel_Edu==4 & encuesta$Nivel_Ocup==1]<-"D"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==4 & encuesta$Nivel_Ocup==2]<-"D"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==4 & encuesta$Nivel_Ocup==3]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==4 & encuesta$Nivel_Ocup==4]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==4 & encuesta$Nivel_Ocup==5]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==4 & encuesta$Nivel_Ocup==6]<-"B"

encuesta$Clasificacion_Social[encuesta$Nivel_Edu==5 & encuesta$Nivel_Ocup==1]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==5 & encuesta$Nivel_Ocup==2]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==5 & encuesta$Nivel_Ocup==3]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==5 & encuesta$Nivel_Ocup==4]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==5 & encuesta$Nivel_Ocup==5]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==5 & encuesta$Nivel_Ocup==6]<-"B"

encuesta$Clasificacion_Social[encuesta$Nivel_Edu==6 & encuesta$Nivel_Ocup==1]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==6 & encuesta$Nivel_Ocup==2]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==6 & encuesta$Nivel_Ocup==3]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==6 & encuesta$Nivel_Ocup==4]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==6 & encuesta$Nivel_Ocup==5]<-"B"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==6 & encuesta$Nivel_Ocup==6]<-"A"

encuesta$Clasificacion_Social[encuesta$Nivel_Edu==7 & encuesta$Nivel_Ocup==1]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==7 & encuesta$Nivel_Ocup==2]<-"CB"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==7 & encuesta$Nivel_Ocup==3]<-"CA"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==7 & encuesta$Nivel_Ocup==4]<-"B"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==7 & encuesta$Nivel_Ocup==5]<-"A"
encuesta$Clasificacion_Social[encuesta$Nivel_Edu==7 & encuesta$Nivel_Ocup==6]<-"A"

View(encuesta)
View(table(encuesta$Nivel_Edu, encuesta$Nivel_Ocup))
View(encuesta %>% count(Ciudad))

encuesta2<-subset(encuesta,!(Edad==0))

cbind(encuesta$Clasificacion_Social,scale(encuesta$Clasificacion_Social))

cbind(encuesta2$Nivel_Ocup,scale(encuesta2$Nivel_Ocup))

en<-cbind(encuesta2$Edad, as.factor(encuesta2$Clasificacion_Social))
colnames(en)<-c("Edad", "Clasificacion_Social")

en<-encuesta2$Edad

k <- kmeans(en, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(en, col=k$clust, pch=16)

str(encuesta2$Edad)

encuesta2["FEdad"]<-k$cluste
a<-encuesta2$Edad[encuesta2$FEdad==3]
max(a)
min(a)





en2<-cbind(encuesta2$Edad, as.factor(encuesta2$Clasificacion_Social),encuesta2$Marca)
colnames(en2)<-c( "Clasificacion_Social","Edad")

k2 <- kmeans(en2, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(en2, col=k$clust, pch=16)
write.csv(cbind(encuesta2$Edad,as.numeric(encuesta2$FEdad),as.factor(encuesta2$Clasificacion_Social),encuesta2$Clasificacion_Social),file = "Encuesta.csv")

en2<-cbind(encuesta2$Edad, as.factor(encuesta2$Clasificacion_Social),encuesta2$Marca)
h<-hclustvar(en2)
plot(h)

h$clusmat

###########################################################################333
as.factor(encuesta2$Clasificacion_Social)


library(party)
fit <- ctree(Marca ~ Edad + as.factor(Clasificacion_Social)+Nivel_Edu,data=encuesta2)
plot(fit, main="Conditional Inference Tree for Kyphosis")


fit






iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_ctree)
plot(iris_ctree)

iris_ctree<-ctree(as.numeric(Interes) ~ as.factor(Edad) + as.factor(Clase), data=encuesta2)

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


##################################
encuesta2["Clase"]<-"NA"
encuesta2$Clase<-as.factor(encuesta2$Clasificacion_Social)
library(rpart)
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
# grow tree
encuesta3<-subset(encuesta2,!(Clasificacion_Social=="NA"))
fit <- rpart(Pagos_puntualmenteUSD5x3m ~ Edad + Clasificacion_Social,method="anova", data=encuesta3)
rpart.plot(fit)

rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           #cex=0.87,
           tweak=1.16)

prp(fit)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results  

# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree
post(fit, file = "c:/tree2.ps",
     title = "Regression Tree for Mileage ")

##########################################
library(party)
fit <- ctree(Interes ~ Edad + as.factor(Clasificacion_Social)+as.factor(Nivel_Edu),data=encuesta2)
plot(fit, main="Conditional Inference Tree for Kyphosis")





###########################################
library(deldir)

# set graphical parameters
default.par <- par(mar = c(5, 4, 4, 2) + 0.1, ask=FALSE)
par(mar=rep(0,4))
on.exit(par(default.par))


########################################
# Setup
########################################
set.seed(28)

# number of clusters
nc = 2

# set true cluster centers
xc <- sample(seq(0.2, 0.8, by=0.1), 2)
yc <- sample(seq(0.2, 0.8, by=0.1), 2)

# generate points based on true centers
np = 30 # no of points
xp <- NULL
yp <- NULL
for (i in 1:nc) {
  xp <- c(xp, rnorm(np, mean = xc[i], sd = 0.15))
  yp <- c(yp, rnorm(np, mean = yc[i], sd = 0.15))
}
# remove points outside plotting region
xp[xp < 0.05 | xp > 0.95] = NA
yp[yp < 0.05 | yp > 0.95] = NA



########################################
# Iteration
########################################
set.seed(234)

# set random pseudo-center
xpc <- sample(seq(0.2, 0.8, by=0.1), 2)
ypc <- sample(seq(0.2, 0.8, by=0.1), 2)

# plot points and intial pseudo-centers
dummy = deldir(0,0, rw=c(0,1,0,1))
plot(tile.list(dummy), pch=NA, close = TRUE)  # voronoi boundaries
points(xp,yp, pch=4, cex=2) # points
points(xpc,ypc, pch=16, asp=1, cex=2) # centers


par(ask=TRUE) # pause each time plot is generated
# start iteration
for (i in 1:5)  {
  
  # assign points to clusters
  z <- deldir(xpc, ypc, rw=c(0,1,0,1))
  w <- tile.list(z)
  
  
  # plot pseudo-centers and points with boundaries colored
  plot(w, pch=NA, fillcol = c("#33ccff", "#ff9999"), close = TRUE)  # voronoi boundaries
  points(xpc,ypc, pch=16, cex=2) # centers
  points(xp,yp, pch=4, cex=2) # points
  
  
  # re-assign cluster members
  cluster <-  NA
  boundary <-  (z$dirsgs$y2 - z$dirsgs$y1) * xp + z$dirsgs$y1
  cluster[yp > boundary] = 2
  cluster[yp <= boundary] = 1
  
  # re-locate cluster center
  xpcnew <- xpc
  ypcnew <- ypc
  
  for (i in 1:2)  {
    xpcnew[i] <- mean(xp[cluster==i], na.rm = T)
    ypcnew[i] <- mean(yp[cluster==i], na.rm = T)
  }
  
  # plot new cluster center with old boundaries
  plot(w, pch=NA, fillcol = c("#33ccff", "#ff9999"), close = TRUE)  # voronoi boundaries
  points(xpcnew,ypcnew, pch=16, cex=2) # centers
  points(xp,yp, pch=4, cex=2) # points
  
  # stop the iteration if cluster centers have stabilized
  if (all(xpcnew != xpc) & all(xpcnew != xpc))  {
    xpc <- xpcnew
    ypc <- ypcnew
  } else break;
  
}
