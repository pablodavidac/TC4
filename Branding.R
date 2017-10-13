library("foreign", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

file.choose()

"C:\\PathToFile\\MyDataFile.sav"

setwd("d:/Users_info/ALBANPD/My Documents/Bases")
dataset = read.spss("Solidario_FINAL_Final.sav", to.data.frame=TRUE,use.value.labels = FALSE)


dataset<-spss.get("Solidario_FINAL_Final.sav", lowernames=FALSE, datevars = NULL,
                  use.value.labels = TRUE, to.data.frame = TRUE,
                  force.single=TRUE,
                  allow=NULL, charfactor=FALSE)

dataset = read.table("Solidario_FINAL_Final.sav", header = TRUE)

setwd("C:/Users/user/Desktop/Nueva carpeta")
library("memisc", lib.loc="C:/Program Files/R/R-3.2.2/library")

data <- as.data.set(spss.system.file('Solidario_FINAL_Final.sav'))

data2<-subset(data, tipomuestra=="Base")

table(data2$q101)

View(data[,c(9973:10000)])


data$holdbrand01
data$q23



data$q23_inv_otro_slice01
d<-data[,c(10334:10639)]

table(data$loopq1_brand1_q2_1001)

q1b<-c("q1b01","q1b02","q1b03","q1b04","q1b05","q1b06","q1b07","q1b08","q1b09","q1b10",
       "q1b11","q1b12","q1b13","q1b14","q1b15","q1b16","q1b17","q1b18","q1b19")
#q1<-c("holdbrand01","holdbrand02","holdbrand03","holdbrand04","holdbrand05","holdbrand06","holdbrand07",
#      "holdbrand08","holdbrand09","holdbrand10","holdbrand11","holdbrand12","holdbrand13","holdbrand14",
#      "holdbrand15","holdbrand16","holdbrand17","holdbrand18","holdbrand19","holdbrand20")
q1<-c("q101","q102","q103","q104","q105","q106","q107","q108","q109","q110","q111",
      "q112","q113","q114","q115","q116","q117","q118","q119","q120")

data$q23_con_brand1_slice17

q23_con<-c("q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           ")

q23<-c(    "q23_1_slice01",
           "q23_2_slice01",
           "q23_3_slice01",
           "q23_4_slice01",
           "q23_5_slice01",
           "q23_6_slice01",
           "q23_7_slice01",
           "q23_8_slice01",
           "q23_9_slice01",
           "q23_10_slice01",
           "q23_11_slice01",
           "q23_12_slice01",
           "q23_13_slice01",
           "q23_14_slice01",
           "q23_15_slice01",
           "q23_16_slice01",
           "q23_17_slice01",
           "q23_1_slice02",
           "q23_2_slice02",
           "q23_3_slice02",
           "q23_4_slice02",
           "q23_5_slice02",
           "q23_6_slice02",
           "q23_7_slice02",
           "q23_8_slice02",
           "q23_9_slice02",
           "q23_10_slice02",
           "q23_11_slice02",
           "q23_12_slice02",
           "q23_13_slice02",
           "q23_14_slice02",
           "q23_15_slice02",
           "q23_16_slice02",
           "q23_17_slice02",
           "q23_1_slice03",
           "q23_2_slice03",
           "q23_3_slice03",
           "q23_4_slice03",
           "q23_5_slice03",
           "q23_6_slice03",
           "q23_7_slice03",
           "q23_8_slice03",
           "q23_9_slice03",
           "q23_10_slice03",
           "q23_11_slice03",
           "q23_12_slice03",
           "q23_13_slice03",
           "q23_14_slice03",
           "q23_15_slice03",
           "q23_16_slice03",
           "q23_17_slice03",
           "q23_1_slice04",
           "q23_2_slice04",
           "q23_3_slice04",
           "q23_4_slice04",
           "q23_5_slice04",
           "q23_6_slice04",
           "q23_7_slice04",
           "q23_8_slice04",
           "q23_9_slice04",
           "q23_10_slice04",
           "q23_11_slice04",
           "q23_12_slice04",
           "q23_13_slice04",
           "q23_14_slice04",
           "q23_15_slice04",
           "q23_16_slice04",
           "q23_17_slice04",
           "q23_1_slice05",
           "q23_2_slice05",
           "q23_3_slice05",
           "q23_4_slice05",
           "q23_5_slice05",
           "q23_6_slice05",
           "q23_7_slice05",
           "q23_8_slice05",
           "q23_9_slice05",
           "q23_10_slice05",
           "q23_11_slice05",
           "q23_12_slice05",
           "q23_13_slice05",
           "q23_14_slice05",
           "q23_15_slice05",
           "q23_16_slice05",
           "q23_17_slice05",
           "q23_1_slice06",
           "q23_2_slice06",
           "q23_3_slice06",
           "q23_4_slice06",
           "q23_5_slice06",
           "q23_6_slice06",
           "q23_7_slice06",
           "q23_8_slice06",
           "q23_9_slice06",
           "q23_10_slice06",
           "q23_11_slice06",
           "q23_12_slice06",
           "q23_13_slice06",
           "q23_14_slice06",
           "q23_15_slice06",
           "q23_16_slice06",
           "q23_17_slice06",
           "q23_1_slice07",
           "q23_2_slice07",
           "q23_3_slice07",
           "q23_4_slice07",
           "q23_5_slice07",
           "q23_6_slice07",
           "q23_7_slice07",
           "q23_8_slice07",
           "q23_9_slice07",
           "q23_10_slice07",
           "q23_11_slice07",
           "q23_12_slice07",
           "q23_13_slice07",
           "q23_14_slice07",
           "q23_15_slice07",
           "q23_16_slice07",
           "q23_17_slice07",
           "q23_1_slice08",
           "q23_2_slice08",
           "q23_3_slice08",
           "q23_4_slice08",
           "q23_5_slice08",
           "q23_6_slice08",
           "q23_7_slice08",
           "q23_8_slice08",
           "q23_9_slice08",
           "q23_10_slice08",
           "q23_11_slice08",
           "q23_12_slice08",
           "q23_13_slice08",
           "q23_14_slice08",
           "q23_15_slice08",
           "q23_16_slice08",
           "q23_17_slice08",
           "q23_1_slice09",
           "q23_2_slice09",
           "q23_3_slice09",
           "q23_4_slice09",
           "q23_5_slice09",
           "q23_6_slice09",
           "q23_7_slice09",
           "q23_8_slice09",
           "q23_9_slice09",
           "q23_10_slice09",
           "q23_11_slice09",
           "q23_12_slice09",
           "q23_13_slice09",
           "q23_14_slice09",
           "q23_15_slice09",
           "q23_16_slice09",
           "q23_17_slice09",
           "q23_1_slice10",
           "q23_2_slice10",
           "q23_3_slice10",
           "q23_4_slice10",
           "q23_5_slice10",
           "q23_6_slice10",
           "q23_7_slice10",
           "q23_8_slice10",
           "q23_9_slice10",
           "q23_10_slice10",
           "q23_11_slice10",
           "q23_12_slice10",
           "q23_13_slice10",
           "q23_14_slice10",
           "q23_15_slice10",
           "q23_16_slice10",
           "q23_17_slice10",
           "q23_1_slice11",
           "q23_2_slice11",
           "q23_3_slice11",
           "q23_4_slice11",
           "q23_5_slice11",
           "q23_6_slice11",
           "q23_7_slice11",
           "q23_8_slice11",
           "q23_9_slice11",
           "q23_10_slice11",
           "q23_11_slice11",
           "q23_12_slice11",
           "q23_13_slice11",
           "q23_14_slice11",
           "q23_15_slice11",
           "q23_16_slice11",
           "q23_17_slice11",
           "q23_1_slice12",
           "q23_2_slice12",
           "q23_3_slice12",
           "q23_4_slice12",
           "q23_5_slice12",
           "q23_6_slice12",
           "q23_7_slice12",
           "q23_8_slice12",
           "q23_9_slice12",
           "q23_10_slice12",
           "q23_11_slice12",
           "q23_12_slice12",
           "q23_13_slice12",
           "q23_14_slice12",
           "q23_15_slice12",
           "q23_16_slice12",
           "q23_17_slice12",
           "q23_1_slice13",
           "q23_2_slice13",
           "q23_3_slice13",
           "q23_4_slice13",
           "q23_5_slice13",
           "q23_6_slice13",
           "q23_7_slice13",
           "q23_8_slice13",
           "q23_9_slice13",
           "q23_10_slice13",
           "q23_11_slice13",
           "q23_12_slice13",
           "q23_13_slice13",
           "q23_14_slice13",
           "q23_15_slice13",
           "q23_16_slice13",
           "q23_17_slice13",
           "q23_1_slice14",
           "q23_2_slice14",
           "q23_3_slice14",
           "q23_4_slice14",
           "q23_5_slice14",
           "q23_6_slice14",
           "q23_7_slice14",
           "q23_8_slice14",
           "q23_9_slice14",
           "q23_10_slice14",
           "q23_11_slice14",
           "q23_12_slice14",
           "q23_13_slice14",
           "q23_14_slice14",
           "q23_15_slice14",
           "q23_16_slice14",
           "q23_17_slice14",
           "q23_1_slice15",
           "q23_2_slice15",
           "q23_3_slice15",
           "q23_4_slice15",
           "q23_5_slice15",
           "q23_6_slice15",
           "q23_7_slice15",
           "q23_8_slice15",
           "q23_9_slice15",
           "q23_10_slice15",
           "q23_11_slice15",
           "q23_12_slice15",
           "q23_13_slice15",
           "q23_14_slice15",
           "q23_15_slice15",
           "q23_16_slice15",
           "q23_17_slice15",
           "q23_1_slice16",
           "q23_2_slice16",
           "q23_3_slice16",
           "q23_4_slice16",
           "q23_5_slice16",
           "q23_6_slice16",
           "q23_7_slice16",
           "q23_8_slice16",
           "q23_9_slice16",
           "q23_10_slice16",
           "q23_11_slice16",
           "q23_12_slice16",
           "q23_13_slice16",
           "q23_14_slice16",
           "q23_15_slice16",
           "q23_16_slice16",
           "q23_17_slice16",
           "q23_1_slice17",
           "q23_2_slice17",
           "q23_3_slice17",
           "q23_4_slice17",
           "q23_5_slice17",
           "q23_6_slice17",
           "q23_7_slice17",
           "q23_8_slice17",
           "q23_9_slice17",
           "q23_10_slice17",
           "q23_11_slice17",
           "q23_12_slice17",
           "q23_13_slice17",
           "q23_14_slice17",
           "q23_15_slice17",
           "q23_16_slice17",
           "q23_17_slice17",
           "q23_1_slice18",
           "q23_2_slice18",
           "q23_3_slice18",
           "q23_4_slice18",
           "q23_5_slice18",
           "q23_6_slice18",
           "q23_7_slice18",
           "q23_8_slice18",
           "q23_9_slice18",
           "q23_10_slice18",
           "q23_11_slice18",
           "q23_12_slice18",
           "q23_13_slice18",
           "q23_14_slice18",
           "q23_15_slice18",
           "q23_16_slice18",
           "q23_17_slice18",
           "q23_1_slice19",
           "q23_2_slice19",
           "q23_3_slice19",
           "q23_4_slice19",
           "q23_5_slice19",
           "q23_6_slice19",
           "q23_7_slice19",
           "q23_8_slice19",
           "q23_9_slice19",
           "q23_10_slice19",
           "q23_11_slice19",
           "q23_12_slice19",
           "q23_13_slice19",
           "q23_14_slice19",
           "q23_15_slice19",
           "q23_16_slice19",
           "q23_17_slice19")

data[q23]
dz<-write.csv(data[q23],file="q23.csv")

d<-data[q1]
x<-array(0,dim=c(769,20))
for (i in 20) {
  x<-rbind(x,d[,i])
}

write.csv(d,file="q23_con.csv",row.names = FALSE)


q1<-data[q1]
write.csv(q1,file="q1.csv",row.names = FALSE)

q1b<-data[q1b]
write.csv(q1b,file="q1b.csv",row.names = FALSE)


#################

q1b<-read.csv("q1bvec2.csv", header = TRUE)
q1<-read.csv("q1vec.csv",header = TRUE)
q23<-read.csv("q23_conocedores.csv",header = TRUE)
q23m<-read.csv("q23b.csv",header = TRUE)
dat<-cbind(q1b,q1,q23)
table(subset(dat,Marca=="Solidario"))

s<-subset(da,Marca=="Solidario")

da<-subset(dat,q1b01==1)
str(da)
da<-dat
str(dat)
View(da)

write.csv(data$tipomuestra,file="tipomuestra.csv")
muestra<-rep(data$tipomuestra,19)
da2<-cbind(dat,muestra)

table(data$tipomuestra)


so<-(subset(da,Marca=="Pichincha"))
m<-array(0,dim = c(17,1))
for (i in 1:17){
m[i]<-mean(so[,i+3])}
mm<-mean(m)
m-mm



##############

pass.m1 <- glm(q101 ~ q23_con_brand1_slice01+
                 q23_con_brand1_slice02+
                 q23_con_brand1_slice03+
                 q23_con_brand1_slice04+
                 q23_con_brand1_slice05+
                 q23_con_brand1_slice06+
                 q23_con_brand1_slice07+
                 q23_con_brand1_slice08+
                 q23_con_brand1_slice09+
                 q23_con_brand1_slice10+
                 q23_con_brand1_slice11+
                 q23_con_brand1_slice12+
                 q23_con_brand1_slice13+
                 q23_con_brand1_slice14+
                 q23_con_brand1_slice15+
                 q23_con_brand1_slice16+
                 q23_con_brand1_slice17-1, data=da, family=binomial(link=logit))

pass.m1 <- glm(q101 ~ q23_con_brand1_slice01+
                 
                 q23_con_brand1_slice03+
                 q23_con_brand1_slice04+
                 q23_con_brand1_slice05+
                 q23_con_brand1_slice06+
                 q23_con_brand1_slice07+
                 
                 q23_con_brand1_slice09+
                
                
                 q23_con_brand1_slice02+
                 q23_con_brand1_slice11+
             
                 q23_con_brand1_slice14+
                 q23_con_brand1_slice15+
                 q23_con_brand1_slice16+
                 q23_con_brand1_slice17, data=da, family=binomial(link=logit))
summary(pass.m1)

pc<-prcomp(da[,-c(1:3,21)])
biplot(pc)


head(da)
colnames(da)<-c("Conocedores","tienetc","Marca",
            "Rápidez en avances",
            "Aceptada",
            "Débitos automáticos",
            "Permite pagar a fin de mes",
           "Transparencia",
           "Bajas tasas de interés",
            "Opciones al diferir",
           "Cashback",
           "Aceptada internacionalmente",
            "Ofertas y descuentos",
            "Prestigiosa",
            "Progresar",
            "Millas/recompensas",
            "Emergencia",
           "Servicio al cliente",
            "Confiable",
            "Miedo o recelo",
            "Otras"
)



#beta<-data.frame(b,dim = c(17,1))
beta<-cbind(b,m-mm)
rownames(beta)<-c(  "Rapidez en avances",
                    "Aceptada en todas partes",
                    "Débitos automáticos",
                    "Permite pagar a fin de mes",
                    "Transparencia",
                    "Bajas tasas de interés",
                    "Opciones al diferir",
                    "Cashback",
                    "Aceptada internacionalmente",
                    "Ofertas y descuentos",
                    "Prestigiosa",
                    "Permite progresar",
                    "Millas/recompensas",
                    "Útil en emergencia",
                    "Servicio al cliente",
                    "Confiable",
                    "Miedo o recelo")
#######################################
set.seed(100)
beta<-as.data.frame(beta)
ggplot(beta) +
  geom_point(aes(beta[,2], beta[,1]), size = 5, color = 'grey') +
  geom_label_repel(
    aes(beta[,2], beta[,1], fill = beta[,2], label = rownames(beta)),
    fontface = 'bold', color = 'white',
    box.padding = unit(0.25, "lines"),
    force = 2,
    point.padding = unit(0.5, "lines")
  ) +
  scale_x_continuous(labels=percent)+
 xlab("BIPs") + ylab("Importacia de atributos")+
   guides(fill=FALSE)+
  theme(axis.title.x=element_text(size=16, lineheight=.9, family="Times"))+
  theme_bw()





library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
            + col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
            + main="\n\n\n\n\nBrand attributes")



############################Muestreo igual base############################3
da2<-cbind(dat,muestra)
da3<-subset(da2,muestra=="Base")

so<-(subset(da3,Marca=="Diners"))
m<-array(0,dim = c(17,1))
for (i in 1:17){
  m[i]<-sum(so[,i+3],na.rm=TRUE)/dim(so)[1]}
mm<-mean(m)
m-mm

beta<-cbind(b,m-mm)
rownames(beta)<-c(  "Rapidez en avances",
                    "Aceptada en todas partes",
                    "Débitos automáticos",
                    "Permite pagar a fin de mes",
                    "Transparencia",
                    "Bajas tasas de interés",
                    "Opciones al diferir",
                    "Cashback",
                    "Aceptada internacionalmente",
                    "Ofertas y descuentos",
                    "Prestigiosa",
                    "Permite progresar",
                    "Millas/recompensas",
                    "Útil en emergencia",
                    "Servicio al cliente",
                    "Confiable",
                    "Miedo o recelo")

set.seed(16)
beta<-as.data.frame(beta)
ggplot(beta) +
  geom_point(aes(beta[,2], beta[,1]), size = 5, color = 'grey') +
  geom_label_repel(
    aes(beta[,2], beta[,1], fill = beta[,2], label = rownames(beta)),
    fontface = 'bold', color = 'white',
    box.padding = unit(0.25, "lines"),
    force = 1.5,
    point.padding = unit(0.5, "lines")
  ) +
  scale_x_continuous(labels=percent)+
  xlab("BIPs") + ylab("Importacia de atributos")+
  guides(fill=FALSE)+
  theme(axis.title.x=element_text(size=16, lineheight=.9, family="Times"))+
  theme_bw()

#########################################
q1b<-read.csv("q1bvec2.csv", header = TRUE)
q1<-read.csv("q1vec.csv",header = TRUE)
muestra<-rep(data$tipomuestra,19)
q23m<-read.csv("q23.csv",header = TRUE)
mercado<-cbind(q1b,q1,muestra,q23m)




m3<-subset(mercado,muestra=="Base")
m3<-subset(m3,q1b01==1)
so<-(subset(m3,Marca=="Produbanco"))
m<-array(0,dim = c(17,1))
std<-array(0,dim = c(17,1))
for (i in 1:17){
  std[i]<-sd(so[,i+4])
  m[i]<-sum(so[,i+4],na.rm=TRUE)/dim(so)[1]}
mm<-mean(m)
m-mm
View(m)
View(std)



beta<-cbind(b,m-mm)
rownames(beta)<-c(  "Rapidez en avances",
                    "Aceptada en todas partes",
                    "Débitos automáticos",
                    "Permite pagar a fin de mes",
                    "Transparencia",
                    "Bajas tasas de interés",
                    "Opciones al diferir",
                    "Cashback",
                    "Aceptada internacionalmente",
                    "Ofertas y descuentos",
                    "Prestigiosa",
                    "Permite progresar",
                    "Millas/recompensas",
                    "Útil en emergencia",
                    "Servicio al cliente",
                    "Confiable",
                    "Miedo o recelo")

set.seed(6)
beta<-as.data.frame(beta)
ggplot(beta) +
  geom_point(aes(beta[,2], beta[,1]), size = 5, color = 'grey') +
  geom_label_repel(
    aes(beta[,2], beta[,1], fill = beta[,2], label = rownames(beta)),
    fontface = 'bold', color = 'white',
    box.padding = unit(0.25, "lines"),
    force = 1.5,
    point.padding = unit(0.5, "lines")
  ) +
  scale_x_continuous(labels=percent)+
  xlab("BIPs") + ylab("Importacia de atributos")+
  guides(fill=FALSE)+
  theme(axis.title.x=element_text(size=16, lineheight=.9, family="Times"))+
  theme_bw()
#################################################
setwd("d:/Users_info/ALBANPD/My Documents/Bases")
datas<- read.spss("PERSONAS NSE.sav", to.data.frame=TRUE,use.value.labels = FALSE)

library("memisc", lib.loc="C:/Program Files/R/R-3.2.2/library")

data <- as.data.set(spss.system.file('VIVIENDA_HOGAR NSE.sav'))
