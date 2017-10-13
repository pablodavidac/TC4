

Aux<-subset(DicAbr,select=c("Objetivo",
  "Max_Dias_sin_consumos_3M",
  "NormCupoUtilizado",
  "Cupo.Utilizado/Cupo.Aprobado",
  "Cupo.Ut.Avances/Cupo.Aprobado",
  "Dias.sin.consumo",
  "MesesRecencia",
  "Prom.DiasMora6",
  "Numero_tarjetas",
  "Otra_Tarjeta",
  "NumeroConsumos6M",
  "NumeroConsumoSinInteres6M",
  #NumeroConsumos12M+
  "NumeroConsumoCorriente12M",
  "SVidaDesgravamen",
  "STotalFamiliar",
  "MedioPagoVoucher12M",
  "Edad.Cuenta.Tc",
  "Max_Dias_sin_avances_3M",
  "Sueldo",
  "SoloSeguro",
  "TipoCliente",
  "T36.CupoUtilizado",
  "Sector",
  "CargasFamiliares"
  ))

write.csv(Aux,file="BaseScore.csv")



####################Descripcion de los P10 no desertores#######################

Aux<-subset(Diciembre,Decil=="P10" & Objetivo==0)
Aux2<-subset(Diciembre,Objetivo==1)
Aux<-rbind(Aux,Aux2)

#################Conjunto de entrenamiento y prueba######################################
Train2 <- createDataPartition(Aux$IdCuentaTarjeta, p=0.7, list=FALSE)
training2 <- Aux[ Train2, ]
testing2 <- Aux[ -Train2, ]
training2 %>% count(Objetivo)
testing2 %>% count(Objetivo)

####################Balancear el conjunto de entrenamiento#################################
muestra<-sample_n(subset(training2, Objetivo==0),758,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(training2, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-236]
########################Malos####################
n<-2
Malos<-do.call("rbind", replicate(n, subset(training2, Objetivo==1), simplify = FALSE))

Balanceada2<-rbind(Buenos,Malos)
Balanceada2 %>% count(Objetivo)
####################################################################################
mod2 <- glm(Objetivo~ 
              NormCupoUtilizado+
              #CodPromCupoUtilizado3M+
              #Max_Dias_sin_consumos_3M+
              #NormPromCupoUtilizado3M+
              #NormPromCupoUtili3MxTipoClienteActivo+
              #NormPromCupoUtili3MxTipoClienteInactivo+
              PagosVencidos+
              Otra_Tarjeta+
              SVidaDesgravamen+
              STotalFamiliar+
              SoloSeguro
            #+
            +TipoCliente
            #+PromMaxDiasSinconsumo3M
            #+Afinidad
            +Provincia
            
            #+`Cupo.Utilizado/Cupo.Aprobado`
            #+T3.CupoUtilizado
            #+T6.CupoUtilizado
            #+`Cupo.Ut.Avances/Cupo.Aprobado`
            #+NumeroConsumosAvance3M
            #+Dias.sin.consumo
            #+Prom.DiasMora3
            #+NumeroConsumoConInteres3M
            #+NumeroConsumoSinInteres3M
            #+NumeroConsumosSuperAvance12M
            #+TieneSuperAvance12M
            #+MontoSeguro
            #+EstadoCivil
            #+NivelInstruccion
            #+Sector
            #+Edad.Cliente
            #+CargasFamiliares
            +CodEstadoCivil
            #+SectorPersonal
            #+SectorComercio
            #+SectorDependencia
            #+SectorServicios
            #+SectorProduccion
            +TieneGold
            +TieneBlack
            +TienePlatinum
            
            #+Numero_tarjetasT6
            +CodT6CupoUtilizado
            
            # +CargasFamiliares
            
            +CupoUsadoAvancesHasta
            +Consume2omasAvances12M
            #+Instruccion
            #+HastaPrimaria
            #+HastaUniversidad
            #+TieneMenos300dSinConsumo
            #+TieneMenos300dSinConsumo
            #+TieneMenos150DiasSinConsumo
                    #+TieneMenos920DiasSinConsumo
            +Menos8Autorizaciones12M
            #+codCupoUti3MXDiasSinCon
            , data=Balanceada2,
            family="binomial"(link = "logit"))

summary(mod2)

View(varImp(mod1))


fitted.results <- predict(mod2,newdata=testingD,type='response')
pr <- prediction(fitted.results, testingD$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
str(fitted.results)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


###############Matriz de confución##############################
fitted.results <- predict(mod2,newdata=testingD,type='response')
fitted.results <- ifelse(fitted.results> 0.5,1,0)
confusionMatrix(testingD$Objetivo, fitted.results)

table(testing$Objetivo, fitted.results)


##############Prueba K-S####################################
fitted.results <- predict(mod2,newdata=testingD,type='response')
aux<-cbind(testingD,fitted.results)

sample1<-subset(aux,Objetivo==1)$fitted.results
sample2<-subset(aux,Objetivo==0)$fitted.results

cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2,na.rm = TRUE), max(sample1, sample2,na.rm = TRUE), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 
y1-y0

# png(file = "c:/temp/ks.png", width = 1024, height = 768, type="cairo-png")
ggplot(aux, aes(x = fitted.results, group = Objetivo, color = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 14) +
  theme(legend.position ="top") +
  xlab("Score") +
  ylab("Distribución acumulada") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=3) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=3) +
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  ggtitle("Prueba K-S") +
  annotate("text", x = .75, y = .25, 
           label = paste("K-S =", round(y1-y0, 3))) +
  theme(legend.title=element_blank())
########################################################

##################Distribuciones Acumuladas####################

################Distribución NormCupoUtilizado##################33
summary(DicAbr$NormCupoUtilizado)

ggplot(DicAbr2, aes(x = NormCupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado Normalizado", x="Cupo Utilizado (Normalizado)", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Distribución NormCupoUtilizado##################33
summary(DicAbr$NormCupoUtilizado)

ggplot(DicAbr2, aes(x = NormCupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado Normalizado", x="Cupo Utilizado (Normalizado)", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Distribución PagosVencidos##################33
summary(DicAbr$PagosVencidos)

ggplot(DicAbr2, aes(x = PagosVencidos)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Pagos Vencidos", x="Pagos", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Distribución Otra##################33
summary(DicAbr$Otra_Tarjeta)

ggplot(DicAbr2, aes(x = Otra_Tarjeta)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Otra tarjeta", x="", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Numero_tarjetasT6##################33
summary(DicAbr$Numero_tarjetasT6)

ggplot(DicAbr2, aes(x = Numero_tarjetasT6)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Número tarjetasT0 - Número tarjetasT6", x="Diferencia", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()


################Seguro##################33
summary(DicAbr$SVidaDesgravamen)

ggplot(DicAbr2, aes(x = SVidaDesgravamen)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Seguro de Desgravamen", x="", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Seguro PTF mensual##################33
summary(DicAbr$STotalFamiliar)

ggplot(DicAbr2, aes(x = STotalFamiliar)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Seguro Total Familiar mensual", x="", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Seguro PTF mensual##################33
summary(DicAbr$STotalFamiliar)

ggplot(DicAbr2, aes(x = STotalFamiliar)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Seguro Total Familiar mensual", x="", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()


################Solo consume seguro##################33
summary(DicAbr$SoloSeguro)

ggplot(DicAbr2, aes(x = SoloSeguro)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Solo consume seguro", x="", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()


################PromMaxDiasSinconsumo3M##################33
summary(DicAbr$PromMaxDiasSinconsumo3M)

ggplot(DicAbr2, aes(x = PromMaxDiasSinconsumo3M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Intervalo de Días más grande sin consumo en 3 meses", x="Días", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################Provincia##################33
summary(DicAbr$Provincia)

ggplot(DicAbr2, aes(x = Provincia)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Variable agrupada Provincia", x="Días", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()
################T6.CupoUtilizado##################33
summary(DicAbr$T6.CupoUtilizado)

ggplot(DicAbr2, aes(x = T6.CupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="CupoUtilizadot/t-6", x="Tasa t/t-6", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

################T6.CupoUtilizado##################33
summary(DicAbr$MesesRecencia)

ggplot(DicAbr2, aes(x = MesesRecencia)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Recencia", x="", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()

####################Deciles###########################
fitted.results <- predict(mod2,newdata=Diciembre,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Diciembre["Score"]<-predict(mod2,newdata=Diciembre,type='response')
Diciembre["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Diciembre[which(Diciembre$Score < quan[2]),]$Decil<-"P1"
Diciembre[which(Diciembre$Score <quan[3] & Diciembre$Score >= quan[2]),]$Decil<-"P2"
Diciembre[which(Diciembre$Score <quan[4] & Diciembre$Score >= quan[3]),]$Decil<-"P3"
Diciembre[which(Diciembre$Score <quan[5] & Diciembre$Score >= quan[4]),]$Decil<-"P4"
Diciembre[which(Diciembre$Score <quan[6] & Diciembre$Score >= quan[5]),]$Decil<-"P5"
Diciembre[which(Diciembre$Score <quan[7] & Diciembre$Score >= quan[6]),]$Decil<-"P6"
Diciembre[which(Diciembre$Score <quan[8] & Diciembre$Score >= quan[7]),]$Decil<-"P7"
Diciembre[which(Diciembre$Score <quan[9] & Diciembre$Score >= quan[8]),]$Decil<-"P8"
Diciembre[which(Diciembre$Score <quan[10] & Diciembre$Score >= quan[9]),]$Decil<-"P9"
Diciembre[which(Diciembre$Score >=quan[10]),]$Decil<-"P10"

View(table(Diciembre$Decil,Diciembre$Objetivo))













