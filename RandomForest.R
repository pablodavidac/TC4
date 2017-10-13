#################Ramdon forest##################

ActivosDic<-subset(BaseDic,TipoCliente=="ACTIVO")


ActivosDic$T3.CupoUtilizado[which(!is.finite(ActivosDic$T3.CupoUtilizado))] <- 0

ActivosDic$T6.CupoUtilizado[which(!is.finite(ActivosDic$T6.CupoUtilizado))] <- 0

ActivosDic$T36.CupoUtilizado[which(!is.finite(ActivosDic$T36.CupoUtilizado))] <- 0

ActivosDic$`CupoUtilizado/Cupo.Apro.Normal`[which(!is.finite(ActivosDic$`CupoUtilizado/Cupo.Apro.Normal`))] <- 0

ActivosDic$`Cupo.Ut.Avances/Cupo.Aprobado`[which(!is.finite(ActivosDic$`Cupo.Ut.Avances/Cupo.Aprobado`))] <- 0

ActivosDic$Meses.sin.consumo[which(!is.finite(ActivosDic$Meses.sin.consumo))]<-mean(ActivosDic$Meses.sin.consumo,na.rm=TRUE)

ActivosDic$Edad.Cliente[which(!is.finite(ActivosDic$Edad.Cliente))]<-mean(ActivosDic$Edad.Cliente,na.rm=TRUE)

ActivosDic$CargasFamiliares[which(!is.finite(ActivosDic$CargasFamiliares))]<-mean(ActivosDic$CargasFamiliares,na.rm=TRUE)

ActivosDic$Edad.Cuenta.Tc[which(!is.finite(ActivosDic$Edad.Cuenta.Tc))]<-mean(ActivosDic$Edad.Cuenta.Tc,na.rm=TRUE)






######################Sancar NA e Inf##############
Aux<-subset(ActivosDic,
            #is.finite(Meses.sin.consumo) &
              #is.finite(Edad.Cliente) &
              #is.finite(CargasFamiliares) &
              #is.finite(Edad.Cuenta.Tc)&
              is.finite(ActivosDic$T3.CupoUtilizado)
              #is.finite(Activos$T6.CupoUtilizado)&
              #is.finite(Activos$T36.CupoUtilizado)&
             #is.finite(Activos$`CupoUtilizado/Cupo.Apro.Normal`)&
              #is.finite(Activos$`Cupo.Ut.Avances/Cupo.Aprobado`)
            
)

#############Conjunto entrenamiento y prueba##############
Train <- createDataPartition(Aux$IdCuentaTarjeta, p=0.7, list=FALSE)
training <- Aux[ Train, ]
testing <- Aux[ -Train, ]


##################Ramdon fores####################
library("party", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
set.seed(42)
crf<-cforest(as.factor(Objetivo)~
               CupoUtilizado+
               CupoAprobado+
               CupoUtilizadoAvance+
               CupoUtilizadoAvance+
               PagosVencidos+
               DiasMora+
               T1.CupoAprobado+
               T1.CupoUtilizado+
               Prom.CupoUtilizado3+
               Prom.CupoUtilizado6+
               Prom.CupoUtilizadoAvance3+
               Prom.CupoUtilizadoAvance+
               Prom.DiasMora3+
               Prom.DiasMora6+
               Prom.DiasMora+
               `Cupo.Ut.Avances/Cupo.Aprobado`+
               `Cupo.Utilizado/Cupo.Aprobado`+
               Dias.sin.consumo+
               Max_Dias_sin_consumos_3M+
               Max_Dias_sin_consumos_6M+
               Max_Dias_sin_consumos_12M+
               Max_Dias_sin_avances_12M+
               NumeroConsumos3M+
               NumeroConsumos6M+
               NumeroConsumos12M+
               NumeroConsumosAvance12M+
               MedioPagoPOS12M+
               MedioPagoVoucher12M+
               Numero_tarjetas+
               Autorizacion12M+
               NumeroConsumoCorriente3M+
               NumeroConsumoCorriente6M+
               NumeroConsumoSinInteres3M+
               NumeroConsumoSinInteres6M+
               NumeroConsumoSinInteres12M+
               #SVidaDesgravamen+
               #STotalFamiliar+
               #EstadoCivil+
               #TieneSeguro+
               #AfinidadTarjeta+
               #Numero_tarjetas_3M+
               #Numero_tarjetas_6M+
               #Otra_Tarjeta+
               #Edad.Cuenta.Tc+
               #Asalariado+
               #NuevaTc3M+
             #NuevaTc6M+
             Edad.Cliente+
               MesesRecencia+
               Sueldo+
               TienePlan+
               #SoloPlan+
               SoloSeguro+
               SoloPlanSeguro
             ,control = cforest_unbiased(mtry = 1, ntree = 3), data=Balanceada)

set.seed(42)
crf<-cforest(Objetivo~
               CupoUtilizado+
               #CupoUtilizadoNormal+
               #Prom.CupoUtilizado3+
               `Cupo.Utilizado/Cupo.Aprobado`+
               #`Cupo.Ut.Avances/Cupo.Aprobado`+
               NumeroConsumosAvance12M+
               #T3.CupoUtilizado+
               #T6.CupoUtilizado+
               #T36.CupoUtilizado+
               #Dias.sin.consumo+
               Prom.DiasMora3+
               #NumeroConsumoCorriente3M+
               NumeroConsumoSinInteres6M+
               #NumeroConsumoConInteres3M+
               #NumeroConsumosSuperAvance12M+
               Max_Dias_sin_consumos_3M+
               Numero_tarjetas+
               Edad.Cliente+
               #CargasFamiliares+
               Edad.Cuenta.Tc+
               #Numero_tarjetasT6
               #EstadoCivil+
               #ProvinciaDomicilio+
               #NivelInstruccion+
               #Sector+
               #TienePlan+
               #SoloPlan+
               SoloSeguro
             #SoloPlanSeguro
               
               ,control = cforest_unbiased(mtry = 1, ntree = 3), data=Balanceada)


c<-varimp(crf)
write.csv(c,file="importancia.csv")
#varImpPlot(crf,type=2)

fitted.results <- predict(crf, newdata = testing, type = "response")

fitted.results <- predict(mod1,newdata=testing,type='response')

Aux2<-cbind(testing,fitted.results)

mean(subset(Aux2,Objetivo==1)$fitted.results,na.rm=TRUE)

fitted.results <- ifelse(fitted.results> 0.5,1,0)


#testingb$Objetivo<-gsub(0,2,testingb$Objetivo)
#testingb$Objetivo<-gsub(1,0,testingb$Objetivo)
#testingb$Objetivo<-gsub(2,1,testingb$Objetivo)


pr <- prediction(fitted.results, testing$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###############Matriz de confución##############################

confusionMatrix(testing$Objetivo, fitted.results)



##################

library("randomForest", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
Balanceada$Objetivo<-as.factor(Balanceada$Objetivo)
fit<-randomForest(as.factor(Objetivo)~
                    CupoUtilizado+
                    CupoUtilizadoNormal+
                    #Prom.CupoUtilizado3+
                    #`Cupo.Utilizado/Cupo.Aprobado`+
                    #`Cupo.Ut.Avances/Cupo.Aprobado`+
                    NumeroConsumosAvance12M+
                    T3.CupoUtilizado+
                    T6.CupoUtilizado+
                    T36.CupoUtilizado+
                    Dias.sin.consumo+
                    Prom.DiasMora3+
                    NumeroConsumoCorriente3M+
                    NumeroConsumoSinInteres6M+
                    NumeroConsumoConInteres3M+
                    NumeroConsumosSuperAvance12M+
                    Max_Dias_sin_consumos_3M+
                    Numero_tarjetas+
                    Edad.Cliente+
                    CargasFamiliares+
                    Edad.Cuenta.Tc+
                    Numero_tarjetasT6+
                    #EstadoCivil+
                    #ProvinciaDomicilio+
                    #NivelInstruccion+
                    Sector+
                    TienePlan+
                    SoloPlan+
                    SoloSeguro+
                  SoloPlanSeguro
                  , data=Balanceada,
                  na.action = na.omit,
                  importance=TRUE, 
                  ntree=100)
View(varImp(fit))
varImpPlot(fit)

Prediction <- predict(fit, testing)

confusionMatrix(testing$Objetivo, Prediction)


Desertores<-read.csv("DesertoresEne-Ago.csv",header = TRUE,sep = ",")



library(plyr)
library(psych)
multi.hist(mpg) #error, not numeric
multi.hist(mpg[,sapply(mpg, is.numeric)])


library(reshape2)
Aux2<-subset(Aux,Objetivo==1)
mpgid <- mutate(Aux2, id=as.numeric(rownames(Aux2)))
mpgstack <- melt(mpgid, id="id")
pp <- qplot(value, data=mpgstack) + facet_wrap(~variable, scales="free")
# pp + stat_bin(geom="text", aes(label=..count.., vjust=-1))
ggsave("Dhistograms2.pdf", pp, scale=2)
