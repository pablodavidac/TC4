library("stats", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
require(caret)
require(ROCR)

#################Regresión lineal#####################
mod1 <- glm(Objetivo ~ 
              CupoUtilizado+
              #CupoUtilizadoNormal+
              #Prom.CupoUtilizado3+
              `Cupo.Utilizado/Cupo.Aprobado`+
              #`Cupo.Ut.Avances/Cupo.Aprobado`+
              NumeroConsumosAvance12M+
              #T3.CupoUtilizado+
              T6.CupoUtilizado+
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
            , data=training, family="binomial")


##################Ver coeficientes de la regresión#############
summary(mod1)


#####################Ver importacia de las variables################
View(varImp(mod1))

######################Predición con el conjunto de prueba###########
fitted.results <- predict(mod1,newdata=testing,type='response')

Aux2<-cbind(testing,fitted.results)

########################Definir punto de corte####################

fitted.results <- ifelse(fitted.results> 0.036,1,0)


#######################Curva ROC###############
pr <- prediction(fitted.results, testing$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
####################AUC#####################
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###############Matriz de confución##############################
confusionMatrix(testing$Objetivo, fitted.results)