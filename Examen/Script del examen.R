infarto<-read.csv("examen/infarto.csv",header=T,sep=",")
infarto$Actividad <- as.factor(infarto$Actividad)
infarto$Colesterol <- as.factor(infarto$Colesterol)

#verifico la disposicion de los niveles
levels(infarto$Actividad)
levels(infarto$Colesterol)

#reajusto los niveles
infarto$Actividad<-factor(infarto$Actividad,
                              levels=c("Baja","Adecuada","Alta"))
infarto$Colesterol<-factor(infarto$Colesterol,
                          levels=c("Bajo","Alto","Extremo"))

#Verifico el cambio
levels(infarto$Actividad)
levels(infarto$Colesterol)

#Ajusto el modelo de regresion
Examen.Multinom.Cuanli<-multinom(Colesterol~Edad+Peso+Actividad,
                                 data=infarto)
#resumen modelo resultante
summary(Examen.Multinom.Cuanli)

#cocientes de ventajas
exp(summary(Examen.Multinom.Cuanli)$coefficients)

#Indique el valor experimental del test de significación de la variable Edad 
#test de rzon de verosimilitudes

anova(multinom(Colesterol~Edad+Peso+Actividad,
               data=infarto),multinom(Colesterol~Peso+Actividad,
                                            data=infarto))

# Indique la probabilidad de nivel de colesterol alto que daría el modelo 
#estimado para una persona de 66 años, 77.4 kilos y con actividad Adecuada
predict(Examen.Multinom.Cuanli,type="probs")

#Indique la ventaja de colesterol bajo frente a alto que daría el modelo 
#estimado para una persona de 66 años, 77.4 kilos y con actividad Adecuada
a<- 1.980751+1.001269*66+ 0.9949607*77.4+ 0.8584229
a

#tabla de clasificacion
table(infarto$Colesterol,
      predict(Examen.Multinom.Cuanli,type="class"))

18/(1+18+17)

(18+31)/(1+18+17+30+31+13)

###################################

infarto2<-read.csv("examen/infarto2.csv",header=T,sep=",")
infarto2$Actividad <- as.factor(infarto2$Actividad)
infarto2$Colesterol <- as.factor(infarto2$Colesterol)

#verifico la disposicion de los niveles
levels(infarto2$Actividad)
levels(infarto2$Colesterol)

#reajusto los niveles
infarto2$Actividad<-factor(infarto2$Actividad,
                          levels=c("Baja","Adecuada","Alta"))
infarto2$Colesterol<-factor(infarto2$Colesterol,
                           levels=c("Bajo","Alto","Extremo"))
#verifico la disposicion de los niveles
levels(infarto2$Actividad)
levels(infarto2$Colesterol)


#Ajusto el modelo de regresion
Examen.Ordinal.Cuanli<-polr(Colesterol~Edad+Peso+Actividad,
                            data=infarto2)

#resumen modelo resultante
summary(Examen.Ordinal.Cuanli)
summary(Examen.Ordinal.Cuanli)$coefficients

#ventajas

exp(-summary(Examen.Ordinal.Cuanli)$coefficients
    [c("Edad","Peso"),"Value"])

exp(-summary(Examen.Ordinal.Cuanli)$coefficients
    [c("Edad","Peso","ActividadAdecuada"),"Value"])

#Indique el valor experimental del test de significación de la variable Edad
anova(polr(Colesterol~Edad+Peso+Actividad,
                data=infarto2),polr(Colesterol~Peso+Actividad,
                                    data=infarto2))

#Indique la probabilidad de nivel de colesterol alto que daría el modelo estimado 
#para una persona de 44 años, 92.36 kilos y con actividad Alta

predict(Examen.Ordinal.Cuanli,type="probs")[14,]

#Indique la ventaja de colesterol bajo frente a alto que daría el modelo estimado 
#para una persona de 44 años, 92.36 kilos y con actividad Alta
b<- -3.786510657 0.004188498*44 -0.029441841*92.36 - 0.377215681
b

#Indique el porcentaje de casos correctamente clasificados por el modelo de entre 
#los que presentan un nivel de colesterol alto

table(infarto2$Colesterol,
      predict(Examen.Ordinal.Cuanli,type="class"))

(8+48)/(8+48+19+22+6+7)
