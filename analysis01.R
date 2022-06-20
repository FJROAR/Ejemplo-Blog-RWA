#url: https://www.kaggle.com/datasets/uciml/default-of-credit-card-clients-dataset

library(ranger)
library(data.table)
library(dplyr)
library(ROCR)

path = "data/"
file = "UCI_Credit_Card.csv"

df = fread(paste0(path, file))

#Se cambia el nombre de las variables a algo adecuado
names_original = names(df)
names_new = names_original
names_new[length(names_original)] = 'target' 
names(df) = names_new

#Se va a hacer algo simple ya que no hay cosas demasiado raras en las variables
#aunque en un proyecto real habría que hacer un EDA en condiciones al respecto

#Train - test split

df_0 = df[which(df$target == 0),]
df_1 = df[which(df$target == 1),]

set.seed(123)
train_index0 <- sample(0.70 * nrow(df_0))
test_index0 <- -train_index0
train_index1 <- sample(0.70 * nrow(df_1))
test_index1 <- -train_index1

df_train <- rbind(df_0[train_index0],
                  df_1[train_index1])

df_test <- rbind(df_0[test_index0],
                  df_1[test_index1])


tasa = nrow(df_1)/(nrow(df_1) + nrow(df_0))
#Calibrado "grosero" a una media de una PD de un 1%

tasac = 0.01/tasa





#Variables training-test. Se podrían crear nuevas variables, pero no se hace para acelerar

v_train <- names(df_train)[c(-1)]
v_test <- names(df_test)[c(-1)]

df_trainf <- df_train %>% select(-ID)
df_testf <- df_test %>% select(-ID)

#Modelo sencillo 1:

mod01 <- glm(target ~., data = df_trainf, family = 'binomial')

summary(mod01)

pred01 <- predict.glm(mod01, df_testf, type = 'response')

pred <- prediction(pred01, df_testf$target)
perf <- performance(pred, measure="tpr",x.measure="fpr")

plot(perf,colorize=TRUE,type="l") 
abline(a=0,b=1)

# Área bajo la curva
AUC1       <- performance(pred,measure="auc")
AUCaltura1 <- AUC1@y.values
AUC1
AUCaltura1

#Se toman las predicciones del modelo
pred_fin01 <-  data.frame(df_test) 
pred_fin01 <- pred_fin01 %>%  mutate(prediction = tasac * as.numeric(pred01)) 


#Modelo avanzado 2:

mod02 <- ranger(target ~., data = df_trainf,
                num.trees = 50,
                mtry = 6,
                seed = 123)

pred02 <- predict(mod02, df_testf)
pred02 <- pred02$predictions

pred <- prediction(pred02, df_testf$target)
perf <- performance(pred, measure="tpr",x.measure="fpr")

plot(perf,colorize=TRUE,type="l") 
abline(a=0,b=1)

# Área bajo la curva
AUC2       <- performance(pred,measure="auc")
AUCaltura2 <- AUC2@y.values
AUC2
AUCaltura2

#Se toman las predicciones del modelo
pred_fin02 <-  data.frame(df_test) 
pred_fin02 <- pred_fin02 %>%  mutate(prediction = tasac * as.numeric(pred02)) 



#Se establece un punto de corte al nivel del 1%

df_corte1 <- pred_fin01[which(pred_fin01$prediction <= 0.01),]
df_corte2 <- pred_fin02[which(pred_fin02$prediction <= 0.01),]


#Cálculo de capital para el modelo 1

pd = df_corte1$prediction
lgd = 0.45
R = 0.15

rw = (lgd * pnorm((1/(1-R)**0.5)*qnorm(pd) + 
                    ((R/(1-R))**0.5)*qnorm(0.999)) - 
        lgd * pd) * 12.5 * 1.06

capital01 = sum(rw)
capital01*150000


#Cálculo de capital para el modelo 2

pd = df_corte2$prediction
lgd = 0.45
R = 0.15

rw = (lgd * pnorm((1/(1-R)**0.5)*qnorm(pd) + 
                    ((R/(1-R))**0.5)*qnorm(0.999)) - 
        lgd * pd) * 12.5 * 1.06

capital02 = sum(rw)
capital02*150000

#Modelo estándard
capitalEstandard1 = 0.35*nrow(df_corte1)
capitalEstandard2 = 0.35*nrow(df_corte2)

capitalEstandard1 * 150000
capitalEstandard2 * 150000