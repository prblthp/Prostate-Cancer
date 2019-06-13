library(VIM)
library(ggplot2)
library(questionr)
library(gower)
library(ResourceSelection)
library(pROC)
library(BootValidation)
library(StatMeasures)
library(pcaGoPromoter)
library(ellipse)
library(caret)
library(psych)
library(polycor)
library(randomForest)
library(mlbench)
library(pROC)
library(caret)
library(mlbench)
library(dplyr)
library(tidyr)
library(caret)
library(lattice)
imputedPRVA2R$bx_result<-as.factor(imputedPRVA2R$bx_result)
imputedPRVA2R<-imputedPRVA2

# create the age groups 
labels = c("45-65", "66-70","71-75","76-92")

age_group <- cut(imputedPRVA2R$age_bx, breaks = c(0,65,70,75, Inf),labels=labels)
famhx1 <- as.factor(imputedPRVA2R$famhx1)

dre_result <- as.factor(imputedPRVA2R$dre_result)
prior_neg <- as.factor(imputedPRVA2R$prior_neg)
race<- as.factor(imputedPRVA2R$race)
bx_result<-as.factor(imputedPRVA2R$bx_result)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 553.4")
psa_group <- cut(imputedPRVA2R$psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
#psa_group <- as.factor(imputedPRVA2R$psa_group)

impPRVA2RF<-data.frame(age_group,race,prior_neg,psa_group,dre_result,famhx1,bx_result)



set.seed(3456)
trainIndex <- createDataPartition(imputedPRVA2R$bx_result, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dtrain<-imputedPRVA2R[trainIndex,]
dtest<-imputedPRVA2R[-trainIndex,]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
)

# Data Prep for Ctree

set.seed(3456)
trainIndex <- createDataPartition(impPRVA2RF$bx_result, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dtrainRF<-impPRVA2RF[trainIndex,]
dtestRF<-impPRVA2RF[-trainIndex,]

fitControlRF <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
)



## Logistic regression
lreg<-caret::train(bx_result~age_bx+psa+dre_result+famhx1+
                     prior_neg+race,data=dtrain,method="glm",family=binomial(),
                   trControl=fitControl)
lreg
lreg$finalModel$confusion
plot(varImp(lreg))
lreg_pred<-predict(lreg,dtest)  #Accuracy= 0.6723
lreg_pred
##results
confusionMatrix(lreg_pred,dtest$bx_result)
TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(77, 20, 35, 35)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")


## Logistic regression with important variables

lreg1<-caret::train(bx_result~age_bx+psa+race+
                      prior_neg,data=dtrain,method="glm",family=binomial(),
                    trControl=fitControl)
lreg1
lreg_pred1<-predict(lreg1,dtest)    #Accuracy=0.6909
lreg_pred1
#lreg_pred1<-as.numeric(lreg_pred1)
dtest$bx_result<-as.numeric(dtest$bx_result)
levels(dtest$bx_result)<-list("1"="0","2"="1")
str(dtest[,-c(6,5)])
##results
confusionMatrix(lreg_pred1,dtest$bx_result)
TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(82, 15, 38, 32)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

## decision tree 
## decision tree 
dtree<-caret::train(bx_result~age_group+psa_group+dre_result+
                      prior_neg,data=dtrainRF,method="ctree",
                    trControl=fitControl)
dtree

lreg
dtree

plot(varImp(lreg))
varImp(dtree)
plot(varImp(dtree))
### predict on test dataset
lreg_pred<-predict(lreg,dtest)
dtree_pred<-predict(dtree,dtestRF)

##results
confusionMatrix(lreg_pred,dtest$bx_result)
TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(68, 29, 28, 42)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

confusionMatrix(dtree_pred,dtestRF$bx_result)
TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(74, 23, 33, 37)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")





# Recursive Feature Elimination
set.seed(7)
results_rfe <- rfe(x = dtrainRF[, -7], 
                   y = dtrainRF$bx_result, 
                   sizes = c(1:6), 
                   rfeControl = rfeControl(functions = rfFuncs, method = "cv", number = 10))
predictors(results_rfe)
results_rfe   # Accuracy with 4: 0.6695 with SD of 0.05506
plot(results_rfe, type=c("g", "o"))
dtrainRF_rfe <- dtrainRF[, c(which(colnames(dtrainRF) %in% predictors(results_rfe)),7)]
dtestRF_rfe<- dtestRF[, c(which(colnames(dtrainRF) %in% predictors(results_rfe)),7)]


lreg_rfe<-caret::train(bx_result~psa+
                         prior_neg+age_bx+dre_result,data=dtrain[,-c(2,6)],method="glm",family=binomial(),
                       trControl=fitControlRF)
lreg_rfe    #Accuracy: 0.68832



### predict on test dataset
lreg_predrfe<-predict(lreg_rfe,dtest[,-c(2,6)])

# Results

confusionMatrix(lreg_predrfe,dtest[,-c(2,6)]$bx_result) # Accuracy: 0.6324 #Sen:0.7705, Spec: 0.4493

TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(141, 42, 76, 62)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")



# CART based on important variables

dtree_rfe<-caret::train(bx_result~age_group+psa_group+dre_result+
                          prior_neg,data=dtrainRF_rfe,method="rpart",
                        trControl=fitControlRF)


### predict on test dataset
dtree_predrfe<-predict(dtree_rfe,dtestRF_rfe)

# Results

confusionMatrix(dtree_predrfe,dtestRF_rfe$bx_result) #Accuracy:0.6636, Sensi:0.7432, Spec:0.5580

TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(136, 47, 61, 77)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")



library(rpart)
library(rpart.plot)

set.seed(42)
fit <- rpart(bx_result ~ .,
             data = dtrainRF_rfe,
             method = "class",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)





#AutoGrid
set.seed(42)
model_rf_tune_auto <- caret::train(bx_result~., data= dtrainRF,
                                   method = "rf",
                                   preProcess = c("scale", "center"),
                                   trControl = trainControl(method = "repeatedcv", 
                                                            number = 10, 
                                                            repeats = 10, 
                                                            savePredictions = TRUE, 
                                                            verboseIter = FALSE,
                                                            search = "random"),
                                   tuneLength = 15)
model_rf_tune_auto
plot(model_rf_tune_auto)     #mtry=2, Accuracy= 0.6722

### predict on test dataset
rf_tune_pred<-predict(model_rf_tune_auto,dtestRF)

# Results

confusionMatrix(rf_tune_pred,dtestRF$bx_result)  #Accuracy: 0.6604, Sens: 0.7869, Spec: 0.4928

TrueClass <- factor(c(0, 0, 1, 1))
PredictedClass <- factor(c(0, 1, 0, 1))
Y      <- c(144, 39, 70, 68)
df <- data.frame(TrueClass, PredictedClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TrueClass, y = PredictedClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")


# C tree
ctree<-caret::train(bx_result~age_group+psa_group+dre_result+
                      prior_neg,data=dtrainRF,method="ctree",
                    trControl=fitControl)
ctree
dtestRF$bx_result<-as.numeric(dtestRF$bx_result)
ctree_pred<-predict(ctree,dtestRF)
ctree_pred

# Results

confusionMatrix(ctree_pred,dtestRF$bx_result)  #Accuracy: 0.6604, Sens: 0.7869, Spec: 0.4928

#ROC
library(pROC)
# Select a parameter setting
selectedIndices <- model_rf_tune_man$pred$mtry == 2
# Plot:
plot.roc(model_rf_tune_man$pred$obs[selectedIndices],
         model_rf_tune_man$pred$M[selectedIndices])

library(ggplot2)
library(plotROC)
ggplot(model_rf_tune_man$pred[selectedIndices, ], 
       aes(m = M, d = factor(obs, levels = c("R", "M")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


library(caret)
library(mlbench)
ctrl <- trainControl(method="cv", 
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
rfFit <- caret::train(bx_result~ ., data=imputedPRVA2R, 
                      method="rf", preProc=c("center", "scale"), 
                      trControl=ctrl)
library(pROC)
# Select a parameter setting
selectedIndices <- rfFit$pred$mtry == 2
# Plot:
plot.roc(rfFit$pred$obs[selectedIndices],
         rfFit$pred$M[selectedIndices])

# Factor Analysis
factanal(imputedPRVA2R[,c(-2,-7)],factors=1)



