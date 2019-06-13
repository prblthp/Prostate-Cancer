library(readxl)
library(ggplot2)
library(reshape)
library(XML)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggalt)
library(ggfortify)
library(ggthemes)
library(ggExtra)
library(VIM)
library(mice)
library(data.table)
library(naniar)
my_data<- read_excel("PBCG.xlsx")
#Taking out the irrelevant columns 
my_data_rel<- data.frame(my_data$mrn, my_data$pca, my_data$age_bx, my_data$race, my_data$priorbx_neg, my_data$psa, my_data$dre_result, my_data$famhx1, my_data$bx_result, my_data$bx_ggs_cat)

my_data_rel$my_data.famhx1 <- as.factor(my_data_rel$my_data.famhx1)
my_data_rel$my_data.dre_result <- as.factor(my_data_rel$my_data.dre_result)
my_data_rel$my_data.priorbx_neg <- as.factor(my_data_rel$my_data.priorbx_neg)
#my_data_rel$my_data.race<- as.factor(my_data_rel$my_data.race)
my_data_rel$my_data.race <- as.factor(ifelse(my_data_rel$my_data.race %in% c("American Indian or Alaska Native","Asian",
                                                                             "Asian-American or Pacific Islander","Asian or Pacific Islander", 
                                                                             "Multiracial/Multicultural","Native Hawaiian or Other Pacific Islander", 
                                                                             "Other"), "Others",{ifelse(my_data_rel$my_data.race%in% c("Black or African American"),
                                                                                                        "Black or African American",{ifelse(my_data_rel$my_data.race%in% c("White"),"White",NA)})}))

#hist(my_data_rel$my_data.race)
sum(is.na(my_data_rel$my_data.pca))
sum(is.na(my_data_rel$my_data.famhx1))

sum(is.na(my_data_rel$my_data.bx_ggs_cat))

# Load data in VIM
data(my_data_rel, package = "VIM")

## Show complete cases
my_data_rel[complete.cases(my_data_rel),]

#md.pattern(my_data_rel)

## Visualizing in number
aggr(my_data_rel, prop = F, numbers = T)

# Visualizing in proportion
aggr(my_data_rel, prop = T, numbers = T)

## Matrix plot. Red for missing values, Darker values are high values.
matrixplot(my_data_rel, interactive = F, sortby = "my_data.bx_ggs_cat")

#library(data.table)
#data_clev <- my_data_rel[my_data_rel$my_data.mrn %like% "CCF",]

library(VIM)
library(mice)
library(gridExtra)
# 1. Cleveland Data
data_clev <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
data_clev1 <- data_clev[,c(-1,-2,-10,-11,-12)]
colnames(data_clev1)[1:7]<-c("age_bx","race","prior_neg","psa","dre_result","famhx1","bx_result")
data(data_clev1, package = "VIM")
aggr(data_clev1, prop = F, numbers = T)
md.pattern(data_clev1)
matrixplot(data_clev1, interactive = F, sortby = "bx_result", main="Cleveland Missing Data")
scattmatrixMiss(data_clev1, interactive = F)
marginplot(data_clev1[,c("race","prior_neg")],main=race vs prior_neg)
                         spineMiss(data_clev1)
                         data_clev1MP<-data_clev1[,c("age_bx","famhx1")]
                         
                         marginplot(data_clev1MP, main="age vs famhx1")
                         marginmatrix(data_clev1)
                         spineMiss(data_clev1[, c("famhx1", "prior_neg")])
                         
                         colSums(is.na(data_clev1))
                         # Histogram for each characteristics
                         data_clev[is.na(data_clev)]<-" NA"
                         
                         p1 <- ggplot(data=data_clev, aes(data_clev$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()
                         p1
                         +geom_text(label = ..count../sum(..count..), group =my_data.bx_result,
                         color = "white")
                         #+scale_fill_discrete(name ="DRE",labels = c("0 negative DRE", "1 positive DRE"))#+facet_grid(~my_data.bx_result) 
                         
                         p2 <- ggplot(data=data_clev, aes(data_clev$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar()
                         p2
                         p3 <- ggplot(data = data_clev,
                         mapping = aes(x = my_data.priorbx_neg, fill=my_data.bx_result))
                         p3 + geom_bar(position = "dodge",
                         mapping = aes(y = ..prop.., group = my_data.bx_result )) 
                         #facet_wrap(~my_data.bx_result, ncol = 1)
                         
                         p4 <- ggplot(data=data_clev, aes(data_clev$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
                         p4
                         
                         # Change the characteritics 
                         # create the age groups 
                         labels = c("55-65", "66-70","71-75","76-93")
                         
                         data_clev$my_data.age_group <- cut(data_clev$my_data.age, breaks = c(0,65,70,75, Inf),labels=labels)
                         
                         # create PSA groups
                         # max(baseline_select$PSA) #2000
                         labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
                         data_clev$my_data.psa_group <- cut(data_clev$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
                         data_clev$my_data.psa_group <- as.factor(data_clev$my_data.psa_group)
                         grid.arrange(
                         ggplot(data=data_clev, aes(data_clev$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar(),
                         ggplot(data=data_clev, aes(data_clev$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar(),
                         ggplot(data=data_clev, aes(data_clev$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar(),
                         ncol=3
                         )  
                         
                         aggr(data_clev, prop = F, numbers = T)
                         
                         matrixplot(data_clev, interactive = F, sortby = "my_data.bx_ggs_cat")
                         
                         
                         
                         
                         
                         
                         # 5.Puerto Rico
                         data_PRVA <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_PRVA<- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_PRVAU<- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_PRVAUC<- data_PRVAU[complete.cases(data_PRVAU),complete.cases(t(data_PRVAU))]
                         
                         
                         #data_PRVA %>% replace_with_na_all(condition = ~.x %in% c("NA"))
                         #data_PRVAU %>% replace(.,"NA",is.na(.))
                         
                         #data(data_PRVA, package = "VIM")
                         #aggr(data_PRVA, prop = F, numbers = T)
                         #matrixplot(data_PRVA, interactive = F, sortby = "my_data.bx_ggs_cat")
                         
                         # Change the characteritics 
                         # create the age groups 
                         #PSmin<-data_PRVA[-c(8923),]
                         labels = c("45-65", "66-70","71-75","76-92")
                         
                         data_PRVAU$my_data.age_group <- cut(data_PRVAU$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)
                         data_PRVAU$my_data.age_group <- as.factor(data_PRVAU$my_data.age_group)
                         
                         # create PSA groups
                         # max(baseline_select$PSA) #5000 custard
                         labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 553.4")
                         data_PRVAU$my_data.psa_group <- cut(data_PRVAU$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
                         data_PRVAU$my_data.psa_group <- as.factor(data_PRVAU$my_data.psa_group)
                         
                         #colMax <- function(data,column) sapply(data$column, max, na.rm = TRUE)
                         
                         #data_PRVAU[is.na(data_PRVAU)]<-" NA"
                         
                         p31 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="Puerto Rico (PRVA)",fill="bx_result",x="DRE_result",y="No. of cases")
                         p31
                         p32 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.pca, fill=factor(my_data.bx_result)))+
                         geom_bar()+labs(title="",fill="bx_result",x="pca",y="No. of cases")
                         p32
                         p33 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar()+ 
                         labs(title="",fill="bx_result",x="priorbx_neg",y="No. of cases")
                         p33
                         p34 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="age_group",y="No. of cases")
                         p34
                         p35 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="psa_group",y="No. of cases")
                         p35
                         p32 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.race, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="race",y="No. of cases")
                         p32
                         p36 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="famhx1",y="No. of cases")
                         p36
                         
                         grid.arrange(p31,p32,p33,p34,p35,p36,ncol=2,top="Puerto Rico (PRVA) with NA Cases")
                         
                         
                         #Complete data analysis
                         
                         data_PRVAU<- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_PRVAUC<- data_PRVAU[complete.cases(data_PRVAU),]
                         
                         #labels = c("21-65", "66-70","71-75","76-91")
                         
                         #data_PRVAUC$my_data.age_group <- cut(data_PRVAUC$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)
                         
                         # create PSA groups
                         # max(baseline_select$PSA) #5000 custard
                         #labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 553.4")
                         #data_PRVAUC$my_data.psa_group <- cut(data_PRVAUC$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
                         #data_PRVAUC$my_data.psa_group <- as.factor(data_PRVAUC$my_data.psa_group)
                         
                         #colMax <- function(data,column) sapply(data$column, max, na.rm = TRUE)
                         
                         #data_PRVAUC[is.na(data_PRVAUC)]<-" NA"
                         
                         p31 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="DRE_result",y="No. of cases")
                         p31
                         p32 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.pca, fill=factor(my_data.bx_result)))+
                         geom_bar()+labs(title="",fill="bx_result",x="pca",y="No. of cases")
                         p32
                         p33 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar()+ 
                         labs(title="",fill="bx_result",x="priorbx_neg",y="No. of cases")
                         p33
                         p34 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="age_group",y="No. of cases")
                         p34
                         p35 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="psa_group",y="No. of cases")
                         p35
                         p32 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.race, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="race",y="No. of cases")
                         p32
                         p36 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar()+
                         labs(title="",fill="bx_result",x="famhx1",y="No. of cases")
                         p36
                         
                         grid.arrange(p31,p32,p33,p34,p35,p36,ncol=2,top="Puerto Rico (PRVA) Complete Cases")
                         
                         # Chi Square Test
                         chisqrace <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.race)
                         chisqrace
                         chisqage_group <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.age_group)
                         chisqage_group
                         chisqpsa_group <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.psa_group)
                         chisqpsa_group
                         chisqfamhx1 <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.famhx1)
                         chisqfamhx1
                         chisqdre_result <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.dre_result)
                         chisqdre_result
                         chisqpriorbx_neg <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.priorbx_neg)
                         chisqpriorbx_neg
                         
                         
                         data(data_PRVA, package = "VIM")
                         aggr(data_PRVA, prop = F, numbers = T)
                         matrixplot(data_PRVA, interactive = F, sortby = "my_data.bx_ggs_cat")
                         
                         # Change the characteritics 
                         # create the age groups 
                         labels = c("55-65", "66-70","71-75","76-93")
                         
                         data_PRVA$my_data.age_group <- cut(data_PRVA$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)
                         
                         # create PSA groups
                         # max(baseline_select$PSA) #2000
                         labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
                         data_PRVA$my_data.psa_group <- cut(data_PRVA$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
                         data_PRVA$my_data.psa_group <- as.factor(data_PRVA$my_data.psa_group)
                         
                         data_PRVA[is.na(data_PRVA)]<-" NA"
                         
                         p51 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
                         p51
                         p52 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
                         p52
                         p53 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
                         p53
                         p54 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
                         p54
                         p55 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
                         p55
                         p56 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
                         p56
                         
                         
                         library("ggplot2")
                         library("pROC")
                         # Function for plotting the ROC curve and calculating the AUC
                         roc_plot <- function(response, prob, data){
                         # calculating the specificity and sensitivity 
                         ROC <- roc(response ~prob, data=)
                         x <- 1-ROC$specificities
                         y<- ROC$sensitivities
                         # getting the AUC for the model
                         AUC <- as.data.frame(ROC$auc)
                         values <- as.data.frame(cbind(y,x))
                         # plotting
                         ggplot(values, aes(y=y, x=x))+
                         geom_smooth()+
                         xlab("1-Specificity")+ 
                         ylab("Sensitivity") +
                         theme_minimal() +
                         geom_text(x = 0.5, y = 0.2, label=paste("Area under the curve:",round(AUC,2)),size =6)
                         }
                         ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probCC,data_MC1)
                         ROC_plot_MC1
                         
                         ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probCC,data_SRF1)
                         ROC_plot_SRF1
                         
                         ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probCC,data_PRVA1)
                         ROC_plot_PRVA1
                         
                         ROC_plot_MSK1<- roc_plot(data_MSK1$my_data.bx_result,data_MSK1$probCC,data_MSK1)
                         ROC_plot_MSK1
                         
                         ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probCC,data_SBC1)
                         ROC_plot_SBC1
                         
                         ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probCC,data_ZUR1)
                         ROC_plot_ZUR1
                         
                         
                         # ROC with imputed model probabilities
                         ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probCCMean,data_MC1)
                         ROC_plot_MC1I
                         
                         ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probCCMean,data_SRF1)
                         ROC_plot_SRF1I
                         
                         ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probCCMean,data_PRVA1)
                         ROC_plot_PRVA1I
                         
                         ROC_plot_MSK1I<- roc_plot(data_MSK1$my_data.bx_result,data_MSK1$probCCMean,data_MSK1)
                         ROC_plot_MSK1I
                         
                         ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probCCMean,data_SBC1)
                         ROC_plot_SBC1I
                         
                         ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probCCMean,data_ZUR1)
                         ROC_plot_ZUR1I
                         
                         # MSK Imputation
                         # MICE for cleveland imputation
                         #clevI- data for imputation
                         #sapply(data_DVA, function(x) sum(is.na(x)))
                         
                         data_clev1 <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
                         data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
                         data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
                         data_PRVA1 <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_MSKI <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
                         data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
                         data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]
                         
                         unique(data_clevI$my_data.race)
                         
                         data_MSKI<-data_MSKI[,-10]
                         colSums(is.na(data_MSKI))
                         
                         library(mice)
                         init = mice(data_MSKI, maxit=0) 
                         meth = init$method
                         predM = init$predictorMatrix
                         
                         meth[c("my_data.mrn")]=""
                         meth[c("my_data.pca")]=""
                         #meth[c("my_data.bx_ggs_cat")]="" 
                         meth[c("my_data.psa")]="" 
                         meth[c("my_data.dre_result")]="logreg" 
                         meth[c("my_data.age_bx")]="" 
                         meth[c("my_data.famhx1")]="" 
                         meth[c("my_data.priorbx_neg")]="" 
                         meth[c("my_data.race")]="polyreg" 
                         meth[c("my_data.bx_result")]="" 
                         
                         set.seed(103)
                         imputedMSK = mice(data_MSKI, method=meth, predictorMatrix=predM, m=5)
                         
                         imputedMSK1 <- complete(imputedMSK)
                         save(file="imputed2.Rda")
                         
                         sapply(imputed2, function(x) sum(is.na(x)))
                         #load("imputed2.Rda")
                         
                         #Normal MSK Data Model
                         
                         model_MSK<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=data_MSK1,family = binomial)
                         summary(model_MSK)
                         
                         # Imputed Data Model 
                         model_MSKimp<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=imputedMSK,family = binomial)
                         summary(model_MSKimp)
                         
                         #prob with original model
                         data_MC1$probMSK <- predict(model_MSK,newdata=data_MC1, type="response")
                         data_SRF1$proMSK <- predict(model_MSK,newdata=data_SRF1, type="response")
                         data_PRVA1$proMSK <- predict(model_MSK,newdata=data_PRVA1, type="response")
                         data_clev1$probMSK <- predict(model_MSK,newdata=data_clev1, type="response")
                         data_SBC1$probMSK <- predict(model_MSK,newdata=data_SBC1, type="response")
                         data_ZUR1$probMSK <- predict(model_MSK,newdata=data_ZUR1, type="response")
                         
                         #Probabilities with imputed model
                         data_MC1$probMSK1 <- predict(model_MSKimp,newdata=data_MC1, type="response")
                         data_SRF1$probMSK1 <- predict(model_MSKimp,newdata=data_SRF1, type="response")
                         data_PRVA1$probMSK1 <- predict(model_MSKimp,newdata=data_PRVA1, type="response")
                         data_clev1$probMSK1 <- predict(model_MSKimp,newdata=data_clev1, type="response")
                         data_SBC1$probMSK1 <- predict(model_MSKimp,newdata=data_SBC1, type="response")
                         data_ZUR1$probMSK1 <- predict(model_MSKimp,newdata=data_ZUR1, type="response")
                         
                         
                         ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probMSK,data_MC1)
                         ROC_plot_MC1
                         
                         ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probMSK,data_SRF1)
                         ROC_plot_SRF1
                         
                         ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probMSK,data_PRVA1)
                         ROC_plot_PRVA1
                         
                         ROC_plot_clev1<- roc_plot(data_clev$my_data.bx_result,data_clev1$probMSK,data_clev1)
                         ROC_plot_clev1
                         
                         ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probMSK,data_SBC1)
                         ROC_plot_SBC1
                         
                         ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probMSK,data_ZUR1)
                         ROC_plot_ZUR1
                         
                         
                         # ROC with imputed model probabilities
                         ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probMSK1,data_MC1)
                         ROC_plot_MC1I
                         
                         ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probMSK1,data_SRF1)
                         ROC_plot_SRF1I
                         
                         ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probMSK1,data_PRVA1)
                         ROC_plot_PRVA1I
                         
                         ROC_plot_clev1I<- roc_plot(data_clev1$my_data.bx_result,data_clev1$probMSK1,data_clev1)
                         ROC_plot_clev1I
                         
                         ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probMSK1,data_SBC1)
                         ROC_plot_SBC1I
                         
                         ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probMSK1,data_ZUR1)
                         ROC_plot_ZUR1I
                         
                         #Inspecting the Distribution of the Imputed Data
                         
                         xyplot(imputedMSK,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)
                         
                         
                         # PRVA Imputation
                         # MICE for cleveland imputation
                         #clevI- data for imputation
                         #sapply(data_DVA, function(x) sum(is.na(x)))
                         
                         data_clev1 <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
                         data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
                         data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
                         data_PRVAI <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_MSKI <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
                         data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
                         data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]
                         
                         #unique(data_clevI$my_data.race)
                         
                         data_PRVAI<-data_PRVAI[,-10]
                         colSums(is.na(data_PRVAI))
                         
                         library(mice)
                         init = mice(data_PRVAI, maxit=0) 
                         meth = init$method
                         predM = init$predictorMatrix
                         
                         meth[c("my_data.mrn")]=""
                         meth[c("my_data.pca")]=""
                         #meth[c("my_data.bx_ggs_cat")]="" 
                         meth[c("my_data.psa")]="" 
                         meth[c("my_data.dre_result")]="logreg" 
                         meth[c("my_data.age_bx")]="norm" 
                         meth[c("my_data.famhx1")]="logreg" 
                         meth[c("my_data.priorbx_neg")]="" 
                         meth[c("my_data.race")]="polyreg" 
                         meth[c("my_data.bx_result")]="" 
                         
                         set.seed(103)
                         imputedPRVA = mice(data_PRVAI, method=meth, predictorMatrix=predM, m=5)
                         
                         imputedPRVA1 <- complete(imputedPRVA,3)
                         imputedPRVA2 <- complete(imputedPRVA,4)
                         imputedPRVA3 <- complete(imputedPRVA,5)
                         imputedPRVA4 <- complete(imputedPRVA,1)
                         imputedPRVA5 <- complete(imputedPRVA,2)
                         
                         save(file="imputed2.Rda")
                         
                         sapply(imputedPRVA1, function(x) sum(is.na(x)))
                         #load("imputed2.Rda")
                         
                         #Normal PRVA Data Model
                         
                         model_PRVA<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=data_PRVAI,family = binomial)
                         summary(model_PRVA)
                         
                         # Imputed Data Model 1
                         model_PRVAimp<- with(imputedPRVA, glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, family = binomial))
                         summary(pool(object=model_PRVAimp))
                         #summary(model_PRVAimp)
                         
                         model_PRVAimp1<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=imputedPRVA1,family = binomial)
                         summary(model_PRVAimp1)
                         
                         model_PRVAimp2<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=imputedPRVA2,family = binomial)
                         summary(model_PRVAimp2)
                         
                         model_PRVAimp3<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=imputedPRVA3,family = binomial)
                         summary(model_PRVAimp3)
                         
                         model_PRVAimp4<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=imputedPRVA4,family = binomial)
                         summary(model_PRVAimp4)
                         
                         model_PRVAimp5<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=imputedPRVA5,family = binomial)
                         summary(model_PRVAimp5)
                         
                         library(MuMIn)
                         out.put<-model.sel(model_PRVAimp1,model_PRVAimp2,model_PRVAimp3,model_PRVAimp4,model_PRVAimp5) 
                         MA.ests<-model.avg(out.put, revised.var = TRUE) 
                         model.selection(object = out.put, revised.var = TRUE)
                         
                         MA.ests$avg.model
                         
                         #prob with original model
                         data_MC1$probPRVA <- predict(model_PRVA,newdata=data_MC1, type="response")
                         data_SRF1$proPRVA <- predict(model_PRVA,newdata=data_SRF1, type="response")
                         data_PRVA1$proPRVA <- predict(model_PRVA,newdata=data_PRVA1, type="response")
                         data_clev1$probPRVA <- predict(model_PRVA,newdata=data_clev1, type="response")
                         data_SBC1$probPRVA <- predict(model_PRVA,newdata=data_SBC1, type="response")
                         data_ZUR1$probPRVA <- predict(model_PRVA,newdata=data_ZUR1, type="response")
                         
                         #Probabilities with imputed model
                         data_MC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_MC1, type="response")
                         data_MC1$probPRVA2 <- predict(model_PRVAimp2,newdata=data_MC1, type="response")
                         data_MC1$probPRVA3 <- predict(model_PRVAimp3,newdata=data_MC1, type="response")
                         data_MC1$probPRVA4 <- predict(model_PRVAimp4,newdata=data_MC1, type="response")
                         data_MC1$probPRVA5 <- predict(model_PRVAimp5,newdata=data_MC1, type="response")
                         
                         #Average value
                         
                         data_MC1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)
                         
                         
                         data_SRF1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
                         data_SRF1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
                         data_SRF1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
                         data_SRF1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
                         data_SRF1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
                         
                         data_SRF1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)
                         
                         
                         
                         
                         #data_PRVA1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_PRVA1, type="response")
                         data_clev1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
                         data_clev1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
                         data_clev1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
                         data_clev1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
                         data_clev1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
                         
                         data_clev1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)
                         
                         data_SBC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
                         data_SBC1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
                         data_SBC1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
                         data_SBC1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
                         data_SBC1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
                         
                         data_SBC1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)
                         
                         
                         
                         data_ZUR1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
                         data_ZUR1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
                         data_ZUR1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
                         data_ZUR1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
                         data_ZUR1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
                         
                         data_ZUR1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)
                         
                         
                         ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVA,data_MC1)
                         ROC_plot_MC1
                         
                         ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVA,data_SRF1)
                         ROC_plot_SRF1
                         
                         ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVA,data_PRVA1)
                         ROC_plot_PRVA1
                         
                         ROC_plot_clev1<- roc_plot(data_clev$my_data.bx_result,data_clev1$probPRVA,data_clev1)
                         ROC_plot_clev1
                         
                         ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVA,data_SBC1)
                         ROC_plot_SBC1
                         
                         ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA,data_ZUR1)
                         ROC_plot_ZUR1
                         
                         
                         # ROC with imputed model probabilities
                         ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVAMean,data_MC1)
                         ROC_plot_MC1I
                         
                         ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVAMean,data_SRF1)
                         ROC_plot_SRF1I
                         
                         ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVAMean,data_PRVA1)
                         ROC_plot_PRVA1I
                         
                         ROC_plot_clev1I<- roc_plot(data_clev1$my_data.bx_result,data_clev1$probPRVAMean,data_clev1)
                         ROC_plot_clev1I
                         
                         ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVAMean,data_SBC1)
                         ROC_plot_SBC1I
                         
                         ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA1,data_ZUR1)
                         ROC_plot_ZUR1I
                         
                         #Inspecting the Distribution of the Imputed Data
                         
                         xyplot(imputedPRVA,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)
                         densityplot(imputedPRVA)
                         stripplot(imputedPRVA,pch=20,cex=1.2)
                         
                         
                         # PRVA Imputation with 5% rule
                         # MICE for cleveland imputation
                         #clevI- data for imputation
                         #sapply(data_DVA, function(x) sum(is.na(x)))
                         
                         percentmiss <- function(x){sum(is.na(x))/length(x)*100}
                         
                         # checking columns
                         apply(data_PRVAI,2,percentmiss)
                         
                         data_clev1 <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
                         data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
                         data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
                         data_PRVAI1 <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
                         data_MSKI <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
                         data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
                         data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]
                         
                         #unique(data_clevI$my_data.race)
                         
                         data_PRVAI1<-data_PRVAI1[,-10]
                         colSums(is.na(data_PRVAI1))
                         
                         percentmiss <- function(x){sum(is.na(x))/length(x)*100}
                         
                         # checking columns
                         missingc<-apply(data_PRVAI1,2,percentmiss)
                         
                         # checking rows
                         missingr<-apply(data_PRVAI1,1,percentmiss)
                         table(missingr)
                         table(missingc)
                         
                         data_PRVAI2<-data_PRVAI1[missingr<15,]
                         data_PRVAI3<-data_PRVAI2[,missingc<20]
                         data_PRVAE1<-data.frame(data_PRVAI2[,missingc>20])
                         colnames(data_PRVAE1)<-c("my_data.famhx1")
                         data_PRVAE2<-data_PRVAI1[missingr>15,]
                         
                         library(mice)
                         init = mice(data_PRVAI3, maxit=0) 
                         meth = init$method
                         predM = init$predictorMatrix
                         
                         meth[c("my_data.mrn")]=""
                         meth[c("my_data.pca")]=""
                         #meth[c("my_data.bx_ggs_cat")]="" 
                         meth[c("my_data.psa")]="" 
                         meth[c("my_data.dre_result")]="logreg" 
                         meth[c("my_data.age_bx")]="norm" 
                         #meth[c("my_data.famhx1")]="logreg" 
                         meth[c("my_data.priorbx_neg")]="" 
                         meth[c("my_data.race")]="polyreg" 
                         meth[c("my_data.bx_result")]="" 
                         
                         set.seed(103)
                         imputedPRVAIU = mice(data_PRVAI3, method=meth, predictorMatrix=predM, m=2)
                         
                         imputedPRVAIU1 <- complete(imputedPRVAIU,2)
                         save(file="imputed2.Rda")
                         
                         completePRVA<-cbind(imputedPRVAIU1,data_PRVAE1)
                         
                         completePRVA<-cbind(imputedPRVAIU1, data_PRVAE1)
                         completePRVA1<-rbind(completePRVA,data_PRVAE2)
                         
                         sapply(completePRVA1, function(x) sum(is.na(x)))
                         #load("imputed2.Rda")
                         
                         #Normal PRVA Data Model
                         
                         model_PRVA<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=data_PRVAI,family = binomial)
                         summary(model_PRVA)
                         
                         # Imputed Data Model 
                         model_PRVAimp<- with(imputedPRVA, glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race,family = binomial));pool(model_PRVAimp)
                         pool(object=model_PRVAimp)
                         summary(model_PRVAimp)
                         
                         model_PRVAimp1<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                         +my_data.priorbx_neg+my_data.race, data=completePRVA1,family = binomial)
                         summary(model_PRVAimp1)
                         
                         
                         #prob with original model
                         data_MC1$probPRVA <- predict(model_PRVA,newdata=data_MC1, type="response")
                         data_SRF1$proPRVA <- predict(model_PRVA,newdata=data_SRF1, type="response")
                         data_PRVA1$proPRVA <- predict(model_PRVA,newdata=data_PRVA1, type="response")
                         data_clev1$probPRVA <- predict(model_PRVA,newdata=data_clev1, type="response")
                         data_SBC1$probPRVA <- predict(model_PRVA,newdata=data_SBC1, type="response")
                         data_ZUR1$probPRVA <- predict(model_PRVA,newdata=data_ZUR1, type="response")
                         
                         #Probabilities with imputed model
                         data_MC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_MC1, type="response")
                         data_SRF1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
                         data_PRVA1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_PRVA1, type="response")
                         data_clev1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
                         data_SBC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
                         data_ZUR1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
                         
                         
                         ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVA,data_MC1)
                         ROC_plot_MC1
                         
                         ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVA,data_SRF1)
                         ROC_plot_SRF1
                         
                         ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVA,data_PRVA1)
                         ROC_plot_PRVA1
                         
                         ROC_plot_clev1<- roc_plot(data_clev$my_data.bx_result,data_clev1$probPRVA,data_clev1)
                         ROC_plot_clev1
                         
                         ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVA,data_SBC1)
                         ROC_plot_SBC1
                         
                         ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA,data_ZUR1)
                         ROC_plot_ZUR1
                         
                         
                         # ROC with imputed model probabilities
                         ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVA1,data_MC1)
                         ROC_plot_MC1I
                         
                         ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVA1,data_SRF1)
                         ROC_plot_SRF1I
                         
                         ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVA1,data_PRVA1)
                         ROC_plot_PRVA1I
                         
                         ROC_plot_clev1I<- roc_plot(data_clev1$my_data.bx_result,data_clev1$probPRVA1,data_clev1)
                         ROC_plot_clev1I
                         
                         ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVA1,data_SBC1)
                         ROC_plot_SBC1I
                         
                         ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA1,data_ZUR1)
                         ROC_plot_ZUR1I
                         
                         #Inspecting the Distribution of the Imputed Data
                         
                         xyplot(imputedPRVA,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)
                         densityplot(imputedPRVA)
                         stripplot(imputedPRVA,pch=20,cex=1.2)
                         
                         library(readxl)
                         library(ggplot2)
                         library(reshape)
                         library(XML)
                         library(reshape2)
                         library(ggplot2)
                         library(plyr)
                         library(dplyr)
                         library(ggalt)
                         library(ggfortify)
                         library(ggthemes)
                         library(ggExtra)
                         library(VIM)
                         library(mice)
                         library(data.table)
                         library(naniar)
                         my_data<- read_excel("PBCG.xlsx")
                         #Taking out the irrelevant columns 
                         my_data_rel<- data.frame(my_data$mrn, my_data$pca, my_data$age_bx, my_data$race, my_data$priorbx_neg, my_data$psa, my_data$dre_result, my_data$famhx1, my_data$bx_result, my_data$bx_ggs_cat)
                         
                         my_data_rel$my_data.famhx1 <- as.factor(my_data_rel$my_data.famhx1)
                         my_data_rel$my_data.dre_result <- as.factor(my_data_rel$my_data.dre_result)
                         my_data_rel$my_data.priorbx_neg <- as.factor(my_data_rel$my_data.priorbx_neg)
                         #my_data_rel$my_data.race<- as.factor(my_data_rel$my_data.race)
                         my_data_rel$my_data.race <- as.factor(ifelse(my_data_rel$my_data.race %in% c("American Indian or Alaska Native","Asian",
                         "Asian-American or Pacific Islander","Asian or Pacific Islander", 
                         "Multiracial/Multicultural","Native Hawaiian or Other Pacific Islander", 
                         "Other"), "Others",{ifelse(my_data_rel$my_data.race%in% c("Black or African American"),
                         "Black or African American",{ifelse(my_data_rel$my_data.race%in% c("White"),"White",NA)})}))
                         
                         #hist(my_data_rel$my_data.race)
                         sum(is.na(my_data_rel$my_data.pca))
                         sum(is.na(my_data_rel$my_data.famhx1))
                         
                         sum(is.na(my_data_rel$my_data.bx_ggs_cat))
                         #my_data_rel_agg<-aggregate(my_data.pca~my_data.mrn, my_data_rel, max, na.rm="True")
                         
                         #p1 <- ggplot(subset(my_data_rel,my_data_rel$variable %in% c("my_data.prevbx_neg")), aes(x = value, group = my_data.pca)) + 
                         #geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "dodge") + 
                         #scale_y_continuous(labels=scales::percent) +
                         #ylab("relative frequencies") +
                         #facet_grid(~my_data.pca)+
                         #geom_text(aes( label = scales::percent(..prop..),
                         #y= ..prop.. ), stat= "count", vjust = -.5)+
                         #ggtitle("Histogramm for people with cancer (=1) without (=0)", "Previous negative biopsy")+
                         #scale_fill_discrete(name ="PROSBX",labels = c("0 no prior biobsy", "1 prior biobsy"))
                         #p1  
                         
                         
                         # Load data in VIM
                         data(my_data_rel, package = "VIM")
                         
                         ## Show complete cases
                         my_data_rel[complete.cases(my_data_rel),]
                         
                         #md.pattern(my_data_rel)
                         
                         ## Visualizing in number
                         aggr(my_data_rel, prop = F, numbers = T)
                         
                         # Visualizing in proportion
                         aggr(my_data_rel, prop = T, numbers = T)
                         
                         ## Matrix plot. Red for missing values, Darker values are high values.
                         matrixplot(my_data_rel, interactive = F, sortby = "my_data.bx_ggs_cat")
                         
                         #library(data.table)
                         #data_clev <- my_data_rel[my_data_rel$my_data.mrn %like% "CCF",]
                         
                         library(VIM)
                         library(mice)
                         library(gridExtra)
                         # 1. Cleveland Data
                         data_clev <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
                         data_clev1 <- data_clev[,c(-1,-2,-10,-11,-12)]
                         colnames(data_clev1)[1:7]<-c("age_bx","race","prior_neg","psa","dre_result","famhx1","bx_result")
                         data(data_clev1, package = "VIM")
                         aggr(data_clev1, prop = F, numbers = T)
                         md.pattern(data_clev1)
                         matrixplot(data_clev1, interactive = F, sortby = "bx_result", main="Cleveland Missing Data")
                         scattmatrixMiss(data_clev1, interactive = F)
                         marginplot(data_clev1[,c("race","prior_neg")],main=race vs prior_neg)
spineMiss(data_clev1)
data_clev1MP<-data_clev1[,c("age_bx","famhx1")]

marginplot(data_clev1MP, main="age vs famhx1")
marginmatrix(data_clev1)
spineMiss(data_clev1[, c("famhx1", "prior_neg")])

colSums(is.na(data_clev1))
# Histogram for each characteristics
data_clev[is.na(data_clev)]<-" NA"

p1 <- ggplot(data=data_clev, aes(data_clev$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()
p1
+geom_text(label = ..count../sum(..count..), group =my_data.bx_result,
           color = "white")
#+scale_fill_discrete(name ="DRE",labels = c("0 negative DRE", "1 positive DRE"))#+facet_grid(~my_data.bx_result) 

p2 <- ggplot(data=data_clev, aes(data_clev$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar()
p2
p3 <- ggplot(data = data_clev,
             mapping = aes(x = my_data.priorbx_neg, fill=my_data.bx_result))
p3 + geom_bar(position = "dodge",
              mapping = aes(y = ..prop.., group = my_data.bx_result )) 
#facet_wrap(~my_data.bx_result, ncol = 1)

p4 <- ggplot(data=data_clev, aes(data_clev$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p4

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_clev$my_data.age_group <- cut(data_clev$my_data.age, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_clev$my_data.psa_group <- cut(data_clev$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_clev$my_data.psa_group <- as.factor(data_clev$my_data.psa_group)
grid.arrange(
  ggplot(data=data_clev, aes(data_clev$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar(),
  ggplot(data=data_clev, aes(data_clev$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar(),
  ggplot(data=data_clev, aes(data_clev$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar(),
  ncol=3
)  

aggr(data_clev, prop = F, numbers = T)

matrixplot(data_clev, interactive = F, sortby = "my_data.bx_ggs_cat")





# 2.

data(data_DVA, package = "VIM")
aggr(data_DVA, prop = F, numbers = T)
matrixplot(data_DVA, interactive = F, sortby = "my_data.bx_ggs_cat")

p21 <- ggplot(data=data_DVA, aes(data_DVA$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p21 Durham Data)
data_DVA$my_data.psa_group <- cut(data_DVA$my_data.psa, breaks = c(-0.01,
data_DVA <- my_data_rel[grep("DVA", my_data_rel$my_data.mrn),]
# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_DVA$my_data.age_group <- cut(data_DVA$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000"1,2,3,4,5, Inf),labels=labels)
data_DVA$my_data.psa_group <- as.factor(data_DVA$my_data.psa_group)

data_DVA[is.na(data_DVA)]<-" NA"

p22 <- ggplot(data=data_DVA, aes(data_DVA$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p22
p23 <- ggplot(data=data_DVA, aes(data_DVA$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p23
p24 <- ggplot(data=data_DVA, aes(data_DVA$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p24
p25 <- ggplot(data=data_DVA, aes(data_DVA$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p25
p26 <- ggplot(data=data_DVA, aes(data_DVA$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p26


# 3.Hamburg
data_PS<- my_data_rel[grep("PS", my_data_rel$my_data.mrn),]
data_PSU<- my_data_rel[grep("PS", my_data_rel$my_data.mrn),]

data_PS %>% replace_with_na_all(condition = ~.x %in% c("NA"))
data_PSU %>% replace(.,"NA",is.na(.))

data(data_PS, package = "VIM")
aggr(data_PS, prop = F, numbers = T)
matrixplot(data_PS, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
PSmin<-data_PS[-c(8923),]
labels = c("21-65", "66-70","71-75","76-91")

data_PSUU$my_data.age_group <- cut(data_PSU$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #5000 custard
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 5000")
data_PSU$my_data.psa_group <- cut(data_PSU$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_PSU$my_data.psa_group <- as.factor(data_PSU$my_data.psa_group)

colMax <- function(data,column) sapply(data$column, max, na.rm = TRUE)

data_PSU[is.na(data_PSU)]<-" NA"

p31 <- ggplot(data=data_PSU, aes(data_PSU$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="Hamburg (PS)",fill="bx_result",x="DRE_result",y="No. of cases")
p31
p32 <- ggplot(data=data_PSU, aes(data_PSU$my_data.pca, fill=factor(my_data.bx_result)))+
  geom_bar()+labs(title="Hamburg (PS)",fill="bx_result",x="pca",y="No. of cases")
p32
p33 <- ggplot(data=data_PSU, aes(data_PSU$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar()+ 
  labs(title="Hamburg (PS)",fill="bx_result",x="priorbx_neg",y="No. of cases")
p33
p34 <- ggplot(data=data_PSU, aes(data_PSU$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="Hamburg (PS)",fill="bx_result",x="age_group",y="No. of cases")
p34
p35 <- ggplot(data=data_PSU, aes(data_PSU$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="Hamburg (PS)",fill="bx_result",x="psa_group",y="No. of cases")
p35
p36 <- ggplot(data=data_PSU, aes(data_PSU$my_data.race, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="Hamburg (PS)",fill="bx_result",x="race",y="No. of cases")
p36
p36 <- ggplot(data=data_PSU, aes(data_PSU$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="Hamburg (PS)",fill="bx_result",x="famhx1",y="No. of cases")
p36



# 4.MSKCC retard
data_MSK<- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]

data(data_MSK, package = "VIM")
aggr(data_MSK, prop = F, numbers = T)
matrixplot(data_MSK, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_MSK$my_data.age_group <- cut(data_MSK$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_MSK$my_data.psa_group <- cut(data_DVA$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_MSK$my_data.psa_group <- as.factor(data_DVA$my_data.psa_group)

data_MSK[is.na(data_MSK)]<-" NA"


p41 <- ggplot(data=data_MSK, aes(data_MSK$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p41
p42 <- ggplot(data=data_MSK, aes(data_MSK$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p42
p43 <- ggplot(data=data_MSK, aes(data_MSK$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p43
p44 <- ggplot(data=data_MSK, aes(data_MSK$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p44
p45 <- ggplot(data=data_MSK, aes(data_MSK$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p45
p46 <- ggplot(data=data_MSK, aes(data_MSK$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p46

# 5.Puerto Rico
data_PRVA <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_PRVA<- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_PRVAU<- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_PRVAUC<- data_PRVAU[complete.cases(data_PRVAU),complete.cases(t(data_PRVAU))]


#data_PRVA %>% replace_with_na_all(condition = ~.x %in% c("NA"))
#data_PRVAU %>% replace(.,"NA",is.na(.))

#data(data_PRVA, package = "VIM")
#aggr(data_PRVA, prop = F, numbers = T)
#matrixplot(data_PRVA, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
#PSmin<-data_PRVA[-c(8923),]
labels = c("45-65", "66-70","71-75","76-92")

data_PRVAU$my_data.age_group <- cut(data_PRVAU$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)
data_PRVAU$my_data.age_group <- as.factor(data_PRVAU$my_data.age_group)

# create PSA groups
# max(baseline_select$PSA) #5000 custard
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 553.4")
data_PRVAU$my_data.psa_group <- cut(data_PRVAU$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_PRVAU$my_data.psa_group <- as.factor(data_PRVAU$my_data.psa_group)

#colMax <- function(data,column) sapply(data$column, max, na.rm = TRUE)

#data_PRVAU[is.na(data_PRVAU)]<-" NA"

p31 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="Puerto Rico (PRVA)",fill="bx_result",x="DRE_result",y="No. of cases")
p31
p32 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.pca, fill=factor(my_data.bx_result)))+
  geom_bar()+labs(title="",fill="bx_result",x="pca",y="No. of cases")
p32
p33 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar()+ 
  labs(title="",fill="bx_result",x="priorbx_neg",y="No. of cases")
p33
p34 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="age_group",y="No. of cases")
p34
p35 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="psa_group",y="No. of cases")
p35
p32 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.race, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="race",y="No. of cases")
p32
p36 <- ggplot(data=data_PRVAU, aes(data_PRVAU$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="famhx1",y="No. of cases")
p36

grid.arrange(p31,p32,p33,p34,p35,p36,ncol=2,top="Puerto Rico (PRVA) with NA Cases")


#Complete data analysis

data_PRVAU<- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_PRVAUC<- data_PRVAU[complete.cases(data_PRVAU),]

#labels = c("21-65", "66-70","71-75","76-91")

#data_PRVAUC$my_data.age_group <- cut(data_PRVAUC$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #5000 custard
#labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 553.4")
#data_PRVAUC$my_data.psa_group <- cut(data_PRVAUC$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
#data_PRVAUC$my_data.psa_group <- as.factor(data_PRVAUC$my_data.psa_group)

#colMax <- function(data,column) sapply(data$column, max, na.rm = TRUE)

#data_PRVAUC[is.na(data_PRVAUC)]<-" NA"

p31 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="DRE_result",y="No. of cases")
p31
p32 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.pca, fill=factor(my_data.bx_result)))+
  geom_bar()+labs(title="",fill="bx_result",x="pca",y="No. of cases")
p32
p33 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar()+ 
  labs(title="",fill="bx_result",x="priorbx_neg",y="No. of cases")
p33
p34 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="age_group",y="No. of cases")
p34
p35 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="psa_group",y="No. of cases")
p35
p32 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.race, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="race",y="No. of cases")
p32
p36 <- ggplot(data=data_PRVAUC, aes(data_PRVAUC$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="famhx1",y="No. of cases")
p36

grid.arrange(p31,p32,p33,p34,p35,p36,ncol=2,top="Puerto Rico (PRVA) Complete Cases")

# Chi Square Test
chisqrace <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.race)
chisqrace
chisqage_group <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.age_group)
chisqage_group
chisqpsa_group <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.psa_group)
chisqpsa_group
chisqfamhx1 <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.famhx1)
chisqfamhx1
chisqdre_result <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.dre_result)
chisqdre_result
chisqpriorbx_neg <-chisq.test(data_PRVAU$my_data.bx_result,data_PRVAU$my_data.priorbx_neg)
chisqpriorbx_neg


data(data_PRVA, package = "VIM")
aggr(data_PRVA, prop = F, numbers = T)
matrixplot(data_PRVA, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_PRVA$my_data.age_group <- cut(data_PRVA$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_PRVA$my_data.psa_group <- cut(data_PRVA$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_PRVA$my_data.psa_group <- as.factor(data_PRVA$my_data.psa_group)

data_PRVA[is.na(data_PRVA)]<-" NA"

p51 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p51
p52 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p52
p53 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p53
p54 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p54
p55 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p55
p56 <- ggplot(data=data_PRVA, aes(data_PRVA$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p56

# 6.SanRaffael
data_SRF <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]

data(data_SRF, package = "VIM")
aggr(data_SRF, prop = F, numbers = T)
matrixplot(data_SRF, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_SRF$my_data.age_group <- cut(data_SRF$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_SRF$my_data.psa_group <- cut(data_SRF$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_SRF$my_data.psa_group <- as.factor(data_SRF$my_data.psa_group)

data_SRF[is.na(data_SRF)]<-" NA"

p61 <- ggplot(data=data_SRF, aes(data_SRF$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p61
p62 <- ggplot(data=data_SRF, aes(data_SRF$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p62
p63 <- ggplot(data=data_SRF, aes(data_SRF$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p63
p64 <- ggplot(data=data_SRF, aes(data_SRF$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p64
p65 <- ggplot(data=data_SRF, aes(data_SRF$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p65
p66 <- ggplot(data=data_SRF, aes(data_SRF$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p66

# 7. SunnyBrook
data_SBC <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]

data(data_SBC, package = "VIM")
aggr(data_SBC, prop = F, numbers = T)
matrixplot(data_SBC, interactive = F, sortby = "my_data.bx_result")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_SBC$my_data.age_group <- cut(data_SBC$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_SBC$my_data.psa_group <- cut(data_SBC$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_SBC$my_data.psa_group <- as.factor(data_SBC$my_data.psa_group)

data_SBC[is.na(data_SBC)]<-" NA"

p71 <- ggplot(data=data_DVA, aes(data_DVA$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p71
p72 <- ggplot(data=data_DVA, aes(data_DVA$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p72
p73 <- ggplot(data=data_DVA, aes(data_DVA$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p73
p74 <- ggplot(data=data_DVA, aes(data_DVA$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p74
p75 <- ggplot(data=data_DVA, aes(data_DVA$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p75
p76 <- ggplot(data=data_DVA, aes(data_DVA$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p76


data_SBCU<- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
data_SBCUC<- data_SBCU[complete.cases(data_SBCU),]

labels = c("41-65", "66-70","71-75","76-95")

data_SBCUC$my_data.age_group <- cut(data_SBCUC$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #5000 custard
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 7276")
data_SBCUC$my_data.psa_group <- cut(data_SBCUC$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_SBCUC$my_data.psa_group <- as.factor(data_SBCUC$my_data.psa_group)

#colMax <- function(data,column) sapply(data$column, max, na.rm = TRUE)

#data_SBCUC[is.na(data_SBCUC)]<-" NA"

p31 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="DRE_result",y="No. of cases")
p31
p32 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.pca, fill=factor(my_data.bx_result)))+
  geom_bar()+labs(title="",fill="bx_result",x="pca",y="No. of cases")
p32
p33 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar()+ 
  labs(title="",fill="bx_result",x="priorbx_neg",y="No. of cases")
p33
p34 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="age_group",y="No. of cases")
p34
p35 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="psa_group",y="No. of cases")
p35
p32 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.race, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="race",y="No. of cases")
p32
p36 <- ggplot(data=data_SBCUC, aes(data_SBCUC$my_data.famhx1, fill=factor(my_data.bx_result)))+geom_bar()+
  labs(title="",fill="bx_result",x="famhx1",y="No. of cases")
p36

grid.arrange(p31,p32,p33,p34,p35,p36,ncol=2,top="Puerto Rico (SBC) Complete Cases")

# Chi Square Test
chisqrace <-chisq.test(data_SBCU$my_data.bx_result,data_SBCU$my_data.race)
chisqrace
chisqage_group <-chisq.test(data_SBCU$my_data.bx_result,data_SBCU$my_data.age_group)
chisqage_group
chisqpsa_group <-chisq.test(data_SBCU$my_data.bx_result,data_SBCU$my_data.psa_group)
chisqpsa_group
chisqfamhx1 <-chisq.test(data_SBCU$my_data.bx_result,data_SBCU$my_data.famhx1)
chisqfamhx1
chisqdre_result <-chisq.test(data_SBCU$my_data.bx_result,data_SBCU$my_data.dre_result)
chisqdre_result
chisqpriorbx_neg <-chisq.test(data_SBCU$my_data.bx_result,data_SBCU$my_data.priorbx_neg)
chisqpriorbx_neg



# 8. UCSF
data_UCSF <- my_data_rel[grep("UCSF", my_data_rel$my_data.mrn),]

data(data_UCSF, package = "VIM")
aggr(data_UCSF, prop = F, numbers = T)
matrixplot(data_UCSF, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_UCSF$my_data.age_group <- cut(data_UCSF$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_UCSF$my_data.psa_group <- cut(data_UCSF$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_UCSF$my_data.psa_group <- as.factor(data_UCSF$my_data.psa_group)

data_UCSF[is.na(data_UCSF)]<-" NA"

p81 <- ggplot(data=data_UCSF, aes(data_UCSF$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p81
p82 <- ggplot(data=data_UCSF, aes(data_UCSF$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p82
p83 <- ggplot(data=data_UCSF, aes(data_UCSF$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p83
p84 <- ggplot(data=data_UCSF, aes(data_UCSF$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p84
p85 <- ggplot(data=data_UCSF, aes(data_UCSF$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p85
p86 <- ggplot(data=data_UCSF, aes(data_UCSF$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p86


# 9. Mayo Clinic
data_MC <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]

data(data_MC, package = "VIM")
aggr(data_MC, prop = F, numbers = T)
matrixplot(data_MC, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_MC$my_data.age_group <- cut(data_MC$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_MC$my_data.psa_group <- cut(data_MC$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_MC$my_data.psa_group <- as.factor(data_MC$my_data.psa_group)

data_MC[is.na(data_MC)]<-" NA"


data(data_MC, package = "VIM")
aggr(data_MC, prop = F, numbers = T)
matrixplot(data_MC, interactive = F, sortby = "my_data.bx_ggs_cat")

p91 <- ggplot(data=data_MC, aes(data_MC$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p91
p92 <- ggplot(data=data_MC, aes(data_MC$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p92
p93 <- ggplot(data=data_MC, aes(data_MC$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p93
p94 <- ggplot(data=data_MC, aes(data_MC$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p94
p95 <- ggplot(data=data_MC, aes(data_MC$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p95
p96 <- ggplot(data=data_MC, aes(data_MC$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p96

# 10. Zurich
data_ZUR <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]

data(data_ZUR, package = "VIM")
aggr(data_ZUR, prop = F, numbers = T)
matrixplot(data_ZUR, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_ZUR$my_data.age_group <- cut(data_ZUR$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_ZUR$my_data.psa_group <- cut(data_ZUR$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_ZUR$my_data.psa_group <- as.factor(data_ZUR$my_data.psa_group)

data_ZUR[is.na(data_ZUR)]<-" NA"


p101 <- ggplot(data=data_ZUR, aes(data_ZUR$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p101
p102 <- ggplot(data=data_ZUR, aes(data_ZUR$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p102
p103 <- ggplot(data=data_ZUR, aes(data_ZUR$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p103
p104 <- ggplot(data=data_ZUR, aes(data_ZUR$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p104
p105 <- ggplot(data=data_ZUR, aes(data_ZUR$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p105
p106 <- ggplot(data=data_ZUR, aes(data_ZUR$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p106


# 11. UTHSCA
data_UTHS <- my_data_rel[grep("UTHS", my_data_rel$my_data.mrn),]

data(data_UTHS, package = "VIM")
aggr(data_UTHS, prop = F, numbers = T)
matrixplot(data_UTHS, interactive = F, sortby = "my_data.bx_ggs_cat")

# Change the characteritics 
# create the age groups 
labels = c("55-65", "66-70","71-75","76-93")

data_UTHS$my_data.age_group <- cut(data_UTHS$my_data.age_bx, breaks = c(0,65,70,75, Inf),labels=labels)

# create PSA groups
# max(baseline_select$PSA) #2000
labels = c("0.0-1.0", "1.1-2.0","2.1-3.0","3.1-4.0","4.1-5.0","5.1 - 2000")
data_UTHS$my_data.psa_group <- cut(data_UTHS$my_data.psa, breaks = c(-0.01,1,2,3,4,5, Inf),labels=labels)
data_UTHS$my_data.psa_group <- as.factor(data_UTHS$my_data.psa_group)

data_UTHS[is.na(data_UTHS)]<-"NA"


data(data_UTHS, package = "VIM")
aggr(data_UTHS, prop = F, numbers = T)
matrixplot(data_UTHS, interactive = F, sortby = "my_data.bx_ggs_cat")

p111 <- ggplot(data=data_UTHS, aes(data_UTHS$my_data.dre_result, fill=factor(my_data.bx_result)))+geom_bar() 
p111
p112 <- ggplot(data=data_UTHS, aes(data_UTHS$my_data.pca, fill=factor(my_data.bx_result)))+geom_bar() 
p112
p113 <- ggplot(data=data_UTHS, aes(data_UTHS$my_data.priorbx_neg, fill=factor(my_data.bx_result)))+geom_bar() 
p113
p114 <- ggplot(data=data_UTHS, aes(data_UTHS$my_data.age_group, fill=factor(my_data.bx_result)))+geom_bar() 
p114
p115 <- ggplot(data=data_UTHS, aes(data_UTHS$my_data.psa_group, fill=factor(my_data.bx_result)))+geom_bar() 
p115
p116 <- ggplot(data=data_UTHS, aes(data_UTHS$my_data.race, fill=factor(my_data.bx_result)))+geom_bar() 
p116



###==== 2. Modell for UTHSCA =====
model_UTHS<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                 +my_data.priorbx_neg+my_data.race, data=data_UTHS,family = binomial)
summary(model_UTHS)
data_UTHS1 <- na.omit(data_UTHS)
model_UTHS1<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                  +my_data.priorbx_neg+my_data.race, data=data_UTHS1,family = binomial)
summary(model_UTHS1)
obsvused<-nobs(model_UTHS)
obsvused

# MICE for cleveland imputation
#clevI- data for imputation
#sapply(data_DVA, function(x) sum(is.na(x)))

data_clevI <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
data_PRVA1 <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_MSK1 <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]

unique(data_clevI$my_data.race)
colSums(is.na(data_clevI))

data_clevI<-data_clevI[,-10]
library(mice)
init = mice(data_clevI, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("my_data.mrn")]=""
meth[c("my_data.pca")]=""
#meth[c("my_data.bx_ggs_cat")]="" 
meth[c("my_data.psa")]="" 
meth[c("my_data.dre_result")]="norm" 
meth[c("my_data.age_bx")]="" 
meth[c("my_data.famhx1")]="logreg" 
meth[c("my_data.priorbx_neg")]="logreg" 
meth[c("my_data.race")]="polyreg" 
meth[c("my_data.bx_result")]="" 

set.seed(103)
imputed2 = mice(data_clevI, method=meth, predictorMatrix=predM, m=5)

#imputed <- complete(imputed2)
save(file="imputed2.Rda")

imputedCC1 <- complete(imputed2,3)
imputedCC2 <- complete(imputed2,4)
imputedCC3 <- complete(imputed2,5)
imputedCC4 <- complete(imputed2,1)
imputedCC5 <- complete(imputed2,2)

#Imputation Analysis
xyplot(imputed2,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)
densityplot(imputed2)
stripplot(imputed2,pch=20,cex=1.2)

#Normal CC Data Model

model_CC<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+as.factor(my_data.famhx1)
               +my_data.priorbx_neg+my_data.race, data=data_clevI,family = binomial)
summary(model_CC)

# Imputed Data Model 1
#model_PRVAimp<- with(imputedPRVA, glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
# +my_data.priorbx_neg+my_data.race, family = binomial))
#summary(pool(object=model_PRVAimp))
#summary(model_PRVAimp)

model_CCimp1<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+as.factor(my_data.famhx1)
                   +my_data.priorbx_neg+my_data.race, data=imputedCC1,family = binomial)
summary(model_CCimp1)

model_CCimp2<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+as.factor(my_data.famhx1)
                   +my_data.priorbx_neg+my_data.race, data=imputedCC2,family = binomial)
summary(model_CCimp2)

model_CCimp3<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+as.factor(my_data.famhx1)
                   +my_data.priorbx_neg+my_data.race, data=imputedCC3,family = binomial)
summary(model_PRVAimp3)

model_CCimp4<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+as.factor(my_data.famhx1)                     +my_data.priorbx_neg+my_data.race, data=imputedCC4,family = binomial)
summary(model_CCimp4)

model_CCimp5<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+as.factor(my_data.famhx1)
                   +my_data.priorbx_neg+my_data.race, data=imputedCC5,family = binomial)
summary(model_CCimp5)

#prob with original model
data_MC1$probCC <- predict(model_CC,newdata=data_MC1, type="response")
data_SRF1$probCC <- predict(model_CC,newdata=data_SRF1, type="response")
#data_PRVA1$probPRVA <- predict(model_CC,newdata=data_PRVA1, type="response")
#data_clev1$probCC <- predict(model_CC,newdata=data_clev1, type="response")
data_SBC1$probCC <- predict(model_CC,newdata=data_SBC1, type="response")
data_ZUR1$probCC <- predict(model_CC,newdata=data_ZUR1, type="response")


#Probabilities with imputed model
data_MC1$probCC1 <- predict(model_CCimp1,newdata=data_MC1, type="response")
data_MC1$probCC2 <- predict(model_CCimp2,newdata=data_MC1, type="response")
data_MC1$probCC3 <- predict(model_CCimp3,newdata=data_MC1, type="response")
data_MC1$probCC4 <- predict(model_CCimp4,newdata=data_MC1, type="response")
data_MC1$probCC5 <- predict(model_CCimp5,newdata=data_MC1, type="response")

#Average value

data_MC1$probCCMean<- rowMeans(data_MC1[c('probCC1', 'probCC2','probCC3','probCC4','probCC5')], na.rm=TRUE)


data_SRF1$probCC1 <- predict(model_CCimp1,newdata=data_SRF1, type="response")
data_SRF1$probCC2 <- predict(model_CCimp2,newdata=data_SRF1, type="response")
data_SRF1$probCC3 <- predict(model_CCimp3,newdata=data_SRF1, type="response")
data_SRF1$probCC4 <- predict(model_CCimp4,newdata=data_SRF1, type="response")
data_SRF1$probCC5 <- predict(model_CCimp5,newdata=data_SRF1, type="response")

data_SRF1$probCCMean<- rowMeans(data_SRF1[c('probPCC1', 'probCC2','probCC3','probCC4','probCC5')], na.rm=TRUE)




#data_PRVA1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_PRVA1, type="response")
#data_clev1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
#data_clev1$probPRVA2 <- predict(model_PRVAimp2,newdata=data_clev1, type="response")
#data_clev1$probPRVA3 <- predict(model_PRVAimp3,newdata=data_clev1, type="response")
#data_clev1$probPRVA4 <- predict(model_PRVAimp4,newdata=data_clev1, type="response")
#data_clev1$probPRVA5 <- predict(model_PRVAimp5,newdata=data_clev1, type="response")

#data_clev1$probCCMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)

data_SBC1$probCC1 <- predict(model_CCimp1,newdata=data_SBC1, type="response")
data_SBC1$probCC2 <- predict(model_CCimp2,newdata=data_SBC1, type="response")
data_SBC1$probCC3 <- predict(model_CCimp3,newdata=data_SBC1, type="response")
data_SBC1$probCC4 <- predict(model_CCimp4,newdata=data_SBC1, type="response")
data_SBC1$probCC5 <- predict(model_CCimp5,newdata=data_SBC1, type="response")

data_SBC1$probCCMean<- rowMeans(data_SBC1[c('probCC1', 'probCC2','probCC3','probCC4','probCC5')], na.rm=TRUE)



data_ZUR1$probCC1 <- predict(model_CCimp1,newdata=data_ZUR1, type="response")
data_ZUR1$probCC2 <- predict(model_CCimp2,newdata=data_ZUR1, type="response")
data_ZUR1$probCC3 <- predict(model_CCimp3,newdata=data_ZUR1, type="response")
data_ZUR1$probCC4 <- predict(model_CCimp4,newdata=data_ZUR1, type="response")
data_ZUR1$probCC5 <- predict(model_CCimp5,newdata=data_ZUR1, type="response")

data_ZUR1$probCCMean<- rowMeans(data_ZUR1[c('probCC1', 'probCC2','probCC3','probCC4','probCC5')], na.rm=TRUE)


sapply(imputed2, function(x) sum(is.na(x)))
#load("imputed2.Rda")



#Normal Clev Data Model

model_clev<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                 +my_data.priorbx_neg+my_data.race, data=data_clev,family = binomial)
summary(model_clev)

# Imputed Data Model 
model_clevimp<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                    +my_data.priorbx_neg+my_data.race, data=imputed2,family = binomial)
summary(model_clevimp)
data_DVA1 <- my_data_rel[300:2954,]

#predUTHS <- with(data_UTHS1,expand.grid(data_UTHS1$my_data.race=levels(model_clevimp$my_data.race)))
data_DVA1$prob <- predict(model_clevimp,newdata=data_DVA1, type="response")

# To check if the levels are attuned
data_clevII <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
data_UCSF1I <- my_data_rel[grep("UCSF", my_data_rel$my_data.mrn),]
data_UTHS1I <- my_data_rel[grep("UTHS", my_data_rel$my_data.mrn),]
model_clevTrial<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                      +my_data.priorbx_neg+my_data.race, data=data_clevII,family = binomial)
summary(model_clevTrial)

data_MC1$prob <- predict(model_clevTrial,newdata=data_MC1, type="response")
data_SRF1$prob <- predict(model_clevTrial,newdata=data_SRF1, type="response")
data_PRVA1$prob <- predict(model_clevTrial,newdata=data_PRVA1, type="response")
data_MSK1$prob <- predict(model_clevTrial,newdata=data_MSK1, type="response")
data_SBC1$prob <- predict(model_clevTrial,newdata=data_SBC1, type="response")
data_ZUR1$prob <- predict(model_clevTrial,newdata=data_ZUR1, type="response")

#Probabilities with imputed model
data_MC1$prob1 <- predict(model_clevimp,newdata=data_MC1, type="response")
data_SRF1$prob1 <- predict(model_clevimp,newdata=data_SRF1, type="response")
data_PRVA1$prob1 <- predict(model_clevimp,newdata=data_PRVA1, type="response")
data_MSK1$prob1 <- predict(model_clevimp,newdata=data_MSK1, type="response")
data_SBC1$prob1 <- predict(model_clevimp,newdata=data_SBC1, type="response")
data_ZUR1$prob1 <- predict(model_clevimp,newdata=data_ZUR1, type="response")

library("ggplot2")
library("pROC")
# Function for plotting the ROC curve and calculating the AUC
roc_plot <- function(response, prob, data){
  # calculating the specificity and sensitivity 
  ROC <- roc(response ~prob, data=)
  x <- 1-ROC$specificities
  y<- ROC$sensitivities
  # getting the AUC for the model
  AUC <- as.data.frame(ROC$auc)
  values <- as.data.frame(cbind(y,x))
  # plotting
  ggplot(values, aes(y=y, x=x))+
    geom_smooth()+
    xlab("1-Specificity")+ 
    ylab("Sensitivity") +
    theme_minimal() +
    geom_text(x = 0.5, y = 0.2, label=paste("Area under the curve:",round(AUC,2)),size =6)
}
ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probCC,data_MC1)
ROC_plot_MC1

ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probCC,data_SRF1)
ROC_plot_SRF1

ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probCC,data_PRVA1)
ROC_plot_PRVA1

ROC_plot_MSK1<- roc_plot(data_MSK1$my_data.bx_result,data_MSK1$probCC,data_MSK1)
ROC_plot_MSK1

ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probCC,data_SBC1)
ROC_plot_SBC1

ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probCC,data_ZUR1)
ROC_plot_ZUR1


# ROC with imputed model probabilities
ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probCCMean,data_MC1)
ROC_plot_MC1I

ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probCCMean,data_SRF1)
ROC_plot_SRF1I

ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probCCMean,data_PRVA1)
ROC_plot_PRVA1I

ROC_plot_MSK1I<- roc_plot(data_MSK1$my_data.bx_result,data_MSK1$probCCMean,data_MSK1)
ROC_plot_MSK1I

ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probCCMean,data_SBC1)
ROC_plot_SBC1I

ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probCCMean,data_ZUR1)
ROC_plot_ZUR1I

# MSK Imputation
# MICE for cleveland imputation
#clevI- data for imputation
#sapply(data_DVA, function(x) sum(is.na(x)))

data_clev1 <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
data_PRVA1 <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_MSKI <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]

unique(data_clevI$my_data.race)

data_MSKI<-data_MSKI[,-10]
colSums(is.na(data_MSKI))

library(mice)
init = mice(data_MSKI, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("my_data.mrn")]=""
meth[c("my_data.pca")]=""
#meth[c("my_data.bx_ggs_cat")]="" 
meth[c("my_data.psa")]="" 
meth[c("my_data.dre_result")]="logreg" 
meth[c("my_data.age_bx")]="" 
meth[c("my_data.famhx1")]="" 
meth[c("my_data.priorbx_neg")]="" 
meth[c("my_data.race")]="polyreg" 
meth[c("my_data.bx_result")]="" 

set.seed(103)
imputedMSK = mice(data_MSKI, method=meth, predictorMatrix=predM, m=5)

imputedMSK1 <- complete(imputedMSK)
save(file="imputed2.Rda")

sapply(imputed2, function(x) sum(is.na(x)))
#load("imputed2.Rda")

#Normal MSK Data Model

model_MSK<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                +my_data.priorbx_neg+my_data.race, data=data_MSK1,family = binomial)
summary(model_MSK)

# Imputed Data Model 
model_MSKimp<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                   +my_data.priorbx_neg+my_data.race, data=imputedMSK,family = binomial)
summary(model_MSKimp)

#prob with original model
data_MC1$probMSK <- predict(model_MSK,newdata=data_MC1, type="response")
data_SRF1$proMSK <- predict(model_MSK,newdata=data_SRF1, type="response")
data_PRVA1$proMSK <- predict(model_MSK,newdata=data_PRVA1, type="response")
data_clev1$probMSK <- predict(model_MSK,newdata=data_clev1, type="response")
data_SBC1$probMSK <- predict(model_MSK,newdata=data_SBC1, type="response")
data_ZUR1$probMSK <- predict(model_MSK,newdata=data_ZUR1, type="response")

#Probabilities with imputed model
data_MC1$probMSK1 <- predict(model_MSKimp,newdata=data_MC1, type="response")
data_SRF1$probMSK1 <- predict(model_MSKimp,newdata=data_SRF1, type="response")
data_PRVA1$probMSK1 <- predict(model_MSKimp,newdata=data_PRVA1, type="response")
data_clev1$probMSK1 <- predict(model_MSKimp,newdata=data_clev1, type="response")
data_SBC1$probMSK1 <- predict(model_MSKimp,newdata=data_SBC1, type="response")
data_ZUR1$probMSK1 <- predict(model_MSKimp,newdata=data_ZUR1, type="response")


ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probMSK,data_MC1)
ROC_plot_MC1

ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probMSK,data_SRF1)
ROC_plot_SRF1

ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probMSK,data_PRVA1)
ROC_plot_PRVA1

ROC_plot_clev1<- roc_plot(data_clev$my_data.bx_result,data_clev1$probMSK,data_clev1)
ROC_plot_clev1

ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probMSK,data_SBC1)
ROC_plot_SBC1

ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probMSK,data_ZUR1)
ROC_plot_ZUR1


# ROC with imputed model probabilities
ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probMSK1,data_MC1)
ROC_plot_MC1I

ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probMSK1,data_SRF1)
ROC_plot_SRF1I

ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probMSK1,data_PRVA1)
ROC_plot_PRVA1I

ROC_plot_clev1I<- roc_plot(data_clev1$my_data.bx_result,data_clev1$probMSK1,data_clev1)
ROC_plot_clev1I

ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probMSK1,data_SBC1)
ROC_plot_SBC1I

ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probMSK1,data_ZUR1)
ROC_plot_ZUR1I

#Inspecting the Distribution of the Imputed Data

xyplot(imputedMSK,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)


# PRVA Imputation
# MICE for cleveland imputation
#clevI- data for imputation
#sapply(data_DVA, function(x) sum(is.na(x)))

data_clev1 <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
data_PRVAI <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_MSKI <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]

#unique(data_clevI$my_data.race)

data_PRVAI<-data_PRVAI[,-10]
colSums(is.na(data_PRVAI))

library(mice)
init = mice(data_PRVAI, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("my_data.mrn")]=""
meth[c("my_data.pca")]=""
#meth[c("my_data.bx_ggs_cat")]="" 
meth[c("my_data.psa")]="" 
meth[c("my_data.dre_result")]="logreg" 
meth[c("my_data.age_bx")]="norm" 
meth[c("my_data.famhx1")]="logreg" 
meth[c("my_data.priorbx_neg")]="" 
meth[c("my_data.race")]="polyreg" 
meth[c("my_data.bx_result")]="" 

set.seed(103)
imputedPRVA = mice(data_PRVAI, method=meth, predictorMatrix=predM, m=5)

imputedPRVA1 <- complete(imputedPRVA,3)
imputedPRVA2 <- complete(imputedPRVA,4)
imputedPRVA3 <- complete(imputedPRVA,5)
imputedPRVA4 <- complete(imputedPRVA,1)
imputedPRVA5 <- complete(imputedPRVA,2)

save(file="imputed2.Rda")

sapply(imputedPRVA1, function(x) sum(is.na(x)))
#load("imputed2.Rda")

#Normal PRVA Data Model

model_PRVA<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                 +my_data.priorbx_neg+my_data.race, data=data_PRVAI,family = binomial)
summary(model_PRVA)

# Imputed Data Model 1
model_PRVAimp<- with(imputedPRVA, glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                                      +my_data.priorbx_neg+my_data.race, family = binomial))
summary(pool(object=model_PRVAimp))
#summary(model_PRVAimp)

model_PRVAimp1<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                     +my_data.priorbx_neg+my_data.race, data=imputedPRVA1,family = binomial)
summary(model_PRVAimp1)

model_PRVAimp2<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                     +my_data.priorbx_neg+my_data.race, data=imputedPRVA2,family = binomial)
summary(model_PRVAimp2)

model_PRVAimp3<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                     +my_data.priorbx_neg+my_data.race, data=imputedPRVA3,family = binomial)
summary(model_PRVAimp3)

model_PRVAimp4<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                     +my_data.priorbx_neg+my_data.race, data=imputedPRVA4,family = binomial)
summary(model_PRVAimp4)

model_PRVAimp5<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                     +my_data.priorbx_neg+my_data.race, data=imputedPRVA5,family = binomial)
summary(model_PRVAimp5)

library(MuMIn)
out.put<-model.sel(model_PRVAimp1,model_PRVAimp2,model_PRVAimp3,model_PRVAimp4,model_PRVAimp5) 
MA.ests<-model.avg(out.put, revised.var = TRUE) 
model.selection(object = out.put, revised.var = TRUE)

MA.ests$avg.model

#prob with original model
data_MC1$probPRVA <- predict(model_PRVA,newdata=data_MC1, type="response")
data_SRF1$proPRVA <- predict(model_PRVA,newdata=data_SRF1, type="response")
data_PRVA1$proPRVA <- predict(model_PRVA,newdata=data_PRVA1, type="response")
data_clev1$probPRVA <- predict(model_PRVA,newdata=data_clev1, type="response")
data_SBC1$probPRVA <- predict(model_PRVA,newdata=data_SBC1, type="response")
data_ZUR1$probPRVA <- predict(model_PRVA,newdata=data_ZUR1, type="response")

#Probabilities with imputed model
data_MC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_MC1, type="response")
data_MC1$probPRVA2 <- predict(model_PRVAimp2,newdata=data_MC1, type="response")
data_MC1$probPRVA3 <- predict(model_PRVAimp3,newdata=data_MC1, type="response")
data_MC1$probPRVA4 <- predict(model_PRVAimp4,newdata=data_MC1, type="response")
data_MC1$probPRVA5 <- predict(model_PRVAimp5,newdata=data_MC1, type="response")

#Average value

data_MC1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)


data_SRF1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
data_SRF1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
data_SRF1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
data_SRF1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
data_SRF1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")

data_SRF1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)




#data_PRVA1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_PRVA1, type="response")
data_clev1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
data_clev1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
data_clev1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
data_clev1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
data_clev1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")

data_clev1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)

data_SBC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
data_SBC1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
data_SBC1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
data_SBC1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
data_SBC1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")

data_SBC1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)



data_ZUR1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
data_ZUR1$probPRVA2 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
data_ZUR1$probPRVA3 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
data_ZUR1$probPRVA4 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")
data_ZUR1$probPRVA5 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")

data_ZUR1$probPRVAMean<- rowMeans(data_MC1[c('probPRVA1', 'probPRVA2','probPRVA3','probPRVA4','probPRVA5')], na.rm=TRUE)


ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVA,data_MC1)
ROC_plot_MC1

ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVA,data_SRF1)
ROC_plot_SRF1

ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVA,data_PRVA1)
ROC_plot_PRVA1

ROC_plot_clev1<- roc_plot(data_clev$my_data.bx_result,data_clev1$probPRVA,data_clev1)
ROC_plot_clev1

ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVA,data_SBC1)
ROC_plot_SBC1

ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA,data_ZUR1)
ROC_plot_ZUR1


# ROC with imputed model probabilities
ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVAMean,data_MC1)
ROC_plot_MC1I

ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVAMean,data_SRF1)
ROC_plot_SRF1I

ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVAMean,data_PRVA1)
ROC_plot_PRVA1I

ROC_plot_clev1I<- roc_plot(data_clev1$my_data.bx_result,data_clev1$probPRVAMean,data_clev1)
ROC_plot_clev1I

ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVAMean,data_SBC1)
ROC_plot_SBC1I

ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA1,data_ZUR1)
ROC_plot_ZUR1I

#Inspecting the Distribution of the Imputed Data

xyplot(imputedPRVA,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)
densityplot(imputedPRVA)
stripplot(imputedPRVA,pch=20,cex=1.2)


# PRVA Imputation with 5% rule
# MICE for cleveland imputation
#clevI- data for imputation
#sapply(data_DVA, function(x) sum(is.na(x)))

percentmiss <- function(x){sum(is.na(x))/length(x)*100}

# checking columns
apply(data_PRVAI,2,percentmiss)

data_clev1 <- my_data_rel[grep("CCF", my_data_rel$my_data.mrn),]
data_MC1 <- my_data_rel[grep("MC", my_data_rel$my_data.mrn),]
data_SRF1 <- my_data_rel[grep("SRF", my_data_rel$my_data.mrn),]
data_PRVAI1 <- my_data_rel[grep("PRVA", my_data_rel$my_data.mrn),]
data_MSKI <- my_data_rel[grep("MSK", my_data_rel$my_data.mrn),]
data_SBC1 <- my_data_rel[grep("SBC", my_data_rel$my_data.mrn),]
data_ZUR1 <- my_data_rel[grep("ZUR", my_data_rel$my_data.mrn),]

#unique(data_clevI$my_data.race)

data_PRVAI1<-data_PRVAI1[,-10]
colSums(is.na(data_PRVAI1))

percentmiss <- function(x){sum(is.na(x))/length(x)*100}

# checking columns
missingc<-apply(data_PRVAI1,2,percentmiss)

# checking rows
missingr<-apply(data_PRVAI1,1,percentmiss)
table(missingr)
table(missingc)

data_PRVAI2<-data_PRVAI1[missingr<15,]
data_PRVAI3<-data_PRVAI2[,missingc<20]
data_PRVAE1<-data.frame(data_PRVAI2[,missingc>20])
colnames(data_PRVAE1)<-c("my_data.famhx1")
data_PRVAE2<-data_PRVAI1[missingr>15,]

library(mice)
init = mice(data_PRVAI3, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("my_data.mrn")]=""
meth[c("my_data.pca")]=""
#meth[c("my_data.bx_ggs_cat")]="" 
meth[c("my_data.psa")]="" 
meth[c("my_data.dre_result")]="logreg" 
meth[c("my_data.age_bx")]="norm" 
#meth[c("my_data.famhx1")]="logreg" 
meth[c("my_data.priorbx_neg")]="" 
meth[c("my_data.race")]="polyreg" 
meth[c("my_data.bx_result")]="" 

set.seed(103)
imputedPRVAIU = mice(data_PRVAI3, method=meth, predictorMatrix=predM, m=2)

imputedPRVAIU1 <- complete(imputedPRVAIU,2)
save(file="imputed2.Rda")

completePRVA<-cbind(imputedPRVAIU1,data_PRVAE1)

completePRVA<-cbind(imputedPRVAIU1, data_PRVAE1)
completePRVA1<-rbind(completePRVA,data_PRVAE2)

sapply(completePRVA1, function(x) sum(is.na(x)))
#load("imputed2.Rda")

#Normal PRVA Data Model

model_PRVA<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                 +my_data.priorbx_neg+my_data.race, data=data_PRVAI,family = binomial)
summary(model_PRVA)

# Imputed Data Model 
model_PRVAimp<- with(imputedPRVA, glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                                      +my_data.priorbx_neg+my_data.race,family = binomial));pool(model_PRVAimp)
pool(object=model_PRVAimp)
summary(model_PRVAimp)

model_PRVAimp1<- glm(my_data.bx_result~as.numeric(my_data.age_bx)+my_data.psa+my_data.dre_result+my_data.famhx1
                     +my_data.priorbx_neg+my_data.race, data=completePRVA1,family = binomial)
summary(model_PRVAimp1)


#prob with original model
data_MC1$probPRVA <- predict(model_PRVA,newdata=data_MC1, type="response")
data_SRF1$proPRVA <- predict(model_PRVA,newdata=data_SRF1, type="response")
data_PRVA1$proPRVA <- predict(model_PRVA,newdata=data_PRVA1, type="response")
data_clev1$probPRVA <- predict(model_PRVA,newdata=data_clev1, type="response")
data_SBC1$probPRVA <- predict(model_PRVA,newdata=data_SBC1, type="response")
data_ZUR1$probPRVA <- predict(model_PRVA,newdata=data_ZUR1, type="response")

#Probabilities with imputed model
data_MC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_MC1, type="response")
data_SRF1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SRF1, type="response")
data_PRVA1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_PRVA1, type="response")
data_clev1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_clev1, type="response")
data_SBC1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_SBC1, type="response")
data_ZUR1$probPRVA1 <- predict(model_PRVAimp1,newdata=data_ZUR1, type="response")


ROC_plot_MC1<- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVA,data_MC1)
ROC_plot_MC1

ROC_plot_SRF1<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVA,data_SRF1)
ROC_plot_SRF1

ROC_plot_PRVA1<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVA,data_PRVA1)
ROC_plot_PRVA1

ROC_plot_clev1<- roc_plot(data_clev$my_data.bx_result,data_clev1$probPRVA,data_clev1)
ROC_plot_clev1

ROC_plot_SBC1<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVA,data_SBC1)
ROC_plot_SBC1

ROC_plot_ZUR1<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA,data_ZUR1)
ROC_plot_ZUR1


# ROC with imputed model probabilities
ROC_plot_MC1I <- roc_plot(data_MC1$my_data.bx_result,data_MC1$probPRVA1,data_MC1)
ROC_plot_MC1I

ROC_plot_SRF1I<- roc_plot(data_SRF1$my_data.bx_result,data_SRF1$probPRVA1,data_SRF1)
ROC_plot_SRF1I

ROC_plot_PRVA1I<- roc_plot(data_PRVA1$my_data.bx_result,data_PRVA1$probPRVA1,data_PRVA1)
ROC_plot_PRVA1I

ROC_plot_clev1I<- roc_plot(data_clev1$my_data.bx_result,data_clev1$probPRVA1,data_clev1)
ROC_plot_clev1I

ROC_plot_SBC1I<- roc_plot(data_SBC1$my_data.bx_result,data_SBC1$probPRVA1,data_SBC1)
ROC_plot_SBC1I

ROC_plot_ZUR1I<- roc_plot(data_ZUR1$my_data.bx_result,data_ZUR1$probPRVA1,data_ZUR1)
ROC_plot_ZUR1I

#Inspecting the Distribution of the Imputed Data

xyplot(imputedPRVA,my_data.bx_result ~ my_data.race+my_data.dre_result,pch=18,cex=1)
densityplot(imputedPRVA)
stripplot(imputedPRVA,pch=20,cex=1.2)

