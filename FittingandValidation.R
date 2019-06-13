rm(list=ls())
load("baseline_final_select.Rdata") # SELECT TRIAL
load("baseline_final_plco.Rdata") # PLCO TRIAl
###============= 0. packeges we need for the calculations:
library("ISLR")
library(Amelia)
library(mlbench)
library(corrplot)
library(ResourceSelection)
library(pROC)
library(zoo)
library(ggplot2)
library(questionr)
library(dplyr)
library(caret)
###============= 1. Data for the model =====
# SELECT
baseline_final_select <- baseline_final_select[,c("pca", "age","dre", "PSA", "race", "PROSBX", "fampca")]
# Deleting those people with unknown family history or unknown prior biopsy + unknown race
baseline_final_select <- subset(baseline_final_select, baseline_final_select$PROSBX != 2)
baseline_final_select <- subset(baseline_final_select, baseline_final_select$fampca != "Unknown")
baseline_final_select <- subset(baseline_final_select, baseline_final_select$race != "Unknown")
# update the levels for each variable 
baseline_final_select$race <- factor(baseline_final_select$race)
baseline_final_select$fampca <- factor(baseline_final_select$fampca)
baseline_final_select$PROSBX <- factor(baseline_final_select$PROSBX)
# 28132 people in the end

# PLCO 
baseline_final_plco <- baseline_final_plco[,c("pca", "age","dre", "PSA", "race", "PROSBX", "fampca")]
# Deleting those people with unknown family history or unknown prior biopsy + unknown race
baseline_final_plco <- subset(baseline_final_plco, baseline_final_plco$PROSBX != 2)
baseline_final_plco <- subset(baseline_final_plco, baseline_final_plco$fampca != "Unknown")
baseline_final_plco <- subset(baseline_final_plco, baseline_final_plco$race != "Unknown")
# update the levels for each variable 
baseline_final_plco$race <- factor(baseline_final_plco$race)
baseline_final_plco$fampca <- factor(baseline_final_plco$fampca)
baseline_final_plco$PROSBX <- factor(baseline_final_plco$PROSBX)
# 32843


# set factor variables to have pretty, meaniningful values
levels(baseline_final_select$fampca)[1:2] <- c("No cancer","Cancer")
levels(baseline_final_plco$fampca)[1:2] <- c("No cancer","Cancer")
levels(baseline_final_plco$dre) <- c("Normal","Suspicious")
levels(baseline_final_select$dre) <- c("Normal","Suspicious")
levels(baseline_final_plco$PROSBX) <- c("No","Yes")
levels(baseline_final_select$PROSBX) <- c("No","Yes")

###==== 2. Modell for SELECT =====
model_select <- glm(pca~age+PSA+fampca+PROSBX+race+dre,
                    data=baseline_final_select,
                    family = binomial)
summary(model_select)
###==== 3. Modell for PLCO =====
model_plco <- glm(pca~age+PSA+fampca+PROSBX+race+dre, 
                  data=baseline_final_plco, family = binomial, 
                  control = list(epsilon = 1e-10,maxit=100,trace=T))

model_plco_1 <- glm(pca~age+PSA+fampca+PROSBX+race+dre, 
                  data=baseline_final_plco, family = binomial,
                  weights = model_plco$weights,
                  control = list(epsilon = 1e-10,maxit=100,trace=T))
confint(model_plco_1)
###===== 4. Predictions  =====

#  Prediction with the model_select on PLCO
baseline_final_plco$probs <- predict(model_select,
                                               newdata = baseline_final_plco,
                                               type = "response")

#  Prediction with the model_plco on SELECT
baseline_final_select$probs <- predict(model_plco_1,
                                                 newdata = baseline_final_select,
                                                 type = "response")

###===== 5. Functions for plotting the Calibration and Discriminations plots  =====
# 5.1. function for plotting the Lemeshow - Hosmer plot (observed risk vs. predicted risk)
plot.result <- function(val.res,val.prob) {
  # Calculate the deciles (10% lowest value, 20 % lowest value, .) of
  # the predicted risks: p1 ,., p9 and make intervals
  qq.val <- unique(quantile(val.prob, probs = seq(0, 1, 1/10)))
  cutyhat.val <- cut(val.prob, breaks = qq.val, include.lowest = TRUE)
  # Calculate the observed number of people in each of the intervals (y0 + y1) 
  # and the observed number of events (cancer cases) (y1)
  observed.val <- xtabs(cbind(y0 = 1 - (as.numeric(val.res)-1), 
                              y1 = as.numeric(val.res)-1) ~ cutyhat.val)
  y0.val <- observed.val[,1]
  y1.val <- observed.val[,2]
  # Calculate the average predicted risk expected/(y1 + y0)
  expected.val <- xtabs(val.prob ~ cutyhat.val)
  #observed risk = O(i)/n(i), O = y1, n = (y0 + y1)
  
  plot.data <- data.frame(obs = y1.val/(y0.val + y1.val), 
                              exp = (expected.val)/(y1.val + y0.val))
  
  plot.data$trial <- c(rep("Validation cohort",10))
  
  #Plot
  ggplot() + xlab("Predicted risk") + ylab("Observed risk") +
    geom_point(data = plot.data, aes(exp.Freq, obs,col=trial), 
               size = 3) +
    geom_line(aes(c(0, 1), c(0, 1)), linetype = 2, 
              color = 'grey50') +
    coord_cartesian(ylim = c(0, 0.65),xlim = c(0, 0.65))+
    theme_bw() +
    guides(fill=guide_legend(title="")) +
    theme(legend.title=element_blank(),
          legend.position=c(0.2,0.8),
          legend.box.background = element_rect())
  
}

# 5.2.  Function for plotting the ROC curve and calculating the AUC
roc_plot <- function(response, prob, data){
  # calculating the specificity and sensitivity 
  ROC <- roc(response ~prob, data=data)
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

# 5.3 Function for plotting the odds ratio 
# 5.3.1 function for plotting the odds ratio for 2 diffrent models 
plot_odds_2models <- function(model1, model2, x, y)
{
  # first model (model1)
  odds <- odds.ratio(model1)
  odds <- odds[-1,] #removing the intercept
  variables <- c("age (in 10 years)", "PSA (log2)", 
                 "Family history" ,"prior negative biobsy", 
                 "race", "DRE (log)")
  odds$trial <- x # which trail 1 for SELECT and 2 for PLCO
  
  # change the age in 10 years
  odds["age","2.5 %"] <- odds["age","OR"]+ 
    ((odds["age","2.5 %"]-odds["age","OR"])*10)
  odds["age","97.5 %"] <- odds["age","OR"]+ 
    ((odds["age","97.5 %"]-odds["age","OR"])*10)
  
  # change the PSA in log2 and DRE in log scale for better plots
  odds["PSA",c("OR","2.5 %","97.5 %")] <- log2(odds["PSA",c("OR","2.5 %","97.5 %")])
  odds["dre1",c("OR","2.5 %","97.5 %")] <- log(odds["dre1",c("OR","2.5 %","97.5 %")])
  
  odds$variables <- variables
  
  # second model
  odds_1 <- odds.ratio(model2)
  odds_1 <- odds_1[-1,] #removing the intercept
  variables <- c("age (in 10 years)", "PSA (log2)",
                 "Family history" ,"prior negative biobsy",
                 "race", "DRE (log)")
  odds_1$trial <- y
  odds_1["age","2.5 %"] <- odds_1["age","OR"]+
    ((odds_1["age","2.5 %"]-odds_1["age","OR"])*10)
  odds_1["age","97.5 %"] <- odds_1["age","OR"]+
    ((odds_1["age","97.5 %"]-odds_1["age","OR"])*10)
  
  odds_1["PSA",c("OR","2.5 %","97.5 %")] <- log2(odds_1["PSA",c("OR","2.5 %","97.5 %")])
  odds_1["dre1",c("OR","2.5 %","97.5 %")] <- log(odds_1["dre1",c("OR","2.5 %","97.5 %")])
  odds_1$variables <- variables
  odds <- bind_rows(odds, odds_1)
  odds$trial <- ifelse(odds$trial == 1, "SELECT", "PLCO")
  odds$p <- NULL
  # creating the plot
  ggplot(odds, aes(x = odds$OR,
                   y = odds$variable,
                   colour=odds$trial)) +
    geom_errorbarh(aes(xmax = odds$`97.5 %`,
                       xmin = odds$`2.5 %`),
                   size = .5, height = .2)+
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed")+
    geom_point(size = 1.5, colour = "black") +
    xlab("Odds ratio")+
    ylab("")+  labs(title = "Odds ratio",subtitle = "")+
    scale_x_log10(breaks = c(seq(0,1,0.5),seq(1,10,4)))+
    theme_bw() +
    guides(fill=guide_legend(title="")) +
    theme(legend.title=element_blank(),
          legend.position=c(0.8,0.8),
          legend.box.background = element_rect())
  
}

# 5.3.2 function for plotting the odds ratio for 1  models 
plot_odds_1model <- function(model1, x)
{
  odds <- odds.ratio(model1)
  odds <- odds[-1,] #removing the intercept
  variables <- c("age (in 10 years)", "PSA (log2)", 
                 "Family history" ,"prior negative biobsy", 
                 "race", "DRE (log)")
  rownames(odds)<- variables
  odds$trial <- x # which trail 1 for SELECT and 2 for PLCO
  
  # change the age in 10 years
  odds["age (in 10 years)","2.5 %"] <- odds["age (in 10 years)","OR"]+ 
    ((odds["age (in 10 years)","2.5 %"]-odds["age (in 10 years)","OR"])*10)
  odds["age (in 10 years)","97.5 %"] <- odds["age (in 10 years)","OR"]+ 
    ((odds["age (in 10 years)","97.5 %"]-odds["age (in 10 years)","OR"])*10)
  
  # change the PSA in log2 and DRE in log scale for better plots
  odds["PSA (log2)",c("OR","2.5 %","97.5 %")] <- log2(odds["PSA (log2)",c("OR","2.5 %","97.5 %")])
  odds["DRE (log)",c("OR","2.5 %","97.5 %")] <- log(odds["DRE (log)",c("OR","2.5 %","97.5 %")])
  
  odds$variables <- variables
  
  odds$trial <- ifelse(odds$trial == 1, "SELECT", "PLCO")
  odds$p <- NULL
  # creating the plot
  ggplot(odds, aes(x = odds$OR,
                   y = odds$variable)) +
    geom_errorbarh(aes(xmax = odds$`97.5 %`,
                       xmin = odds$`2.5 %`),
                   size = .5, height = .2)+
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed")+
    geom_point(size = 1.5, colour = "black") +
    xlab("Odds ratio")+
    ylab("")+  labs(title = "Odds ratio",subtitle = "subsampling model based on PLCO")+
    scale_x_log10(breaks = c(seq(0,1,0.5),seq(1,10,4)))+
    theme_bw() +
    guides(fill=guide_legend(title="")) +
    theme(legend.title=element_blank(),
          legend.position=c(0.8,0.8),
          legend.box.background = element_rect())
}

###===== 6. Calibration and Discrimination for the PLCO and SELECT MODEL (normal) =====

# 6.1. Calculating the hoslem.test
hl_select <- hoslem.test(as.numeric(baseline_final_plco$pca)-1,
                         baseline_final_plco$probs, g=10)
# X-squared = 1678.6, df = 8, p-value < 2.2e-16

hl_plco <- hoslem.test(as.numeric(baseline_final_select$pca)-1, 
                       baseline_final_select$probs, g=10)
# X-squared = 1235.6, df = 8, p-value < 2.2e-16

# 6.2 Plotting
Lemeshow_select <- plot.result(baseline_final_plco$pca,
                               baseline_final_plco$probs)+
  labs(title = "Calibration",
       subtitle = "Lemeshow - Hosmer for Model based on SELECT data")

Lemeshow_plco <- plot.result(baseline_final_select$pca, 
                             baseline_final_select$probs)+
  labs(title = "Calibration",
       subtitle = "Lemeshow - Hosmer for Model based on PLCO data")

roc_plot_plco <- roc_plot(baseline_final_select$pca, # response
                baseline_final_select$probs, # probability
                baseline_final_select)

roc_plot_select <- roc_plot(baseline_final_plco$pca, # response
               baseline_final_plco$probs, # probability
               baseline_final_plco)

odds_both_models <- plot_odds_2models(model_select, model_plco_1,1, 2)

# 6.3 saving the plots 

pdf(file = "roc_select.pdf")
roc_plot_select
dev.off()
pdf(file = "heslem_select.pdf")
Lemeshow_select
dev.off()
pdf(file = "odds_both.pdf")
odds_both_models
dev.off()
pdf(file = "roc_plco.pdf")
roc_plot_plco
dev.off()
pdf(file = "heslem_plco.pdf")
Lemeshow_plco
dev.off()




###===== 7. Subsampling of the PLCO Data ===============
# 7.1 Create a training set and a test set for the model
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999) 
set.seed(100)
baseline_final_plco$index <- c(seq(1,nrow(baseline_final_plco),1)) # create a index
trainDataIndex <- createDataPartition(baseline_final_plco$index, p=0.7, list = F)  # 70% training data
plco_subsampling_trainData <- baseline_final_plco[trainDataIndex, ]
plco_subsampling_testData <- baseline_final_plco[-trainDataIndex, ]

# saving the data, otherwise its always different
save(plco_subsampling_trainData, file = "plco_subsampling_trainData.Rdata")
save(plco_subsampling_testData, file = "plco_subsampling_testData.Rdata")
load("plco_subsampling_trainData.Rdata" )
load("plco_subsampling_testData.Rdata")

# 7.2 Creating the model with the 70 % of data
model_plco_sub <- glm(pca~age+PSA+fampca+PROSBX+race+dre,
                      data=plco_subsampling_trainData,
                      family = binomial)

#summary(model_plco_sub)

# make predictions on SELECT and the test set of plco
baseline_final_select$prob_subsampling <- predict(model_plco_sub,
                                                  newdata=baseline_final_select, 
                                                  type="response")
#plco_subsampling_testData$prob_subsampling <- predict(model_plco_sub,
                                                   #   newdata=plco_subsampling_testData, 
                                                   #   type="response")
# 7.3 Testing if the model is good (with prediction on SELECT)
# ROC curve for the subsampling model
g_subsampling<- roc_plot(baseline_final_select$pca, # response
         baseline_final_select$prob_subsampling, # probability
         baseline_final_select)
# Calibration (Heslem ... )
p_subsampling <- plot.result(baseline_final_select$pca, baseline_final_select$prob_subsampling)+
  labs(title = "Calibration",subtitle = "Lemeshow - Hosmer for subsampling Model based on PLCO data")

hoslem.test(as.numeric(baseline_final_select$pca)-1, 
            baseline_final_select$prob_subsampling, g=10)
# X-squared = 866.09, df = 8, p-value < 0.00000000000000022
# odds ratio for subsampling model
odds_sub <- plot_odds_1model(model_plco_sub, 2) 


pdf("model_subsampling_select_data.pdf")
g_subsampling
p_subsampling
odds_sub
dev.off()



pdf(file = "roc_sub_plco.pdf")
g_subsampling
dev.off()
pdf(file = "heslem_sub_plco.pdf")
p_subsampling
dev.off()
pdf(file = "odds_sub_plco.pdf")
odds_sub
dev.off()

###===== 8. smooth lowess method for calibration plots #####

# Plot for Model SELECT:

#y1 <- predict(loess(as.numeric(as.character(baseline_final_plco$pca))~baseline_final_plco$probs), se =T)
#save(y1, file ="loess_select_model.Rdata")
load("loess_select_model.Rdata")
y1 <- y
g1 <- ggplot(baseline_final_plco, aes(x = probs, fill = pca)) + 
  geom_histogram(aes(y=..count../sum(..count..)),
                 data = subset(baseline_final_plco, baseline_final_plco$pca ==1),
                 bins = 10) +
  geom_point(aes(baseline_final_plco$probs, y1$fit), size  =1)+
  geom_line(aes(y = y1$fit-1.96*y1$se.fit,x = baseline_final_plco$probs),
            lty="dashed", col="blue", lwd=1)+
  geom_line(aes(y = y1$fit+1.96*y1$se.fit,x = baseline_final_plco$probs),
            lty="dashed", col="blue", lwd=1)+
  geom_abline(aes(intercept = 0,slope = 1), size = .5, linetype = "dashed")+
  xlab("risk %") + ylab("observed percentage with prostate cancer") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = ""))+
  labs(title = "Smooth lowess method for calibration plots",subtitle = "Model based on SELECT data")+
  theme(legend.position="none")

# Plot for Model PLCO
#y2 <- predict(loess(as.numeric(as.character(baseline_final_select$pca))~baseline_final_select$probs),se =T)
#save(y2, file ="loess_plco_model.Rdata")
load("loess_plco_model.Rdata")

g2 <-ggplot(baseline_final_select, aes(x = probs, fill = pca)) + 
  geom_histogram(aes(y=..count../sum(..count..)),
                 data = subset(baseline_final_select, baseline_final_select$pca ==1),
                 bins = 10) +
  geom_point(aes(baseline_final_select$probs, y2$fit), size  =1)+
  geom_line(aes(y = y2$fit-1.96*y2$se.fit,x = baseline_final_select$probs),
            lty="dashed", col="blue", lwd=1)+
  geom_line(aes(y = y2$fit+1.96*y2$se.fit,x = baseline_final_select$probs),
            lty="dashed", col="blue", lwd=1)+
  geom_abline(aes(intercept = 0,slope = 1), size = .5, linetype = "dashed")+
  xlab("risk %") + ylab("observed percentage with prostate cancer") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = ""))+
  labs(title = "Smooth lowess method for calibration plots",subtitle = "Model based on PLCO data")+
  theme(legend.position="none")

# Plot for Model subsampling
baseline_final_select$index <- c(seq(1,nrow(baseline_final_select),1)) # create a index
index <- createDataPartition(baseline_final_select$index, p=0.7, list = F) 
df <- baseline_final_select[index,]
#y3 <-predict(loess(as.numerc(as.character(baseline_final_select$pca))~
      #            baseline_final_select$prob_subsampling),se =T)
loess_subsampling <- loess(as.numeric(as.character(df$pca))~df$prob_subsampling)
y4 <- predict(loess_subsampling, se = T)
save(y4, file ="loess_plco_subsamplingmodel_2.Rdata")
load("loess_plco_subsamplingmodel_2.Rdata")

g3 <- ggplot(df, aes(x = prob_subsampling, fill = pca)) + 
  geom_histogram(aes(y=..count../sum(..count..)),
                 data = subset(df, df$pca ==1),
                 bins = 10) +
  geom_point(aes(df$prob_subsampling, y4$fit), size  =1)+
  geom_line(aes(y = y4$fit-1.96*y4$se.fit,x = df$prob_subsampling),
            lty="dashed", col="blue", lwd=1)+
  geom_line(aes(y = y4$fit+1.96*y4$se.fit,x = df$prob_subsampling),
            lty="dashed", col="blue", lwd=1)+
  geom_abline(aes(intercept = 0,slope = 1), size = .5, linetype = "dashed")+
  xlab("risk %") + ylab("observed percentage with prostate cancer") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = ""))+
  labs(title = "Smooth lowess method for calibration plots",subtitle = "Model based on PLCO (with subsampling)")+
  theme(legend.position="none")

pdf(file = "smooth_select.pdf")
g1
dev.off()
pdf(file = "smooth_plco.pdf")
g2
dev.off()
pdf(file = "smooth_subsampling.pdf")
g3
dev.off()



