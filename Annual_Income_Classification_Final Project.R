library(readr)
library(dplyr)
library(tidyverse)
library(metrics)
library(MASS)
library(pROC)
library(RColorBrewer)
library(caret)
library(InformationValue)
library(glmnet)


#selecting only numeric variables
College_Num <- select_if(CollegeScoreCard_New, is.numeric)
College_Num1 <- na.omit(College_Num)

# Converting meanearnings8yrs into Low and High
College_Num1$meanearnings8yrs<- as.factor(ifelse(College_Num1$meanearnings8yrs<=43000,"Low","High"))

#Split Data into Train and Test Dataset
set.seed(1234)
trainIndex <- createDataPartition(College_Num1$meanearnings8yrs, p = 0.7, list=FALSE, times = 1)
train <- College_Num1[ trainIndex,]
test <- College_Num1[-trainIndex,]

#Regression model 1
collegescorecard_model1 <- glm(meanearnings8yrs~.,data=train,family=binomial(link="logit"))

#Regression model 1 - using Step AIC for feature selection
collegescorecard_model2 <- stepAIC(collegescorecard_model1)

#comparing AIC values of Model 1 & 2
collegescorecard_model1$aic
collegescorecard_model2$aic

#summary of Model 2
summary(collegescorecard_model2)

#prediction
predicted <- predict(collegescorecard_model2,newdata=test,type='response')
test$meanearnings8yrs=as.factor(ifelse(test$meanearnings8yrs=="Low",1,0))

#confusion matrix
cf_matrix <- confusionMatrix(test$meanearnings8yrs, predicted)
cf_matrix

#Misclassification error
misClassError(test$meanearnings8yrs, predicted, threshold=optimal)

TruP <- cf_matrix$`0`[1]
FalN <- cf_matrix$`1`[1]
FalP <- cf_matrix$`0`[2]
TruN <- cf_matrix$`1`[2]

#Accuracy
Accuracy <- (TruP+TruN)/sum(cf_matrix)
Accuracy
#Precision
Precision <- TruP / (TruP+FalP)
Precision
#Recall
Recall <- TruP / (TruP+FalN)
Recall
#Specificity
Specificity <- TruN / (TruN+FalP)
Specificity

#ROC Curve
plotROC(test$meanearnings8yrs, predicted)

ROC_Curve =roc(test$meanearnings8yrs, predicted)
plot(ROC_Curve,col="darkblue",ylab="Sensitivity - TP rate", xlab="Specificity - FP rate")

#AUC Value
Area=auc(test$meanearnings8yrs, predicted)
Area
