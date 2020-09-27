## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(zoo)
#library(varian)
library(tidyverse)
library(glmnet)
library(FactoMineR)
library(factoextra)
library(MASS)
library(caret)
library(MLmetrics)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL_df = read.csv("/hpc/group/sta440-f20/mr328/master_df.csv", header = T)[,-c(1:2)]
#ALL_df = read.csv("master_df.csv", header = T)[,-c(1:2)] #REPLACE THIS LINE


## ----wristcovariates------------------------------------------------------------------------------------------------------------------------------------------------------------
wrist_only = ALL_df %>%
  dplyr::select(contains("wrist")) %>%
  dplyr::select(-contains("ACC_wrist_x_mean")) %>%
  dplyr::select(-contains("ACC_wrist_y_mean")) %>%
  dplyr::select(-contains("ACC_wrist_z_mean")) %>%
  dplyr::select(-contains("ACC_wrist_3D_mean"))

wrist_only = cbind(wrist_only, as.data.frame(ALL_df$Subject), as.data.frame(ALL_df$Label))
wrist_only = wrist_only %>%
  rename(Subject = `ALL_df$Subject`) %>%
  rename(Label = `ALL_df$Label`) %>%
  mutate(Label = ifelse(Label==2, 1, 0)) %>%
  dplyr::select(-contains("min")) %>%
  dplyr::select(-contains("max")) 

non_biological = c("Label", "Subject")
wrist_nb = wrist_only[ , !(names(wrist_only) %in% non_biological)]


## ----pca------------------------------------------------------------------------------------------------------------------------------------------------------------------------
scaled_wrist_nb = scale(wrist_nb, center = TRUE, scale = TRUE)
res.pca.wrist <- PCA(scaled_wrist_nb, graph = FALSE, ncp = 5)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
wrist_pca_vals <- as.data.frame(res.pca.wrist$ind$coord)
wrist_pca_vals$Label = wrist_only$Label
wrist_pca_vals$Subject = wrist_only$Subject


## ----pcakfold-------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

kfoldcv_no_subject = function(df) {
  
  actual_values = c()
  predicted_values = c()
  fitted_probs = c()
  fold_no = c()
  subject = c()
  
  folds <- createFolds(factor(df$Subject), k = 5)
  
  for (i in 1:5) {
    col = paste0("Fold", i)
    indx = unlist(folds[col])

    train<- df[-indx,]
    test<- df[indx, ]

  
  
    pca.model = glm(Label ~ .-Subject, family = binomial(link="logit"), data=train)

  pred.test = pca.model %>% predict(test, type= "response")
  fitted_probs = c(fitted_probs, pred.test)
  pred.test = ifelse(pred.test>0.5, 1,0)
  predicted_values = c(predicted_values, pred.test)
  actual_values = c(actual_values, test$Label)

  subject = c(subject, test$Subject)
  fold_no = c(fold_no, rep(i, nrow(test)))
  
}

  
  cvoutput = data.frame(cbind(actual_values, predicted_values, fitted_probs, 
        fold_no, subject))
  
  return(cvoutput)
}



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
wristcvoutput = kfoldcv_no_subject(wrist_pca_vals)



## ---- include = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
wrist_acc_per_fold = wristcvoutput %>%
  group_by(fold_no) %>%
  summarize(accuracy = mean(actual_values==predicted_values))

wrist_f1_per_fold = wristcvoutput %>%
  group_by(fold_no) %>%
  summarize(f1 = F1_Score(actual_values, predicted_values, positive = NULL)) 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Overall Accuracy of the model with ONLY wrist sensor data across five stratified folds:")
percentage = round(mean(wrist_acc_per_fold$accuracy), 2)*100
print(paste0(percentage,"%"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("---------------------------------------------------------------------------------------------")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Overall F-1 Score of the model with ONLY wrist sensor data across five stratified folds:")
percentage = round(mean(wrist_f1_per_fold$f1), 2)*100
print(paste0(percentage,"%"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("pca_wrist_model.Rmd", output = "pca_wrist_model.R")

