## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(zoo)
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get rid of chest EDA and temp and all min max (underdispersion)
remove_select_cov = ALL_df %>%
  dplyr::select(-contains("ACC_wrist")) %>%
  dplyr::select(-EDA_mean, -EDA_sd, -EDA_max, -EDA_min) %>%
  dplyr::select(-Temp_mean, -Temp_sd, -Temp_max, -Temp_min) %>%
  dplyr::select(-contains("BVP")) %>%
  dplyr::select(-contains("ECG")) %>%
  dplyr::select(-contains("min")) %>%
  dplyr::select(-contains("max")) %>%
  dplyr::select(-contains("ACC_chest_X_mean")) %>%
  dplyr::select(-contains("ACC_chest_Y_mean")) %>%
  dplyr::select(-contains("ACC_chest_Z_mean")) %>%
  dplyr::select(-contains("ACC_chest_3D_mean")) 


remove_select_cov$Label <- ifelse(remove_select_cov$Label=="2", 1, 0)

non_biological = c("Label", "Subject")
remove_select_cov_nb = remove_select_cov[ , !(names(remove_select_cov) %in% non_biological)]

scaled_select_cov_nb = scale(remove_select_cov_nb, center = TRUE, scale = TRUE)
res.pca <- PCA(scaled_select_cov_nb, graph = FALSE, ncp = 4)
pca_vals <- as.data.frame(res.pca$ind$coord)
pca_vals$Label = remove_select_cov$Label
pca_vals$Subject = remove_select_cov$Subject



## ----pca------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
combocvoutput = kfoldcv_no_subject(pca_vals)


## ----combo-kfold-res, include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
combo_acc_per_fold = combocvoutput %>%
  group_by(fold_no) %>%
  summarize(accuracy = mean(actual_values==predicted_values))

combo_f1_per_fold = combocvoutput %>%
  group_by(fold_no) %>%
  summarize(f1 = F1_Score(actual_values, predicted_values, positive = NULL)) 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Overall Accuracy of the model with chest and wrist sensor data across five stratified folds:")
percentage = round(mean(combo_acc_per_fold$accuracy), 2)*100
print(paste0(percentage,"%"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("---------------------------------------------------------------------------------------------")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Overall F-1 Score of the model with chest and wrist sensor data across five stratified folds:")
percentage = round(mean(combo_f1_per_fold$f1), 2)*100
print(paste0(percentage,"%"))



