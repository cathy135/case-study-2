## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(zoo)
#library(varian)
library(tidyverse)
library(glmnet)
library(FactoMineR)
library(factoextra)
library(MASS)
library(caret)
library(MLmetrics)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL_df = read.csv("/hpc/group/sta440-f20/mr328/master_df.csv", header = T)[,-c(1:2)]
#ALL_df = read.csv("master_df.csv", header = T)[,-c(1:2)] #REPLACE THIS LINE


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----pca-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
scaled_select_cov_nb = scale(remove_select_cov_nb, center = TRUE, scale = TRUE)
res.pca.full <- PCA(scaled_select_cov_nb, graph = FALSE, ncp = 4)


## ----pca-vars--------------------------------------------------------------------------------------------------------------------------------------------------------------------
pca_vals <- as.data.frame(res.pca.full$ind$coord)
pca_vals$Label = remove_select_cov$Label
pca_vals$Subject = remove_select_cov$Subject


## ---- warning = F----------------------------------------------------------------------------------------------------------------------------------------------------------------
# HOPEFULLY THE RIGHT ONE model with no main effect for Subject but with interactions
pca.model.interact = glm(Label ~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.1:Subject
                  +Dim.2:Subject
                  +Dim.3:Subject
                  +Dim.4:Subject
                 , family = binomial(link="logit"), data=pca_vals)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Below, we show the coefficients for the interaction terms between each PCA component and Subject, as a means of quantifying heterogeneity among subjects in the relationship between affective state and physiological characteristics. We also note the standard errors for the estimtaes. These values can be verified in Table __ in the appendix of our report. For an interpretation of these values, see section _____")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
coeff_df <- as.data.frame(summary(pca.model.interact)$coefficients[,])
coeff_df$ID <- seq.int(nrow(coeff_df))
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
interaction_coeff <- coeff_df %>%
  filter(ID > 5) %>%
  select(Estimate, `Std. Error`)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(interaction_coeff)




