library(here)
library(FedData)
pkg_test("zoo")
pkg_test("varian")
pkg_test("tidyverse")
pkg_test("corrplot")
pkg_test("glmnet")
pkg_test("FactoMineR")
pkg_test("factoextra")
pkg_test("MASS")
pkg_test("caret")
pkg_test("MLmetrics")
pkg_test("pROC")
pkg_test("gridExtra")
pkg_test("grid")
pkg_test("broom")
pkg_test("knitr")
pkg_test("sjPlot")
pkg_test("RColorBrewer")
pkg_test("kableExtra")

Sense_Final <- function(PCAdim, win_size){
  
  win_size = 5
  PCAdim = 5
  
  ############################
  #### Define Window Size ####
  ############################
  features_all_modalities = function(fn, win_size, shift) {
    
    print(fn)
    df_all = read.csv(fn) %>%
      mutate(ACC_chest_3D = sqrt(ACC_chest_X^2+ACC_chest_Y^2+ACC_chest_Z^2)) %>%
      mutate(ACC_wrist_3D = sqrt(ACC_wrist_x^2+ACC_wrist_y^2+ACC_wrist_z^2))
    drops <- c("X", "Label", "subject")
    df = df_all[ , !(names(df_all) %in% drops)]
    replace_rows =  length(rollapply(df[,1], width = win_size*4, by = shift, FUN = mean, align = "left"))
    features_df <- data.frame(matrix(ncol = ncol(df)*4, nrow = replace_rows))
    new_names = sapply(1:length(df), function(c) {
      c(paste0(colnames(df)[c],"_mean"),
        paste0(colnames(df)[c],"_sd"),
        #paste0(colnames(df)[c],"_range"),
        paste0(colnames(df)[c],"_min"),
        paste0(colnames(df)[c],"_max")
        # paste0(colnames(df)[c],"_skew")
      )
    })
    colnames(features_df) = new_names
    for (c in 1:length(df)) {
      # finding mu
      mu_vals = rollapply(df[,c], width = win_size*4, by = shift, FUN = mean, align = "left")
      new_col_name = paste0(colnames(df)[c],"_mean")
      cindx = which(colnames(features_df)==new_col_name)
      features_df[, cindx] = mu_vals
      # finding sd
      sd_vals = rollapply(df[,c], width = win_size*4, by = shift, FUN = var, align = "left")
      new_col_name = paste0(colnames(df)[c],"_sd")
      cindx = which(colnames(features_df)==new_col_name)
      features_df[, cindx] = sqrt(sd_vals)
      # finding max
      max_vals = rollapply(df[,c], width = win_size*4, by = shift, FUN = max, align = "left")
      new_col_name = paste0(colnames(df)[c],"_max")
      cindx = which(colnames(features_df)==new_col_name)
      features_df[, cindx] = max_vals
      # finding min
      min_vals = rollapply(df[,c], width = win_size*4, by = shift, FUN = min, align = "left")
      new_col_name = paste0(colnames(df)[c],"_min")
      cindx = which(colnames(features_df)==new_col_name)
      features_df[, cindx] = min_vals
    }
    # make sure merge label, subject back into feature dataframe
    features_df$Label = df_all[1:replace_rows, "Label"]
    features_df$Subject = df_all[1:replace_rows, "subject"]
    return(features_df)
  } 
  
  
  
  ############################
  ###### HR Upsampling ####### 
  ############################
  HR_calc = function(df, win_size, shift) {
    df <- df[rep(seq_len(nrow(df)), each = 4), ]
    mu_vals = rollapply(df, width = win_size*4, by = shift, FUN = mean, align = "left")
    sd_vals = rollapply(df, width = win_size*4, by = shift, FUN = sd, align = "left")
    min_vals = rollapply(df, width = win_size*4, by = shift, FUN = min, align = "left")
    max_vals = rollapply(df, width = win_size*4, by = shift, FUN = max, align = "left")
    return(list(mu = mu_vals, sd = sd_vals,
                min = min_vals, max = max_vals))
  }
  
  ############################
  ######## Make Data ######### 
  ############################
  
  file_list <- list.files()
  subject_data = file_list[grepl("df_S", file_list, fixed = TRUE)]
  ALL_df = NULL
  
  for (i in 1:length(subject_data)) {
    
    feature_data = features_all_modalities(subject_data[i], win_size, 1) #CHANGE FILE PATH 
    subject_no = gsub("\\..*","", sub('.*_', '', subject_data[i]))
    
    HR_fn = paste0("~/case-study-2/WESAD/", subject_no, "/","HR.csv") 
    
    #CHANGE FILEPATH to be : paste0("/hpc/group/sta440-f20/WESAD/WESAD/", subject_no, "/" )
    HR_data = read.csv(HR_fn)[-1,]
    hr = HR_calc(as.data.frame(HR_data), 5, 1)
    df_hr = data.frame(matrix(unlist(hr), nrow= length(hr$mu),
                              ncol=4, byrow = F))
    colnames(df_hr) = c("hr_wrist_mu","hr_wrist_sd", "hr_wrist_min", "hr_wrist_max")#, "hr_wrist_range")
    df_hr$ID =seq.int(nrow(df_hr))
    
    feature_data = feature_data[-c(1:40),]
    feature_data$ID <- seq.int(nrow(feature_data))
    S_df = merge(feature_data, df_hr, by="ID")
    ALL_df = rbind(ALL_df, S_df)
  }
  ALL_df = ALL_df %>% filter(Label %in% c("2","3"))
  
  
  # Don't Save your f*king Data   write.csv(ALL_df, "master_df.csv")
  # And don't load your f*cking Data ALL_df = read.csv("master_df.csv", header = T)[,-c(1:2)]
  
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
  # maybe reorder the variables
  col_order <- c("ACC_chest_X_sd", "ACC_chest_Y_sd", "ACC_chest_Z_sd", "ACC_chest_3D_sd", "EMG_mean", "EMG_sd", "Resp_mean", "Resp_sd", "EDA_wrist_mean", "EDA_wrist_sd", "TEMP_wrist_mean", "TEMP_wrist_sd",  "hr_wrist_mu", "hr_wrist_sd", "Subject", "Label")
  remove_select_cov <- remove_select_cov[, col_order]
  remove_select_cov$Subject <- factor(remove_select_cov$Subject, levels = c("S2", "S3","S4", "S5","S6", "S7","S8", "S9","S10", "S11","S13", "S14","S15", "S16","S17"))
  remove_select_cov$Label <- ifelse(remove_select_cov$Label=="2", 1, 0)
  non_biological = c("Label", "Subject")
  remove_select_cov_nb = remove_select_cov[ , !(names(remove_select_cov) %in% non_biological)]
  
  ############################
  ########## Do PCA ########## 
  ############################
  scaled_select_cov_nb = scale(remove_select_cov_nb, center = TRUE, scale = TRUE)
  res.pca <- PCA(scaled_select_cov_nb, graph = FALSE, ncp = PCAdim)
  
  pca_vals <- as.data.frame(res.pca$ind$coord)
  pca_vals$Label = remove_select_cov$Label
  pca_vals$Subject = remove_select_cov$Subject
  
  set.seed(123)
  kfoldcv_final = function(df) {
    
    actual_values = c()
    predicted_values = c()
    fitted_probs = c()
    
    # fold_accuracies = c()
    # fold_f1 = c()
    
    fold_no = c()
    subject = c()
    
    print("at folds")
    folds <- createFolds(factor(df$Subject), k = 5)
    
    print("past folds")
    for (i in 1:5) {
      #train and test
      col = paste0("Fold", i)
      indx = unlist(folds[col])
      train<- df[-indx,]
      print(nrow(train))
      test<- df[indx, ]
      print(nrow(test))
      
      
      # run model
      pca.model.interact = glm(Label ~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.1:Subject
                               +Dim.2:Subject
                               +Dim.3:Subject
                               +Dim.4:Subject
                               , family = binomial(link="logit"), data=train)
      #summary(wrist.full)
      pred.test = pca.model.interact %>% predict(test, type= "response")
      fitted_probs = c(fitted_probs, pred.test)
      pred.test = ifelse(pred.test>0.5, 1,0)
      predicted_values = c(predicted_values, pred.test)
      actual_values = c(actual_values, test$Label)
      subject = c(subject, test$Subject)
      fold_no = c(fold_no, rep(i, nrow(test)))
      
      
      
      #acc1 = mean(test1$Label==pred.test)
      #fold_accuracies = c(fold_accuracies, acc1)
      
      # add function to compute f1
      # blah
    }
    #train on training set
    #test on testing set 
    #predict labels
    #find accuracy and f1, add to fold_accuracies and fold_f1
    #put predicted values in predicted_values
    #put actual values in actual_values
    
    cvoutput = data.frame(cbind(actual_values, predicted_values, fitted_probs, 
                                fold_no, subject))
    
    return(cvoutput)
  }
  #combocvoutput = kfoldcv_no_subject(pca_vals)
  finalcvoutput = kfoldcv_final(pca_vals)
  
  
  final_acc_per_fold = finalcvoutput %>%
    group_by(fold_no) %>%
    summarize(accuracy = mean(actual_values==predicted_values))
  final_f1_per_fold = finalcvoutput %>%
    group_by(fold_no) %>%
    summarize(f1 = F1_Score(actual_values, predicted_values, positive = NULL)) 
  
  
  acc <- mean(final_acc_per_fold$accuracy)
  f1 <- mean(final_f1_per_fold$f1)
  
  return( c(acc, f1) )
  
}