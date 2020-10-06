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

#wristOnly <- function(PCAdim, win_size){
 
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
    #HR_fn <- here("WESAD", subject_no, "HR.csv")
    
    HR_fn = paste0("./WESAD/", subject_no, "/", subject_no, "_E4_Data/HR.csv") 
    
    HR_fn = paste0("~/WESAD/S10/S10_E4_Data/HR.csv") 
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
  # maybe reorder the variables
  col_order <- c("ACC_wrist_x_sd", "ACC_wrist_y_sd",   "ACC_wrist_z_sd",  "ACC_wrist_3D_sd", "EDA_wrist_mean", "EDA_wrist_sd", "TEMP_wrist_mean", "TEMP_wrist_sd",  "hr_wrist_mu", "hr_wrist_sd", "Subject", "Label")
  wrist_only <- wrist_only[, col_order]
  wrist_only$Subject <- factor(wrist_only$Subject, levels = c("S2", "S3","S4", "S5","S6", "S7","S8", "S9","S10", "S11","S13", "S14","S15", "S16","S17"))
  non_biological = c("Label", "Subject")
  wrist_nb = wrist_only[ , !(names(wrist_only) %in% non_biological)]
  
  
  ############################
  ########## Do PCA ########## 
  ############################
  scaled_wrist_nb = scale(wrist_nb, center = TRUE, scale = TRUE)
  res.pca.wrist <- PCA(scaled_wrist_nb, graph = FALSE, ncp = PCAdim)
  
  wrist_pca_vals <- as.data.frame(res.pca.wrist$ind$coord)
  wrist_pca_vals$Label = wrist_only$Label
  wrist_pca_vals$Subject = wrist_only$Subject
  
  set.seed(123)
  kfoldcv_no_subject = function(df) {
    
    actual_values = c()
    predicted_values = c()
    fitted_probs = c()
    
    # fold_accuracies = c()
    # fold_f1 = c()
    
    fold_no = c()
    subject = c()
    
    #print("at folds")
    folds <- createFolds(factor(df$Subject), k = 5)
    
    #print("past folds")
    for (i in 1:5) {
      #train and test
      col = paste0("Fold", i)
      indx = unlist(folds[col])
      train<- df[-indx,]
      #print(nrow(train))
      test<- df[indx, ]
      #print(nrow(test))
      
      
      # run model
      pca.model = glm(Label ~ .-Subject, family = binomial(link="logit"), data=train)
      #summary(wrist.full)
      pred.test = pca.model %>% predict(test, type= "response")
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
  combocvoutput = kfoldcv_no_subject(pca_vals)
  wristcvoutput = kfoldcv_no_subject(wrist_pca_vals)
  
  
  wrist_acc_per_fold = wristcvoutput %>%
    group_by(fold_no) %>%
    summarize(accuracy = mean(actual_values==predicted_values))
  wrist_f1_per_fold = wristcvoutput %>%
    group_by(fold_no) %>%
    summarize(f1 = F1_Score(actual_values, predicted_values, positive = NULL)) 
  
  
  return( c(wrist_acc_per_fold, wrist_f1_per_fold) )
  
#}


#results <- wristOnly(4, 5)
  