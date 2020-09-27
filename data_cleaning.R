## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)



## ----library, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
library(zoo)
library(tidyverse)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Define all the csv files we are interested in.
subject_data <- c("df_S2.csv", "df_S3.csv", "df_S4.csv", "df_S5.csv", "df_S6.csv", "df_S7.csv", "df_S8.csv", "df_S9.csv","df_S10.csv" ,"df_S11.csv", "df_S13.csv", "df_S14.csv","df_S15.csv","df_S16.csv","df_S17.csv")


## ----calc-stats-features--------------------------------------------------------------------------------------------------------------------------------------------------------
features_all_modalities = function(fn, win_size) {
  df_all = read.csv(fn) %>%
    mutate(ACC_chest_3D = sqrt(ACC_chest_X^2+ACC_chest_Y^2+ACC_chest_Z^2)) %>%
    mutate(ACC_wrist_3D = sqrt(ACC_wrist_x^2+ACC_wrist_y^2+ACC_wrist_z^2))
  drops <- c("X", "Label", "subject")
  df = df_all[ , !(names(df_all) %in% drops)]
  replace_rows =  length(rollapply(df[,1], width = win_size*4, by = 1, FUN = mean, align = "left"))
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
    mu_vals = rollapply(df[,c], width = win_size*4, by = 1, FUN = mean, align = "left")
    new_col_name = paste0(colnames(df)[c],"_mean")
    cindx = which(colnames(features_df)==new_col_name)
    features_df[, cindx] = mu_vals
    
    # finding sd
    sd_vals = rollapply(df[,c], width = win_size*4, by = 1, FUN = var, align = "left")
    new_col_name = paste0(colnames(df)[c],"_sd")
    cindx = which(colnames(features_df)==new_col_name)
    features_df[, cindx] = sqrt(sd_vals)
    
    # finding max
    max_vals = rollapply(df[,c], width = win_size*4, by = 1, FUN = max, align = "left")
    new_col_name = paste0(colnames(df)[c],"_max")
    cindx = which(colnames(features_df)==new_col_name)
    features_df[, cindx] = max_vals
    
    # finding min
    min_vals = rollapply(df[,c], width = win_size*4, by = 1, FUN = min, align = "left")
    new_col_name = paste0(colnames(df)[c],"_min")
    cindx = which(colnames(features_df)==new_col_name)
    features_df[, cindx] = min_vals
    
  }
  
 # make sure merge label, subject back into feature dataframe
  features_df$Label = df_all[1:replace_rows, "Label"]
  features_df$Subject = df_all[1:replace_rows, "subject"]
  return(features_df)
}


## ----hr-calc--------------------------------------------------------------------------------------------------------------------------------------------------------------------
HR_calc = function(df, win_size) {
  df <- df[rep(seq_len(nrow(df)), each = 4), ]
  mu_vals = rollapply(df, width = win_size*4, by = 1, FUN = mean, align = "left")
  sd_vals = rollapply(df, width = win_size*4, by = 1, FUN = sd, align = "left")
  min_vals = rollapply(df, width = win_size*4, by = 1, FUN = min, align = "left")
  max_vals = rollapply(df, width = win_size*4, by = 1, FUN = max, align = "left")

  return(list(mu = mu_vals, sd = sd_vals,
              min = min_vals, max = max_vals))
}


## ----run-all-featengine-combine-------------------------------------------------------------------------------------------------------------------------------------------------
ALL_df = NULL
for (i in 1:length(subject_data)) {
  subject_file = paste0("/hpc/group/sta440-f20/mr328/", subject_data[i])
  feature_data = features_all_modalities(subject_file, 5) #CHANGE FILE PATH 
  subject_no = gsub("\\..*","", sub('.*_', '', subject_data[i]))
  #HR_fn = paste0("~/case-study-2/WESAD/", subject_no, "/", subject_no, "_E4_Data/HR.csv") 
  HR_fn = paste0("/hpc/group/sta440-f20/WESAD/WESAD/", subject_no, "/HR.csv")
  HR_data = read.csv(HR_fn)[-1,]
  hr = HR_calc(as.data.frame(HR_data), 5)

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
write.csv(ALL_df, "master_df.csv")

