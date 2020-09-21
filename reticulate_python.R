## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- function checking for installed packages, include=FALSE-------------------------------------------------------------------------------------------------------------------
# Validate that all necessary packaged have been downloaded, install otherwise or throw err package DNE
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,repos = "http://cran.r-project.org", dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


## ----install packages, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
#Installing packages 
pkgTest("reticulate")


## ----library, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
library(reticulate)


## ----install pandas and itertools-----------------------------------------------------------------------------------------------------------------------------------------------
py_install("pandas")


## import pandas as pd

## from itertools import chain


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pycode <-"import pandas as pd
from itertools import chain

def flatten(listOfLists):
    return chain.from_iterable(listOfLists)

def compiledata(filename, subject):
    data = pd.read_pickle(filename) 
    df_chest = pd.DataFrame()

    df_chest['ACC_chest_X'] = data['signal']['chest']['ACC'][:,0]
    df_chest['ACC_chest_Y'] = data['signal']['chest']['ACC'][:,1]
    df_chest['ACC_chest_Z'] = data['signal']['chest']['ACC'][:,2]
    df_chest['EMG'] = list(flatten(data['signal']['chest']['EMG']))
    df_chest['ECG'] = list(flatten(data['signal']['chest']['ECG']))
    df_chest['EDA'] = list(flatten(data['signal']['chest']['EDA']))
    df_chest['Temp'] = list(flatten(data['signal']['chest']['Temp']))
    df_chest['Resp'] = list(flatten(data['signal']['chest']['Resp']))
    df_chest['Label'] = data['label']

    df_chest = df_chest[df_chest.index % 175 == 0]  # Selects every 175th raw starting from 0
    df_chest = df_chest.reset_index()


    ACC_wrist = pd.DataFrame()
    BVP_wrist = pd.DataFrame()
    EDA_wrist = pd.DataFrame()
    TEMP_wrist = pd.DataFrame()

    ACC_wrist['ACC_wrist_x'] = data['signal']['wrist']['ACC'][:,0]
    ACC_wrist['ACC_wrist_y'] = data['signal']['wrist']['ACC'][:,1]
    ACC_wrist['ACC_wrist_z'] = data['signal']['wrist']['ACC'][:,2]
    BVP_wrist['BVP'] = list(flatten(data['signal']['wrist']['BVP']))
    EDA_wrist['EDA_wrist'] = list(flatten(data['signal']['wrist']['EDA']))
    TEMP_wrist['TEMP_wrist'] = list(flatten(data['signal']['wrist']['TEMP']))

    ACC_wrist = ACC_wrist[ACC_wrist.index % 8 == 0]  # Selects every 8th raw starting from 0
    BVP_wrist = BVP_wrist[BVP_wrist.index % 16 == 0]  # Selects every 8th raw starting from 0
    ACC_wrist = ACC_wrist.reset_index()
    BVP_wrist = BVP_wrist.reset_index()

    df_final = pd.concat([df_chest, ACC_wrist, BVP_wrist, EDA_wrist, TEMP_wrist], axis=1)


    df_final = df_final.drop(['index'], axis=1)

    states = [2,3]
    df_final = df_final[df_final['Label'].isin(states)]

    df_final['subject'] = subject

    path= '/hpc/group/sta440-f20/mr328/'
    file_name = ''.join([path, 'df_', 'subject', '.csv'])
    df_final.to_csv(file_name)

sub_num = ['S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9','S10' ,'S11','S13', 'S14','S15','S16','S17']
for subject in sub_num:
    path= '/hpc/group/sta440-f20/'
    file_name = ''.join([path, 'WESAD/WESAD/', subject ,'/' , subject, '.pkl'])
    compiledata(file_name, subject)"
cat(pycode, file = "ret.py")
py_run_file("/hpc/group/sta440-f20/mr328/ret.py")



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("reticulate_python.Rmd", output = "reticulate_python.R")

