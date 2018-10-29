
library(tidyverse)
library(magrittr)
library(here)
library(haven)

# Utility functions

# read one SAS file, 
# add file name as column,
# rewrite column name as upper case, 
# and nest everything except source file and patient ID
read_SASdata_toNested <- function(file_name) {
  df <- read_sas(file_name) %>%  
    mutate( source = basename(file_name) )
  
  colnames(df) <- toupper(colnames(df))
  
  df %>% 
    nest(-SOURCE, -D_ID_PATIENT)
}

# Proof of Concept  
# 1. Transferring the data files to a NoSQL database after transformation by the stats into SAS files
# 2. Using this dtaa format (could be generalized to aggregate the different studies) to create a patient profile
#   (one page per patient giving the most relevant inforamtion, for review during data review commmittees)

data_path <- here("FFCD0904_data", "9. Analyse phase II ASCO 2019 - dec 2018","Bases","Modif")
fileNames <- list.files(path = data_path, pattern = "*.sas7bdat")
fullpath_fileNames <- file.path(data_path, fileNames)

# Read data from SAS files and bind into one unique dataframe
df <- map_df(fullpath_fileNames, read_SASdata_toNested ) 


# Create a database from the DF



# Query the DF directly using dplyr functions


################################## SANDBOX

