recode_dateTime <- function(varName) {
  varName %>% 
    as.character() %>%  
    lubridate::ymd_hms()
}

recode_dateOnly <- function(varName) {
  varName %>% 
    as.character() %>%  
    lubridate::ymd()
}


data_path <- here("FFCD0904_data", "Origine")
fileNames <- list.files(path = data_path, pattern = "*_sas.csv")

data <- data_frame(fileName = fileNames) %>%
  mutate(file_df = map(fileName,
                       ~ read.csv(
                         file = file.path(data_path, .),
                         header = TRUE,
                         sep = ";"
                       )
                       )
         )

df_F3 <- data %>% 
  dplyr::filter( fileName == "fiche_3_sas.csv" ) %>% 
  select(file_df) %>% 
  unnest()

# Some infos
colnames(df_F3)
str(df_F3)

F3_dateTimeFields <- c("D_DATE_SAISIE", "D_MODIF", "D_VALID_DATE")

df_F3 %<>% 
  mutate_at( vars(F3_dateTimeFields), funs(recode_dateTime) ) 

F3_dateOnlyFields <- c("F3_PANI_DAT_TTT", "F3_5FU_DAT_DEB_TTT", "F3_MITO_DAT_TTT",
                       "F3_5FU_DAT_FIN_TTT", "F3_DAT_INJECTION", "F3_DAT_CONSULT" )

df_F3 %<>% 
  mutate_at( vars(F3_dateOnlyFields), funs(recode_dateOnly) ) 

# Select a limited number of variables
df <- df_F3 %>% 
  select(D_ID, D_ID_PATIENT, 
         F3_PANI_DOSE, F3_PANI_DAT_TTT, 
         F3_5FU_DOSE, F3_5FU_DAT_DEB_TTT, F3_5FU_DAT_FIN_TTT,
         F3_MITO_DOSE, F3_MITO_DAT_TTT)

# Transform data frame into long format
str(df)

test <- df %>% 
  gather(chemo, dose, -D_ID, -D_ID_PATIENT, 
         -F3_PANI_DAT_TTT, -F3_5FU_DAT_DEB_TTT, -F3_5FU_DAT_FIN_TTT, F3_MITO_DAT_TTT )
