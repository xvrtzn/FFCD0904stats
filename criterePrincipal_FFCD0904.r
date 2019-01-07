# 07/01/2019
# Xavier Tizon

# Critère principal = taux de réponse complète à 8 semaines après la fin 
# du traitement par 5FU-panitumumab (anti EGFR)+mitomycine+radiothérapie
# FICHE 8.1
# 
# 

library(tidyverse)
library(here)

########## FUNCTIONS

# Data frame used to recode RECIST evaluation as [1-5] into the most often used acronyms
# 1=Réponse complète , 2=Réponse partielle , 3=Stabilité , 4=Progression , 5=Non évaluable
recode_eval <- frame_data(
  ~response_numericCode,~response_explicitCode_FR,~response_explicitCode_EN,
  1, "RC", "CR",
  2, "RP", "PR",
  3, "S", "SD",
  4, "P", "PD", 
  5, "NE", "NE"
)

##### READ DAT ----
data_path <- here("FFCD0904_data", "export 03122018_final")

# Lire seulement les fichiers *_sas.csv - TODO - essayer avec les fichiers origine
fileNames <- list.files(path = data_path, pattern = "*.csv")


# Create a df of data frames (one row per file)
# The result is a 2-columns DF with the file name (w/o extension) in the first column, and a nested DF in the 2nd column
data <- data_frame(fileName = fileNames) %>%
  mutate(file_df = map(fileName,          
                       ~ read.csv( file = file.path(data_path, .),
                                   header = TRUE,
                                   sep = ";" )
  )
  )  


df_F8 <- data %>% 
  dplyr::filter( fileName == "fiche81.csv" ) %>% 
  select(file_df) %>% 
  unnest()

# colnames(df_F8) 
# [1] "D_ID"               "D_ID_PATIENT"       "D_ID_OPERATOR"      "D_ID_MED"          "D_ID_CENTRE"       
# [6] "D_DATE_SAISIE"      "D_MODIF"            "D_ORIGINE"          "D_VALID_DATE"      "D_ETAT"            
# [11] "F81_ARC"           "F81_BILI_TOT"       "F81_KAL"            "F81_MG"            "F81_BILI_CONJ"     
# [16] "F81_NORM_PAL"      "F81_NORM_ALAT"      "F81_NORM_ASAT"      "F81_SCC"           "F81_TRANS_MONIT"   
# [21] "F81_DAT_CONSULT"   "F81_PDS"            "F81_OMS"            "F81_CHIR"          "F81_DAT_CHIR"      
# [26] "F81_DAT_TDM"       "F81_BILAN_BIO_FAIT" "F81_DAT_BILAN_BIO"  "F81_HB"            "F81_PNN"           
# [31] "F81_NA"            "F81_CA"             "F81_PAL"            "F81_ALAT"          "F81_ASAT"          
# [36] "F81_CREAT"         "F81_PLAQ"           "F81_ALBU"           "F81_PLAQ_ND"       "F81_SCC_ND"        
# [41] "F81_TTT_RECIDIVE"  "F81_HB_ND"          "F81_BILI_T_ND"      "F81_PAL_ND"        "F81_ALAT_ND"       
# [46] "F81_ASAT_ND"       "F81_CREAT_ND"       "F81_ALBU_ND"        "F81_KAL_ND"        "F81_MG_ND"         
# [51] "F81_BILI_C_ND"     "F81_META"           "F81_LOC_META"       "F81_AUT_LOC_META"  "F81_EVAL"          
# [56] "F81_RECIDIVE"      "F81_PROGRESSION"    "F81_DAT_RECIDIVE"   "F81_TYPE_RECIDIVE" "F81_AUT_LOC"       
# [61] "F81_CHIMIO"        "F81_AUT_TTT"        "F81_COMMENTAIRE"    "F81_PNN_ND"        "F81_NA_ND"         
# [66] "F81_CA_ND"         "F81_LYMPHOCYT"      "F81_LYMPHOCYT_ND"   "F81_DAT_RECIDIVE_J" "F81_DAT_RECIDIVE_M"
# [71] "F81_DAT_RECIDIVE_A"

# Calcul de la réponse objective
df_F8 %<>% 
  mutate(response = F81_EVAL) %>% 
  left_join(recode_eval, by = c("response" = "response_numericCode") )

df_F8 %>% 
  select(response_explicitCode_EN) %>% 
  mutate( response_explicitCode_EN = 
            factor(response_explicitCode_EN,  
                   levels = recode_eval$response_explicitCode_EN ) ) %>% 
  ggplot( aes(x = response_explicitCode_EN, 
              fill = response_explicitCode_EN) ) +
  geom_bar( ) + 
  ggthemes::scale_fill_tableau( drop=FALSE )

# Table of results (put zero if level not present)
df_F8 %>% 
  select(ID = D_ID_PATIENT, response = response_explicitCode_FR) %>% 
  mutate(response = factor(response, levels = recode_eval$response_explicitCode_FR) ) %>% 
  group_by(response) %>% 
  tally( ) %>% 
  complete(response, fill = list(n=0)) 

# TODO - reconciliate with excel file provided by CSI
eval_CSI <- readxl::read_excel(file.path(here("FFCD0904_data"), 
                                         "Evaluation a 8 semaine apres ttt BDD VS CSI.xlsx") ) %>% 
  select( ID = `Numero du patient`, 
          response_base = `Réponse globale a 8 semaines en base`, 
          response_CSI = `Réponse globale a 8 semaines validé par le CSI` )

# colnames(eval_CSI)
# [1] "Numero du patient"                              "Réponse globale a 8 semaines en base"          
# [3] "Réponse globale a 8 semaines validé par le CSI"

resp_df <- df_F8 %>% 
  select(ID = D_ID_PATIENT, response = response_explicitCode_FR) %>% 
  left_join( eval_CSI, by = "ID" ) %>% 
  mutate( response_final = ifelse( is.na(response_CSI),
                                   response, 
                                   response_CSI) ) 

resp_df %>% 
  mutate(response_final = factor(response_final, 
                                 levels = recode_eval$response_explicitCode_FR) ) %>% 
  group_by(response_final) %>% 
  tally( ) %>% 
  complete(response_final, fill = list(n=0)) 

library(writexl)
# Write the first data set in a new workbook
write_xlsx(resp_df, path = "critPrincip0904.xlsx")


