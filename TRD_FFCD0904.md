---
title: "Analyse critère principal FFCD0904"
author: "Xavier Tizon"
date: "18/09/2018"
output: 
  html_document: 
    keep_md: yes
---

# Introduction

Test d'analyse du critère princpal pour l'étude FFCD0904.

Objectif : reproduire l'analyse faite en mars 2017 (L:\\STATISTIQUES\\3. Etudes\\ANUS\\FFCD 0904\\7. Analyse actualisation DDN phase I - mars 2017)






# Exploration - fiche 1

## Recodage

```r
df_test <- data %>% 
  dplyr::filter( fileName == "Fiche01_0904.csv" ) %>% 
  select(file_df) %>% 
  unnest()

# Recodages :
# "INCONNUE" en NA dans le champ F1_CRIT_CD4
# "MANQUANT" en NA dans le champe F1_DAT_ACCORD_DEV

df_test %<>% 
  mutate_at( vars(F1_CRIT_CD4), funs(as.numeric(as.character(.))) ) %>% 
  mutate_at( vars(F1_DAT_ACCORD_DEV), recode, "MANQUANT" = NA_character_) 
```

```
## Warning in evalq(as.numeric(as.character(F1_CRIT_CD4)), <environment>): NAs
## introduits lors de la conversion automatique
```

```r
# Recodage des champs date

# algo 
# 1. Transforme les champs en chaine de caractère
# 2. ne garde que les 8 premiers carcatères
# 3. utilise lubridate::ymd

Date_field_names <- c("D_DATE_SAISIE", "D_MODIF", 
                      "D_VALID_DATE", "F1_DAT_CONSENT", "F1_DAT_CONSENT_BIO",
                      "F1_DAT_RANDO", "F1_DAT_ACCORD_DEV", "F1_DAT_NAISS")

Date_recode <- function(x) {
  x %>% 
    as.character() %>%  
    stringr::str_sub( start = 1L, end = 8L ) %>% 
    lubridate::ymd()
}

df_test %<>% 
  mutate_at( vars(Date_field_names), funs(Date_recode) ) 
```

```
## Warning: 1 failed to parse.
```


```
## Warning in .x(x): Variable contains value(s) of "" that have been converted
## to "empty".

## Warning in .x(x): Variable contains value(s) of "" that have been converted
## to "empty".
```

```
## Skim summary statistics  
##  n obs: 10    
##  n variables: 48    
## 
## Variable type: Date<table>
##  <thead>
##   <tr>
##    <th> variable </th>
##    <th> missing </th>
##    <th> complete </th>
##    <th> n </th>
##    <th> min </th>
##    <th> max </th>
##    <th> median </th>
##    <th> n_unique </th>
##   </tr>
##  </thead>
## <tbody>
##   <tr>
##    <td> D_DATE_SAISIE </td>
##    <td> 2 </td>
##    <td> 8 </td>
##    <td> 10 </td>
##    <td> 2012-09-20 </td>
##    <td> 2015-03-24 </td>
##    <td> 2013-10-16 </td>
##    <td> 6 </td>
##   </tr>
##   <tr>
##    <td> D_MODIF </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2012-10-04 </td>
##    <td> 2015-03-24 </td>
##    <td> 2013-10-30 </td>
##    <td> 9 </td>
##   </tr>
##   <tr>
##    <td> D_VALID_DATE </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2012-10-04 </td>
##    <td> 2015-03-24 </td>
##    <td> 2013-10-30 </td>
##    <td> 9 </td>
##   </tr>
##   <tr>
##    <td> F1_DAT_ACCORD_DEV </td>
##    <td> 9 </td>
##    <td> 1 </td>
##    <td> 10 </td>
##    <td> 2012-06-14 </td>
##    <td> 2012-06-14 </td>
##    <td> 2012-06-14 </td>
##    <td> 1 </td>
##   </tr>
##   <tr>
##    <td> F1_DAT_CONSENT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2012-06-14 </td>
##    <td> 2015-02-16 </td>
##    <td> 2013-08-11 </td>
##    <td> 10 </td>
##   </tr>
##   <tr>
##    <td> F1_DAT_CONSENT_BIO </td>
##    <td> 2 </td>
##    <td> 8 </td>
##    <td> 10 </td>
##    <td> 2012-06-14 </td>
##    <td> 2015-02-16 </td>
##    <td> 2013-08-20 </td>
##    <td> 8 </td>
##   </tr>
##   <tr>
##    <td> F1_DAT_NAISS </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 1941-05-27 </td>
##    <td> 1973-01-26 </td>
##    <td> 1956-09-13 </td>
##    <td> 10 </td>
##   </tr>
##   <tr>
##    <td> F1_DAT_RANDO </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2012-06-14 </td>
##    <td> 2015-03-03 </td>
##    <td> 2013-08-17 </td>
##    <td> 10 </td>
##   </tr>
## </tbody>
## </table>
## 
## Variable type: factor<table>
##  <thead>
##   <tr>
##    <th> variable </th>
##    <th> missing </th>
##    <th> complete </th>
##    <th> n </th>
##    <th> n_unique </th>
##    <th> top_counts </th>
##    <th> ordered </th>
##   </tr>
##  </thead>
## <tbody>
##   <tr>
##    <td> D_ID </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> D22: 1, D22: 1, D22: 1, D74: 1 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> D_ID_CENTRE </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 7 </td>
##    <td> C00: 2, C00: 2, C00: 2, C00: 1 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> D_ID_MED </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 8 </td>
##    <td> I00: 2, I29: 2, I00: 1, I00: 1 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> D_ID_OPERATOR </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 3 </td>
##    <td> U21: 5, U22: 3, U32: 2, NA: 0 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> D_ORIGINE </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 1 </td>
##    <td> FAD: 10, NA: 0 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> F1_ARC </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 4 </td>
##    <td> LN: 4, FGK: 3, NP: 2, HF: 1 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> F1_COMMENTAIRE </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 3 </td>
##    <td> emp: 8, CD4: 1, Dos: 1, NA: 0 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_INCL </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 1 </td>
##    <td> 1;2: 10, NA: 0 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> F1_LISTE_DEVIATION </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 3 </td>
##    <td> emp: 8, IRM: 1, Mg+: 1, NA: 0 </td>
##    <td> FALSE </td>
##   </tr>
##   <tr>
##    <td> F1_MEDECIN </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 8 </td>
##    <td> LEM: 2, VEN: 2, APA: 1, BOU: 1 </td>
##    <td> FALSE </td>
##   </tr>
## </tbody>
## </table>
## 
## Variable type: integer<table>
##  <thead>
##   <tr>
##    <th> variable </th>
##    <th> missing </th>
##    <th> complete </th>
##    <th> n </th>
##    <th> mean </th>
##    <th> sd </th>
##    <th> p0 </th>
##    <th> p25 </th>
##    <th> p50 </th>
##    <th> p75 </th>
##    <th> p100 </th>
##    <th> hist </th>
##   </tr>
##  </thead>
## <tbody>
##   <tr>
##    <td> D_ETAT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> </td>
##   </tr>
##   <tr>
##    <td> D_ID_PATIENT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 5.5 </td>
##    <td> 3.03 </td>
##    <td> 1 </td>
##    <td> 3.25 </td>
##    <td> 5.5 </td>
##    <td> 7.75 </td>
##    <td> 10 </td>
##    <td> <U+2587><U+2583><U+2583><U+2583><U+2583><U+2583><U+2583><U+2587> </td>
##   </tr>
##   <tr>
##    <td> F1_ACCEPTATION_DEV </td>
##    <td> 8 </td>
##    <td> 2 </td>
##    <td> 10 </td>
##    <td> 1 </td>
##    <td> 0 </td>
##    <td> 1 </td>
##    <td> 1 </td>
##    <td> 1 </td>
##    <td> 1 </td>
##    <td> 1 </td>
##    <td> <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_ALAT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 19.8 </td>
##    <td> 9.65 </td>
##    <td> 9 </td>
##    <td> 13.5 </td>
##    <td> 16.5 </td>
##    <td> 24 </td>
##    <td> 39 </td>
##    <td> <U+2585><U+2587><U+2582><U+2585><U+2581><U+2582><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_ASAT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 20.7 </td>
##    <td> 8.29 </td>
##    <td> 11 </td>
##    <td> 15.25 </td>
##    <td> 18 </td>
##    <td> 26.25 </td>
##    <td> 36 </td>
##    <td> <U+2585><U+2587><U+2582><U+2582><U+2581><U+2582><U+2582><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_LEUCO </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 8403 </td>
##    <td> 2394.68 </td>
##    <td> 4600 </td>
##    <td> 6750 </td>
##    <td> 8505 </td>
##    <td> 10480 </td>
##    <td> 11500 </td>
##    <td> <U+2583><U+2583><U+2587><U+2581><U+2587><U+2581><U+2587><U+2587> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_N </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2.3 </td>
##    <td> 0.95 </td>
##    <td> 1 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> 3 </td>
##    <td> 4 </td>
##    <td> <U+2583><U+2581><U+2587><U+2581><U+2581><U+2586><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_NORM_ALAT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 41.9 </td>
##    <td> 9.9 </td>
##    <td> 31 </td>
##    <td> 34 </td>
##    <td> 38.5 </td>
##    <td> 52.5 </td>
##    <td> 55 </td>
##    <td> <U+2587><U+2582><U+2581><U+2582><U+2582><U+2581><U+2581><U+2586> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_NORM_ASAT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 33.6 </td>
##    <td> 2.72 </td>
##    <td> 31 </td>
##    <td> 31.25 </td>
##    <td> 34 </td>
##    <td> 34 </td>
##    <td> 40 </td>
##    <td> <U+2587><U+2581><U+2587><U+2582><U+2581><U+2581><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_OMS </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 0.3 </td>
##    <td> 0.48 </td>
##    <td> 0 </td>
##    <td> 0 </td>
##    <td> 0 </td>
##    <td> 0.75 </td>
##    <td> 1 </td>
##    <td> <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2583> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_PLAQ </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 290 </td>
##    <td> 71.12 </td>
##    <td> 210 </td>
##    <td> 240.25 </td>
##    <td> 282.5 </td>
##    <td> 314.5 </td>
##    <td> 450 </td>
##    <td> <U+2587><U+2585><U+2582><U+2585><U+2582><U+2581><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_PNN </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 5160 </td>
##    <td> 1972.12 </td>
##    <td> 2660 </td>
##    <td> 3864 </td>
##    <td> 4565 </td>
##    <td> 6743 </td>
##    <td> 8701 </td>
##    <td> <U+2585><U+2582><U+2587><U+2581><U+2582><U+2585><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_T </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 3.2 </td>
##    <td> 0.63 </td>
##    <td> 2 </td>
##    <td> 3 </td>
##    <td> 3 </td>
##    <td> 3.75 </td>
##    <td> 4 </td>
##    <td> <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2583> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_TAILLE_TUM </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 48.2 </td>
##    <td> 21.43 </td>
##    <td> 15 </td>
##    <td> 36 </td>
##    <td> 40 </td>
##    <td> 67.75 </td>
##    <td> 80 </td>
##    <td> <U+2582><U+2582><U+2585><U+2585><U+2582><U+2581><U+2581><U+2587> </td>
##   </tr>
##   <tr>
##    <td> F1_ETUD_BIO </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 1.8 </td>
##    <td> 0.42 </td>
##    <td> 1 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> <U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2587> </td>
##   </tr>
##   <tr>
##    <td> F1_PALIER </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 1.3 </td>
##    <td> 0.48 </td>
##    <td> 1 </td>
##    <td> 1 </td>
##    <td> 1 </td>
##    <td> 1.75 </td>
##    <td> 2 </td>
##    <td> <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2583> </td>
##   </tr>
##   <tr>
##    <td> F1_SEXE </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 10.8 </td>
##    <td> 0.42 </td>
##    <td> 10 </td>
##    <td> 11 </td>
##    <td> 11 </td>
##    <td> 11 </td>
##    <td> 11 </td>
##    <td> <U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2587> </td>
##   </tr>
##   <tr>
##    <td> F1_TRANS_MONIT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2 </td>
##    <td> 0 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> 2 </td>
##    <td> <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> </td>
##   </tr>
## </tbody>
## </table>
## 
## Variable type: logical<table>
##  <thead>
##   <tr>
##    <th> variable </th>
##    <th> missing </th>
##    <th> complete </th>
##    <th> n </th>
##    <th> mean </th>
##    <th> count </th>
##   </tr>
##  </thead>
## <tbody>
##   <tr>
##    <td> F1_CRIT_EXCL </td>
##    <td> 10 </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> NaN </td>
##    <td> 10 </td>
##   </tr>
## </tbody>
## </table>
## 
## Variable type: numeric<table>
##  <thead>
##   <tr>
##    <th> variable </th>
##    <th> missing </th>
##    <th> complete </th>
##    <th> n </th>
##    <th> mean </th>
##    <th> sd </th>
##    <th> p0 </th>
##    <th> p25 </th>
##    <th> p50 </th>
##    <th> p75 </th>
##    <th> p100 </th>
##    <th> hist </th>
##   </tr>
##  </thead>
## <tbody>
##   <tr>
##    <td> F1_CLAIRANCE </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 88.28 </td>
##    <td> 24.07 </td>
##    <td> 59.07 </td>
##    <td> 74.53 </td>
##    <td> 82.3 </td>
##    <td> 98.23 </td>
##    <td> 138.19 </td>
##    <td> <U+2587><U+2587><U+2587><U+2583><U+2583><U+2583><U+2581><U+2583> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_BILI_SUP </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 19.52 </td>
##    <td> 2.93 </td>
##    <td> 15 </td>
##    <td> 17.1 </td>
##    <td> 20.5 </td>
##    <td> 21 </td>
##    <td> 25 </td>
##    <td> <U+2582><U+2585><U+2581><U+2581><U+2587><U+2581><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_BILI_TOT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 7.2 </td>
##    <td> 2.36 </td>
##    <td> 5.13 </td>
##    <td> 5.5 </td>
##    <td> 6.15 </td>
##    <td> 8.25 </td>
##    <td> 11.6 </td>
##    <td> <U+2587><U+2582><U+2582><U+2581><U+2582><U+2581><U+2581><U+2583> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_CA </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2.4 </td>
##    <td> 0.096 </td>
##    <td> 2.3 </td>
##    <td> 2.3 </td>
##    <td> 2.38 </td>
##    <td> 2.5 </td>
##    <td> 2.5 </td>
##    <td> <U+2587><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581><U+2587> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_CD4 </td>
##    <td> 9 </td>
##    <td> 1 </td>
##    <td> 10 </td>
##    <td> 963 </td>
##    <td> NA </td>
##    <td> 963 </td>
##    <td> 963 </td>
##    <td> 963 </td>
##    <td> 963 </td>
##    <td> 963 </td>
##    <td> <U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_CREAT </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 66.47 </td>
##    <td> 17.08 </td>
##    <td> 39.7 </td>
##    <td> 57.78 </td>
##    <td> 61.15 </td>
##    <td> 75.99 </td>
##    <td> 99 </td>
##    <td> <U+2582><U+2581><U+2587><U+2583><U+2581><U+2582><U+2582><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_HB </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 13.16 </td>
##    <td> 0.8 </td>
##    <td> 12 </td>
##    <td> 12.6 </td>
##    <td> 13.15 </td>
##    <td> 13.4 </td>
##    <td> 14.6 </td>
##    <td> <U+2582><U+2587><U+2581><U+2585><U+2585><U+2581><U+2582><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_MG </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 0.84 </td>
##    <td> 0.094 </td>
##    <td> 0.7 </td>
##    <td> 0.79 </td>
##    <td> 0.82 </td>
##    <td> 0.9 </td>
##    <td> 1.03 </td>
##    <td> <U+2582><U+2582><U+2587><U+2585><U+2581><U+2585><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_NORM_CA </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 2.18 </td>
##    <td> 0.12 </td>
##    <td> 2.1 </td>
##    <td> 2.1 </td>
##    <td> 2.17 </td>
##    <td> 2.2 </td>
##    <td> 2.5 </td>
##    <td> <U+2587><U+2586><U+2581><U+2581><U+2581><U+2581><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_NORM_MG </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 0.72 </td>
##    <td> 0.1 </td>
##    <td> 0.65 </td>
##    <td> 0.66 </td>
##    <td> 0.7 </td>
##    <td> 0.74 </td>
##    <td> 0.99 </td>
##    <td> <U+2587><U+2583><U+2586><U+2581><U+2581><U+2581><U+2581><U+2582> </td>
##   </tr>
##   <tr>
##    <td> F1_CRIT_PDS </td>
##    <td> 0 </td>
##    <td> 10 </td>
##    <td> 10 </td>
##    <td> 65.05 </td>
##    <td> 13.4 </td>
##    <td> 48 </td>
##    <td> 56.03 </td>
##    <td> 61.9 </td>
##    <td> 70.75 </td>
##    <td> 87 </td>
##    <td> <U+2587><U+2587><U+2583><U+2583><U+2587><U+2581><U+2581><U+2587> </td>
##   </tr>
## </tbody>
## </table>
```

```r
# Fiche 8 - Suivi à 6 semaines

df_F8 <- data %>% 
  dplyr::filter( fileName == "Fiche08_0904.csv" ) %>% 
  select(file_df) %>% 
  unnest()

# colnames(df_F8)

Date_field_names_F8 <- c("D_DATE_SAISIE", "D_MODIF", 
                      "D_VALID_DATE", "F8_DAT_BILAN_BIO", "F8_DAT_CHIR",
                      "F8_DAT_PROG")

df_F8 %<>% 
  mutate_at( vars(Date_field_names_F8), funs(Date_recode) ) 
```


# Test - Evaluation morphologique à 6 et 8 semaines  

Création de 2 tableaux "Evaluation morphologique" du rapport stats, en suivant la démarche du programme `02_Listing.sas`. 


```
## 'data.frame':	9 obs. of  22 variables:
##  $ D_ID_PATIENT       : num  1 2 3 4 6 7 8 9 10
##  $ palier             : num  -1 -1 -1 0 0 0 0 0 0
##  $ F101_DAT_PROCTO_E6 : Date, format: "2012-07-30" "2012-08-24" ...
##  $ F101_PROCTO_REP_E6 : Factor w/ 5 levels "","1","3","5",..: 3 2 1 5 3 1 4 1 1
##  $ F101_DAT_ECHO_E6   : Date, format: NA "2012-08-24" ...
##  $ F101_ECHO_REP_E6   : int  NA 1 NA NA 3 NA NA NA NA
##  $ F101_ECHO_UST_E6   : Factor w/ 3 levels "","3","MANQUANTE": 1 3 1 1 2 1 1 1 1
##  $ F101_ECHO_USN_E6   : int  NA 1 NA NA 1 NA NA NA NA
##  $ F101_DAT_PROCTO_E8 : Date, format: "2012-10-16" "2012-11-29" ...
##  $ F101_PROCTO_REP_E8 : Factor w/ 4 levels "","1","3","INCONNUE": 2 2 2 4 2 3 3 3 1
##  $ F101_DAT_ECHO_E8   : Date, format: NA NA ...
##  $ F101_ECHO_REP_E8   : int  NA NA 3 NA 1 3 NA NA NA
##  $ F101_ECHO_UST_E8   : Factor w/ 3 levels "","INCONNUE",..: 1 1 3 1 2 3 1 1 1
##  $ F101_ECHO_USN_E8   : Factor w/ 4 levels "","2","INCONNUE",..: 1 1 2 1 3 4 1 1 1
##  $ F102_DAT_IRM_E6    : Date, format: "2012-07-26" "2012-08-27" ...
##  $ F102_IRM_REP_E6    : int  3 3 1 3 1 NA NA 1 3
##  $ F102_DAT_PETSCAN_E6: Date, format: NA NA ...
##  $ F102_PETSCAN_REP_E6: int  NA NA NA 3 NA 5 NA NA NA
##  $ F102_DAT_IRM_E8    : Date, format: "2012-10-15" "2012-11-29" ...
##  $ F102_IRM_REP_E8    : int  1 1 1 3 1 NA 3 1 3
##  $ F102_DAT_PETSCAN_E8: Date, format: NA NA ...
##  $ F102_PETSCAN_REP_E8: Factor w/ 4 levels "","3","5","INCONNUE": 1 1 2 2 2 3 4 1 1
```
## Tableau 1 - suivi à 6 semaines
<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>This is a nice table</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> D_ID_PATIENT </th>
   <th style="text-align:left;"> F101_DAT_PROCTO_E6 </th>
   <th style="text-align:left;"> F101_PROCTO_REP_E6 </th>
   <th style="text-align:left;"> F101_DAT_ECHO_E6 </th>
   <th style="text-align:right;"> F101_ECHO_REP_E6 </th>
   <th style="text-align:left;"> F101_ECHO_UST_E6 </th>
   <th style="text-align:right;"> F101_ECHO_USN_E6 </th>
   <th style="text-align:left;"> F102_DAT_IRM_E6 </th>
   <th style="text-align:right;"> F102_IRM_REP_E6 </th>
   <th style="text-align:left;"> F102_DAT_PETSCAN_E6 </th>
   <th style="text-align:right;"> F102_PETSCAN_REP_E6 </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="3"><td colspan="11" style="border-bottom: 1px solid;"><strong>palier -1</strong></td></tr>
<tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 1 </td>
   <td style="text-align:left;"> 2012-07-30 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2012-07-26 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 2 </td>
   <td style="text-align:left;"> 2012-08-24 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 2012-08-24 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> MANQUANTE </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 2012-08-27 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2012-08-23 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr grouplength="1"><td colspan="11" style="border-bottom: 1px solid;"><strong>palier 0</strong></td></tr>
<tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 4 </td>
   <td style="text-align:left;"> 2013-09-09 </td>
   <td style="text-align:left;"> INCONNUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2013-09-12 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 2013-08-28 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> 2013-10-15 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 2013-10-15 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 2013-10-14 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2013-10-15 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> 2014-07-11 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2014-12-01 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2015-04-20 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>

## Tableau 2 - suivi à 8 semaines
<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>a nice table</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> D_ID_PATIENT </th>
   <th style="text-align:right;"> palier </th>
   <th style="text-align:left;"> F101_DAT_PROCTO_E8 </th>
   <th style="text-align:left;"> F101_PROCTO_REP_E8 </th>
   <th style="text-align:left;"> F101_DAT_ECHO_E8 </th>
   <th style="text-align:right;"> F101_ECHO_REP_E8 </th>
   <th style="text-align:left;"> F101_ECHO_UST_E8 </th>
   <th style="text-align:left;"> F101_ECHO_USN_E8 </th>
   <th style="text-align:left;"> F102_DAT_IRM_E8 </th>
   <th style="text-align:right;"> F102_IRM_REP_E8 </th>
   <th style="text-align:left;"> F102_DAT_PETSCAN_E8 </th>
   <th style="text-align:left;"> F102_PETSCAN_REP_E8 </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="3"><td colspan="12" style="border-bottom: 1px solid;"><strong>palier -1</strong></td></tr>
<tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 1 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 2012-10-16 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 2012-10-15 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 2 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 2012-11-29 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 2012-11-29 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 3 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 2012-10-31 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 2012-10-31 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> MANQUANTE </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2012-11-05 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 2012-11-29 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr grouplength="1"><td colspan="12" style="border-bottom: 1px solid;"><strong>palier 0</strong></td></tr>
<tr>
   <td style="text-align:right; padding-left: 2em;" indentlevel="1"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2013-12-11 </td>
   <td style="text-align:left;"> INCONNUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 2013-11-28 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 2013-11-25 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2014-01-13 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 2014-01-13 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> INCONNUE </td>
   <td style="text-align:left;"> INCONNUE </td>
   <td style="text-align:left;"> 2013-12-30 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 2013-12-31 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2014-01-20 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 2014-01-20 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> MANQUANTE </td>
   <td style="text-align:left;"> MANQUANTE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> 2014-02-25 </td>
   <td style="text-align:left;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2014-09-26 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 2014-09-18 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 2014-10-08 </td>
   <td style="text-align:left;"> INCONNUE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2015-02-24 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 2015-02-19 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 2015-06-30 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>






