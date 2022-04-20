
library(tidyverse)
library(vroom)
library(dplyr)
library(lme4)
library(performance)
library(haven)
library(sjlabelled)
library(rio)
library(readxl)

# data loading

wave_codes = letters[1:11]
list_df_waves = list()
wd = 'C:/Users/ru21406/YandexDisk/PhD Research/Data/Survey/'

for (i in 1:length(wave_codes)){
  
  wave_code = wave_codes[i]
  
  # Individual
  setwd(paste0(wd, '6614spss_main/UKDA-6614-spss/spss/spss25/ukhls'))
  names_selected = c('pidp', 'hidp','_month','_sex', '_dvage',
                     '_marstat', '_livesp', '_livewith',
                     '_employ', '_ethn_dv',
                    '_fimnlabnet_dv', '_fimnnet_dv',
                     
                     '_scsf1',
                     '_scsf2a', '_scsf2b', '_scsf3a',
                     '_scsf3b', '_scsf4a', '_scsf4b',
                     '_scsf5', '_scsf6a', '_scsf6b',
                     '_scsf6c', '_scsf7',
                     
                     '_sf1',
                     '_sf2a', '_sf2b', '_sf3a',
                     '_sf3b', '_sf4a', '_sf4b',
                     '_sf5', '_sf6a', '_sf6b',
                     '_sf6c', '_sf7',
                    
                     '_scghqa',
                     '_scghqb', '_scghqc', '_scghqd',
                     '_scghqe', '_scghqf', '_scghqg',
                     '_scghqh', '_scghqi', '_scghqj',
                     '_scghqk','_scghql', '_sclfsat1',
                     '_sclfsat2', '_sclfsat7','_sclfsato',
                     '_jbisco88_cc', '_jlisco88_cc', 
                     '_jbnssec3_dv', '_jlnssec3_dv', '_scghq1_dv',
                     '_scghq2_dv', '_sf12pcs_dv', '_sf12mcs_dv')
  df_indresp =  read_sav(paste0(wave_code, "_indresp.sav")) %>%
    select(ends_with(names_selected)) 
  
  df_indresp = df_indresp %>%
      rename_at(vars(ends_with(c('pidp', 'hidp'))), ~ sub('_', '', c('pidp', 'hidp')))
  df_indresp = remove_all_labels(df_indresp)
  
  # Income
  #names_selected = c('pidp', '_ficode', '_frmnthimp_dv')
  #df_income = read_sav(paste0(wave_code, "_income.sav"))%>%
  #  select(ends_with(names_selected)) %>%
  #  rename_at(vars(all_of(colnames(.))), ~ sub('_', '', names_selected))
  #df_income = remove_all_labels(df_income)
  
  # Household data
  names_selected = c('_hidp', '_ivfho', '_urban_dv', '_country',
                     '_intdated', '_intdatem', '_intdatey')
  df_hhresp = read_sav(paste0(wave_code, "_hhresp.sav")) %>%
    select(ends_with(names_selected)) %>%
    rename_at(vars(all_of(colnames(.))), ~ sub('_', '', names_selected))
  df_hhresp = remove_all_labels(df_hhresp)
  
  # LA indication
  setwd(paste0(wd, '6670tab_lsoa/UKDA-6670-tab/tab/ukhls'))
  df_lsoa = vroom(paste0(wave_code, "_lsoa01_protect.tab"))
  col = colnames(df_lsoa)
  df_lsoa = df_lsoa %>% rename_at(vars(all_of(colnames(.))), 
                                  ~ sub(paste0(wave_codes[i], '_'), '', col))
  
  # Joining datasets
  df_wave = df_indresp %>%
    #left_join(df_income) %>%
    left_join(df_hhresp) %>%
    left_join(df_lsoa)
  
  # Adding a wave variable
  df_wave$wave = wave_code
  
  # Selecting only England
  df_wave = df_wave %>% filter(country == 1)
  df_wave = as.data.frame(df_wave)
  
  # Adding to the list
  list_df_waves[[i]] = df_wave
  print(wave_code)
}

# Fixing naming
for (i in seq_along(list_df_waves)){
  colnames(list_df_waves[[i]]) = sub(paste0(letters[i], '_'), '', colnames(list_df_waves[[i]]))
}

# Combine all the waves
#df_ukhls = data.table::rbindlist(list_df_waves, fill = T)
df_ukhls = do.call(plyr::rbind.fill, list_df_waves)

# add constant variables
setwd(paste0(wd, '6614tab_main/UKDA-6614-tab/tab/ukhls'))
df_xwaveid = vroom("xwaveid.tab") %>%
  select(pidp, psu, strata, quarter)
df_ukhls = df_ukhls %>% left_join(df_xwaveid)

# sampling month
df_ukhls$year <- ifelse(df_ukhls$wave == 'a' & df_ukhls$quarter %in% 1:4, 2009,
                                  ifelse(df_ukhls$wave == 'a' & df_ukhls$quarter %in% 5:8|
                                           df_ukhls$wave == 'b' & df_ukhls$quarter %in% 1:4, 2010,
                                         ifelse(df_ukhls$wave == 'b' & df_ukhls$quarter %in% 5:8|
                                                  df_ukhls$wave == 'c' & df_ukhls$quarter %in% 1:4, 2011,
                                                ifelse(df_ukhls$wave == 'c' & df_ukhls$quarter %in% 5:8|
                                                         df_ukhls$wave == 'd' & df_ukhls$quarter %in% 1:4, 2012,
                                                       ifelse(df_ukhls$wave == 'd' & df_ukhls$quarter %in% 5:8|
                                                                df_ukhls$wave == 'e' & df_ukhls$quarter %in% 1:4, 2013,
                                                              ifelse(df_ukhls$wave == 'e' & df_ukhls$quarter %in% 5:8|
                                                                       df_ukhls$wave == 'f' & df_ukhls$quarter %in% 1:4, 2014,
                                                                     ifelse(df_ukhls$wave == 'f' & df_ukhls$quarter %in% 5:8|
                                                                              df_ukhls$wave == 'g' & df_ukhls$quarter %in% 1:4, 2015,
                                                                            ifelse(df_ukhls$wave == 'g' & df_ukhls$quarter %in% 5:8|
                                                                                     df_ukhls$wave == 'h' & df_ukhls$quarter %in% 1:4, 2016,
                                                                                   ifelse(df_ukhls$wave == 'h' & df_ukhls$quarter %in% 5:8|
                                                                                            df_ukhls$wave == 'i' & df_ukhls$quarter %in% 1:4, 2017,
                                                                                          ifelse(df_ukhls$wave == 'i' & df_ukhls$quarter %in% 5:8|
                                                                                                   df_ukhls$wave == 'j' & df_ukhls$quarter %in% 1:4, 2018,
                                                                                                 ifelse(df_ukhls$wave == 'j' & df_ukhls$quarter %in% 5:8|
                                                                                                          df_ukhls$wave == 'k' & df_ukhls$quarter %in% 1:4, 2019,
                                                                                                        ifelse(df_ukhls$wave == 'k' & df_ukhls$quarter %in% 5:8, 2020, NA))))))))))))


table(df_ukhls$year)

# LSOA matching with LAD
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data')
lad21 = read_excel('OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.xlsx', sheet = 1)
lsoa01 = read.csv2('lsoa01.csv', sep = ',')
names(lsoa01)[1] = 'LSOA01CD'
la_all_codes = read.csv('la_all_codes.csv')

# selecting relevant vars
lad21 = lad21 %>% select(LAD21CD, LSOA11CD) %>%
  distinct(.keep_all = TRUE)
lsoa01 = lsoa01 %>% select(LSOA11CD, LSOA01CD, LSOA01NM)
la_all_codes = la_all_codes %>% select(LADCD, UTLACD)

# matching all codes
match_all = lad21 %>%
  left_join(lsoa01) %>%
  left_join(la_all_codes, by = c('LAD21CD' = 'LADCD'), .keep = T)
table(is.na(match_all)) # 4 lsoas ?
match_all = match_all %>% distinct(LSOA01CD, .keep_all = T)

# adding to the main dataset
df_ukhls = df_ukhls %>% dplyr::rename(LSOA01CD = lsoa01)
df_ukhls = df_ukhls %>% 
  left_join(match_all, by = 'LSOA01CD')

length(unique(df_ukhls$LSOA11CD))
length(unique(df_ukhls$LAD21CD))

# how many people changed LAs
la_check = df_ukhls %>%
  group_by(pidp) %>%
  dplyr::count(LAD21CD) %>%
  dplyr::summarise(unique_la = n())
round(prop.table(table(la_check$unique_la))*100, 1) # 91% remain in the same LAD

# health indicators
table(df_ukhls$sf1, df_ukhls$wave)
table(df_ukhls$scsf1, df_ukhls$wave)

#In Wave 1, SF1 was asked of everyone. From Wave 2 onwards,
#this question was asked of the proxy respondents. The same 
#question was included in the self-completion part of the 
#individual questionnaire and was renamed SCSF1. For Waves 2-5,
#SF1 is the combined response, that is, it includes responses 
#in SCSF1 for non-proxy respondents. For Waves 6-9, users will
#need to combine responses from SF1 and SCSF1.

df_ukhls$SRH = ifelse(df_ukhls$sf1 == -8, df_ukhls$scsf1, df_ukhls$sf1)
table(df_ukhls$SRH, df_ukhls$wave)
table(df_ukhls$SRH, df_ukhls$year)

# Filtering
df_ukhls = df_ukhls[df_ukhls$year >= 2013,]

# adding general policy data
RSX_RG = readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/RSX_RG.RData'))

# fixing Northamptonshire (North + West) in the survey dataset
df_ukhls[df_ukhls$UTLACD %in% c('E06000062', 'E06000061'), 'UTLACD'] = 'E10000021'

# Christchurch to Dorset before 2019
pidp_Christchurch_to_Dorset = df_ukhls[grepl('Christchurch', df_ukhls$LSOA01NM) & df_ukhls$year < 2019, c('year', 'pidp')]
df_ukhls[df_ukhls$pidp %in% pidp_Christchurch_to_Dorset$pidp &
           df_ukhls$year %in% pidp_Christchurch_to_Dorset$year &
           df_ukhls$UTLACD == 'E06000058', 'UTLACD'] = 'E06000059'

# test the match
df_ukhls = df_ukhls %>%
  left_join(RSX_RG, by = c('UTLACD', 'year'), keep = F)
summary(df_ukhls$population) # no NAs - great!

df_ukhls$BCP_fix = ifelse(df_ukhls$UTLACD == 'E06000058' & grepl('Bournemouth', df_ukhls$LSOA01NM) & grepl('Bournemouth', df_ukhls$la), 1,
                          ifelse(df_ukhls$UTLACD == 'E06000058' & grepl('Poole', df_ukhls$LSOA01NM) & grepl('Poole', df_ukhls$la), 1,
                                 ifelse(df_ukhls$la == 'Bournemouth, Christchurch and Poole UA', 1,
                                        ifelse(!df_ukhls$UTLACD == 'E06000058', 1, 0))))
df_ukhls = df_ukhls %>% filter(!BCP_fix == 0)

# checking n of times each person participated in the survey
df_ukhls = df_ukhls %>%
  group_by(pidp) %>%
  mutate(n = n()) %>%
  group_by(pidp, wave) %>%
  mutate(n_2 = n())
table(df_ukhls$n)
table(df_ukhls$n_2)

# Saving
df = df_ukhls
saveRDS(df, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/df.rds'))

