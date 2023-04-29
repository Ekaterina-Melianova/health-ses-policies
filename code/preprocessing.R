# Pre-processing.R

library(readxl)
library(plyr); library(dplyr)
library(magrittr)
library(imputeTS)
library(panelr)
library(IMD)
library(glue)
library(tidyverse)
library(lavaan)
library(semTable)
library(ggplot2)
library(ggpubr)
library(stargazer)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.R')

# load
spending_data = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/spending_data.csv')[-1]
names(spending_data) <- gsub("_total", "", names(spending_data))

# def
deflator = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial/GDP_Deflators_Budget_March_2021_update.xlsx',
                      range = 'H7:I73')
names(deflator) = c('year', 'def')
deflator$year = as.numeric(deflator$year)

# merging with outturns
spending_data = spending_data %>%
  left_join(deflator, by = 'year')

# in prices of 2020
spending_data %<>%
  mutate(across(education:other_inc, ~ . /def*100 ))

# Small Area Mental Health Index (SAMHI)
health_lsoa = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/samhi_21_01_v4.00_2011_2019_LSOA_tall.csv')
lsoa_lad = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.csv')
lsoa_lad %<>% dplyr::select(LAD21CD, lsoa11 = LSOA11CD, MSOA11CD)
lsoa_lad = lsoa_lad %>% distinct()
health_lsoa %<>% left_join(lsoa_lad, by = "lsoa11")
#table(is.na(health_lsoa$LAD21CD))
#tab = health_lsoa %>% group_by(lsoa11) %>% summarise(n = n())

# filtering missings
# to_filter = as.data.frame(table(spend_data[is.na(spend_data$education_inc), 'LAD21CD']))
# spend_data %<>%
#   filter(!LAD21CD %in% as.character(to_filter[to_filter$Freq>1, 'Var1']))
# spend_data = spend_data %>% group_by(LAD21CD) %>%
#   dplyr::mutate(across(c(education:police, other),
#                        ~ if_else(year >= 2012, na_ma(.x), .x)))

# fixing Northamptonshire (if Net spending)
#spend_data[spend_data$LAD21CD == 'E07000152', 'LAD21CD'] = 'E06000061'
#spend_data[spend_data$LAD21CD == 'E07000155', 'LAD21CD'] = 'E06000062'

df = health_lsoa %>% 
  filter(year %in% 2013:2019) %>%
  left_join(spending_data, by = c('LAD21CD', 'year'))
tab = df %>% group_by(lsoa11) %>%
  summarize(n_non_missing = sum(!is.na(police)))

df %<>% filter(!lsoa11 %in% as.data.frame(tab[tab$n_non_missing == 0, 'lsoa11'])$lsoa11)
table(df$year) # 31317
summary(df)

# IMD
# dep_lsoa = IMD::imd_england_lsoa
# dep_lsoa = dep_lsoa %>% dplyr::select(lsoa11 = lsoa_code, everything())
# 
# dep_lad = IMD::imd_england_lad
# dep_lad = dep_lad %>% dplyr::select(LAD21CD = lad_code, Income_Score, Income_Proportion)
# 
# df = df %>% dplyr::left_join(dep_lsoa) %>%
#   dplyr::left_join(dep_lad)

# from the Department for Levelling Up, Housing and Communities
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data')
dep15 = read.csv('imd2015lsoa.csv')
table(dep15$Indices.of.Deprivation)
# length(unique(dep15$FeatureCode)) n LSOAs in 2019
dep15 %<>% filter(Indices.of.Deprivation %in% c('b. Income Deprivation Domain', 
                                                'c. Employment Deprivation Domain',
                                                'd. Education, Skills and Training Domain',
                                                'i. Income Deprivation Affecting Children Index (IDACI)',
                                                'j. Income Deprivation Affecting Older People Index (IDAOPI)'
                                                )) %>%
  filter(Measurement == 'Score') %>% 
  dplyr::select(FeatureCode, Value, Indices.of.Deprivation) %>%
  pivot_wider(id_cols = FeatureCode, names_from = Indices.of.Deprivation,
              values_from = Value, names_sort = T) 
names(dep15) = c('lsoa11', 'inc_dep', 'empl_dep', 'edu_dep', 'yinc_dep', 'oinc_dep')
dep15$lsoa_ses_score = rowMeans(dep15[, c('inc_dep', 'empl_dep', 'yinc_dep', 'oinc_dep')])

# joining with the main df
df %<>% left_join(dep15)

# codes for merging LSOAa with CCGs
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data')
lsoa_to_ccg15 = read.csv('Lower_Layer_Super_Output_Area_(2011)_to_Clinical_15.csv')
lsoa_to_ccg16 = read.csv('Lower_Layer_Super_Output_Area_(2011)_to_Clinical_16.csv')
lsoa_to_ccg17 = read.csv('Lower_Layer_Super_Output_Area_(2011)_to_Clinical_17.csv')
lsoa_to_ccg18 = read.csv('Lower_Layer_Super_Output_Area_(2011)_to_Clinical_18.csv')
lsoa_to_ccg19 = read.csv('Lower_Layer_Super_Output_Area_(2011)_to_Clinical_19.csv')

lsoa_to_ccg = list(
  cbind(lsoa_to_ccg15 %>% dplyr::select(LSOA11CD), 'year' = 2013, 'CCG' = lsoa_to_ccg15$CCG15CDH),
  cbind(lsoa_to_ccg15 %>% dplyr::select(LSOA11CD), 'year' = 2014, 'CCG' = lsoa_to_ccg15$CCG15CDH),
  cbind(lsoa_to_ccg15 %>% dplyr::select(LSOA11CD), 'year' = 2015, 'CCG' = lsoa_to_ccg15$CCG15CDH),
  cbind(lsoa_to_ccg16 %>% dplyr::select(LSOA11CD), 'year' = 2016, 'CCG' = lsoa_to_ccg16$CCG16CDH),
  cbind(lsoa_to_ccg17 %>% dplyr::select(LSOA11CD), 'year' = 2017, 'CCG' = lsoa_to_ccg17$CCG17CDH),
  cbind(lsoa_to_ccg19 %>% dplyr::select(LSOA11CD), 'year' = 2018, 'CCG' = lsoa_to_ccg19$CCG19CDH),
  cbind(lsoa_to_ccg19 %>% dplyr::select(LSOA11CD), 'year' = 2019, 'CCG' = lsoa_to_ccg19$CCG19CDH)) %>% 
  purrr::reduce(rbind)

# from dashboard
dash = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/CCG-allocations-dashboard-to-2023-24.xlsx',
                  sheet = 'Real terms data', range = 'D2:X222')
dash = dash[,c(1,15:ncol(dash))]
colnames(dash) = c('CCG', 2013:2019)
dash %<>%  filter(!is.na(CCG)) %>% filter(CCG != '01K(old)')
#dash[dash$CCG == '01K(old)', 'CCG'] <- '01K'
dash %<>%# group_by(CCG) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% 
  pivot_longer(cols = '2013':'2019', values_to = 'ccg', names_to ='year') 
dash$year = as.numeric(dash$year)
dash_lsoa = dash %>% left_join(lsoa_to_ccg)

# testing merging
dfn = df %>% left_join(dash_lsoa, by = c('lsoa11' = 'LSOA11CD', 'year'))%>%
  filter(year >=2013)

# checking missing or 0
table(dfn[dfn$ccg==0,'CCG'], dfn[dfn$ccg==0,'year']) # 01H
table(dfn[is.na(dfn$ccg),'CCG'], dfn[is.na(dfn$ccg),'year']) # 01K

# On the 1st April 2017 NHS Cumbria CCG (01H) and NHS Lancashire North CCG (01K)
# were reapportioned structurally to form NHS North Cumbria CCG (01H) and Morecambe Bay CCG (01K)
# Therefore, replace 01K and 01H with a spline 

# replacing with NA 01H
dash$ccg = ifelse(dash$CCG=='01H' & dash$year<=2016, NA, dash$ccg)
table(dash[is.na(dash$ccg), 'CCG'])

dash = dash %>% group_by(CCG) %>%
  mutate(ccg = if_else(CCG %in% c('01H', '01K'), na_interpolation(ccg, option='spline'), ccg))
dash_lsoa = dash %>% left_join(lsoa_to_ccg)

# final merging
df = df %>% left_join(dash_lsoa, by = c('lsoa11' = 'LSOA11CD', 'year'))%>%
  filter(year >=2013)
table(df[is.na(df$ccg),'CCG'], df[is.na(df$ccg),'year']) # < table of extent 0 x 0 >
summary(df)

# control variables from census 2011

# ethnicity
census1 = read.csv('census_2011_ethnicity.csv')[,c(3,5,6)]
colnames(census1) = c('lsoa11', 'all', 'whites')
census1 %<>% mutate(nonwhite = 1 - whites/all) %>% 
  dplyr::select(lsoa11, pop_census11 = all, nonwhite)

# sex
census2 = read.csv('census_2011_sex.csv')[,c(3,4,5)]
colnames(census2) = c('lsoa11', 'all', 'males')
census2 %<>% mutate(females = 1 - males/all) %>% dplyr::select(lsoa11, females)

# age
census3_ = read.csv('census_2011_age.csv')
census3 = census3_[,c(3,5,71:ncol(census3_))]
colnames(census3) = c('lsoa11', 'all', paste0(65:100))
census3 %<>% mutate(older = rowSums(.[3:ncol(census3)])/all) %>%
  dplyr::select(lsoa11, older)

# rural-urban
census4 = read.csv('rural_urban_lsoa.csv')
table(census4$RUC11, census4$RUC11CD)
census4$rural = ifelse(census4$RUC11CD %in% c('D1', 'D2', 'E1', 'E2'), 1, 0)
census4 %<>% dplyr::select(lsoa11 = LSOA11CD, rural)

# merging all controls with df
controls_census = list(census1, census2, census3, census4) %>%
  reduce(dplyr::left_join, by = 'lsoa11')
df %<>% left_join(controls_census, by = 'lsoa11')

# filtering City of London and Isles of Scilly
df %<>% filter(!LAD21CD %in% c('E09000001',
                               'E06000053')) %>%
  ungroup()

# number of lsoas within each lad
df %<>% group_by(year, LAD21CD) %>% dplyr::mutate(n = n())

# time variable
df = as.data.frame(df)
df$time = df$year - (min(df$year)-1)

# policy groups
#df$public_health = ifelse(is.na(df$public_health), 0, df$public_health)
df$health = df$social_care #+ df$public_health
df$healthcare = df$ccg#/1000 # to make it in thousand £ as other spends
df$env = df$environment + df$planning + df$cultural
df$education = df$education
df$law_order = df$housing + df$police
df$infrastructure = df$transport + df$fire + df$central + df$other
df$total = df$health + df$healthcare + df$education + df$env + df$law_order + df$infrastructure
df %<>% group_by(LAD21CD) %>%
  mutate(public_health_mean = mean(public_health))

# inc average
df = cbind.data.frame(df, 'inc' = rowMeans(df[, grep("inc$", names(df))]))
df %<>% group_by(LAD21CD) %>%
  mutate(inc_mean = mean(inc))
summary(df)

# class
df$London = ifelse(df$class == 'L', 1, 0)
df$MD = ifelse(df$class == 'MD', 1, 0)
df$SD = ifelse(df$class == 'SD', 1, 0)

#df = df %>% filter(housing_transport > 0 & environment_planning_cultural > 0 & pub_health_soc_care > 0)

# saving the final df
saveRDS(df, 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')




