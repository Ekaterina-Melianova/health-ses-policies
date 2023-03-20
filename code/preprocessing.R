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
options(max.print=600)

wd = 'C:/Users/ru21406/YandexDisk/PhD Research/Data/Spending'
setwd(wd)

# lading files
files = list.files(wd)
nm = substr(files, 1, nchar(files)-4)
list = list()
for(i in 1:length(files)){
  assign(nm[i], read.csv(files[i]))
  list[[i]] = read.csv(files[i])
  
}
names(list) = nm
colnames(list[[13]])[2] = setNames('LTLA20CD', colnames(list[[13]])[2])

spend_data = list %>% 
  purrr::reduce(dplyr::full_join, by = c("Year", "LTLA20CD")) 

spend_data = spend_data[, c(1:3, 5, grep('PerCap', names(spend_data)))]
#names(spend_data)

# naming columns
spend_names = c()
for (i in list){
  spend_names = c(spend_names, gsub('_Services_NetExpen_PerCap', '', colnames(i)[6]))
}
colnames(spend_data) = c('year', 'LAD21CD', 'name', 'pop', tolower(spend_names))
policy_names = tolower(spend_names)
spend_data$year = as.numeric(spend_data$year)
spend_data[policy_names] = sapply(spend_data[policy_names], as.numeric)
#table(spend_data$year)
#table(spend_data$LAD21CD)

# def
deflator = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial/GDP_Deflators_Budget_March_2021_update.xlsx',
                      range = 'H7:I73')
names(deflator) = c('year', 'def')
deflator$year = as.numeric(deflator$year)

# merging with outturns
spend_data = spend_data %>%
  left_join(deflator, by = 'year')

# in prices of 2020
spend_data[policy_names] = spend_data[policy_names]*spend_data[,'def']/100

# Small Area Mental Health Index (SAMHI)
health_lsoa = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/samhi_21_01_v4.00_2011_2019_LSOA_tall.csv')
lsoa_lad = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.csv')
lsoa_lad %<>% dplyr::select(LAD21CD, lsoa11 = LSOA11CD, MSOA11CD)
lsoa_lad = lsoa_lad %>% distinct()
health_lsoa %<>% left_join(lsoa_lad, by = "lsoa11")
#table(is.na(health_lsoa$LAD21CD))

tab = health_lsoa %>% group_by(lsoa11) %>% summarise(n = n())

# filtering
to_filter = as.data.frame(table(spend_data[is.na(spend_data$education), 'LAD21CD']))
spend_data %<>%
  filter(!LAD21CD %in% as.character(to_filter[to_filter$Freq>1, 'Var1']))
spend_data = spend_data %>% group_by(LAD21CD) %>%
  dplyr::mutate(across(c(education:police, other),
                       ~ if_else(year >= 2012, na_ma(.x), .x)))

# fixing Northamptonshire
spend_data[spend_data$LAD21CD == 'E07000152', 'LAD21CD'] = 'E06000061'
spend_data[spend_data$LAD21CD == 'E07000155', 'LAD21CD'] = 'E06000062'

# obesity
#obes = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/QOF_4_09__LSOA.csv')
#obes = obes %>% select(lsoa11, year, obes_rate = p_rate)

# stroke
#stroke = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/QOF_4_10__LSOA.csv')
#stroke = stroke %>% select(lsoa11, year, stroke_rate = p_rate)

# vulnerability clusters
#v_clusters = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/consumervulnerabiltyclusters.csv')
#v_clusters %<>% dplyr::select(OA11CD = output_area, cluster)

# NHS for UK
#nhs = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/ukhajhaq2022hfxfscsv.csv')
#nhs = nhs %>% 
#  filter(Measure == 'Real terms' & HF_code == 'HFTOT') %>%
#  dplyr::select(HF_code, starts_with('X')) %>%
#  pivot_longer(cols = X1997:X2020, names_to = 'year',
#               values_to = 'nhs_tot') %>%
#  dplyr::select(nhs_tot, year) %>%
#  group_by(year) %>%
#  dplyr::summarise(nhs_tot = sum(nhs_tot))
# nhs$year = as.numeric(sub('X', '', nhs$year))

# joining
#letter2number = function(x) {utf8ToInt(x) - utf8ToInt("a") + 1L}
#myletters = letters[sample(26,8)]
#unname(sapply(myletters, letter2number))

df = health_lsoa %>% 
  left_join(spend_data, by = c('LAD21CD', 'year')) #%>%
  #left_join(obes, by = c('lsoa11', 'year')) %>%
  #left_join(stroke, by = c('lsoa11', 'year')) %>%
  #left_join(v_clusters) %>% 
  #left_join(nhs)
#table(df[is.na(df$education), 'LAD21CD'])
tab = df %>% group_by(lsoa11) %>% summarise(n = n())

# remove Copeland and Isles of Scilly with missing data
# also remove Dorset Council and Bournemouth, Christchurch and Poole due to unstable borders
df = df %>% filter(!LAD21CD %in% c('E06000053', 'E07000029',
                                   'E06000058', 'E06000059')) 

# from 2013 for now
#df = df %>% filter(year>=2013)
#table(df$year)
#length(unique(df$MSOA11CD))

#### regional gross disposable household income (GDHI) from ONS
#gross_raw = nomisr::nomis_get_data(id = "NM_185_1",
#                                   geography = c('TYPE431', 'TYPE432'),
#                                   time = 2013:2019)
#
#1
#gross_inc = gross_raw %>% filter(grepl('E', GEOGRAPHY_CODE) &
#                                   !grepl('T', GEOGRAPHY_CODE)) %>%
#  filter(MEASURE_NAME == 'Gross Disposable Household Income - GDHI (£m)') %>%
#  filter(COMPONENT_OF_GDHI == 0) %>%
#  dplyr::select(c(DATE, GEOGRAPHY_CODE,
#                  MEASURE_NAME, OBS_VALUE,
#                  GEOGRAPHY_TYPE)) %>%
#  filter(grepl('local authorities', GEOGRAPHY_TYPE))
#
#gross_inc = gross_inc %>% 
#  dplyr::rename(year = DATE, LAD21CD = GEOGRAPHY_CODE, 
#                gross_inc = OBS_VALUE) %>%
#  dplyr::select(year, LAD21CD, gross_inc) %>%
#  distinct()
#
#df = df %>% dplyr::left_join(gross_inc, by = c('LAD21CD', 'year'))

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

#df = df %>% group_by(MSOA11CD) %>% 
#  dplyr::mutate(Income_decile_msoa = mean(Income_decile)) %>%
# ungroup()
#df = df %>% group_by(LAD21CD) %>% 
#  dplyr::mutate(Income_decile_lad = mean(Income_decile))
#summary(df$Income_decile_lad)

# deflate earnings
#df$gross_inc = df$gross_inc*df$def/100

# CCGs
# ccg_16 = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/q-total-place-based-pace-change.xlsx',
#                     sheet = '2016-17', skip = 9)[, c('CCG', 'Target per head')] %>% na.omit()
# ccg_17 = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/q-total-place-based-pace-change.xlsx',
#                     sheet = '2017-18', skip = 9)[, c('CCG', 'Target per head')] %>% na.omit()
# ccg_18 = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/q-total-place-based-pace-change.xlsx',
#                     sheet = '2018-19', skip = 9)[, c('CCG', 'Target per head')] %>% na.omit()
# ccg_19 = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/q-total-place-based-pace-change.xlsx',
#                     sheet = '2019-20', skip = 9)[, c('CCG', 'Target per head')] %>% na.omit()
# ccg_13_15 = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/j-ccg-pace-change.xlsx',
#                     sheet = 'Option 1', skip = 6)
# colsel = grep('Target per head', colnames(ccg_13_15 %>% rename_with(., ~gsub(".", "", .x, fixed = T))%>% 
#   rename_with(., ~gsub("\r\n", " ", .x, fixed = T))))
# colsel = c('CCG', 'Allocation per head\r\n£', colnames(ccg_13_15)[colsel[c(2,4)]])
# ccg_13_15 = ccg_13_15[,colsel] %>% na.omit()

# ccg = list(ccg_13_15, ccg_16, ccg_17, ccg_18, ccg_19) %>% 
#   purrr::reduce(dplyr::full_join, by = 'CCG') 
# colnames(ccg) = c('CCG', 2013:2019)
# # tweaking some restructuring
# ccg[ccg$CCG == '13T', c('2013', '2014', '2015')] = 
#   as.list(colMeans(ccg[ccg$CCG %in% c('00F', '00G', '00H'), c('2013', '2014', '2015')]))
# ccg %<>% na.omit() %>% pivot_longer(cols = '2013':'2019', values_to = 'ccg',
#                                     names_to ='year')

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

#ccg %<>% left_join(lsoa_to_ccg[,c('CCG16CDH', 'LSOA11CD')], by = c('CCG' = 'CCG16CDH'))
#ccg$year = as.numeric(ccg$year)

# merging with the main df
#df = df %>% left_join(ccg, by = c('lsoa11' = 'LSOA11CD', 'year'))%>%
#  filter(year >=2013)
#df$ccg = df$ccg*df$def/100
#hist(df$ccg)

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
table(df[is.na(df$ccg),'CCG'], df[is.na(df$ccg),'year'])
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

# number of lsoas within each lad
df %<>% group_by(LAD21CD) %>% dplyr::mutate(n = n())

# filtering City of London
df %<>% filter(!LAD21CD %in% c('E09000001')) %>%
  ungroup()

# time variable
df = as.data.frame(df)
df$time = df$year - (min(df$year)-1)

# policy groups
df$public_health = ifelse(is.na(df$public_health), 0, df$public_health)
df$health = df$social_care + df$public_health
df$healthcare = df$ccg/1000 # to make it in thousand £ as other spends
df$env = df$environment + df$planning + df$cultural
df$education = df$education
df$law_order = df$housing + df$police
df$infrastructure = df$transport + df$fire + df$central + df$other
df$total = df$health + df$healthcare + df$education + df$env + df$law_order + df$infrastructure

# Budget size
#avr = mean(df$total)
#df$budget_size = ifelse(df$total > avr, 1, 0)

summary(df)

#df = df %>% filter(housing_transport > 0 & environment_planning_cultural > 0 & pub_health_soc_care > 0)


# saving the final df
saveRDS(df, 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')




