 
library(plyr); library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(readODS)
library(zoo)
source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/prelims/functions.r')

####################
####################  RSX
####################

policy_RSX_RG = loadLists(sites = sites[7:length(sites)], year_seq = 2013:2020,
                       list_number = 3, type = 'RSX|RG')

# cleaning the range in excel worksheets 

RSX_RG_cleaned = policy_RSX_RG
for (i in seq_along(policy_RSX_RG)){
  for (j in seq_along(policy_RSX_RG[[i]])){
    
  # selecting a proper starting row (which matches a certain string)
  range_start = NULL
  col_skip = 0
  search_string = "E-code"
  max_cols_to_search = 10
  max_rows_to_search = 10
  
  # defining the position of the starting row
  while (length(range_start) == 0) {
    col_skip = col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_start = which(str_detect(policy_RSX_RG[[i]][[j]][1:max_rows_to_search, col_skip],search_string))
  }
  
  # selecting a proper ending row
  range_end = NULL
  col_skip = 0
  search_string = "E7055"
  max_cols_to_search = 10
  max_rows_to_search = 490
  
  # defining the position of the ending row
  while (length(range_end) == 0) {
    col_skip = col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_end = which(str_detect(policy_RSX_RG[[i]][[j]][1:max_rows_to_search, col_skip],search_string))
  }
  
  # cutting based on the new starting and ending points
  RSX_RG_cleaned[[i]][[j]] = policy_RSX_RG[[i]][[j]][range_start:range_end,]
  colnames(RSX_RG_cleaned[[i]][[j]]) = RSX_RG_cleaned[[i]][[j]][1,]
  RSX_RG_cleaned[[i]][[j]] = RSX_RG_cleaned[[i]][[j]][-1,]
} 
}

# extracting only columns with Net Current Expenditure plus first 5 columns with ids
RG_col = 'Adult and Community Learning from Skills Funding Agency'
RSX_col = 'Net Current Expenditure'
ids = c('E-code',
        'Local authority',
        'Class',
        'ONS Code')
RSX_RG_tmp = lapply(seq_along(RSX_RG_cleaned),
                    function(i) {
                      lapply(seq_along(RSX_RG_cleaned[[i]]),
                             function(j){
                               RSX_RG_cleaned[[i]][[j]][as.logical(replace(grepl(paste0(c(RG_col, RSX_col), collapse = '|'),
                                                                                 colnames(RSX_RG_cleaned[[i]][[j]])),
                                                                           which(colnames(RSX_RG_cleaned[[i]][[j]]) %in% ids), 'TRUE'))]
                               }
                      ) 
                      }
                    )


# saving
RSX_RG = RSX_RG_tmp

# removing redundant rows and columns with NAs
RSX_RG = lapply(seq_along(RSX_RG), function(i){
  lapply(seq_along(RSX_RG[[i]]),
         function(j) RSX_RG[[i]][[j]][!rowSums(is.na(RSX_RG[[i]][[j]])) == ncol(RSX_RG[[i]][[j]]), ])
  #lapply(i, function(j) j[,! (grepl('NA', colnames(j)) | is.na(colnames(j)))])
  }
)

# checking naming sequence
for (i in seq_along(RSX_RG)){
  for (j in seq_along(RSX_RG[[i]])){
      print(names(RSX_RG[[i]][[j]][,1:4]))
  }
}

# keep lists' naming
for (i in seq_along(RSX_RG)){
  names(RSX_RG[[i]]) = names(RSX_RG_cleaned[[i]])
}

# filtering ids
#la_ids = colnames(RSX_RG[[3]][1:4])
#RSX_RG = lapply(RSX_RG, function(i) i %>% select(contains(c(la_ids, grep('Net', colnames(i))))))

# renaming columns
general_policies = c('education',
                     'highways_transport',
                     'children_social_care',
                     'adult_social_care',
                     'public_health',
                     'housing',
                     'cultural',
                     'environmental',
                     'planning_development',
                     'police',
                     'fire_rescue',
                     'central',
                     'other',
                     'total')
grant_policies = 'skills_funding'
  
# 2013
ids.1 = c('code',
          'la',
          'class')

# 2014-2020
ids.2 = c('code',
          'LAD',
          'la',
          'class')

# year vector
year = seq(2013, 2020)

# list number for policy types
RSX_list_number = which(grepl('RSX', names(RSX_RG[[1]])))
RG_list_number = which(grepl('RG', names(RSX_RG[[1]])))

# renaming columns
for (i in seq_along(RSX_RG)){
  for (j in seq_along(RSX_RG[[i]])){

    if (year[i] == 2013){
    colnames(RSX_RG[[i]][[RSX_list_number]]) = c(ids.1, general_policies)
    colnames(RSX_RG[[i]][[RG_list_number]]) = c(ids.1, grant_policies)
    }
    else{
      colnames(RSX_RG[[i]][[RSX_list_number]]) = c(ids.2, general_policies)
      colnames(RSX_RG[[i]][[RG_list_number]]) = c(ids.2, grant_policies)
    }
  }
  RSX_RG[[i]][[RSX_list_number]]['year'] = as.data.frame(year[i])
  RSX_RG[[i]][[RG_list_number]]['year'] = as.data.frame(year[i])
}

# ONS code = LSOA01CD for 2013
for (j in seq_along(RSX_RG[[1]])){
  RSX_RG[[1]][[j]] = RSX_RG[[1]][[j]] %>%
  left_join(RSX_RG[[2]][[j]][, c('code', 'LAD')])
}

# long format
RSX_RG_long = do.call(plyr::rbind.fill, lapply(RSX_RG, function(i) purrr::reduce(i, dplyr::left_join)))
table(table(RSX_RG_long$LAD)) # 342 LAs appear 8 times

# saving
RSX_RG = RSX_RG_long

# replace n/a - absent la codes by codes from other years
RSX_RG$LAD = ifelse(is.na(RSX_RG$LAD), 'n/a', RSX_RG$LAD)

while (length(RSX_RG[RSX_RG$LAD == 'n/a' ,'LAD']) > 0){
  la_string = RSX_RG[RSX_RG$LAD == 'n/a','la'][1]
  seq = as.character(na.omit(RSX_RG[RSX_RG$la == la_string, 'LAD']))
  place = !seq == 'n/a' & !is.na(seq)

  if (all(place == F)){
    RSX_RG[RSX_RG$LAD == 'n/a' ,]$LAD[1] = 'NA'
    
  }
  else{
     res = unique(seq[place])
     RSX_RG[RSX_RG$LAD == 'n/a' ,]$LAD[1] = res 
  }

  #print(la_string)
}

# check
length(RSX_RG[RSX_RG$LAD == 'n/a'& !is.na(RSX_RG$LAD),'LAD']) == 0

# replace character NA with NA as a missing value (7 Waste and Transport Authorities)
RSX_RG[RSX_RG == "NA"] <- NA
RSX_RG = RSX_RG[!is.na(RSX_RG$LAD),] # remove them 

# population data
population_raw = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Socio-demographics/Population.xls',
                             sheet = 'MYE 5', skip = 7)
population_raw = population_raw[as.logical(replace(grepl("Population",
                                                          colnames(population_raw)), 1, 'TRUE'))]
population_raw = population_raw[, 1:(length(year) + 1)] # selecting 8 years plus the first 'code' column
names(population_raw) = c('LAD', year)

# long format
population = population_raw %>%
  pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "population")
population$year = as.numeric(population$year)

# old population data (before 2018)
population_18_raw = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/Socio-demographics/MYEB3_summary_components_of_change_series_UK_(2018).csv')
population_18_raw = population_18_raw[as.logical(replace(grepl("population",
                                                         colnames(population_18_raw)), 1, 'TRUE'))]
population_18_raw = population_18_raw[, c(1, (ncol(population_18_raw) - (length(year) - 3)):ncol(population_18_raw))] # selecting years
names(population_18_raw)[-1] <- 2013:2018

# long format
population_18 = population_18_raw %>%
  pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "population")
population_18$year = as.numeric(population_18$year)

########## To UPPER TIER LAs

setwd("C:/Users/ru21406/YandexDisk/PhD Research/Data")

# removing SD (lower tier la)
RSX_RG = RSX_RG[!RSX_RG$class == 'SD',]

# merging with la codes
la_all_codes = read.csv('la_all_codes.csv') %>% 
  select(LADCD, LADCD_ACTIVE, UTLACD, CAUTHCD) %>%
  distinct_at(vars(UTLACD), .keep_all = T)

RSX_RG = RSX_RG %>% 
  left_join(la_all_codes, by = c('LAD' = 'UTLACD'), keep = T) %>%
  left_join(population, by = c('LAD', 'year'))

# check missings in LA codes which mean restructuring across the years
#vec = c()
#for (i in 1:nrow(RSX_RG)){
#  vec = rbind.data.frame(vec, RSX_RG_population[RSX_RG_population$la == RSX_RG[i, 'la'], c('LAD', 'la')])

#}

# getting problematic LAs without population data
changed_las = RSX_RG[is.na(RSX_RG$population) & !RSX_RG$class == 'O', 'la']
changed_las = unique(str_remove(changed_las, ' CC| UA'))
changed_las

# fixing problematic LAs manually
# Buckinghamshire (became UA in 2020)
RSX_RG[RSX_RG$LAD == 'E10000002', 'UTLACD'] = 'E06000060'
RSX_RG[RSX_RG$LAD == 'E10000002', 'LAD'] = 'E06000060'
RSX_RG = RSX_RG %>% select(-population) %>% 
  left_join(population, by = c('LAD', 'year'))

# Bournemouth, Christchurch and Poole UA joined in 2019, 2020
for (j in c('E06000028', 'E06000029')){
  for (i in 2013:2018){
    RSX_RG[RSX_RG$LAD == j & RSX_RG$year == i, 'population'] =
    population_18[population_18$ladcode18 == (RSX_RG[RSX_RG$LAD == j, 'LAD'])[1] &
                    population_18$year == i, 'population']  
  }
}
RSX_RG[RSX_RG$LAD == 'E06000029' , 'UTLACD'] = 'E06000058'
RSX_RG[RSX_RG$LAD == 'E06000028' , 'UTLACD'] = 'E06000058'
RSX_RG[RSX_RG$LAD == 'E06000029' , 'LAD'] = 'E06000058'
RSX_RG[RSX_RG$LAD == 'E06000028' , 'LAD'] = 'E06000058'

# Dorset (became UA in 2019)
RSX_RG[RSX_RG$LAD == 'E10000009', 'UTLACD'] = 'E06000059'
RSX_RG[RSX_RG$LAD == 'E10000009', 'LAD'] = 'E06000059'
RSX_RG = RSX_RG %>% select(-population) %>% 
  left_join(population, by = c('LAD', 'year'))

# North Yorkshire (something happened in 2015, 2017, 2018)
RSX_RG[RSX_RG$LAD == 'E10000022', 'UTLACD'] = 'E10000023'
RSX_RG[RSX_RG$LAD == 'E10000022', 'LAD'] = 'E10000023'
RSX_RG = RSX_RG %>% select(-population) %>% 
  left_join(population, by = c('LAD', 'year'))

# Northamptonshire = North + West
population$LAD = substr(population$LAD, 1, 9) # removing an extra symbol in the end of codes
for (i in seq_along(year)){
    RSX_RG[RSX_RG$LAD == 'E10000021' & RSX_RG$year == year[i], 'population'] = 
      sum(population[population$LAD %in% c('E06000062', 'E06000061') & population$year == year[i], 'population'])
}
RSX_RG[RSX_RG$LAD == 'E10000021', 'UTLACD'] = 'E10000021'

# numeric format
RSX_RG[, c(general_policies, grant_policies)] = 
  sapply(RSX_RG[, c(general_policies, grant_policies)], as.numeric)

# categories filtering
RSX_RG = RSX_RG %>% select(-c(other, total))
policies = c(general_policies[!general_policies %in% c('other', 'total')], grant_policies)

# Dealing with London Boroughs

Greater_London = RSX_RG[grepl('greater london', RSX_RG[,'la'], ignore.case = T),]
Greater_London = Greater_London %>% 
  select(all_of(c(policies, 'year'))) %>%
  rename_at(all_of(policies), function(x) paste0(x, '_g'))

London_Boroughs = RSX_RG[RSX_RG$class == 'L' ,]
London_Boroughs %<>% group_by(year) %>%
  mutate(margins = population/sum(population)) 

London = London_Boroughs %>%
  left_join(Greater_London, by = 'year') %>%
  ungroup()

# obtain final values for London Boroughs
London[, policies] = (London[, policies] +
                                t(London[,"margins"])*(London %>% select(ends_with('_g'))))
London = London %>% select(!c(ends_with('_g'), margins))
London = London %>% dplyr::rename(police_fin = 'police',
                           fire_rescue_fin = 'fire_rescue',
                           central_fin = 'central')

# remove Greater London from the main df
RSX_RG = RSX_RG %>% filter(!grepl('greater london', RSX_RG$la, ignore.case = T))

##### Dealing with Combined Authorities

# special case - The Broads Authority (Norfolk + Suffolk)
broads_code = unique(RSX_RG[grepl('Broads', RSX_RG$la, ignore.case = T), 'LAD'])
RSX_RG$CAUTHCD = ifelse(!RSX_RG$class == 'O' & grepl('Norfolk|Suffolk', RSX_RG$la, ignore.case = T), broads_code, 
                     RSX_RG$CAUTHCD)

# the rest
Combined_Authorities = RSX_RG[grepl('combined authority|broads', RSX_RG$la, ignore.case = T),]
Combined_Authorities = Combined_Authorities %>% 
  dplyr::select(all_of(c(policies, 'year', 'LAD'))) %>%
  dplyr::rename_at(all_of(policies), function(x) paste0(x, '_ca'))

Combined_Authorities_lower = RSX_RG %>% filter(!CAUTHCD == '')
Combined_Authorities_lower %<>% group_by(year, CAUTHCD) %>%
  mutate(margins_comb = population/sum(population)) 

CA = Combined_Authorities_lower %>%
  left_join(Combined_Authorities, by = c('year', 'CAUTHCD' = 'LAD')) %>%
  ungroup()

# remove lower tier LA (within the combined ones) in periods preceding the introduction of the respective CAs
CA = CA[!is.na(CA$education_ca),]

# obtain values for the Combined Authorities
CA[, policies] = (CA[, policies] + t(CA[,"margins_comb"])*(CA %>% select(ends_with('_ca'))))
CA = CA %>% select(!c(ends_with('_ca'), margins_comb))

# remove CAs
RSX_RG = RSX_RG %>% filter(!grepl('combined authority|broads', RSX_RG$la, ignore.case = T))

##### Dealing with Police, Fire, Waste, Park, and Transport Authorities

# Didn't find Waste, Park, and Transport Authorities - filter them out for now
RSX_RG = RSX_RG %>% filter(!grepl('waste|transport|park', la, ignore.case = T))

# keys for police
la_police = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/la_police_fire.xlsx',
                            sheet = 'police')
colnames(la_police) = c('PLAD', 'police_name', 'LAD', 'la_name')

# merging police with RSX_RG
la_police = la_police  %>% 
  dplyr::mutate(across(c(1,2), funs(ifelse(is.na(.), na.locf(.), .)))) %>%
  select(-c(PLAD, police_name, police_name_ifelse)) %>%
  dplyr::rename(PLAD = 'PLAD_ifelse')

la_police = la_police %>%
  left_join(la_all_codes[, c('LADCD', 'UTLACD')], by = c('LAD' = 'LADCD'))
la_police$UTLACD = ifelse(is.na(la_police$UTLACD), la_police$LAD, la_police$UTLACD)

# fixing Northamptonshire Police, Fire and Crime Commissioner Fire and Rescue Authority
RSX_RG[RSX_RG$la == 'Northamptonshire Police, Fire and Crime Commissioner Fire and Rescue Authority', 'la'] =
  'Northamptonshire Fire, and Crime Commissioner Fire and Rescue Authority'

plc = RSX_RG %>% filter(grepl('police', la, ignore.case = T)) %>%
  select(LAD, year, police, central) %>% distinct(.)

la_police_RSX_RG = la_police %>%
  left_join(plc, by = c('PLAD' = 'LAD'), keep = T) %>%
  filter(!is.na(LAD.y)) %>%
  dplyr::rename(LAD_p = 'LAD.x') %>%
  select(-LAD.y)

# fixing Dorset (Bournemouth, Christchurch and Poole UA) and Northamptonshire manually

# Dorset
Dorset = la_police_RSX_RG %>% filter(LAD_p == 'E06000059')
Dorset$LAD_p = 'E06000058'
Dorset$UTLACD = 'E06000058'
la_police_RSX_RG = rbind(la_police_RSX_RG, Dorset)

# Dorset missing in 2016 -> replace with the moving average
la_police_RSX_RG[la_police_RSX_RG$year == 2016 & la_police_RSX_RG$la_name == 'Dorset', c('police', 'central')] = 
  as.list(colMeans(la_police_RSX_RG[la_police_RSX_RG$year %in% c(2015, 2017) &
                                   la_police_RSX_RG$la_name == 'Dorset',
                                 c('police', 'central')]))

# Northamptonshire
la_police_RSX_RG[la_police_RSX_RG$PLAD == 'E23000022', c('LAD_p', 'UTLACD')] = 'E10000021'
la_police_RSX_RG[la_police_RSX_RG$la_name %in% c('North Northamptonshire',
                                           'West Northamptonshire'), 'la_name'] = 'Northamptonshire'
la_police_RSX_RG = la_police_RSX_RG %>% distinct(.)

RSX_RG = RSX_RG %>% filter(!grepl('police', la, ignore.case = T)) %>%
  dplyr::rename(police_lower = 'police', central_lower = 'central') %>%
  left_join(la_police_RSX_RG, by = c('UTLACD', 'year'))

# obtain values for the police category
RSX_RG = RSX_RG %>% group_by(PLAD, year) %>%
  mutate(margins_plc = population/sum(population))
RSX_RG[, c('police_fin', 'central_fin')] = (t(RSX_RG[, "margins_plc"])*RSX_RG[, c('police', 'central')]) + 
  RSX_RG[, c('police_lower', 'central_lower')]

# checking the merging
as.data.frame(table(RSX_RG[is.na(RSX_RG$PLAD), 'class'])) # 233 Fire Authorities remained plus 264 London Boroughs

# ungrouping 
RSX_RG = RSX_RG %>%
  ungroup()


###### keys for fire
la_fire = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/la_police_fire.xlsx',
                     sheet = 'fire')
colnames(la_fire) = c('FLAD', 'fire_name', 'LAD', 'la_name')

# merging fire with RSX_RG
la_fire = la_fire  %>% 
  dplyr::mutate(across(c(1,2), funs(ifelse(is.na(.), na.locf(.), .)))) %>%
  select(-c(FLAD, fire_name, fire_name_ifelse)) %>%
  dplyr::rename(FLAD = 'FLAD_ifelse')

la_fire = la_fire %>%
  left_join(la_all_codes[, c('LADCD', 'UTLACD')], by = c('LAD' = 'LADCD'))%>%
  filter(grepl('E', FLAD, ignore.case = T)) # England only
la_fire$UTLACD = ifelse(is.na(la_fire$UTLACD), la_fire$LAD, la_fire$UTLACD)

fire = RSX_RG %>% filter(grepl('fire', la, ignore.case = T)) %>%
  select(la, LAD, year, fire_rescue) %>% distinct(.)

# fixing restructuring in Dorset and Wiltshire Combined Fire and Rescue Authority before 2016
Swindon = la_fire %>% filter(la_name == 'Swindon')
Wiltshire = la_fire %>% filter(la_name == 'Wiltshire')
Dorset = la_fire %>% filter(la_name == 'Dorset')

SWD =
  rbind(cbind(Swindon[rep(1, each = 3),], (fire %>% filter(la == 'Wiltshire Combined Fire and Rescue Authority'))), 
        cbind(Wiltshire[rep(1, each = 3),], (fire %>% filter(la == 'Wiltshire Combined Fire and Rescue Authority'))), 
        cbind(Dorset[rep(1, each = 3),], (fire %>% filter(la == 'Dorset Combined Fire and Rescue Authority'))) 
  )
SWD = SWD[,-6]

# merging
la_fire_RSX_RG = la_fire %>%
  left_join(fire, by = c('FLAD' = 'LAD'), keep = T)  %>% 
  dplyr::rename(LAD = 'LAD.x') %>%
  select(-LAD.y)

la_fire_RSX_RG = rbind(la_fire_RSX_RG, SWD) %>% 
  dplyr::rename(LAD_f = 'LAD')

# additionally fixing Dorset (Bournemouth, Christchurch and Poole UA) and Northamptonshire manually

# Dorset
Dorset = la_fire_RSX_RG %>% filter(LAD_f == 'E06000059') 
Dorset$LAD_f = 'E06000058'
Dorset$UTLACD = 'E06000058'
la_fire_RSX_RG = rbind(la_fire_RSX_RG, Dorset)

# Northamptonshire
la_fire_RSX_RG[la_fire_RSX_RG$FLAD == 'E31000028', c('LAD_f', 'UTLACD')] = 'E10000021'
la_fire_RSX_RG[la_fire_RSX_RG$la_name %in% c('North Northamptonshire',
                                       'West Northamptonshire'), 'la_name'] = 'Northamptonshire'

la_fire_RSX_RG = la_fire_RSX_RG  %>% 
  filter(!is.na(fire_rescue)) %>%
  select(-c(la_name, la)) %>% distinct(.)

# adding cleaned fire data to the main dataset
RSX_RG = RSX_RG %>% filter(!grepl('fire', la, ignore.case = T)) %>%
  dplyr::rename(fire_rescue_lower = 'fire_rescue') %>%
  left_join(la_fire_RSX_RG, by = c('UTLACD', 'year'))

# fixing NA for authorities that belong to the combined ones (MD) or London
RSX_RG[is.na(RSX_RG$fire_rescue), 'fire_rescue'] = 0
RSX_RG[is.na(RSX_RG$police), 'police'] = 0
RSX_RG[is.na(RSX_RG$central), 'central'] = 0

# checking missing LA codes in the merged data
table(RSX_RG[is.na(RSX_RG$FLAD), 'class'])

# fixing NAs in LAD for fire LAs (happened due to merging)
lvec = which(RSX_RG$la_name == 'Northamptonshire')
RSX_RG[lvec, 'FLAD'] = 'E31000028'

for (i in 1:nrow(RSX_RG)){
  if (is.na(RSX_RG[i, 'FLAD'])){
      RSX_RG[i,'FLAD'] = la_fire[which(la_fire[,'UTLACD'] == as.character(RSX_RG[i,'UTLACD'])),'FLAD']
  }
}

# obtain values for the fire policy
RSX_RG = RSX_RG %>% group_by(FLAD, year) %>%
  mutate(margins_fire = population/sum(population))
RSX_RG[, "fire_rescue_fin"] = (t(RSX_RG[, "margins_fire"])*RSX_RG[, "fire_rescue"]) +
  RSX_RG[, 'fire_rescue_lower']

# bringing London Boroughs and Combined Authorities back to the main df
RSX_RG = as.data.frame(RSX_RG)
CA = as.data.frame(CA)

# Combined Authorities

for (i in 2013:2020){
  for (j in unique(CA$LAD)){
    if (!length(CA[CA$year == i & CA$LAD == j, 'police']) == 0){
      RSX_RG[RSX_RG$year == i & RSX_RG$LAD == j, 'police_fin'] = RSX_RG[RSX_RG$year == i & RSX_RG$LAD == j, 'police_fin'] +
        CA[CA$year == i & CA$LAD == j, 'police']
    }
    
    if (!length(CA[CA$year == i & CA$LAD == j, 'fire_rescue']) == 0){
      RSX_RG[RSX_RG$year == i & RSX_RG$LAD == j, 'fire_rescue_fin'] = RSX_RG[RSX_RG$year == i & RSX_RG$LAD == j, 'fire_rescue_fin'] +
        CA[CA$year == i & CA$LAD == j, 'fire_rescue']
    }
    
    if (!length(CA[CA$year == i & CA$LAD == j, 'central']) == 0){
      RSX_RG[RSX_RG$year == i & RSX_RG$LAD == j, 'central_fin'] = RSX_RG[RSX_RG$year == i & RSX_RG$LAD == j, 'central_fin'] +
        CA[CA$year == i & CA$LAD == j, 'central']
    }
  }
}

# London
RSX_RG = RSX_RG %>% filter(!RSX_RG$LAD %in% London$LAD)
RSX_RG = plyr::rbind.fill(RSX_RG, London)

# inspecting the merged output
RSX_RG_inspect = RSX_RG %>% select(c('code', 'la', 'class', 'year', 'LAD',
                         'LADCD', 'UTLACD', all_of(policies),
                         'police_lower', 'central_lower', 'fire_rescue_lower',
                         'police_fin', 'central_fin', 'fire_rescue_fin',
                         'population', 'margins_plc', 'margins_fire'))
RSX_RG = RSX_RG_inspect %>% select(-c('police', 'fire_rescue', 'central',
                                'police_lower', 'central_lower',
                                'fire_rescue_lower')) %>%
  dplyr::rename(police = 'police_fin',
         fire_rescue = 'fire_rescue_fin',
         central = 'central_fin')

# checking the presence of all LAs in the final df
table(RSX_RG$class, RSX_RG$year)
table(RSX_RG_long[RSX_RG_long$class %in% c('L', 'MD', 'SC', 'UA'), 'class'],
      RSX_RG_long[RSX_RG_long$class %in% c('L', 'MD', 'SC', 'UA'), 'year'])

# Replacing missing values with the moving average

# Isles of Scilly 2016
# Reading UA 2020
# Slough UA 2020
# Medway Towns UA 2020

for (i in 1:nrow(RSX_RG)){
  for (j in policies){
      if(is.na(RSX_RG[i, j])){
        
        before = RSX_RG[RSX_RG$year == (RSX_RG[i, 'year'] + 1) & RSX_RG$LAD == RSX_RG[i, 'LAD'], j]
        after = RSX_RG[RSX_RG$year == (RSX_RG[i, 'year'] - 1) & RSX_RG$LAD == RSX_RG[i, 'LAD'], j]
        
        if (!length(before) == 0 & !length(after) == 0){
          RSX_RG[i, j] = mean(before, after)
        }
        else{
          non_null = Filter(Negate(function(x) length(x) == 0), c(before, after))
          RSX_RG[i, j] = non_null
        }
      }
    }
  }


# deflator
deflator = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial/GDP_Deflators_Budget_March_2021_update.xlsx',
                       range = 'H7:I73')
names(deflator) = c('year', 'def')
deflator = deflator %>% filter(year %in% .GlobalEnv$year)
deflator$year = as.numeric(deflator$year)

# merging with outturns
RSX_RG = RSX_RG %>%
  left_join(deflator, by = 'year')

# computing per 1,000 people in prices of 2020
RSX_RG[policies] = 
  (RSX_RG[policies]/(RSX_RG[,'population']/1000))*(RSX_RG[, 'def']/100)[row(RSX_RG[policies])]

# hist
hist(RSX_RG$education)
hist(RSX_RG$public_health)
hist(RSX_RG[RSX_RG$skills_funding > 0 & RSX_RG$skills_funding < 30,
            'skills_funding'])

# saving
saveRDS(RSX_RG, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/RSX_RG.RData'))

