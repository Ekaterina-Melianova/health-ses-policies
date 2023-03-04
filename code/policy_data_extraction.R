 
library(plyr, exclude = 'mutate'); library(dplyr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(readODS)
library(zoo)
source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.r')

# data loading
POLICYDATA_LIST = loadLists(site = sites[7:length(sites)], year_seq = 2013:2020,
                       list_number = 3, type = 'RSX|RO3')

# special case for 2014 (data are split between lists)
POLICYDATA_LIST_14 = loadLists(site = sites[8], year_seq = 2014,
                            list_number = 3:5, type = 'RO3')

# special case for 2013 (Net Current Expenditure data are on a different list)
POLICYDATA_LIST_13 = loadLists(site = sites[7], year_seq = 2013,
                               list_number = 5, type = 'RO3')

# replace 2013 and 2014 in the main list
list_to_replace = c('POLICYDATA_LIST_14', 'POLICYDATA_LIST_13')
list_df_to_replace = c(names(POLICYDATA_LIST_14[[1]]), names(POLICYDATA_LIST_13[[1]]))

for (i in seq_along(list_to_replace)){
  l = lapply(POLICYDATA_LIST, function(x) which(grepl(list_df_to_replace[i], names(x)))) 
  POLICYDATA_LIST[[which(l > 0)]][[l[[which(l > 0)]]]] = eval(parse(text = paste0(list_to_replace[i], '[[1]][[1]]'))) 
  names(POLICYDATA_LIST[[which(l > 0)]][[l[[which(l > 0)]]]]) = names(eval(parse(text = paste0(list_to_replace[i], '[[1]]'))))
}


# cleaning the range of data in excel worksheets 
POLICYDATA_RAW = POLICYDATA_LIST
for (i in seq_along(POLICYDATA_LIST)){
  for (j in seq_along(POLICYDATA_LIST[[i]])){
    
  # selecting a proper starting row (which matches a certain string)
  range_start = NULL
  col_skip = 0
  search_string = "E-code"
  max_cols_to_search = 10
  max_rows_to_search = 20
  
  # defining the position of the starting row
  while (length(range_start) == 0) {
    col_skip = col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_start = which(str_detect(POLICYDATA_LIST[[i]][[j]][1:max_rows_to_search, col_skip],search_string))
  }
  
  # selecting a proper ending row
  range_end = NULL
  col_skip = 0
  #search_string = "E7055"
  search_string = 'ENGLAND'
  max_cols_to_search = 10
  max_rows_to_search = 480
  
  # defining the position of the ending row
  while (length(range_end) == 0) {
    col_skip = col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_end = which(str_detect(POLICYDATA_LIST[[i]][[j]][1:max_rows_to_search, col_skip],search_string))
  }
  
  # cutting based on the new starting and ending points
  POLICYDATA_RAW[[i]][[j]] = POLICYDATA_LIST[[i]][[j]][range_start:range_end,]
  colnames(POLICYDATA_RAW[[i]][[j]]) = POLICYDATA_RAW[[i]][[j]][1,]
  POLICYDATA_RAW[[i]][[j]] = POLICYDATA_RAW[[i]][[j]][-1,]
} 
}

# quick look 
# quick = POLICYDATA_RAW[[1]][[2]]
# quick = POLICYDATA_LIST_14[[1]][[1]]

# extracting  columns with Total Expenditure: general and health-specific outturns
# plus columns with ids

main_col = 'Net Current Expenditure'
RO3_cols = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Public_Health_subcat.xlsx')
RO3_cols[, !colnames(RO3_cols) %in% 'name'] = as.data.frame(apply(RO3_cols[, !colnames(RO3_cols) %in% 'name'],
                                                                  2, function(y) gsub('[[:punct:] ]+', ' ', y)))
RO3_cols = as.data.frame(RO3_cols)

# finding columns for the selected policies from health expenditure data - RO3
list_indicator = 'RO3'
POLICYDATA_COLNAMES = POLICYDATA_LIST
list_keep = lapply(POLICYDATA_COLNAMES, function(x) grep(list_indicator, names(x)))
POLICYDATA_COLNAMES = lapply(1:length(POLICYDATA_COLNAMES),
                             function(i) POLICYDATA_COLNAMES[[i]][[list_keep[[i]]]])

fin = data.frame(character())
for (i in seq_along(POLICYDATA_COLNAMES)){
  
  # selecting a proper ending row
  range_end = NULL
  col_skip = 0
  search_string = 'E-code'
  max_cols_to_search = 10
  max_rows_to_search = 20
  
  # defining the position of the ending row
  while (length(range_end) == 0) {
    col_skip = col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_end = which(str_detect(POLICYDATA_COLNAMES[[i]][1:max_rows_to_search, col_skip],search_string))
  }
  
  # cutting based on the new starting and ending points
  POLICYDATA_COLNAMES[[i]] = POLICYDATA_COLNAMES[[i]][1:range_end,]
  POLICYDATA_COLNAMES[[i]] = apply(POLICYDATA_COLNAMES[[i]], 2, function(y) gsub('[[:punct:] ]+', ' ', y))
  POLICYDATA_COLNAMES[[i]] = as.data.frame(POLICYDATA_COLNAMES[[i]])
  
  # selecting subcategories
  vec = c()
  
  if (grepl('2013-14', colnames(POLICYDATA_COLNAMES[[i]])[1])){
    to_match = as.vector(na.omit(RO3_cols[, '2013']))
    for (j in 1:length(to_match)){
      #matched = Map(grep, to_match[j], POLICYDATA_COLNAMES[[i]], fixed = TRUE)
      matched = as.data.frame(POLICYDATA_COLNAMES[[i]] == to_match[j])
      matched = Map(grep, TRUE, matched)
      names(matched) = 1:length(matched)
      n = as.numeric(names(unlist(lapply(matched, function(x) which(length(x) > 0))))) 
      vec = c(vec, n)
    }
  } else{
    budget_subname = (RO3_cols %>% select(as.character(str_extract_all(colnames(POLICYDATA_COLNAMES[[i]])[1], "\\d{4}"))) %>% na.omit)[,1]
      for (j in budget_subname){
        #matched = Map(grep, j, POLICYDATA_COLNAMES[[3]], fixed = TRUE)
        matched = as.data.frame(POLICYDATA_COLNAMES[[i]] == j)
        matched = Map(grep, TRUE, matched)
        names(matched) = 1:length(matched)
        n = as.numeric(names(unlist(lapply(matched, function(x) which(length(x) > 0))))) + 6 # adding 6 since we are looking for a column @Net Current Expenditure
        vec = c(vec, n)
       
    }
  }      
  fin = cbind.fill(fin, vec)
}

#see = POLICYDATA_COLNAMES[[3]]

fin = fin[-1]
names(fin) = 2013:2020
#rownames(fin) = health_col

## selecting respective columns in policy data

# ids
ids = c('E-code',
        'Local authority',
        'Class',
        'ONS Code')

# health policies
RO3_lnumber = lapply(POLICYDATA_RAW, function(x) which(grepl('RO3', names(x))))
for (i in seq_along(POLICYDATA_RAW)){
  
  ids_observed = which(colnames(POLICYDATA_RAW[[i]][[RO3_lnumber[[i]]]]) %in% ids)
  
  if (length(ids) > length(ids_observed)){
    # 2013 - special case - there were no ONS codes
    POLICYDATA_RAW[[i]][[RO3_lnumber[[i]]]] = POLICYDATA_RAW[[i]][[RO3_lnumber[[i]]]] %>%
    subset(select = c(which(colnames(.) %in% ids)[1:length(ids_observed)], as.vector(na.omit(fin[,i]))))
  } else {
    POLICYDATA_RAW[[i]][[RO3_lnumber[[i]]]] = POLICYDATA_RAW[[i]][[RO3_lnumber[[i]]]] %>%
      subset(select = c(which(colnames(.) %in% ids)[1:length(ids)], as.vector(na.omit(fin[,i]))))
  }

}

# general policies
POLICYDATA_tmp = lapply(seq_along(POLICYDATA_RAW),
                    function(i) {
                      lapply(seq_along(POLICYDATA_RAW[[i]]),
                             function(j){
                               POLICYDATA_RAW[[i]][[j]][c(which(colnames(POLICYDATA_RAW[[i]][[j]]) %in% ids),
                                                          grep(main_col,
                                                               colnames(POLICYDATA_RAW[[i]][[j]])))]
                               }
                      ) 
                      }
                    )

# removing redundant rows and columns with NAs
POLICYDATA = lapply(seq_along(POLICYDATA_tmp), function(i){
  lapply(seq_along(POLICYDATA_tmp[[i]]), function(j) {
    POLICYDATA_tmp[[i]][[j]][!rowSums(is.na(POLICYDATA_tmp[[i]][[j]])) == ncol(POLICYDATA_tmp[[i]][[j]]), ]
  })
  }
)

# removing total for England
POLICYDATA = lapply(seq_along(POLICYDATA), function(i){
  lapply(seq_along(POLICYDATA[[i]]),
         function(j) POLICYDATA[[i]][[j]][-which(POLICYDATA[[i]][[j]]$`Local authority` == 'ENGLAND'),])
})

# checking naming sequence
for (i in seq_along(POLICYDATA)){
  for (j in seq_along(POLICYDATA[[i]])){
      print(names(POLICYDATA[[i]][[j]][,1:4]))
  }
}

# keep lists' naming
for (i in seq_along(POLICYDATA)){
  names(POLICYDATA[[i]]) = names(POLICYDATA_RAW[[i]])
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
Glist_number = which(grepl('RSX', names(POLICYDATA[[1]])))
Hlist_number = which(grepl('RO3', names(POLICYDATA[[1]])))

# renaming columns
for (i in seq_along(POLICYDATA)){
  
  if (year[i] == 2013){
    colnames(POLICYDATA[[i]][[Glist_number]]) = c(ids.1, general_policies)
    colnames(POLICYDATA[[i]][[Hlist_number]]) = c(ids.1, as.vector(RO3_cols[,'2013'] %>% na.omit))
    }
    else{
      col = RO3_cols %>% select(as.character(str_extract_all(names(POLICYDATA[[i]])[Hlist_number], "\\d{4}"))) 
      colnames(POLICYDATA[[i]][[Glist_number]]) = c(ids.2, general_policies)
      colnames(POLICYDATA[[i]][[Hlist_number]]) = c(ids.2, as.data.frame(na.omit(col))[,1])
    }
  
  POLICYDATA[[i]][[Glist_number]]['year'] = as.data.frame(year[i])
  POLICYDATA[[i]][[Hlist_number]]['year'] = as.data.frame(year[i])
}

# ONS code = LSOA01CD for 2013
l13 = which(unlist(lapply(POLICYDATA, function(x) length(grep('2013-14', names(x)) == 0L))) > 0)
for (j in seq_along(POLICYDATA[[l13]])){
  POLICYDATA[[l13]][[j]] = POLICYDATA[[l13]][[j]] %>%
  left_join(POLICYDATA[[l13 + 1]][[j]][, c('code', 'LAD')])
}

# long format
POLICYDATA_long = do.call(plyr::rbind.fill, lapply(POLICYDATA, function(i) purrr::reduce(i, dplyr::left_join)))
table(table(POLICYDATA_long$LAD)) # 342 LAs appear 8 times

# saving
POLICYDATA = POLICYDATA_long

# replace n/a - absent la codes by codes from other years
POLICYDATA$LAD = ifelse(is.na(POLICYDATA$LAD), 'n/a', POLICYDATA$LAD)

while (length(POLICYDATA[POLICYDATA$LAD == 'n/a', 'code']) > 0){
  la_code = POLICYDATA[POLICYDATA$LAD == 'n/a', 'code'][1]
  seq = as.character(na.omit(POLICYDATA[POLICYDATA$code == la_code, 'LAD']))
  place = !seq == 'n/a' & !is.na(seq)

  if (all(place == F)){
    POLICYDATA[POLICYDATA$LAD == 'n/a' ,]$LAD[1] = 'NA'
    
  }
  else{
     res = unique(seq[place])
     POLICYDATA[POLICYDATA$LAD == 'n/a' ,]$LAD[1] = res 
  }

  #print(la_string)
}

# checking
length(POLICYDATA[POLICYDATA$LAD == 'n/a'& !is.na(POLICYDATA$LAD),'LAD']) == 0
#POLICYDATA[POLICYDATA$LAD == "NA", 'la']

# LADs for 4 Transport Authorities in 2013 are missing because they were replaced
# by Combined Authorities later on -> recording them manually
POLICYDATA[POLICYDATA$la == 'Merseyside Integrated Transport Authority', 'LAD'] =
  POLICYDATA[POLICYDATA$la == 'The Halton, Knowsley, Liverpool, St Helens, Sefton and Wirral Combined Authority', 'LAD'][1]
POLICYDATA[POLICYDATA$la == 'South Yorkshire Integrated Transport Authority', 'LAD'] =
  POLICYDATA[POLICYDATA$la == 'The Barnsley, Doncaster, Rotherham and Sheffield Combined Authority', 'LAD'][1]
POLICYDATA[POLICYDATA$la == 'Tyne and Wear Integrated Transport Authority', 'LAD'] =
  POLICYDATA[POLICYDATA$la == 'The Durham, Gateshead, Newcastle, North Tyneside, Northumberland, South Tyneside and Sunderland Combined Authority', 'LAD'][1]
POLICYDATA[POLICYDATA$la == 'West Yorkshire Integrated Transport Authority', 'LAD'] =
  POLICYDATA[POLICYDATA$la == 'The West Yorkshire Combined Authority', 'LAD'][1]


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



#------------------------------------ TO UPPER TIER 



setwd("C:/Users/ru21406/YandexDisk/PhD Research/Data")

# merging with la codes
la_all_codes = read.csv('la_all_codes.csv') %>% 
  select(LADCD, UTLACD, CAUTHCD) 

POLICYDATA = POLICYDATA %>% 
  left_join(la_all_codes, by = c('LAD' = 'LADCD'), keep = T) %>%
  left_join(population, by = c('LAD', 'year'))

POLICYDATA$UTLACD = ifelse(is.na(POLICYDATA$UTLACD), POLICYDATA$LAD, POLICYDATA$UTLACD)
POLICYDATA$LADCD = ifelse(is.na(POLICYDATA$LADCD), POLICYDATA$LAD, POLICYDATA$LADCD)

# getting problematic LAs without population data
changed_las = POLICYDATA[is.na(POLICYDATA$population) & !POLICYDATA$class %in% c('O', 'SD'), 'la']
changed_las = unique(str_remove(changed_las, ' CC |UA '))
changed_las

# fixing problematic LAs manually
# Buckinghamshire (became UA in 2020)
POLICYDATA[POLICYDATA$LAD == 'E10000002', 'UTLACD'] = 'E06000060'
POLICYDATA[POLICYDATA$LAD == 'E10000002', 'LAD'] = 'E06000060'
POLICYDATA = POLICYDATA %>% select(-population) %>% 
  left_join(population, by = c('LAD', 'year'))

# Bournemouth, Christchurch and Poole UA joined in 2019, 2020
for (j in c('E06000028', 'E06000029')){
  for (i in 2013:2018){
    POLICYDATA[POLICYDATA$LAD == j & POLICYDATA$year == i, 'population'] =
    population_18[population_18$ladcode18 == (POLICYDATA[POLICYDATA$LAD == j, 'LAD'])[1] &
                    population_18$year == i, 'population']  
  }
}
POLICYDATA[POLICYDATA$LAD == 'E06000029' , 'UTLACD'] = 'E06000059'
POLICYDATA[POLICYDATA$LAD == 'E06000028' , 'UTLACD'] = 'E06000059'
POLICYDATA[POLICYDATA$LAD == 'E06000029' , 'LAD'] = 'E06000059'
POLICYDATA[POLICYDATA$LAD == 'E06000028' , 'LAD'] = 'E06000059'
POLICYDATA[POLICYDATA$UTLACD == 'E06000058' , 'UTLACD'] = 'E06000059'
POLICYDATA[POLICYDATA$LAD == 'E06000058' , 'LAD'] = 'E06000059'

# Dorset (became UA in 2019)
POLICYDATA[POLICYDATA$LAD == 'E10000009', 'UTLACD'] = 'E06000059'
POLICYDATA[POLICYDATA$LAD == 'E10000009', 'LAD'] = 'E06000059'
POLICYDATA = POLICYDATA %>% select(-population) %>% 
  left_join(population, by = c('LAD', 'year'))

# North Yorkshire (something happened in 2015, 2017, 2018)
POLICYDATA[POLICYDATA$LAD == 'E10000022', 'UTLACD'] = 'E10000023'
POLICYDATA[POLICYDATA$LAD == 'E10000022', 'LAD'] = 'E10000023'
POLICYDATA = POLICYDATA %>% select(-population) %>% 
  left_join(population, by = c('LAD', 'year'))

# Northamptonshire = North + West
population$LAD = substr(population$LAD, 1, 9) # removing an extra symbol in the end of codes
for (i in seq_along(year)){
    POLICYDATA[POLICYDATA$LAD == 'E10000021' & POLICYDATA$year == year[i], 'population'] = 
      sum(population[population$LAD %in% c('E06000062', 'E06000061') & population$year == year[i], 'population'])
}
POLICYDATA[POLICYDATA$LAD == 'E10000021', 'UTLACD'] = 'E10000021'
POLICYDATA[POLICYDATA$UTLACD == 'E06000061', 'UTLACD'] = 'E10000021'
POLICYDATA[POLICYDATA$UTLACD == 'E06000062', 'UTLACD'] = 'E10000021'

# numeric format
ids = c('code', 'la', 'class', 'LAD', 'LADCD', 'UTLACD', 'CAUTHCD')
non_ids = colnames(POLICYDATA)[!colnames(POLICYDATA) %in% ids]
POLICYDATA[, non_ids] =  sapply(POLICYDATA[, non_ids], as.numeric)
POLICYDATA$CAUTHCD  = ifelse(POLICYDATA$CAUTHCD == '', NA, POLICYDATA$CAUTHCD)

## Districts to Counties
# filter only SD and SC
SD_SC = POLICYDATA %>% filter(class %in% c('SD', 'SC'))

my_fun = function(x){
  if(is.numeric(x)){
    sum(x, na.rm = T)
  } else {
    data.table::first(x)
  }
}

SC = SD_SC %>%
  group_by(UTLACD, year) %>%
  dplyr::summarise(across(everything(), my_fun), .groups = 'drop')

POLICYDATA = POLICYDATA %>% filter(!class %in% c('SD', 'SC'))
POLICYDATA = rbind.data.frame(POLICYDATA, SC)
table(POLICYDATA$class) # no SD

# categories creating and filtering
#POLICYDATA = POLICYDATA %>% select(-c(other))
#policies = c(general_policies[!general_policies %in% c('other', 'total')], health_policies)

#RO3_cols = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Public_Health_subcat.xlsx')
detailed_public_health = as.data.frame(character())
for (i in 1:ncol(POLICYDATA)){
  name = RO3_cols[unique(which(colnames(POLICYDATA)[i] == RO3_cols,
                              arr.ind = T)[,'row']), 'name']
  
  detailed_public_health = rbind.data.frame(detailed_public_health, c(i, ifelse(length(name) == 0, colnames(POLICYDATA)[i], name)))
  colnames(detailed_public_health) = c('n_col', 'name')
}

colnames(POLICYDATA) = detailed_public_health[,'name']
ids = c('code', 'la', 'class', 'LAD', 'LADCD', 'UTLACD', 'CAUTHCD', 'year', 'population')
non_ids = colnames(POLICYDATA)[!colnames(POLICYDATA) %in% ids]

cols = setdiff(non_ids, ids)
POLICYDATA[cols] = lapply(cols, function(x)
  rowSums(POLICYDATA[names(POLICYDATA) == x], na.rm = T)*NA^(rowSums(!is.na(POLICYDATA[names(POLICYDATA) == x])) == 0))
POLICYDATA = POLICYDATA[c(ids, cols)]
policies = cols

#E06000005


#------------------------------------ London Boroughs

Greater_London = POLICYDATA[grepl('greater london', POLICYDATA[,'la'], ignore.case = T),]
Greater_London = Greater_London %>% 
  select(all_of(c(policies, 'year'))) %>%
  rename_at(all_of(policies), function(x) paste0(x, '_g'))

London_Boroughs = POLICYDATA[POLICYDATA$class == 'L' ,]
London_Boroughs %<>% group_by(year) %>%
  dplyr::mutate(margins = population/sum(population)) 

London = London_Boroughs %>%
  left_join(Greater_London, by = 'year') %>%
  ungroup()

London = as.data.frame(London)

# obtain final values for London Boroughs
London[, policies] = (London[, policies] + t(London[,"margins"])*(London %>% select(ends_with('_g'))))
London = London %>% select(!c(ends_with('_g'), margins))
London = London %>% dplyr::rename(police_fin = 'police',
                           fire_rescue_fin = 'fire_rescue',
                           central_fin = 'central')
# remove Greater London from the main df
POLICYDATA = POLICYDATA %>% filter(!grepl('greater london', POLICYDATA$la, ignore.case = T))



#------------------------------------ Waste and Park Authorities



#LAD_park = POLICYDATA %>% filter(grepl('park', la, ignore.case = T)) %>% 
#  select(LAD) %>% distinct()
#park = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/NSPL_FEB_2022_UK.csv') %>%
#  select(cty, park)
#park_selected = park %>% filter(park %in% LAD_park$LAD) %>% distinct()
#saveRDS(park_selected, 'park_selected.rds')

# loading keys
waste = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/waste_park.xlsx')
park = readRDS('park_selected.rds')

# creating a column with keys for waste and park
for (i in 1:nrow(waste)){
  row = POLICYDATA$la %in% unlist(strsplit(waste$la[i], ', '))
  POLICYDATA[which(row), 'waste_park'] = waste$LAD[i]
}
for (i in 1:nrow(park)){
  row = POLICYDATA$LAD  == park$cty[i]
  POLICYDATA[which(row), 'waste_park'] = park$park[i]
}

# subsetting waste and park authorities
waste_park_Authorities = POLICYDATA[grepl('waste|park', POLICYDATA$la, ignore.case = T),]
waste_park_Authorities = waste_park_Authorities %>% 
  dplyr::select(all_of(c(policies, 'year', 'LAD'))) %>%
  dplyr::rename_at(all_of(policies), function(x) paste0(x, '_wp'))

# margins
POLICYDATA %<>% group_by(year, waste_park) %>%
  dplyr::mutate(margins_waste_park = population/sum(population)) 

# merging the main df with the subset
POLICYDATA = POLICYDATA %>%
  left_join(waste_park_Authorities, by = c('year', 'waste_park' = 'LAD')) %>%
  ungroup()

# replace NA with 0
names_wp = names(POLICYDATA %>% select(ends_with('_wp'), margins_waste_park))
logical = is.na(POLICYDATA[, names_wp])
for (x in 1:length(names_wp)){
  POLICYDATA[logical[, x], names_wp[x]] = 0
}

# computing the outturns
POLICYDATA[, policies] = (POLICYDATA[, policies] + t(POLICYDATA[,"margins_waste_park"])*(POLICYDATA %>% select(ends_with('_wp'))))
POLICYDATA = POLICYDATA %>% select(!c(ends_with('_wp'), margins_waste_park, waste_park))

# removing waste and park authorities
POLICYDATA = POLICYDATA %>% filter(!grepl('waste|park', POLICYDATA$la, ignore.case = T))



#------------------------------------ Combined Authorities



# special case - The Broads Authority (Norfolk + Suffolk)
broads_code = unique(POLICYDATA[grepl('Broads', POLICYDATA$la, ignore.case = T), 'LAD'])
POLICYDATA$CAUTHCD = ifelse(!POLICYDATA$class == 'O' & grepl('Norfolk|Suffolk', POLICYDATA$la, ignore.case = T), broads_code, 
                        POLICYDATA$CAUTHCD)

# the rest
Combined_Authorities = POLICYDATA[grepl('combined authority|broads|Transport', POLICYDATA$la, ignore.case = T),]
Combined_Authorities = Combined_Authorities %>% 
  dplyr::select(all_of(c(policies, 'year', 'LAD'))) %>%
  dplyr::rename_at(all_of(policies), function(x) paste0(x, '_ca'))

Combined_Authorities_lower = POLICYDATA %>% filter(!CAUTHCD == '')
Combined_Authorities_lower %<>% group_by(year, CAUTHCD) %>%
  dplyr::mutate(margins_comb = population/sum(population)) 
Combined_Authorities_lower$CAUTHCD = as.character(Combined_Authorities_lower$CAUTHCD)

CA = Combined_Authorities_lower %>%
  left_join(Combined_Authorities, by = c('year', 'CAUTHCD' = 'LAD')) %>%
  ungroup()

# remove LAs (within the combined ones) in periods preceding the introduction of the respective CAs
CA = CA[!is.na(CA$education_ca),]

# obtain values for the Combined Authorities
CA[, policies] = (CA[, policies] + t(CA[,"margins_comb"])*(CA %>% select(ends_with('_ca'))))
CA = CA %>% select(!c(ends_with('_ca'), margins_comb))

# remove CAs
POLICYDATA = POLICYDATA %>% filter(!grepl('combined authority|broads|Transport', POLICYDATA$la, ignore.case = T))



#------------------------------------ Police Authorities



# keys for police
la_police_ = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/la_police_fire.xlsx',
                            sheet = 'police')
colnames(la_police_) = c('PLAD', 'police_name', 'LAD', 'la_name')

# merging police with POLICYDATA
la_police = la_police_  %>% 
  dplyr::mutate(across(c(1,2), ~ifelse(is.na(.x), na.locf(.x), .x)))

la_police = la_police %>%
  left_join(la_all_codes[, c('LADCD', 'UTLACD')], by = c('LAD' = 'LADCD'))
la_police$UTLACD = ifelse(is.na(la_police$UTLACD), la_police$LAD, la_police$UTLACD)

# fixing Northamptonshire Police, Fire and Crime Commissioner Fire and Rescue Authority
POLICYDATA[POLICYDATA$la == 'Northamptonshire Police, Fire and Crime Commissioner Fire and Rescue Authority', 'la'] =
  'Northamptonshire Fire, and Crime Commissioner Fire and Rescue Authority'

plc = POLICYDATA %>% filter(grepl('police', la, ignore.case = T)) %>%
  select(LAD, year, police, central) %>% distinct(.)

la_police_POLICYDATA = la_police %>%
  left_join(plc, by = c('PLAD' = 'LAD'), keep = T) %>%
  filter(!is.na(LAD.y)) %>%
  dplyr::rename(LAD_p = 'LAD.x') %>%
  select(-LAD.y) %>%
  select(-LAD_p)

# fixing Dorset (Bournemouth, Christchurch and Poole UA) and Northamptonshire manually

# Dorset
Dorset = la_police_POLICYDATA %>% filter(UTLACD == 'E06000059')
#Dorset$LAD_p = 'E06000058'
Dorset$UTLACD = 'E06000058'
la_police_POLICYDATA = rbind(la_police_POLICYDATA, Dorset)

# Dorset missing in 2016 -> replace with the moving average
la_police_POLICYDATA[la_police_POLICYDATA$year == 2016 & la_police_POLICYDATA$la_name == 'Dorset', c('police', 'central')] = 
  as.list(colMeans(la_police_POLICYDATA[la_police_POLICYDATA$year %in% c(2015, 2017) &
                                   la_police_POLICYDATA$la_name == 'Dorset',
                                 c('police', 'central')]))

# Northamptonshire
la_police_POLICYDATA[la_police_POLICYDATA$PLAD == 'E23000022', 'UTLACD'] = 'E10000021'
la_police_POLICYDATA[la_police_POLICYDATA$la_name %in% c('North Northamptonshire',
                                           'West Northamptonshire'), 'la_name'] = 'Northamptonshire'
la_police_POLICYDATA = la_police_POLICYDATA %>% select(-la_name) %>% distinct(.)

POLICYDATA = POLICYDATA %>% filter(!grepl('police', la, ignore.case = T)) %>%
  dplyr::rename(police_lower = 'police', central_lower = 'central') %>%
  left_join(la_police_POLICYDATA, by = c('UTLACD', 'year'))

# obtain values for the police category
POLICYDATA = POLICYDATA %>% group_by(PLAD, year) %>%
  dplyr::mutate(margins_plc = population/sum(population))
POLICYDATA[, c('police_fin', 'central_fin')] = (t(POLICYDATA[, "margins_plc"])*POLICYDATA[, c('police', 'central')]) + 
  POLICYDATA[, c('police_lower', 'central_lower')]

# checking the merging
as.data.frame(table(POLICYDATA[is.na(POLICYDATA$PLAD), 'class'])) # 233 Fire Authorities remained plus 264 London Boroughs

# ungrouping 
POLICYDATA = POLICYDATA %>%
  ungroup()



#------------------------------------ Fire Authorities



###### keys for fire
la_fire_ = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/la_police_fire.xlsx',
                     sheet = 'fire')
colnames(la_fire_) = c('FLAD', 'fire_name', 'LAD', 'la_name')

# merging fire with POLICYDATA
la_fire = la_fire_  %>% 
  dplyr::mutate(across(c(1,2), ~ ifelse(is.na(.x), na.locf(.x), .x)))

la_fire = la_fire %>%
  left_join(la_all_codes[, c('LADCD', 'UTLACD')], by = c('LAD' = 'LADCD')) %>%
  filter(grepl('E', FLAD, ignore.case = T)) # England only
la_fire$UTLACD = ifelse(is.na(la_fire$UTLACD), la_fire$LAD, la_fire$UTLACD)

fire = POLICYDATA %>% filter(grepl('fire', la, ignore.case = T)) %>%
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
SWD = SWD[,-c(7)]

# merging
la_fire_POLICYDATA = la_fire %>%
  left_join(fire, by = c('FLAD' = 'LAD'), keep = T)  %>% 
  dplyr::rename(LAD = 'LAD.x') %>%
  select(-LAD.y)

la_fire_POLICYDATA = rbind(la_fire_POLICYDATA, SWD) %>% 
  dplyr::rename(LAD_f = 'LAD')

# additionally fixing Dorset (Bournemouth, Christchurch and Poole UA) and Northamptonshire manually

# Dorset
Dorset = la_fire_POLICYDATA %>% filter(LAD_f == 'E06000059') 
Dorset$LAD_f = 'E06000058'
Dorset$UTLACD = 'E06000058'
la_fire_POLICYDATA = rbind(la_fire_POLICYDATA, Dorset)

# Northamptonshire
la_fire_POLICYDATA[la_fire_POLICYDATA$FLAD == 'E31000028', c('LAD_f', 'UTLACD')] = 'E10000021'
la_fire_POLICYDATA[la_fire_POLICYDATA$la_name %in% c('North Northamptonshire',
                                       'West Northamptonshire'), 'la_name'] = 'Northamptonshire'

la_fire_POLICYDATA = la_fire_POLICYDATA  %>% 
  filter(!is.na(fire_rescue)) %>%
  select(-c(la_name, la, LAD_f)) %>% distinct(.)

# adding cleaned fire data to the main dataset
POLICYDATA = POLICYDATA %>% filter(!grepl('fire', la, ignore.case = T)) %>%
  dplyr::rename(fire_rescue_lower = 'fire_rescue') %>%
  left_join(la_fire_POLICYDATA, by = c('UTLACD', 'year'))

# fixing NA for authorities that belong to the combined ones (MD) or London
POLICYDATA[is.na(POLICYDATA$fire_rescue), 'fire_rescue'] = 0
POLICYDATA[is.na(POLICYDATA$police), 'police'] = 0
POLICYDATA[is.na(POLICYDATA$central), 'central'] = 0

POLICYDATA = as.data.frame(POLICYDATA)
la_fire = as.data.frame(la_fire)

# checking missing LA codes in the merged data
table(POLICYDATA[is.na(POLICYDATA$FLAD), 'class'])

# fixing NAs in LAD for fire LAs (happened due to merging)
lvec = which(POLICYDATA$la == 'Northamptonshire')
POLICYDATA[lvec, 'FLAD'] = 'E31000028'

save = POLICYDATA

for (i in 1:nrow(POLICYDATA)){
  if (is.na(POLICYDATA[i, 'FLAD'])){
      POLICYDATA[i,'FLAD'] = la_fire[which(la_fire[,'UTLACD'] == as.character(POLICYDATA[i,'UTLACD'])),'FLAD'][1]
  }
}

# obtain values for the fire policy
POLICYDATA = POLICYDATA %>% group_by(FLAD, year) %>%
  dplyr::mutate(margins_fire = population/sum(population))
POLICYDATA[, "fire_rescue_fin"] = (t(POLICYDATA[, "margins_fire"])*POLICYDATA[, "fire_rescue"]) +
  POLICYDATA[, 'fire_rescue_lower']



#------------------ London Boroughs and Combined Authorities back to the main df



POLICYDATA = as.data.frame(POLICYDATA)
CA = as.data.frame(CA)

# Combined Authorities

for (i in 2013:2020){
  for (j in unique(CA$LAD)){
    if (!length(CA[CA$year == i & CA$LAD == j, 'police']) == 0){
      POLICYDATA[POLICYDATA$year == i & POLICYDATA$LAD == j, 'police_fin'] = POLICYDATA[POLICYDATA$year == i & POLICYDATA$LAD == j, 'police_fin'] +
        CA[CA$year == i & CA$LAD == j, 'police']
    }
    
    if (!length(CA[CA$year == i & CA$LAD == j, 'fire_rescue']) == 0){
      POLICYDATA[POLICYDATA$year == i & POLICYDATA$LAD == j, 'fire_rescue_fin'] = POLICYDATA[POLICYDATA$year == i & POLICYDATA$LAD == j, 'fire_rescue_fin'] +
        CA[CA$year == i & CA$LAD == j, 'fire_rescue']
    }
    
    if (!length(CA[CA$year == i & CA$LAD == j, 'central']) == 0){
      POLICYDATA[POLICYDATA$year == i & POLICYDATA$LAD == j, 'central_fin'] = POLICYDATA[POLICYDATA$year == i & POLICYDATA$LAD == j, 'central_fin'] +
        CA[CA$year == i & CA$LAD == j, 'central']
    }
  }
}

# London
POLICYDATA = POLICYDATA %>% filter(!POLICYDATA$LAD %in% London$LAD)
POLICYDATA = plyr::rbind.fill(POLICYDATA, London)

# inspecting the merged output
POLICYDATA_inspect = POLICYDATA %>% select(c('code', 'la', 'class', 'year', 'LAD',
                         'LADCD', 'UTLACD', all_of(policies),
                         'police_lower', 'central_lower', 'fire_rescue_lower',
                         'police_fin', 'central_fin', 'fire_rescue_fin',
                         'population', 'margins_plc', 'margins_fire'))
POLICYDATA = POLICYDATA_inspect %>% select(-c('police', 'fire_rescue', 'central',
                                'police_lower', 'central_lower',
                                'fire_rescue_lower')) %>%
  dplyr::rename(police = 'police_fin',
         fire_rescue = 'fire_rescue_fin',
         central = 'central_fin')

# checking the presence of all LAs in the final df
table(POLICYDATA$class, POLICYDATA$year)
table(POLICYDATA_long[POLICYDATA_long$class %in% c('L', 'MD', 'SC', 'UA'), 'class'],
      POLICYDATA_long[POLICYDATA_long$class %in% c('L', 'MD', 'SC', 'UA'), 'year'])

# replacing missing values with moving average

# Isles of Scilly 2016
# Reading UA 2020
# Slough UA 2020
# Medway Towns UA 2020

for (i in 1:nrow(POLICYDATA)){
  for (j in policies[-which(policies == 'covid')]){
      if(is.na(POLICYDATA[i, j])){
        
        before = POLICYDATA[POLICYDATA$year == (POLICYDATA[i, 'year'] + 1) & POLICYDATA$LAD == POLICYDATA[i, 'LAD'], j]
        after = POLICYDATA[POLICYDATA$year == (POLICYDATA[i, 'year'] - 1) & POLICYDATA$LAD == POLICYDATA[i, 'LAD'], j]
        
        if (!length(before) == 0 & !length(after) == 0){
          POLICYDATA[i, j] = mean(before, after)
        }
        else{
          non_null = Filter(Negate(function(x) length(x) == 0), c(before, after))
          POLICYDATA[i, j] = non_null
        }
      }
    }
  }

## lineplot for mental

library(ggplot2)
library(ggpubr)
panel_m = panel_data(POLICYDATA, id = UTLACD, wave = year)

panel_m  %>% 
    ggplot(aes(year, mental)) +
    scale_x_continuous(name = NULL, 
                       breaks = 2013:2019)+ 
    scale_y_continuous(name = NULL#, breaks = seq(-3, 1.5, 1)
    ) + 
    theme(axis.text = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24)) +
    geom_line(aes(group = UTLACD), color = "lightblue") +
    geom_smooth(method = loess, se = F, fullrange = T, color="darkred") +
    theme_pubclean()

#-------------------------- deflator



deflator = read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial/GDP_Deflators_Budget_March_2021_update.xlsx',
                       range = 'H7:I73')
names(deflator) = c('year', 'def')
deflator = deflator %>% filter(year %in% .GlobalEnv$year)
deflator$year = as.numeric(deflator$year)

# merging with outturns
POLICYDATA = POLICYDATA %>%
  left_join(deflator, by = 'year')

# computing per 1,000 people in prices of 2020
POLICYDATA[policies] = 
  (POLICYDATA[policies]/(POLICYDATA[,'population']/1000))*(POLICYDATA[, 'def']/100)[row(POLICYDATA[policies])]

# hist
hist(POLICYDATA$education)
hist(POLICYDATA$public_health)
#hist(log(POLICYDATA$smoking))


# saving
saveRDS(POLICYDATA, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/POLICYDATA.RData'))

