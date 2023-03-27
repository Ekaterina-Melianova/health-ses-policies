# policy_data_extraction.R 

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
                       list_number = 3, type = 'RSX')

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
# quick = POLICYDATA_RAW[[2]][[2]]

# extracting  columns with Total Expenditure

main_col = 'Total Expenditure'

# ids
ids = c('E-code',
        'Local authority',
        'Class',
        'ONS Code')

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

# renaming columns
for (i in seq_along(POLICYDATA)){
  
  if (year[i] == 2013){
    colnames(POLICYDATA[[i]][[Glist_number]]) = c(ids.1, general_policies)
    }
    else{
      colnames(POLICYDATA[[i]][[Glist_number]]) = c(ids.2, general_policies)
    }
  
  POLICYDATA[[i]][[Glist_number]]['year'] = as.data.frame(year[i])
}

# ONS code = LSOA01CD for 2013
l13 = which(unlist(lapply(POLICYDATA, function(x) length(grep('2013-14', names(x)) == 0L))) > 0)
for (j in seq_along(POLICYDATA[[l13]])){
  POLICYDATA[[l13]][[j]] = POLICYDATA[[l13]][[j]] %>%
  left_join(POLICYDATA[[l13 + 1]][[j]][, c('code', 'LAD')])
}

# long format
POLICYDATA_long = do.call(plyr::rbind.fill, lapply(POLICYDATA, function(i) purrr::reduce(i, dplyr::full_join)))
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

# replace '...'
POLICYDATA <- POLICYDATA %>% 
  mutate_all(~ifelse(. == "…", NA, .))  %>%
  mutate_at(vars(education:total), as.numeric)

# quick population
POLICYDATA %<>% rename(LAD21CD = LAD)
policydata = POLICYDATA %>%
  left_join(spend_data_net[,c('year', 'LAD21CD', 'pop')])%>%
  select(code, LAD21CD, la, class, year, pop, everything())%>%
  left_join(spend_data_gross[,c('year', 'LAD21CD', 'pop')] 
            %>% rename(pop.y = pop), by = c('year', 'LAD21CD')) %>%
  filter(year %in% 2013:2019)

# return population data for Buckinghamshire since it changed in 2020 but I use data up to 2019

Buck = paste0('E0', 7000004:7000007)
policydata[policydata$LAD21CD %in% Buck, "pop"] =
  policydata[policydata$LAD21CD %in% Buck, "pop.y"]

policydata %<>% select(-pop.y)

# for Buckinghamshire retrieve population with a spline in 2019
buck_imputed = policydata %>% group_by(LAD21CD) %>%
  filter(LAD21CD %in% Buck) %>%
  mutate(pop = imputeTS::na_ma(pop)) 
policydata[policydata$LAD21CD %in% Buck,] = buck_imputed

# remove Dorset and its SD completely since it changed in 2019
policydata %<>% filter(!LAD21CD %in% c('E10000009',
                                       'E06000059',
                                       'E06000058',
                                       paste0('E0', 7000048:7000053),
                                       'E06000029',
                                       'E06000028'))

# obtain total population for SC by summing values for their respective SD

policydata$code_sub = substr(policydata$code, 1, 3) # code for upper tier connected to lower tier
scd = policydata %>% filter(class %in% c('SC', 'SD')) %>%
  group_by(year, code_sub) %>%
  mutate(pop_sum = sum(pop, na.rm = T))
scd$pop = ifelse(scd$class == 'SC', scd$pop_sum, scd$pop)
policydata[policydata$LAD21CD %in% scd$LAD21CD, 'pop'] = scd[,'pop']

# remove the changed parts of Suffolk
policydata %<>% filter(!LAD21CD %in% c('E10000029',
                                       'E07000201',
                                       paste0('E0', 7000204:7000206),
                                       'E07000244',
                                       'E07000245'))

# remove Taunton Deane and West Somerset and their merge Somerset West and Taunton in 2019
policydata %<>% filter(!LAD21CD %in% c('E07000190',
                                       'E07000191',
                                       'E07000246'))

# spline for Isles of Scilly and Dorset Police and Crime Commissioner and Chief Constable
scilly_imputed = policydata %>% group_by(LAD21CD) %>%
  filter(LAD21CD %in% 'E06000053') %>%
  mutate(across(pop:total, ~ imputeTS::na_ma(.))) 
police_dorset_imputed = policydata %>% group_by(LAD21CD) %>%
  filter(LAD21CD %in% 'E23000039') %>%
  mutate(across(education:total, ~ imputeTS::na_ma(.))) 

policydata[policydata$LAD21CD %in% 'E06000053',] = scilly_imputed
policydata[policydata$LAD21CD %in% 'E23000039',] = police_dorset_imputed

# # find percentage for each lower tier la
# policydata$class2 = ifelse(policydata$class == 'SC', 'SD', policydata$class)
# policydata %<>% group_by(year, class2) %>%
#   mutate(pop_pct_scd = ifelse(class == 'SD', pop/first(pop), 1)) %>%
#   ungroup()

# out
policydata %<>% select(code:total)

# saving
write.csv(policydata, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/policydata.csv'))

###
###
###
###
###


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





