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
POLICYDATA %<>% rename(LAD19CD = LAD)
policydata = POLICYDATA %>%
  left_join(spend_data_net[,c('year', 'LAD19CD', 'pop')])%>%
  select(code, LAD19CD, la, class, year, pop, everything())%>%
  left_join(spend_data_gross[,c('year', 'LAD19CD', 'pop')] 
            %>% rename(pop.y = pop), by = c('year', 'LAD19CD')) %>%
  filter(year %in% 2013:2019)

# return population data for Buckinghamshire since it changed in 2020 but I use data up to 2019

Buck = paste0('E0', 7000004:7000007)
policydata[policydata$LAD19CD %in% Buck, "pop"] =
  policydata[policydata$LAD19CD %in% Buck, "pop.y"]

policydata %<>% select(-pop.y)

# for Buckinghamshire retrieve population with a spline in 2019
buck_imputed = policydata %>% group_by(LAD19CD) %>%
  filter(LAD19CD %in% Buck) %>%
  mutate(pop = imputeTS::na_ma(pop)) 
policydata[policydata$LAD19CD %in% Buck,] = buck_imputed

# remove Dorset and its SD completely since it changed in 2019
policydata %<>% filter(!LAD19CD %in% c('E10000009',
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
policydata[policydata$LAD19CD %in% scd$LAD19CD, 'pop'] = scd[,'pop']

# remove the changed parts of Suffolk
policydata %<>% filter(!LAD19CD %in% c('E10000029',
                                       'E07000201',
                                       paste0('E0', 7000204:7000206),
                                       'E07000244',
                                       'E07000245'))

# remove Taunton Deane and West Somerset and their merge Somerset West and Taunton in 2019
policydata %<>% filter(!LAD19CD %in% c('E07000190',
                                       'E07000191',
                                       'E07000246'))

# spline for Isles of Scilly and Dorset Police and Crime Commissioner and Chief Constable
scilly_imputed = policydata %>% group_by(LAD19CD) %>%
  filter(LAD19CD %in% 'E06000053') %>%
  mutate(across(pop:total, ~ imputeTS::na_ma(.))) 
police_dorset_imputed = policydata %>% group_by(LAD19CD) %>%
  filter(LAD19CD %in% 'E23000039') %>%
  mutate(across(education:total, ~ imputeTS::na_ma(.))) 

policydata[policydata$LAD19CD %in% 'E06000053',] = scilly_imputed
policydata[policydata$LAD19CD %in% 'E23000039',] = police_dorset_imputed

# out
policydata %<>% select(code:total)

# LADs for 4 Transport Authorities in 2013 are missing because they were replaced
# by Combined Authorities later on -> recording them manually
policydata[policydata$la == 'Merseyside Integrated Transport Authority', 'LAD19CD'] =
  policydata[policydata$la == 'The Halton, Knowsley, Liverpool, St Helens, Sefton and Wirral Combined Authority', 'LAD19CD'][1]
policydata[policydata$la == 'South Yorkshire Integrated Transport Authority', 'LAD19CD'] =
  policydata[policydata$la == 'The Barnsley, Doncaster, Rotherham and Sheffield Combined Authority', 'LAD19CD'][1]
policydata[policydata$la == 'Tyne and Wear Integrated Transport Authority', 'LAD19CD'] =
  policydata[policydata$la == 'The Durham, Gateshead, Newcastle, North Tyneside, Northumberland, South Tyneside and Sunderland Combined Authority', 'LAD19CD'][1]
policydata[policydata$la == 'West Yorkshire Integrated Transport Authority', 'LAD19CD'] =
  policydata[policydata$la == 'The West Yorkshire Combined Authority', 'LAD19CD'][1]


# set wd
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data/')

## combined authorities
comb = read.csv('combined_lookup.csv')
policydata %<>% left_join(comb[,c('LAD19CD', 'CAUTH19CD')], by = 'LAD19CD')
comb_uniq = unique(policydata$LAD19CD[policydata$class == 'O' &
                                        grepl('Combined', policydata$la) &
                                        !grepl('Fire', policydata$la)])
comb_uniq[!comb_uniq %in% unique(comb$CAUTH19CD)] 
# "E47000005" missing in 2018

# retrieve manually:
policydata[policydata$LAD19CD %in%
c('E06000047',
  'E06000057',
  'E08000021',
  'E08000022',
  'E08000023',
  'E08000024',
  'E08000037'), 'CAUTH19CD'] = "E47000005"

# "E47000010" "E47000011" appear only in 2019, making replacement
comb_to_replace1 = comb[comb$CAUTH19CD == 'E47000010', 'LAD19CD']
policydata[policydata$LAD19CD %in% comb_to_replace1 &
             policydata$year %in% 2019, 'CAUTH19CD'] = "E47000010"

comb_to_replace2 = comb[comb$CAUTH19CD == 'E47000011', 'LAD19CD']
policydata[policydata$LAD19CD %in% comb_to_replace2 &
             policydata$year %in% 2019, 'CAUTH19CD'] = "E47000011"

## parks
parks = read.csv('parks_la_match.csv')
policydata %<>% left_join(parks[,c('LAD19CD', 'NPARK22CD')], by = 'LAD19CD')
parks_uniq = unique(policydata$LAD19CD[policydata$class == 'O' &
                                        grepl('Park', policydata$la)])
parks_uniq[!parks_uniq %in% unique(parks$NPARK22CD)] # "E26000003" "E26000008" missing in 2013:2014
# changing to their recent names
# 'E26000003' -> 'E26000011'
# "E26000008" -> 'E26000012'

lad_parks_to_replace1 = parks[parks$NPARK22CD == 'E26000011', 'LAD19CD']
policydata[policydata$LAD19CD %in% lad_parks_to_replace1 &
             policydata$year %in% 2013:2014, 'NPARK22CD'] = "E26000003"

lad_parks_to_replace2 = parks[parks$NPARK22CD == 'E26000012', 'LAD19CD']
policydata[policydata$LAD19CD %in% lad_parks_to_replace2 &
             policydata$year %in% 2013:2014, 'NPARK22CD'] = "E26000008"


## fire
fire = read.csv('fire_lookup.csv')
policydata %<>% left_join(fire[,c('LAD19CD', 'FRA19CD')], by = 'LAD19CD')
fire_uniq = unique(policydata$LAD19CD[policydata$class == 'O' &
                                        grepl('Fire', policydata$la)])
fire_uniq[!fire_uniq %in% unique(fire$FRA19CD)] 

# "E31000012" "E31000038" missing in 2013-2015


## police
police = read.csv('police_lookup.csv')
policydata %<>% left_join(police[,c('LAD20CD', 'PFA20CD')] %>%
                            rename(LAD19CD = LAD20CD), by = 'LAD19CD')
police_uniq = unique(policydata$LAD19CD[policydata$class == 'O' &
                                        grepl('Police', policydata$la)])
police_uniq[!police_uniq %in% unique(police$PFA20CD)] # "E31000028" missing


## fixing missing fire and police

# Fire Dorset "E31000012" should be removed since the LAD itself was removed earlier
policydata %<>% filter(!LAD19CD == 'E31000012')

# Wiltshire 'E31000038'can be taken from their police counterpart 'E23000038'
police_to_fire = police[police$PFA20CD == 'E23000038', 'LAD20CD']
policydata[policydata$LAD19CD %in% police_to_fire  &
             policydata$year %in% 2013:2015, 'FRA19CD'] = "E31000038"

# Police Northamptonshire "E31000028" we can take from the respective fire LA
policydata[policydata$FRA19CD == 'E31000028' &
             policydata$year %in% 2019 & 
             !is.na(policydata$FRA19CD), 'PFA20CD'] = 'E31000028'

# Police Dorset remove as well
policydata %<>% filter(!LAD19CD == 'E23000039')

# waste

waste <- data.frame(
  LAD = c("E50000001", "E50000006", "E50000002", "E50000004", "E50000003", 'E50000005'),
  la_waste = c("East London Waste Authority", "Merseyside Waste Disposal Authority",
               "North London Waste Authority", "Western Riverside Waste Authority",
               "West London Waste Authority", 'Greater Manchester Waste Disposal Authority'),
  la = c("Barking & Dagenham, Bexley, Greenwich, Hackney, Havering, Lewisham, Newham, Redbridge, Tower Hamlets, Waltham Forest", 
         "Liverpool, Knowsley, Sefton, Wirral, St Helens",
         "Barnet, Enfield, Haringey", 
         "Hammersmith & Fulham, Kensington & Chelsea, Lambeth, Wandsworth",
         "Brent, Ealing, Hammersmith and Fulham, Harrow, Richmond upon Thames, Hillingdon, Hounslow",
         'Bolton, Bury, Manchester, Oldham, Rochdale, Salford, Stockport, Tameside, Trafford, Wigan')
)

#policydata_ = policydata

for (i in 1:nrow(waste)){
  if (i == 6) {# special case for Manchester since its waste authority disappeared in 2018
    row = policydata$la %in% unlist(strsplit(waste$la[i], ', '))
    policydata[row & policydata$year %in% 2013:2017, 'waste'] = waste$LAD[i]
  }else{
    row = policydata$la %in% unlist(strsplit(waste$la[i], ', '))
    policydata[which(row), 'waste'] = waste$LAD[i]
  }
}

# check if all 'O' LADs are matched (should only Greater London)
selected_values = policydata[policydata$class == 'O', 'LAD19CD']
id_vars = c('CAUTH19CD', 'NPARK22CD', 'FRA19CD', 'PFA20CD', 'waste')
setdiff(selected_values, unlist(policydata[, id_vars]))

# matching Greater London
policydata[policydata$class == 'L', 'CAUTH19CD'] = 'E12000007'

# finding a mismatch between the original special LAs and their merged (LA, year) pairs
vec = c()
for (col in id_vars){
  match1 = policydata[policydata$class == 'O',
                    c('LAD19CD', 'year')]
  match2 = unique(na.omit(cbind.data.frame(LAD19CD  = as.vector(unlist(policydata[, col])),
                 year = as.vector(unlist(policydata[, 'year'])))))
  vec = c(vec, setdiff(paste(match2$LAD19CD, match2$year), paste(match1$LAD19CD, match1$year)))
  }
vec = substr(vec, 1,9)
table(vec)

# those are problematic
fix_year = c('E31000028', 'E31000040', 'E47000006', 'E47000008', 'E47000009')

# the rest should be replaced with NA since they have their upper LAs
fix_with_na = unique(vec[!vec %in% fix_year])

# SC to SD



# find percentage for each lower tier la
policydata$class2 = ifelse(policydata$class == 'SC', 'SD', policydata$class)
policydata %<>% group_by(year, class2) %>%
  mutate(pop_pct_scd = ifelse(class == 'SD', pop/first(pop), 1)) %>%
  ungroup()



