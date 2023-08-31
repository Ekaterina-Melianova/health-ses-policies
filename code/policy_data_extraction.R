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
POLICYDATA_LIST = loadLists(site = sites[7:length(sites)],
                            year_seq = 2013:2020,
                            list_number = 3,
                            type = 'RSX')

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

main_col = c('Total Expenditure|Net Current Expenditure')

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
general_policies_ = c('education',
                     'transport',
                     'social_care_children',
                     'social_care_adult',
                     'public_health',
                     'housing',
                     'cultural',
                     'environment',
                     'planning',
                     'police',
                     'fire',
                     'central',
                     'other',
                     'total')
general_policies_2 = rep(general_policies_, each = 2)
suffix = rep(c("_total", "_net"), length(general_policies_))
general_policies = paste0(general_policies_2, suffix)

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
  mutate_all(~ifelse(. %in% c("…", '...'), NA, .))  %>%
  mutate_at(vars(general_policies), as.numeric)
POLICYDATA %<>% rename(LAD19CD = LAD)

# population data
populationData = function(file_name, skip, year){
  dat = read_excel(paste0('C:/Users/ru21406/YandexDisk/PhD Research/Data/',
                          file_name, '.xls'),
                   sheet = 'MYE 5',
                   skip = skip)
  dat = dat %>% select(Code, contains('Population') & contains(as.character(year)))
  names(dat) = c('LAD19CD', rev(year)) # column names are reversed
  # long format
  dat = dat %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "year",
               values_to = "pop")
  dat$year = as.numeric(dat$year)
  return(dat)
}

pop18 = populationData('pop18', 4, 2013:2018)
pop19 = populationData('pop19', 4, 2019)
Suffolk = c('E07000201',
            paste0('E0', 7000204:7000206))
Dorset = c('E10000009',
           'E06000059',
           'E06000058',
           paste0('E0', 7000048:7000053),
           'E06000029',
           'E06000028')
Somerset = c('E07000190',
             'E07000191')
changed_in_2019 = c(Somerset, Suffolk, Dorset)
pop17 = populationData('pop17', 4, 2013:2017) %>%
  filter(LAD19CD %in% changed_in_2019)
pop = na.omit(rbind(pop18, pop19, pop17))

# mistake in North Yorkshire
POLICYDATA[POLICYDATA$LAD19CD %in% 'E10000022', 'LAD19CD'] = 'E10000023'

# merging the main df with population df
policydata = POLICYDATA %>%
  left_join(pop, by = c('LAD19CD', 'year')) %>%
  select(code, LAD19CD, la, class, year, pop, everything())%>%
  filter(year %in% 2013:2019) %>%
  group_by(LAD19CD) %>%
  mutate(across(pop, ~ ifelse(LAD19CD %in% changed_in_2019,
                              imputeTS::na_ma(.), pop))) %>%
  ungroup()

# spline for Isles of Scilly and Dorset Police and Crime Commissioner and Chief Constable
scilly_imputed = policydata %>% group_by(LAD19CD) %>%
  filter(LAD19CD %in% 'E06000053') %>%
  mutate(across(general_policies, ~ imputeTS::na_ma(.))) 
police_dorset_imputed = policydata %>% group_by(LAD19CD) %>%
  filter(LAD19CD %in% 'E23000039') %>%
  mutate(across(general_policies, ~ imputeTS::na_ma(.))) 

policydata[policydata$LAD19CD %in% 'E06000053',] = scilly_imputed
policydata[policydata$LAD19CD %in% 'E23000039',] = police_dorset_imputed

# out
policydata %<>% select(code:total_net)
policydata = as.data.frame(policydata)

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

# setwd
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data/')

## combined authorities
comb = read.csv('combined_lookup.csv')
comb = comb %>% select(LAD19CD_ = LAD19CD, CAUTH19CD) %>% 
  full_join(policydata %>% select(LAD19CD, la, year) %>%
              filter((grepl('Combined', policydata$la) |
                       grepl('Transport', policydata$la))&
                       !grepl('Fire', policydata$la)),
            by = c('CAUTH19CD' = 'LAD19CD'))
table(comb[is.na(comb$LAD19CD_), 'CAUTH19CD'])# "E47000005" missing in 2013:2018
policydata %<>% left_join(comb %>% select(LAD19CD = LAD19CD_, CAUTH19CD, year),
                          by = c('LAD19CD', 'year'))
# retrieve manually:
policydata[policydata$LAD19CD %in%
             c('E06000047',
               'E06000057',
               'E08000021',
               'E08000022',
               'E08000023',
               'E08000024',
               'E08000037') &
             policydata$year %in% 2013:2018, 'CAUTH19CD'] = "E47000005"

# parks
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

# upper tier to lower tier
# E10000012 E10000015
scd_codes = read.csv('upper_to_lower_lookup.csv') %>%
  select(LAD19CD = LTLA19CD, UTLACD = UTLA19CD)
lad_parks_to_replace3 = scd_codes[scd_codes$UTLACD %in% c('E10000012', 'E10000015'), 'LAD19CD']
policydata[policydata$LAD19CD %in% lad_parks_to_replace3 &
             policydata$year %in% 2013:2019, 'NPARK22CD'] = "E6803"

## fire
fire = read.csv('fire_lookup.csv')
fire = fire %>% select(LAD19CD_ = LAD19CD, FRA19CD) %>% 
  full_join(policydata %>% select(LAD19CD, la, year) %>%
              filter(grepl('Fire', policydata$la)),
            by = c('FRA19CD' = 'LAD19CD'))
table(fire[is.na(fire$LAD19CD_), 'FRA19CD']) # "E31000012" "E31000038" 'missing in 2013-2015
policydata %<>% left_join(fire %>% select(LAD19CD = LAD19CD_, FRA19CD, year),
                          by = c('LAD19CD', 'year'))

## police
police = read.csv('police_lookup.csv')
police = police %>% select(LAD19CD = LAD20CD, PFA20CD) %>% 
  full_join(policydata %>% select(LAD19CD, la, year) %>%
              filter(grepl('Police', policydata$la)),
            by = c('PFA20CD' = 'LAD19CD'))
table(police[is.na(police$LAD19CD), ]) # "E31000028" missing in 2019 but it's actually fire, not police
policydata %<>% left_join(police %>% select(LAD19CD, PFA20CD, year),
                          by = c('LAD19CD', 'year'))

## fixing missing fire

# Wiltshire 'E31000038'can be taken from their police counterpart 'E23000038'
police_to_fire = police[police$PFA20CD == 'E23000038', 'LAD19CD']
policydata[policydata$LAD19CD %in% police_to_fire  &
             policydata$year %in% 2013:2015, 'FRA19CD'] = "E31000038"

# waste
waste = read.csv('waste_lookup.csv')
waste = waste %>% select(code, LAD19CD = waste) %>% 
  full_join(policydata %>% ungroup() %>% select(LAD19CD, la, year) %>%
              filter(grepl('Waste', policydata$la)),
            by = 'LAD19CD')

policydata %<>% left_join(waste %>% select(waste = LAD19CD, code, year),
                          by = c('code', 'year'))

# SC to SD
policydata %<>% left_join(scd_codes, by = 'LAD19CD')

# for changed regions (Suffolk and Taunton Deane and West Somerset) return the same ids
# changed_in_2019
id_vars = c('CAUTH19CD', 'NPARK22CD',
            'FRA19CD', 'PFA20CD', 
            'waste', 'UTLACD')
policydata[policydata$LAD19CD %in% Suffolk, id_vars] = 
  policydata[policydata$LAD19CD == 'E07000245', id_vars]
policydata[policydata$LAD19CD %in% Somerset, id_vars] = 
  policydata[policydata$LAD19CD == 'E07000246', id_vars]
# Dorset
policydata[policydata$LAD19CD %in% c('E06000028', 'E06000029', 'E07000048'), id_vars] = 
  policydata[policydata$LAD19CD == 'E06000058', id_vars]
policydata[policydata$LAD19CD %in% paste0('E0', 7000048:7000053), id_vars] = 
  policydata[policydata$LAD19CD == 'E06000059', id_vars]

# # Fire Dorset "E31000012" 
policydata[policydata$LAD19CD %in%  c('E07000048',
                                      'E07000049',
                                      'E07000050',
                                      'E07000051',
                                      'E07000052',
                                      'E07000053',
                                      "E06000029",
                                      "E06000028")
           &
             policydata$year %in% 2013:2015, 'FRA19CD'] = "E31000012"

# matching Greater London
policydata[policydata$class == 'L', 'CAUTH19CD'] = 'E12000007'

# check if all 'O' LADs are matched 
selected_values = policydata[policydata$class == 'O', 'LAD19CD']
setdiff(selected_values, unlist(policydata[, id_vars]))

# finding a mismatch between the original special LAs and their merged (LA, year) pairs
# vec = c()
# for (col in id_vars){
#   match1 = policydata[policydata$class == 'O',
#                     c('LAD19CD', 'year')]
#   match2 = unique(na.omit(cbind.data.frame(LAD19CD  = as.vector(unlist(policydata[, col])),
#                  year = as.vector(unlist(policydata[, 'year'])))))
#   vec = c(vec, setdiff(paste(match1$LAD19CD, match1$year), paste(match2$LAD19CD, match2$year)))
#   }
# vec = substr(vec, 1,9)
# table(vec) # should be < table of extent 0 >


# in 2020 police name for Buckinghamshire changed due to restructuring of the LAD
# (although I use data up until 2019, police codes are available for 2020)
policydata[policydata$UTLACD %in% 'E10000002', 'PFA20CD'] = "E23000029"

# find percentage for each lower tier la
table(policydata[is.na(policydata$UTLACD), 'class']) # should be 'O  SC'

# general_policies_2 <- rep(general_policies, each = 2)
# suffix <- rep(c("_1", "_2"), length(general_policies))
# paste0(general_policies_2, suffix)

vars_sum = names(policydata)[grep('transport|cultural|environment|planning|central|other', names(policydata))]
vars_mean = names(policydata)[grep('education|social_care_children|social_care_adult|housing|public_health|police|fire', names(policydata))]
general_policies = general_policies[1:(length(general_policies)-2)] # remove total

policydata = policydata %>%
  group_by(LAD19CD, year) %>%
  summarise(across(all_of(vars_sum), sum),
            across(c(code, la, class, pop, all_of(id_vars)),
                   function(x) data.table::first(x)),
            across(all_of(vars_mean), mean)) %>%
  select(code, LAD19CD, la, class, year, pop, 
         all_of(general_policies), all_of(id_vars))

policydata %<>%
  group_by(CAUTH19CD, year) %>%
  mutate(pct_cmb = pop/sum(pop)) %>% 
  group_by(NPARK22CD, year) %>%
  mutate(pct_prk = pop/sum(pop))%>% 
  group_by(FRA19CD, year) %>%
  mutate(pct_fr = pop/sum(pop))%>% 
  group_by(PFA20CD, year) %>%
  mutate(pct_plc = pop/sum(pop))%>% 
  group_by(waste, year) %>%
  mutate(pct_wst = pop/sum(pop))%>% 
  group_by(UTLACD, year) %>%
  mutate(pct_ut = pop/sum(pop))

# separating between datasets

ut_other = policydata %>% ungroup() %>%
  filter(class %in% c('O', 'SC')) %>%
  select(code:year, general_policies)

spends = policydata %>% ungroup() %>%
  filter(!class %in% c('O', 'SC'))

join_cols = c('CAUTH19CD', 'NPARK22CD', 'FRA19CD', 'PFA20CD', 'waste', 'UTLACD')
suffixes = c('_cmb', '_prk', '_fr', '_plc', '_wst', '_ut')

# Perform the joins in a loop
for (i in 1:length(join_cols)) {
  col_name = join_cols[i]
  suffix = c('', suffixes[i])
  colnames(spends)[colnames(spends) == col_name] = 'id'
  spends = spends %>%
    ungroup() %>%
    left_join(ut_other %>% select(id = LAD19CD, year,
                                  general_policies),
              by = c('id', 'year'),
              suffix = suffix)
  colnames(spends)[colnames(spends) == 'id'] = col_name
}

# multiplying by the respective population proportions

for (suffix in suffixes) {
  cols = names(spends)[grep(suffix, names(spends))]
  spends = spends %>%
    mutate(across(cols[2:length(cols)], ~ . * !!rlang::sym(cols[1])))
}

# summing with the lad-specific values and obtaining per capita
policy_cols = colnames(spends %>% select(general_policies))

for (i in policy_cols){
  spends = spends %>%
  mutate(!!rlang::sym(i) := (rowSums(select(., starts_with(i)), na.rm = TRUE)/pop))
}

policy_df = spends %>% select(code:UTLACD)

policy_df %<>% group_by(LAD19CD, year)  %>%
  mutate(social_care_total = social_care_children_total + social_care_adult_total,
         social_care_net = social_care_children_net + social_care_adult_net) %>%
  ungroup()

# remove the changed parts of Suffolk
policy_df %<>% filter(!LAD19CD %in% c(
  'E07000201',
  paste0('E0', 7000204:7000206),
  'E07000244',
  'E07000245'))

# remove Taunton Deane and West Somerset and their merge Somerset West and Taunton in 2019
policy_df %<>% filter(!LAD19CD %in% c('E07000190',
                                      'E07000191',
                                       'E07000246'))

# remove Dorset 
policy_df %<>% filter(!LAD19CD %in% c('E06000059',
                                       'E06000058',
                                       paste0('E0', 7000048:7000053),
                                       'E06000029',
                                       'E06000028'
                                       ))
# West Somerset, Taunton Deane, Somerset West and Taunton; Dorset, Bournemouth, Christchurch and Poole; Suffolk Coastal, Waveney District, East Suffolk
# Dorset, Bournemouth, and Poole; Taunton Deane and West Somerset; Forest Heath, St Edmundsbury, Suffolk Coastal, and Waveney.
# ----------------------------------------------------------------------

# testing the difference with 2018 data by Alexiou and Barr

upload_la_finance = function(wd = 'C:/Users/ru21406/YandexDisk/PhD Research/Data/Spending',
                             starts_from = 15,
                             ends_with = length(list),
                             type = c('gross', 'net'),
                             time = 2013:2019){
  
  setwd(wd)
  
  # loading files
  files = list.files(wd)
  nm = substr(files, 1, nchar(files)-4)
  list = list()
  for(i in 1:length(files)){
    assign(nm[i], read.csv(files[i]))
    list[[i]] = read.csv(files[i])
    
  }
  names(list) = nm
  
  # gross or net
  list = list[starts_from:ends_with]
  
  # list to df
  merge_by = colnames(list[[1]])[c(1,2)]
  spend_data = list %>% 
    purrr::reduce(dplyr::full_join, by = merge_by) 
  
  spend_data = spend_data[, c(1:3, 5, grep('PerCap', names(spend_data)))]
  names(spend_data)
  
  # naming columns
  spend_names = c()
  for (i in list){
    spend_names = c(spend_names, gsub('_Services|_Net|_Gross|Expen_PerCap|Expen_PerCap|\\.', '',
                                      colnames(i)[6]))
  }
  colnames(spend_data) = c('year', 'LAD21CD', 'name', 'pop', tolower(spend_names))
  
  if (type == 'gross'){
    spend_data %<>% dplyr::rename(social_care_children = children_social_care,
                                  social_care_adult = adult_social_care,
                                  transport = highways_and_transport_services,
                                  environment = environmental)
  }
  
  spend_data$year = as.numeric(spend_data$year)
  spend_data %<>%
    mutate(across(education:other, as.numeric))
  
  # filtering year
  
  spend_data %<>% filter(year %in% time)
  
  return(spend_data)
}

spending_data_gross_my = policy_df %>% 
  select(!ends_with('_net') & !ends_with('_inc'))
names(spending_data_gross_my) = gsub("_total", "", names(spending_data_gross_my))
spend_data_gross  = upload_la_finance(type = 'gross')
# spend_data_net  = upload_la_finance(type = 'net', 
#                                      starts_from = 1,
#                                      ends_with = 14)


test = spend_data_gross %>%
  left_join(spending_data_gross_my, by = c('year', 'LAD21CD'='LAD19CD')) %>%
  filter(!is.na(education.y)) # remove 2019

policy_cols_new = colnames(spending_data_gross_my %>% select(education:other, social_care))
colnames_1 <- paste0(policy_cols_new, '.x')
colnames_2 <- paste0(policy_cols_new, '.y')

# subtract each pair of columns ending in "1" and "2"
for (i in seq_along(colnames_1)) {
  colname_1 <- colnames_1[i]
  colname_2 <- colnames_2[i]
  test[[paste0('res_', i)]] <- round(test[[colname_1]] - test[[colname_2]], 3)
}

test %<>% filter(is.na(NPARK22CD) & !LAD21CD == 'E06000053') 
# summary(test %>% select(starts_with('res_'))) # minor differences mainly due to park LAs

# ----------------------------------------------------------------------

all_policies = c(general_policies,
                 'social_care_total',
                 'social_care_net')
spending_data = policy_df %>% select(year,
                                     class,
                                     LAD21CD = LAD19CD,
                                     name = la,
                                     pop,
                                     all_of(all_policies)) %>%
  mutate(across(all_of(all_policies), ~ . * 1000))

# compute incomes
general_policies_ = c('social_care', general_policies_[1:(length(general_policies_)-1)]) # remove total

for (nm in general_policies_) {
  cols = grep(paste0("^", nm), names(spending_data), value=TRUE)
  spending_data[paste0(nm, "_inc")] = spending_data[,cols[1]] - spending_data[,cols[2]]
}

# n = df_fin %>% group_by(LAD19CD) %>% summarise(n = n())
write.csv(spending_data, 'C:/Users/ru21406/YandexDisk/PhD Research/Data/spending_data.csv')


