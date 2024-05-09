


library(plyr, exclude = 'mutate'); library(dplyr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(readODS)
library(zoo)

setwd("C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial/Children")

# list all xlsx files in the current directory
xlsx_files <- list.files(pattern = "\\.xlsx$")

# load to a list, sheet number 3
data <- lapply(xlsx_files, function(x) as.data.frame(read_excel(x, sheet = 3)))
POLICYDATA_RAW = data
POLICYDATA_LIST = data

for (i in seq_along(POLICYDATA_LIST)){

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
      range_start = which(str_detect(POLICYDATA_LIST[[i]][1:max_rows_to_search, col_skip],search_string))
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
      range_end = which(str_detect(POLICYDATA_LIST[[i]][1:max_rows_to_search, col_skip],search_string))
    }
    
    # cutting based on the new starting and ending points
    POLICYDATA_RAW[[i]] = POLICYDATA_LIST[[i]][range_start:range_end,]
    colnames(POLICYDATA_RAW[[i]]) = POLICYDATA_RAW[[i]][1,]
    POLICYDATA_RAW[[i]] = POLICYDATA_RAW[[i]][-1,]
  
}

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
                          POLICYDATA_RAW[[i]][c(which(colnames(POLICYDATA_RAW[[i]]) %in% ids),
                                                     grep(main_col,
                                                          colnames(POLICYDATA_RAW[[i]])))]
                        }
)

# removing redundant rows and columns with NAs
POLICYDATA = lapply(seq_along(POLICYDATA_tmp), function(i){
  POLICYDATA_tmp[[i]][!rowSums(is.na(POLICYDATA_tmp[[i]])) == ncol(POLICYDATA_tmp[[i]]), ]
  POLICYDATA_tmp[[i]][-which(POLICYDATA_tmp[[i]]$`Local authority` == 'ENGLAND'),] #removing total for England
}
)

for (i in seq_along(POLICYDATA)){
    print(names(POLICYDATA[[i]][,1:4]))
  
}

# renaming columns
policy_names_ch = c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s')
general_policies_ = policy_names_ch
general_policies_2 = rep(general_policies_, each = 2)
suffix = rep(c("_total", "_net"), length(general_policies_))
general_policies = paste0(general_policies_2, suffix)

# 2014-2020
ids.2 = c('code',
          'LAD',
          'la',
          'class')

# year vector
year = seq(2014, 2020)

# renaming columns
for (i in seq_along(POLICYDATA)){
  POLICYDATA[[i]] = POLICYDATA[[i]][1:20]
  colnames(POLICYDATA[[i]]) = c(ids.2, general_policies)
  POLICYDATA[[i]]['year'] = as.data.frame(year[i])
}


# long format
POLICYDATA_long = purrr::reduce(POLICYDATA, dplyr::full_join)
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
POLICYDATA %<>% dplyr::rename(LAD19CD = LAD)

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
  dplyr::select(code, LAD19CD, la, class, year, pop, everything())%>%
  filter(year %in% 2013:2019) %>%
  group_by(LAD19CD) %>%
  dplyr::mutate(across(pop, ~ ifelse(LAD19CD %in% changed_in_2019,
                              imputeTS::na_ma(.), pop))) %>%
  ungroup()

# spline for Isles of Scilly and Dorset Police and Crime Commissioner and Chief Constable
scilly_imputed = policydata %>% group_by(LAD19CD) %>%
  filter(LAD19CD %in% 'E06000053') %>%
  dplyr::mutate(across(general_policies, ~ imputeTS::na_ma(.))) 
policydata[policydata$LAD19CD %in% 'E06000053',] = scilly_imputed

# out
policydata %<>% select(code:c8s_net)
policydata = as.data.frame(policydata)

#remove NA
policydata = policydata %>% filter(!is.na(code) & !class=='O')


# setwd
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data/')
# upper tier to lower tier
# E10000012 E10000015
scd_codes = read.csv('upper_to_lower_lookup.csv') %>%
  select(LAD19CD = LTLA19CD, UTLACD = UTLA19CD)

# SC to SD
policydata %<>% left_join(scd_codes, by = 'LAD19CD')

# for changed regions (Suffolk and Taunton Deane and West Somerset) return the same ids
# changed_in_2019
id_vars = c('UTLACD')
policydata[policydata$LAD19CD %in% Suffolk, id_vars] = 
  policydata[policydata$LAD19CD == 'E07000245', id_vars]
policydata[policydata$LAD19CD %in% Somerset, id_vars] = 
  policydata[policydata$LAD19CD == 'E07000246', id_vars]
# Dorset
policydata[policydata$LAD19CD %in% c('E06000028', 'E06000029', 'E07000048'), id_vars] = 
  policydata[policydata$LAD19CD == 'E06000058', id_vars]
policydata[policydata$LAD19CD %in% paste0('E0', 7000048:7000053), id_vars] = 
  policydata[policydata$LAD19CD == 'E06000059', id_vars]

# find percentage for each lower tier la
table(policydata[is.na(policydata$UTLACD), 'class']) # should be 'SC'

policydata = policydata %>%
  group_by(LAD19CD, year) %>%
  dplyr::summarise(across(c(code, la, class, pop,
                            all_of(id_vars)),
                   function(x) data.table::first(x)),
            across(all_of(general_policies), mean)) %>%
  dplyr::select(code, LAD19CD, la, class, year, pop, 
         all_of(general_policies), all_of(id_vars)) %>% ungroup()

policydata %<>%
  group_by(UTLACD, year) %>%
  dplyr::mutate(pct_ut = pop/sum(pop))

# separating between datasets

join_cols = c('UTLACD')
suffixes = c('_ut')

ut_other = policydata %>% ungroup() %>%
  filter(class %in% c('SC')) %>%
  dplyr::select(code:year, general_policies)

spends = policydata %>% 
  ungroup() %>%
  filter(!class %in% c('SC')) 

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
    dplyr::mutate(across(cols[2:length(cols)], ~ . * !!rlang::sym(cols[1])))
}

# summing with the lad-specific values and obtaining per capita
for (i in general_policies){
  spends = spends %>%
    dplyr::mutate(!!rlang::sym(i) := (rowSums(dplyr::select(., starts_with(i)), na.rm = TRUE)/pop))
}

policy_df = spends %>% select(code:UTLACD)

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



spending_data_ch = policy_df %>% dplyr::select(year,
                                     class,
                                     LAD21CD = LAD19CD,
                                     name = la,
                                     pop,
                                     all_of(general_policies)) %>%
  dplyr::mutate(across(all_of(general_policies), ~ . * 1000))

# compute incomes
for (nm in general_policies_) {
  cols = grep(paste0("^", nm), names(spending_data_ch), value=TRUE)
  spending_data[paste0(nm, "_inc")] = spending_data_ch[,cols[1]] - spending_data_ch[,cols[2]]
}

# n = df_fin %>% group_by(LAD19CD) %>% summarise(n = n())
write.csv(spending_data_ch, 'C:/Users/ru21406/YandexDisk/PhD Research/Data/spending_data_ch.csv')


