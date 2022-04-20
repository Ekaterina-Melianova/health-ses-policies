# prelim1a.R

# Exploring the data for revenue out-turn service expenditures by local authorities in England
# Data source: https://www.gov.uk/government/collections/local-authority-revenue-expenditure-and-financing

# libraries
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(stringr)
library(hrbrthemes)
library(heatmaply)
library(lattice)

# wd
setwd("C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial")

# listing files in the wd
files <- list.files(pattern = "[0-9].xls")
outturn_raw <- vector(mode = 'list', length(files))

# loading excel worksheets 
for (i in 1:length(outturn_raw)){
  
  # loading row data
  temp_read <- readxl::read_excel(files[i], sheet = 3)
  
  # selecting a proper starting row (which matches a certain string)
  range_start <- NULL
  col_skip <- 0
  search_string <- "E-code"
  max_cols_to_search <- 10
  max_rows_to_search <- 10
  
  # defining the position of the starting row
  while (length(range_start) == 0) {
    col_skip <- col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_start <- which(stringr::str_detect(temp_read[1:max_rows_to_search,col_skip][[1]],search_string)) + 1 
  }
  
  # selecting a proper ending row
  range_end <- NULL
  col_skip <- 0
  search_string <- "E6803"
  max_cols_to_search <- 10
  max_rows_to_search <- 490
  
  # defining the position of the ending row
  while (length(range_end) == 0) {
    col_skip <- col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_end <- which(stringr::str_detect(temp_read[1:max_rows_to_search,col_skip][[1]],search_string)) + 1
  }
  
  # re-reading from the new starting and ending points
  outturn_raw[[i]] <- readxl::read_excel(
    files[i],
    sheet = 3,
    range = cell_rows(range_start:range_end)
  )
}  

# selecting only last years with unified names for expenditures
outturn_tmp <- outturn_raw[8:length(outturn_raw)]
outturn_tmp <- lapply(1:length(outturn_tmp),
                      function(i) as.data.frame(outturn_tmp[[i]])[as.logical(replace(grepl("Total Expenditure",
                                                                                           colnames(outturn_tmp[[i]])), 1:5, 'TRUE'))])
# renaming columns
vars <- c('Code',
          'local_authority',
          'region',
          'class',
          'education',
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
for (i in 1:length(outturn_tmp)){
  colnames(outturn_tmp[[i]]) <- c('E-code', vars)
  
  outturn_tmp[[i]] <- outturn_tmp[[i]][rowSums(is.na(outturn_tmp[[i]])) != ncol(outturn_tmp[[i]]),]
  outturn_tmp[[i]]['year'] <- 2013 + i
}

# long format
outturn_long <- do.call('rbind.data.frame', outturn_tmp)

# population data
population_raw <- read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Socio-demographics/Population.xls',
                            sheet = 'MYE 5', skip = 7)
population_raw <- population_raw[as.logical(replace(grepl("Population",
                                        colnames(population_raw)), 1, 'TRUE'))]
population_raw <- population_raw[, 1:7]
names(population_raw) <- c('Code', '2019', '2018', '2017', '2016', '2015', '2014')

# long format
population <- population_raw %>%
  pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "population")
population$year <- as.numeric(population$year)

# deflator
deflator <- read_excel('C:/Users/ru21406/YandexDisk/PhD Research/Data/Financial/GDP_Deflators_Budget_March_2021_update.xlsx',
                             range = 'H7:I73')
names(deflator) <- c('year', 'def')
deflator %<>% filter(year %in% 2014:2019)
deflator$year <- as.numeric(deflator$year)

# merging with outturns
outturn_long_ <- outturn_long %>%
  left_join(population, by = c('Code', 'year')) %>%
  left_join(deflator, by = 'year')

outturn_long_[, 6:19] <- sapply(outturn_long_[, 6:19], as.numeric)

# computing exp per 1,000 people in prices of 2020

df <- outturn_long_
outturn_col <- c(1:5, which(colnames(df) %in% c('year', 'population', 'def')))
df[-outturn_col] <- (df[-outturn_col]/(df[,'population']/1000))*(df[, 'def']/100)[row(df[-outturn_col])]

# cleaning the resulting dataset
df <- df %>% select(-region)
df <- na.omit(df)
summary(df)

# norm by mean
vars <- vars[-which(vars %in% c('Code', 'local_authority', 'class', 'region'))]
df_normed <- df %>%
  group_by(local_authority) %>%
  mutate_at(vars, list(function(x) x/mean(x))) %>% 
   # districts with zeros have no variance -> replacing with 0
  mutate_at(vars, ~ replace(., is.nan(.), 0)) %>%
  mutate_at(list(sd = sd), .vars = vars)

#-------- Education

df_edu_sorted <- df_normed %>% arrange(desc(education_sd))
top <- df_edu_sorted %>% distinct(local_authority, .keep_all = T) %>%
  ungroup() %>%
  top_n(30, education_sd)
df_edu_sorted_top <- df_edu_sorted %>% filter(local_authority %in% top$local_authority)

#-------- Public Health

df_ph_sorted <- df_normed %>% # too many zeros (?)
  filter(!public_health == 0 & !is.na(public_health)) %>% arrange(desc(public_health_sd))
top <- df_ph_sorted %>% distinct(local_authority, .keep_all = T) %>%
  ungroup() %>%
  top_n(60, public_health_sd)
df_ph_sorted_top <- df_ph_sorted %>% filter(local_authority %in% top$local_authority)

#-------- Housing

df_hs_sorted <- df_normed %>% # too many zeros (?)
  filter(!housing == 0 & !is.na(housing)) %>% arrange(desc(public_health_sd))
top <- df_hs_sorted %>% distinct(local_authority, .keep_all = T) %>%
  ungroup() %>%
  top_n(40, public_health_sd)
df_hs_sorted_top <- df_hs_sorted %>% filter(local_authority %in% top$local_authority)

#------------------------------- Heatmaps -------------------------------#
# Education
mat_edu_ <- df_edu_sorted_top %>%
  pivot_wider(id_cols = local_authority, 
            names_from = year, values_from = education) %>% drop_na %>%
  ungroup() %>% filter(!local_authority == 'Greater London Authority')
mat_edu <- mat_edu_  %>%
  select(-local_authority)
mat_edu <- as.matrix(mat_edu)
rownames(mat_edu) <- mat_edu_$local_authority
  
# Heatmap
hm_edu <- heatmaply(mat_edu, 
               xlab = "", ylab = "", 
               main = "",
               margins = c(20,20,5,20),
               grid_color = "white",
               grid_width = 0.00000001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.001,
               label_names = c("Local Authority", "Year", "Outturn, normed"),
               fontsize_row = 5, fontsize_col = 10,
               labCol = colnames(mat_edu),
               labRow = rownames(mat_edu),
               heatmap_layers = theme(axis.line=element_blank())
)
hm_edu

# Public Health
mat_ph_ <- df_ph_sorted_top %>%
  pivot_wider(id_cols = local_authority, 
              names_from = year, values_from = public_health) %>%
  # replacing NA since some districts were not present for all years
  mutate_at(vars(-group_cols()), ~ replace(., is.na(.), 0)) %>%
  filter_all(all_vars(. != 0)) %>%
  ungroup() %>% filter(!local_authority == 'Greater London Authority')
mat_ph <- mat_ph_  %>%
  select(-local_authority)
mat_ph <- as.matrix(mat_ph)
rownames(mat_ph) <- mat_ph_$local_authority

# Heatmap
hm_ph <- heatmaply(mat_ph, 
                    xlab = "", ylab = "", 
                    main = "",
                    margins = c(0,0,0,0),
                    grid_color = "white",
                    grid_width = 0.00000001,
                    titleX = FALSE,
                    hide_colorbar = TRUE,
                    branches_lwd = 0.001,
                    label_names = c("Local Authority", "Year", "Outturn, normed"),
                    fontsize_row = 5, fontsize_col = 10,
                    labCol = colnames(mat_ph),
                    labRow = rownames(mat_ph),
                    heatmap_layers = theme(axis.line=element_blank())
)
hm_ph

# Housing
mat_hs_ <- df_hs_sorted_top %>%
  pivot_wider(id_cols = local_authority, 
              names_from = year, values_from = housing) %>%
  # replacing NA since some districts were not present for all years
  mutate_at(vars(-group_cols()), ~ replace(., is.na(.), 0)) %>%
  filter_all(all_vars(. != 0)) %>%
  ungroup() %>% filter(!local_authority == 'Greater London Authority')
mat_hs <- mat_hs_  %>%
  select(-local_authority)
mat_hs <- as.matrix(mat_hs)
rownames(mat_hs) <- mat_hs_$local_authority

# Heatmap
hm_hs <- heatmaply(mat_hs, 
                   xlab = "", ylab = "", 
                   main = "",
                   margins = c(20,20,5,20),
                   grid_color = "white",
                   grid_width = 0.00000001,
                   titleX = FALSE,
                   hide_colorbar = TRUE,
                   branches_lwd = 0.001,
                   label_names = c("Local Authority", "Year", "Outturn, normed"),
                   fontsize_row = 5, fontsize_col = 10,
                   labCol = colnames(mat_hs),
                   labRow = rownames(mat_hs),
                   heatmap_layers = theme(axis.line=element_blank())
)
hm_hs



