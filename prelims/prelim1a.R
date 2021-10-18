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
for (i in 1:length(outturn_tmp)){
  colnames(outturn_tmp[[i]]) <- c('E-code',
                     'Code',
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

# selecting top districts by temporal sd in education outturns
var_tab <- aggregate(education ~ Code, data = df, sd) %>%
  arrange(desc(education)) %>% top_n(20, education)
df_edu_top <- df %>% filter(Code %in% var_tab$Code)

# plotting
ggplot(df_edu_top, aes(x = year, y = education, colour = local_authority,
                       group = local_authority)) +
  geom_line() + 
  xlab("")  + 
  xlab("Total expenditure in education per 1,000 people, base year = 2020")+ 
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#------------------------------- Heatmaps -------------------------------#
mat <- as.matrix(mat)

# Heatmap
#d3heatmap(mat, scale="column", dendrogram = "none", width="800px", height="80Opx", colors = "Blues")

p <- heatmaply(mat, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Country", "Feature:", "Value"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(mat),
               labRow = rownames(mat),
               heatmap_layers = theme(axis.line=element_blank())
)







