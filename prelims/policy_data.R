# policy_data.R

# Exploring policy data, 2007-2020

library(httr)
library(stringr)
library(XML)
library(readODS)
library(readxl)
library(stringr)
library(plyr); library(dplyr)
#install.packages('stringi')
#install.packages('stringr')

cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
}

#-------------------------------------------------------------------------------

# setting a vector with website links
sites <- c(
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2007-to-2008-individual-local-authority-data',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2008-to-2009-individual-local-authority-data',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2009-to-2010-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2010-to-2011-individual-local-authority-data--5',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2011-to-2012-individual-local-authority-data--2',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2012-to-2013-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2013-to-2014-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistical-data-sets/local-authority-revenue-expenditure-and-financing-england-2014-to-2015-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2015-to-2016-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2016-to-2017-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2017-to-2018-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2018-to-2019-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2019-to-2020-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2020-to-2021-individual-local-authority-data-outturn'
)

# creating a vector with fiscal years
year_seq <- seq(2007, 2020)

# creating an empty list for all financial data
list_df <- vector("list", length(year_seq))

# downloading excel files with individual local authority outturn data from gov.uk
for (i in 1:length(sites)){
  
  # extracting links with files
    html <- paste(readLines(sites[i]), collapse="\n")
    if (i == which(year_seq == 2014)){ # there was a different html code in 2014
      matched <- str_match_all(html, '><a href=\"(.*?)\"')
    }
    else{
      matched <- str_match_all(html, '<a aria-hidden="true" class="thumbnail" tabindex="-1" href=\"(.*?)\"')
      
    }
    links <- matched[[1]][, 2]
    
 # storing files in folders by years   
   for (j in 1:length(links)){ 
    folder <- paste0("C:/Users/", Sys.getenv("USERNAME"),
                  '/YandexDisk/PhD Research/Data/Financial/',
                  substr(year_seq[i], 1, 4))
    file_name <- sub(".*/", "", links[j])
    #GET(links[j], write_disk(tf <- tempfile(tmpdir = folder, fileext = ".ods")))
    download.file(links[j], file.path(folder, file_name), mode = 'wb')
    
    # full path
    path <- file.path(folder, file_name)

    # uploading all data into a large list
    if (i %in% which(year_seq %in% c(2019, 2020))){ # conditioning on a file extension
      
      # selecting a sheet (removing a hidden sheet 'Col refs' and picking the 3rd one out of the remaning)
      sheet_to_import <- list_ods_sheets(path)
      sheet_to_import <- sheet_to_import[which(!sheet_to_import == 'Col refs')][3]
      list_df[[i]][[j]] <- read_ods(path,  sheet = sheet_to_import)
      } else {
        sheet_to_import <- excel_sheets(path)
        sheet_to_import <- sheet_to_import[which(!sheet_to_import == 'Col refs')][3]
        list_df[[i]][[j]] <- read_excel(path,  sheet = sheet_to_import)
        }
  }

}

# unifying the class of tables
for (i in seq_along(list_df)){
  list_df[[i]] <- sapply(list_df[[i]], as.data.frame)  
}

# saving the list
# saveRDS(list_df, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/list_df.RData'))
# list_df <- readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/list_df.RData'))

### retrieving variable names (column names of the respective excel tables)

# empty df
policy_names_raw <- data.frame(character()) 

for (i in seq_along(list_df)){
  
  temp <- list_df[[i]]
  policy_names_raw_ <- data.frame(character()) 
  
  for (j in seq_along(temp)){
  # selecting a proper ending row (which matches a string of interest)
  range_start <- NULL
  col_skip <- 0
  search_string <- "E-code|LA Code"
  max_cols_to_search <- 5
  max_rows_to_search <- 15
  
  # defining the position of the ending row
  while (length(range_start) == 0) {
    col_skip <- col_skip + 1
    if (col_skip == max_cols_to_search) break
    range_start <- which(str_detect(temp[[j]][1:max_rows_to_search, col_skip],search_string))
  }

  # truncating the area with the text data of interest
  text_raw <- temp[[j]][3:range_start,]
  colnames(text_raw) <- 1:length(text_raw)
  
  # turning text to a character vector
  text_vec <- c()
  for (k in 1:ncol(text_raw)){
    text_vec <- c(text_vec, pull(text_raw, k))
  }
  
  # cleaning 
  text_vec <- gsub("\\d+", "", text_vec)
  text_vec <- as.data.frame(text_vec[!is.na(text_vec)& text_vec!= ''])
  policy_names_raw_ <- cbind.fill(policy_names_raw_, text_vec)
  
  print(c(i,j))
  }
  # final output
  policy_names_raw <- cbind.fill(policy_names_raw, policy_names_raw_)
}

# removing empty columns
policy_names_raw <- 
  policy_names_raw[,colSums(is.na(policy_names_raw)) < nrow(policy_names_raw)]

# checking dimensionality
sum(unlist(lapply(seq_along(list_df), function(x){
  length(list_df[[x]])
  })
)) == ncol(policy_names_raw)







































