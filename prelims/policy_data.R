# policy_data.R

# Exploring policy data from gov.uk, 2007-2020

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

# downloading excel files with individual local authority outturn data from gov.uk
loadLists <- function(sites, year_seq = seq(2007, 2020), download = F, list_number){
  
  # creating an empty list
  list_df <- vector("list", length(year_seq))
  
  for (i in seq_along(list_df)){
  
  # extracting links with files
  html <- paste(readLines(sites[i]), collapse="\n")
  if (i == which(year_seq == 2014)){ # there was a different html code in 2014
    matched <- str_match_all(html, '><a href=\"(.*?)\"')
  }else{
    matched <- str_match_all(html, '<a aria-hidden="true" class="thumbnail" tabindex="-1" href=\"(.*?)\"')
    
  }
  links <- matched[[1]][, 2]
  
  file_name_vec <- c()
  
  # storing files in folders by years   
  for (j in seq_along(links)){ 
    folder <- paste0("C:/Users/", Sys.getenv("USERNAME"),
                     '/YandexDisk/PhD Research/Data/Financial/', year_seq[i])
    file_name <- sub(".*/", "", links[j])
    
    if (download == T){
       download.file(links[j], file.path(folder, file_name), mode = 'wb')
     }
    
      # full path
      path <- file.path(folder, file_name)
    
      # uploading all data into a large list
      if (i %in% which(year_seq %in% c(2019, 2020))){ # conditioning on a file extension
      
      # selecting a sheet (removing a hidden sheet 'Col refs' and picking the 3rd one out of the remaning)
      sheet_to_import <- list_ods_sheets(path)
      sheet_to_import <- sheet_to_import[which(!sheet_to_import == 'Col refs')][list_number]
      list_df[[i]][[j]] <- read_ods(path,  sheet = sheet_to_import)
      
      } else {
      sheet_to_import <- excel_sheets(path)
      sheet_to_import <- sheet_to_import[which(!sheet_to_import == 'Col refs')][list_number]
      list_df[[i]][[j]] <- read_excel(path,  sheet = sheet_to_import)
      
      }
    file_name_vec <- c(file_name_vec, file_name)
    
  }
  
  names(list_df[[i]]) <- file_name_vec
  }

  # unifying the class of tables
  for (i in seq_along(list_df)){
  list_df[[i]] <- sapply(list_df[[i]], as.data.frame)  
  }
  
  return(list_df)
}

# applying the function
list_df <- loadLists(sites = sites, list_number = 2)

# saving the list
# saveRDS(list_df, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/list_df.RData'))
# list_df <- readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/list_df.RData'))

# selecting ROs - Revenue Account Outturns 
RO_logical <- lapply(seq_along(list_df),
       function(j){
      unlist(lapply(seq_along(list_df[[j]]),
                    function(i)
                      grepl('RO', names(list_df[[j]])[i]) & !grepl('suppl', names(list_df[[j]])[i], ignore.case = T)))
         })
list_df_RO <- list()
for (i in seq_along(RO_logical)){
  RO_names <- names(list_df[[i]])[which(RO_logical[[i]] == TRUE)]
  list_df_RO[[i]] <- list_df[[i]][RO_names]
  
}
list_df_RO[[1]] <- NULL
#eval <- eval(parse(text = paste0('c(', paste0('"', RO_names, collapse = '", '), '")')))

# empty list
policy_names_raw <- vector('list', length(list_df_RO[[1]]))

# retrieving variable names (column names of the respective excel tables)
for (i in seq_along(list_df_RO)){
  
  temp <- list_df_RO[[i]]
  policy_names_raw_ <- data.frame(character()) 
  
  for (j in seq_along(temp)){
   
    # truncating the area with the text data of interest
    text_raw <- temp[[j]][,1:4]
    colnames(text_raw) <- 1:length(text_raw)
    
    # turning text to a character vector
    text_vec <- c()
    for (k in 1:ncol(text_raw)){
      text_vec <- c(text_vec, pull(text_raw, k))
    }
    
    # cleaning 
    text_vec <- gsub("\\d+", "", text_vec)
    text_vec <- as.data.frame(text_vec[!is.na(text_vec)& text_vec!= ''])
    
    # adding a year and file name
    text_vec <- as.character(unlist(c(names(list_df_RO[[i]][j]), text_vec)))
    
    policy_names_raw_ <- as.data.frame(cbind.fill(policy_names_raw_, text_vec))
    
    # removing empty columns
    policy_names_raw_ <- 
      policy_names_raw_[,colSums(is.na(policy_names_raw_)) < nrow(policy_names_raw_)]
    
    print(c(i,j))
      
  }
  # final output
  for (m in 1:ncol(policy_names_raw_)){
      policy_names_raw[[m]][[i]] <- policy_names_raw_[,m]
  }

}

# grouping by policy categories (RO1, RO2, etc.)
for (i in seq_along(policy_names_raw)){
  assign(paste0('RO', i), do.call('cbind.fill', policy_names_raw[[i]]))
 }

# cleaning
clean <- function(df){
  
  # removing redundant rows
  df <- lapply(seq_along(df), function(x) replace(df[,x], grep("Source|Produced|ENGLAND|drop-down|Employees|(C)", df[,x]), NA))
  df <- lapply(seq_along(df), function(x) replace(df[[x]], which(df[[x]] == '.'), NA))
  
  # pushing NA to the end
  df <- lapply(seq_along(df), function(x) c(df[[x]][!is.na(df[[x]])], df[[x]][is.na(df[[x]])]))
  
  # pushing file names to the front
  df <- lapply(seq_along(df), function(x) c(df[[x]][grep('RO', df[[x]])], df[[x]][-grep('RO', df[[x]])]))
  
  # turning lists to a df
  df <- do.call('cbind.fill', df)
  
  # removing rows with all NAs
  df <- df[rowSums(is.na(df)) < ncol(df),]
}

# applying the function
RO1 <- clean(RO1)
RO2 <- clean(RO2)
RO3 <- clean(RO3)
RO4 <- clean(RO4)
RO5 <- clean(RO5)
RO6 <- clean(RO6)


