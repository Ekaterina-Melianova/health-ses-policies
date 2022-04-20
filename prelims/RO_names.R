# policy_data.R

# Exploring policy data from gov.uk, 2007-2020

library(httr)
library(stringr)
library(XML)
library(readODS)
library(readxl)
library(plyr); library(dplyr)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/prelims/functions.R')

# applying data extraction function
policy_list = loadLists(sites = sites, list_number = 2, type = 'RO')
policy_list[[1]] = NULL

# removing unnessesary files
type_logical = lapply(seq_along(policy_list),
                    function(j){
                      unlist(lapply(seq_along(policy_list[[j]]),
                                    function(i)
                                      grepl('RO', names(policy_list[[j]])[i]) & !grepl('suppl', names(policy_list[[j]])[i], ignore.case = T)))
                    })
policy_list_fin = list()
for (i in seq_along(type_logical)){
  type_names = names(policy_list[[i]])[which(type_logical[[i]] == TRUE)]
  policy_list_fin[[i]] = policy_list[[i]][type_names]
  
}
#eval = eval(parse(text = paste0('c(', paste0('"', type_names, collapse = '", '), '")')))

# saving the list
# saveRDS(policy_list_fin, paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/policy_list_fin.RData'))
# policy_list_fin = readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/policy_list_fin.RData'))

# empty list
policy_names_raw = vector('list', length(policy_list_fin[[1]]))

# retrieving variable names (column names of the respective excel tables)
for (i in seq_along(policy_list_fin)){
  
  temp = policy_list_fin[[i]]
  policy_names_raw_ = data.frame(character()) 
  
  for (j in seq_along(temp)){
   
    # truncating the area with the text data of interest
    text_raw = temp[[j]][,1:4]
    colnames(text_raw) = 1:length(text_raw)
    
    # turning text to a character vector
    text_vec = c()
    for (k in 1:ncol(text_raw)){
      text_vec = c(text_vec, pull(text_raw, k))
    }
    
    # cleaning 
    text_vec = gsub("\\d+", "", text_vec)
    text_vec = as.data.frame(text_vec[!is.na(text_vec)& text_vec!= ''])
    
    # adding a year and file name
    text_vec = as.character(unlist(c(names(policy_list_fin[[i]][j]), text_vec)))
    
    policy_names_raw_ = as.data.frame(cbind.fill(policy_names_raw_, text_vec))
    
    # removing empty columns
    policy_names_raw_ = 
      policy_names_raw_[,colSums(is.na(policy_names_raw_)) < nrow(policy_names_raw_)]
    
    print(c(i,j))
      
  }
  # final output
  for (m in 1:ncol(policy_names_raw_)){
      policy_names_raw[[m]][[i]] = policy_names_raw_[m]
  }

}

# grouping by policy categories (RO1, RO2, etc.)
for (i in seq_along(policy_names_raw)){
  assign(paste0('RO', i), do.call('cbind.fill', policy_names_raw[[i]]))
 }

# cleaning
clean = function(df){
  
  # removing redundant rows
  df = lapply(seq_along(df), function(x) replace(df[,x], grep("Source|Produced|ENGLAND|drop-down|Employees|(C)", df[,x]), NA))
  df = lapply(seq_along(df), function(x) replace(df[[x]], which(df[[x]] == '.'), NA))
  
  # pushing NA to the end
  df = lapply(seq_along(df), function(x) c(df[[x]][!is.na(df[[x]])], df[[x]][is.na(df[[x]])]))
  
  # pushing file names to the front
  df = lapply(seq_along(df), function(x) c(df[[x]][grep('RO', df[[x]])], df[[x]][-grep('RO', df[[x]])]))
  
  # turning lists to a df
  df = do.call('cbind.fill', df)
  
  # removing rows with all NAs
  df = df[rowSums(is.na(df)) < ncol(df),]
}

# applying the function
RO1 = clean(RO1)
RO2 = clean(RO2)
RO3 = clean(RO3)
RO4 = clean(RO4)
RO5 = clean(RO5)
RO6 = clean(RO6)


