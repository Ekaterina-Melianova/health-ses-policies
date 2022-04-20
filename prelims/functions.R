# functions

# cbind dataframes with unequal number of rows

cbind.fill = function(...) {                                                                                                                                                       
  transpoted = lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe = lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
}

#-------------------------------------------------------------------------------

# setting a vector with website links
sites = c(
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
loadLists = function(sites, year_seq = 2007:2020,
                     download = F, list_number, type){
  
  # creating an empty list
  list_df = vector("list", length(year_seq))
  
  for (i in seq_along(list_df)){
    
    # extracting links with files
    html = paste(readLines(sites[i]), collapse="\n")
    if (i == which(year_seq == 2014)){ # there was a different html code in 2014
      matched = str_match_all(html, '><a href=\"(.*?)\"')
    }
    else{
      matched = str_match_all(html, '<a aria-hidden="true" class="thumbnail" tabindex="-1" href=\"(.*?)\"')
      
    }
    links = matched[[1]][,2]
    
    file_name_vec = c()
    
    # storing files in folders by years   
    for (j in seq_along(links)){   
      
      folder = paste0("C:/Users/", Sys.getenv("USERNAME"),
                      '/YandexDisk/PhD Research/Data/Financial/', year_seq[i])
      file_name = sub(".*/", "", links[j])
      
      if (download == T){
        download.file(links[j], file.path(folder, file_name), mode = 'wb')
      }
      
      # retrieving from folders the files of interest
      if (grepl(type, file_name)){
        
        # full path
        path = file.path(folder, file_name)
        
        # uploading all data into a large list
        if (i %in% which(year_seq %in% c(2019, 2020))){ # conditioning on a file extension
          
          # selecting a sheet (removing a hidden sheet 'Col refs' and picking the 3rd one out of the remaning)
          sheet_to_import = list_ods_sheets(path)
          sheet_to_import = sheet_to_import[which(!sheet_to_import == 'Col refs')][list_number]
          list_df[[i]][[j]] = read_ods(path,  sheet = sheet_to_import)
          
        } 
        else {
          sheet_to_import = excel_sheets(path)
          sheet_to_import = sheet_to_import[which(!sheet_to_import == 'Col refs')][list_number]
          list_df[[i]][[j]] = read_excel(path,  sheet = sheet_to_import)
          
        }
        
        # removing empty lists
        list_df[[i]] = list_df[[i]] %>% purrr::discard(function(x) length(x) == 0L)
        #list_df[[i]] = rlist::list.clean(list_df[[i]], function(x) length(x) == 0L, TRUE)
        
        # assigning names to lists
        file_name_vec = c(file_name_vec, file_name)
        names(list_df[[i]]) = file_name_vec
        
      }     
    }
    
    # unifying the class of tables 
    for (i in seq_along(list_df)){
      list_df[[i]] = lapply(list_df[[i]], as.data.frame)  
    }   
    
  }
  
  return(list_df)
}

