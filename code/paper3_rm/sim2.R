library(dplyr)
library(tidyr)
library(faux)


onlyPositive = function(vector){
  
  # Step 1: Calculate the sum of the vector
  sum_vector = sum(vector)
  
  # Step 2: Determine the minimum value in the vector
  min_value = min(vector)
  
  # Step 3: Add the absolute value of the minimum value to each element
  vector_positive = vector + abs(min_value) + 10000
  
  prop = sum_vector / sum(vector_positive)
  
  normalized_vector = vector_positive * prop
  
  # Output the normalized vector
  return(normalized_vector)
  
}


normaliseVectorToSum = function(vector, desired_sum){
  
  # Step 1: Calculate the current sum
  current_sum = sum(vector)
  
  # Step 2: Divide each element by the sum
  proportions = vector / current_sum
  
  # Step 3: Multiply each proportion by the desired sum
  normalized_vector = proportions * desired_sum
  
  # Step 4: Transform to keep only positive numbers
  normalized_vector = onlyPositive(normalized_vector)
  
  return(normalized_vector)
  
}


simulateLAD = function(df_lad, corr_sh, autocorr_s){
  
  # Spending in LAD in chronological order
  vec_lad_year_S = unique(df_lad$S)
  
  # To wide format
  df_lad = pivot_wider(
    df_lad,
    id_cols = c(lsoa11, LAD21CD),
    names_from = year,
    values_from = H,
    names_prefix = 'H_'
  )
  
  # Create Correlated spending for the 1st period
  df_lad$S_2013 = rnorm_pre(df_lad[,c('H_2013')],
                             mu = 10, sd = 2,
                             r = c(corr_sh), empirical = F)
  
  # Create Correlations and Autocorrelations for the next periods
  vec_H = paste0('H_', seq(2014, 2019))
  vec_S = paste0('S_', seq(2013, 2019))
  
  for (i in 1:length(vec_H)){
    
    df_lad[vec_S[i+1]] = rnorm_pre(df_lad[,c(vec_S[i], vec_H[i])],
                                    mu = 10, sd = 2,
                                    r = c(autocorr_s, corr_sh), empirical = T)
    
  }
  
  # Normalise spending
  vec_S = paste0('S_', seq(2013, 2019))
  
  for (i in 1:length(vec_S)){
    
    df_lad[vec_S[i]] = normaliseVectorToSum(df_lad[vec_S[i]],
                                             vec_lad_year_S[i])
    
  }
  
  return(df_lad)
  
}


simulateDataset = function(df, name_spending, name_health, corr_sh, autocorr_s){
  
  df['S'] = df[name_spending]
  df['H'] = df[name_health]
  
  df = df %>% dplyr::select(lsoa11, LAD21CD, year, H, S)
  
  vec_LADs = unique(df$LAD21CD)
  
  list_dfs = list()
  
  for (LAD in vec_LADs){
    
    df_lad = df %>% filter(LAD21CD==LAD)
    
    df_lad = simulateLAD(df_lad, corr_sh, autocorr_s)
    
    list_dfs[[LAD]] = df_lad
    # print(LAD)
    
  }
  
  df = do.call(rbind.data.frame, list_dfs)
  
  return(df)
  
}

#cor(df.full$samhi_index[df.full$year==2013],
#    df.full$social_care_adult[df.full$year==2013])
#
## ----------------------------------------------------------------------------
## ---------------------- 1. DATA LOADING AND PREPROCESSING
## ----------------------------------------------------------------------------
#
## LOAD DATASET
df.full = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df.rds')

# PREPROCESSING
df.full = df.full %>% ungroup()
df.full$social_care_adult = df.full$pop * df.full$social_care_adult
df.full$law_order = df.full$pop * df.full$law_order
#
## ~~~~~~~~~~~~~~~~~ EXAMPLES
#
#
#df.1 = simulateDataset(df = df.full,
#                        name_spending = 'social_care_adult',
#                        name_health = 'samhi_index',
#                        corr_sh = -0.5,
#                        autocorr_s = 0.8)
#
#cor(df.1$S_2013, df.1$H_2013)
#
#df.2 = simulateDataset(df = df.full,
#                        name_spending = 'social_care_adult',
#                        name_health = 'samhi_index',
#                        corr_sh = 0.5,
#                        autocorr_s = 0.8)
#
#cor(df.2$S_2013, df.2$H_2013)
#
#df.3 = simulateDataset(df = df.full,
#                        name_spending = 'social_care_adult',
#                        name_health = 'samhi_index',
#                        corr_sh = 0,
#                        autocorr_s = 0.9)
#
#cor(df.3$S_2013, df.3$H_2013)
#
#df.4 = simulateDataset(df = df.full,
#                        name_spending = 'social_care_adult',
#                        name_health = 'samhi_index',
#                        corr_sh = 0.8,
#                        autocorr_s = 0.8)
#
#cor(df.4$S_2013, df.4$H_2013)
#
#long_data = pivot_longer(df.1,
#                          cols = contains('_'),
#                          names_to = c(".value", "year"),
#                          names_pattern = "(\\w)_(\\d+)")
#
#
#####
#
#df_lad = df.full %>% group_by(year, LAD21CD) %>%
#  summarise(M = mean(samhi_index),
#            S = mean(social_care_adult)) %>%
#  filter(year == 2013)
#
#cor(df_lad$M, df_lad$S)
  



