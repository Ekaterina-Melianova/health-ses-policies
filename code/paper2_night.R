dvs_temp = dvs[1]

list_combined = list()
i = 1
for (dv in dvs_temp){
  
  for (lsoa in lsoa_group){
    
    synt_n = 1
    
    for (synt in syntax){
      
      list_combined[[i]] = list(dv, lsoa, synt, synt_n)
      i = i + 1
      synt_n = synt_n + 1
      
      
    }
    
  }
}



# running

cluster = makeCluster(12) 
registerDoParallel(cluster)

tic()
constrained_models = foreach(comb = list_combined,
                             .packages = c('lavaan', 'tidyverse',
                                           'dplyr', 'tidyr',
                                           'magrittr'),
                             .combine = c) %dopar% {
                               groupSEM(comb)
                             }

toc()
stopCluster(cluster)
saveRDS(constrained_models,
        paste0('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/',
               dvs_temp, '_', 'constrained_models', '.rds'))



Sys.sleep(60)


####
dvs_temp = dvs[4]

list_combined = list()
i = 1
for (dv in dvs_temp){
  
  for (lsoa in lsoa_group){
    
    synt_n = 1
    
    for (synt in syntax){
      
      list_combined[[i]] = list(dv, lsoa, synt, synt_n)
      i = i + 1
      synt_n = synt_n + 1
      
      
    }
    
  }
}



# running

cluster = makeCluster(12) 
registerDoParallel(cluster)

tic()
constrained_models = foreach(comb = list_combined,
                             .packages = c('lavaan', 'tidyverse',
                                           'dplyr', 'tidyr',
                                           'magrittr'),
                             .combine = c) %dopar% {
                               groupSEM(comb)
                             }

toc()
stopCluster(cluster)

saveRDS(constrained_models,
        paste0('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/',
               dvs_temp, '_', 'constrained_models', '.rds'))


Sys.sleep(60)


####
dvs_temp = dvs[5]

list_combined = list()
i = 1
for (dv in dvs_temp){
  
  for (lsoa in lsoa_group){
    
    synt_n = 1
    
    for (synt in syntax){
      
      list_combined[[i]] = list(dv, lsoa, synt, synt_n)
      i = i + 1
      synt_n = synt_n + 1
      
      
    }
    
  }
}



# running

cluster = makeCluster(12) 
registerDoParallel(cluster)

tic()
constrained_models = foreach(comb = list_combined,
                             .packages = c('lavaan', 'tidyverse',
                                           'dplyr', 'tidyr',
                                           'magrittr'),
                             .combine = c) %dopar% {
                               groupSEM(comb)
                             }

toc()
stopCluster(cluster)

saveRDS(constrained_models,
        paste0('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/',
               dvs_temp, '_', 'constrained_models', '.rds'))

