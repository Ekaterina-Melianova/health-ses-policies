

# func

LongToWide2 = function(dat){
  
  dat = pivot_longer(dat,
                     cols = contains('_'),
                     names_to = c(".value", "year"),
                     names_pattern = "(\\w)_(\\d+)")
  dat$year = as.numeric(dat$year)
  dat %<>% left_join(df.full[,c('lsoa11', 'year', 'pop_census11')],
                     by = c('lsoa11', 'year'))
  
  dat[, 'S'] = scale(log(dat[, 'S']/dat[, 'pop_census11']))
  dat[, 'H'] = -dat[, 'H']
  
  out = dat %>%
    dplyr::rename(all_of(setNames(c('H', 'S'),
                                  c('HE', 'as')))) %>%
    mutate(time = year - (min(year)-1)) %>% ungroup()
  
  return(out)
  
}



####

summary(lmer(samhi_index ~ social_care_adult + year + (1|LAD21CD) + (1|lsoa11),
             data = df_hlm))

icc(lmer(as ~ 1 + (1|lsoa11), data = df_hlm))
icc(lmer(as ~ 1 + (1|lsoa11), data = df_wide_unscaled))

# Get the list of .csv files in the folder
csv_files = list.files(path = wd, pattern = "\\.Rdata$", full.names = TRUE)
length(csv_files)

csv_files_unique = c()
for (file in csv_files) {
  file_name = basename(file)
  
  unique = str_remove(file_name, "_\\d+(\\.\\d+)?[^\\d]*$")
  csv_files_unique = c(csv_files_unique, unique)
}
n_params = length(unique(csv_files_unique))

simfit_hlm = list()
tic()
for (i in 1:20){
  
  df_list = vector("list", length = 100)
  
  csv_files_n = unique(csv_files_unique)[i]
  csv_files_n = csv_files[grepl(csv_files_n, csv_files)]
  
  # Loop through each .csv file
  for (file in csv_files_n) {
    file_name = basename(file)
    last_number = as.numeric(sub(".*_(\\d+)\\.Rdata", "\\1", file_name))
    load(file)
    df_list[last_number] = list(out)
  }
  
  df_list =  Filter(Negate(is.null), df_list)[1:10]
  wdf_list = lapply(df_list, LongToWide2)
  
  results = map(wdf_list, function(dataset) {
    model = lmer(HE ~ as + year + (1|LAD21CD) + (1|lsoa11), data = dataset)
    summary(model)
  })
  
  coefficients_list = lapply(results, function(model_summary) {
    coef_table = model_summary$coefficients
    fixed_effects = coef_table[, "Estimate"]
    return(fixed_effects)
  })
  
  # Convert the list of coefficients to a matrix for easier aggregation
  coefficients_matrix = as.data.frame(do.call(rbind, coefficients_list))
  
  # Calculate summary statistics (e.g., mean, standard deviation) across models
  mean_coefficients = colMeans(coefficients_matrix)
  sd_coefficients = apply(coefficients_matrix, 2, sd)
  
  simfit_hlm[[i]] = list(mean_coefficients, sd_coefficients)
  
  print(i)
  gc()
}
toc()

sumtab_hlm = as.data.frame(do.call(rbind.data.frame,
                      lapply(simfit_hlm, function(x) x[[1]]))[,2])
names(sumtab_hlm) = 'fixed'
sumtab_hlm$vec_names = sub('df_', '', unique(csv_files_unique)[1:20])
sumtab_hlm %<>% separate(vec_names, c('corr', 'autocorr'), sep = '_')
sumtab_hlm$corr = as.numeric(sumtab_hlm$corr)
sumtab_hlm$autocorr = as.numeric(sumtab_hlm$autocorr)




#####
coefficients_list = lapply(results, function(model_summary) {
  coef_table = model_summary$coefficients
  fixed_effects = coef_table[, "Estimate"]
  return(fixed_effects)
})

# Convert the list of coefficients to a matrix for easier aggregation
coefficients_matrix = as.data.frame(do.call(rbind, coefficients_list))

# Calculate summary statistics (e.g., mean, standard deviation) across models
mean_coefficients = colMeans(coefficients_matrix)
sd_coefficients = apply(coefficients_matrix, 2, sd)

sumtab_hlm = as.data.frame(do.call(rbind.data.frame,
                                   lapply(simfit_hlm, function(x) x[[1]]))[,2])
names(sumtab_hlm) = 'fixed'
sumtab_hlm$vec_names = sub('df_', '', unique(csv_files_unique)[1:20])
sumtab_hlm %<>% separate(vec_names, c('corr', 'autocorr'), sep = '_')
sumtab_hlm$corr = as.numeric(sumtab_hlm$corr)
sumtab_hlm$autocorr = as.numeric(sumtab_hlm$autocorr)
####


###


library(gridExtra)
library(cowplot)

ggplot(sumtab_hlm, aes(corr, autocorr, fill = fixed)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme_ipsum()+
  theme(legend.position = "bottom")


