

library(parallel)
library(doParallel)
library(tictoc)
library(simsem)

list_combined = list()
a = 1
for (i in 1:100){
  for (corr_sh in seq(-.9, .9, .1)){
    for (autocorr_s in seq(.5, .9, .1)){
       list_combined[[a]] = list(corr_sh, autocorr_s, i)
       a = a + 1
       }
    }
}

wd = "C:/Users/ru21406/OneDrive - University of Bristol/Desktop/test2"
setwd(wd)

num_cores = 12

cl = makeCluster(num_cores)
registerDoParallel(cl)


generate_datasets = function(comb) {
  
  corr_sh = comb[[1]]
  autocorr_s = comb[[2]]
  i = comb[[3]]
  
  max_attempts = 10
  attempts = 0
  success = FALSE
  
  while (!success && attempts < max_attempts) {
    attempts = attempts + 1
    
    tryCatch({
      out = simulateDataset(df = df.full,
                            name_spending = 'social_care_adult',
                            name_health = 'samhi_index',
                            corr_sh = corr_sh,
                            autocorr_s = autocorr_s)
      success = TRUE
    }, error = function(e) {
      cat("Attempt", attempts, "failed. Retrying...\n")
    })
  }
  
  if (success) {
    file_name = paste0('df_', corr_sh, '_', autocorr_s, '_', i, '.Rdata')
    save(out, file = file_name)
    cat("Dataset", file_name, "successfully generated.\n")
  } else {
    cat("Maximum attempts reached. Dataset generation failed.\n")
  }
  print(i)
}

tic()
df_list = foreach(comb = list_combined, .combine = c,
                      .packages = c('dplyr', 'tidyr', 'faux')) %dopar% {
                        generate_datasets(comb)
}
toc()

stopCluster(cl)

# to wide format
LongToWide = function(dat){
  
  dat = pivot_longer(dat,
                     cols = contains('_'),
                     names_to = c(".value", "year"),
                     names_pattern = "(\\w)_(\\d+)")
  dat$year = as.numeric(dat$year)
  dat %<>% left_join(df.full[,c('lsoa11', 'year', 'pop_census11')])
  
  dat[, 'S'] = scale(log(dat[, 'S']/dat[, 'pop_census11']))
  dat[, 'H'] = -dat[, 'H']
  
  out = dat %>%
  dplyr::rename(all_of(setNames(c('H', 'S'),
                                c('HE', 'as')))) %>%
  mutate(time = year - (min(year)-1)) %>%
  tidyr::pivot_wider(id_cols = all_of(c('lsoa11', 'LAD21CD')),
                     names_from = time, 
                     values_from = all_of(c('HE', 'as')),
                     names_sep = '')
  return(out)

}


## Running simulations

## get the list of .csv files in the folder
csv_files = list.files(path = wd, pattern = "\\.Rdata$", full.names = TRUE)
length(csv_files)

csv_files_unique = c()
for (file in csv_files) {
  file_name = basename(file)
  
  unique = str_remove(file_name, "_\\d+(\\.\\d+)?[^\\d]*$")
  csv_files_unique = c(csv_files_unique, unique)
}
n_params = length(unique(csv_files_unique))

## function to run through each combination of parameters

df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df_lv.RDS')

simulate_models = function(model_fit_type,
                           max_samples = 100,
                           max_params = n_params,
                           data_base = df_lv){
  
  syntax_sim = RC_GCLM_syntax(model = model_fit_type,
                              endogeneous = c('HE', 'as'),
                              control = NULL)
  fit_sim = sem(syntax_sim,
                data = data_base,
                estimator = "mlr",
                cluster = 'LAD21CD',
                orthogonal = T)
  
  simfit = list()
  for (i in 1:n_params){
    
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
    
    df_list =  Filter(Negate(is.null), df_list)[1:max_samples]
    wdf_list = lapply(df_list, LongToWide)
    
    simfit[[i]] = sim(seed = 12345,
                      multicore = TRUE,
                      numProc = 12,
                      rawData = wdf_list,
                      model = fit_sim,
                      silent =T)
    print(i)
    gc()
  }
  
  return(simfit)
}

## applying the simulation function
simfit_rcclpm = simulate_models(model_fit_type = 'rcclpm')
simfit_rcgclm = simulate_models(model_fit_type = 'rcgclm')

## function to extract parameters for plotting
extract_heatmap_data = function(lst_data,
                                params,
                                column = 'Estimate Average',
                                param_name,
                                param_comb_names = unique(csv_files_unique)[1:40]){
  
  names(lst_data) = param_comb_names
  extract_params = function(lst,
                            params2 = params,
                            column2 = column){
    sumtab = summaryParam(lst)
    sumtab = sumtab[params2,column2]
    return(sumtab)
  }
  
  sumtab_list = list()
  sumtab_list = lapply(lst_data, extract_params)
  
  sumtab_df = do.call(rbind.data.frame, sumtab_list)
  names(sumtab_df) = param_name
  vec_names = names(sumtab_list)
  sumtab_df = cbind(sumtab_df, vec_names)
  sumtab_df$vec_names = sub('df_', '', sumtab_df$vec_names)
  sumtab_df %<>% separate(vec_names, c('corr', 'autocorr'), sep = '_')
  sumtab_df$corr = as.numeric(sumtab_df$corr)
  sumtab_df$autocorr = as.numeric(sumtab_df$autocorr)
  
  return(sumtab_df)
}

## applying the function
sumtab_rcclpm = extract_heatmap_data(lst_data = simfit_rcclpm,
                                     params = c("d_HEas <- (e_HE3~e_as2)"),
                                     param_name = 'total')
sumtab_rcgclm = extract_heatmap_data(lst_data = simfit_rcgclm,
                                     params = c('b_HEas <- (HE2~as1)',
                                                'd_HEas <- (HE2~e_as1)'),
                                     param_name = c('long', 'short'))

## plotting

library(gridExtra)
library(cowplot)
library(hrbrthemes)

# rcgclm 
long = ggplot(sumtab_rcgclm, aes(corr, autocorr, fill= long)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme_ipsum()+
  theme(legend.position = "none")

short = ggplot(sumtab_rcgclm, aes(corr, autocorr, fill= short)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme_ipsum()+
  theme(legend.position = "none")

# rcclpm
total = ggplot(sumtab_rcclpm, aes(corr, autocorr, fill = total)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme_ipsum()+
  theme(legend.position = "none")

# combine both plots using grid.arrange()
combined_plot1 = grid.arrange(long, short, ncol = 2)
combined_plot2 = grid.arrange(total, ncol = 1)

# plots with legend
plot1_legend = ggplot(sumtab_rcgclm, aes(corr, autocorr, fill= long)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank())

plot2_legend = ggplot(sumtab_rcclpm, aes(corr, autocorr, fill= total)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank())

# function to extract legend from plot
get_only_legend = function(plot) {
  plot_table = ggplot_gtable(ggplot_build(plot))
  legend_plot = which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend = plot_table$grobs[[legend_plot]]
  return(legend)
}

# extracting legend
legend1 = get_only_legend(plot1_legend)
legend2 = get_only_legend(plot2_legend)

# final combined plot with a shared legend
grid.arrange(combined_plot1, legend1, nrow = 2, heights = c(10, 1))
grid.arrange(combined_plot2, legend2, nrow = 2, heights = c(10, 1))


