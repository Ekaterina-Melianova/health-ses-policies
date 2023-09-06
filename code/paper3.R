

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
LongToWide = function(dat, hlm = F){
  
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
  mutate(time = year - (min(year)-1))
  
  if (hlm == F){
    out = out %>% tidyr::pivot_wider(id_cols = all_of(c('lsoa11', 'LAD21CD')),
                     names_from = time, 
                     values_from = all_of(c('HE', 'as')),
                     names_sep = '')
  }

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


## data
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df_lv.RDS')
df_hlm = df_lv %>% select(lsoa11, LAD21CD, starts_with(c('as', 'HE'))) %>%
  pivot_longer(cols = starts_with(c('HE', 'as')),
               names_to = c(".value", "time"),
               names_pattern = "(\\w+)(\\d+)") 
df_hlm$time = as.numeric(df_hlm$time)

## function to run through each combination of parameters
simulate_models = function(model_fit_type = NULL,
                           max_samples = 100,
                           max_params = n_params,
                           data_base = df_lv,
                           class = 'sem'){
  

  simfit = list()
  for (i in 1:max_params){
    
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
    
    
    if (!model_fit_type == 'hlm'){
      
      wdf_list = lapply(df_list, LongToWide)
      
      syntax_sim = RC_GCLM_syntax(model = model_fit_type,
                                  endogeneous = c('HE', 'as'),
                                  control = NULL)
      fit_sim = sem(syntax_sim,
                    data = data_base,
                    estimator = "mlr",
                    cluster = 'LAD21CD',
                    orthogonal = T)
      

   
    
    } else {
      
      wdf_list = lapply(df_list, function(d) LongToWide(d, hlm = T))
      
      fit_sim = function(data){
        model = lme4::lmer(HE ~ as + time + (1|LAD21CD) + (1|lsoa11), data = data)
        out_list = list(coef = summary(model)[["coefficients"]][,'Estimate'],
                        se = summary(model)[["coefficients"]][,'Std. Error'],
                        converged = TRUE)
        return(out_list)
      }

    }
    
    simfit[[i]] = sim(seed = 12345,
                      multicore = TRUE,
                      numProc = 12,
                      rawData = wdf_list,
                      model = fit_sim,
                      silent =T) 
    
    gc()
    print(i)

  }
  
  return(simfit)
}

## applying the simulation function
simfit_rcclpm = simulate_models(model_fit_type = 'reclpm',
                                max_params = 5,
                                max_samples = 5)
simfit_rcgclm = simulate_models(model_fit_type = 'regclm',
                                max_params = 5,
                                max_samples = 5)
simfit_hlm = simulate_models(model_fit_type = 'hlm',
                             max_params = 20,
                             max_samples = 5)

## naive estimates

# rcclpm
syntax_rcclpm = RC_GCLM_syntax(model = 'reclpm',
                               endogeneous = c('HE', 'as'),
                               control = NULL)
naive_rcclpm = sem(syntax_rcclpm,
                   data = df_lv,
                   estimator = "mlr",
                   cluster = 'LAD21CD',
                   orthogonal = T)
naive_rcclpm_sum = tidy(naive_rcclpm) %>% filter(label %in% 'd_HEas') %>%
  slice(1) %>% select(label, estimate, std.error) %>%
  mutate(label = ifelse(label == 'd_HEas', 'total', ''))

# rcgclm
syntax_rcgclm = RC_GCLM_syntax(model = 'regclm',
                               endogeneous = c('HE', 'as'),
                               control = NULL)
naive_rcgclm = sem(syntax_rcgclm,
                   data = df_lv,
                   estimator = "mlr",
                   cluster = 'LAD21CD',
                   orthogonal = T)
naive_rcgclm_sum = tidy(naive_rcgclm) %>% filter(label %in% c('b_HEas', 'd_HEas')) %>%
  slice(1:2) %>% select(label, estimate, std.error) %>%
  mutate(label = ifelse(label == 'b_HEas', 'long', 'short'))

# hlm
naive_hlm = lmer(HE ~ as + time + (1|LAD21CD) + (1|lsoa11), data = df_hlm)
naive_hlm_sum = as.data.frame(coef(summary(naive_hlm))) %>%
  select(estimate = Estimate , std.error = `Std. Error`) %>%
  slice(2)  %>% mutate(label = 'total')



## function to extract parameters for plotting
extract_heatmap_data = function(lst_data,
                                params,
                                param_name,
                                param_comb_names = unique(csv_files_unique)[1:5],
                                naive_model = naive_rcgclm_sum){
  
  names(lst_data) = param_comb_names
  extract_params = function(lst,
                            params2 = params){
    sumtab = summaryParam(lst)
    sumtab = sumtab[params2,]
    return(sumtab)
  }
  
  sumtab_list = list()
  sumtab_list = lapply(lst_data, extract_params)
  
  sumtab_df = do.call(rbind.data.frame, sumtab_list)
  sumtab_df = cbind(sumtab_df, vec_names = rownames(sumtab_df))
  sumtab_df$vec_names = gsub("df_", "", sumtab_df$vec_names)
  sumtab_df %<>%
    separate(vec_names, into = c("corr", "autocorr"), sep = ".b|.d|_") %>%
    mutate(label = gsub(".*?\\.([a-zA-Z_]+) <- (.*)", "\\1 <- \\2", 
                              rownames(sumtab_df))) 
  sumtab_df$label = setNames(param_name,params)[sumtab_df$label]
  sumtab_df$corr = as.numeric(sumtab_df$corr)
  sumtab_df$autocorr = as.numeric(sumtab_df$autocorr)
  rownames(sumtab_df) = NULL
  
  sumtab_df$label = ifelse(is.na(sumtab_df$label), param_name, sumtab_df$label)
  
  ## adding the naive model parameters and compute bias
  sumtab_df %<>% left_join(naive_model, by = 'label') %>%
    mutate(bias = (estimate - `Estimate Average`)/`Estimate Average`)%>%
    mutate_if(is.numeric, ~ round(., 3))

  return(sumtab_df)
}

## applying the function
sumtab_rcclpm = extract_heatmap_data(lst_data = simfit_rcclpm,
                                     params = c("d_HEas <- (e_HE3~e_as2)"),
                                     param_name = 'total',
                                     naive_model = naive_rcclpm_sum)
sumtab_rcgclm = extract_heatmap_data(lst_data = simfit_rcgclm,
                                     params = c('b_HEas <- (HE2~as1)',
                                                'd_HEas <- (HE2~e_as1)'),
                                     param_name = c('long', 'short'),
                                     naive_model = naive_rcgclm_sum)
sumtab_hlm = extract_heatmap_data(lst_data = simfit_hlm,
                                  params = c('as'),
                                  param_name = c('total'),
                                  naive_model = naive_hlm_sum,
                                  param_comb_names = unique(csv_files_unique)[1:20])

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


