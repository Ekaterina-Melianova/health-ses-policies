library(readxl)
library(plyr); library(dplyr)
library(magrittr)
library(imputeTS)
library(panelr)
library(IMD)
library(glue)
library(tidyverse)
library(lavaan)
library(data.table)
library(tidyr)
library(broom)

library(parallel)
library(doParallel)
library(tictoc)
library(simsem)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/sim2.R')

## LOAD DATASET
df.full = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')

## PREPROCESSING
df.full = df.full %>% ungroup()
df.full$social_care_adult = df.full$pop * df.full$social_care_adult
df.full$law_order = df.full$pop * df.full$law_order


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
#wd = "C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/test4"
setwd(wd)

num_cores = 12

cl = makeCluster(num_cores)
registerDoParallel(cl)


generate_datasets = function(comb) {
  
  corr_sh = comb[[1]]
  autocorr_s = comb[[2]]
  i = comb[[3]]
  
  max_attempts = 40
  attempts = 0
  success = FALSE
  
  while (!success && attempts < max_attempts) {
    attempts = attempts + 1
    
    tryCatch({
      out = simulateDataset(df = df.full,
                            name_spending = 'law_order',
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
beepr::beep()

## data
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df_lv.RDS')
df_hlm = df_lv %>% dplyr::select(lsoa11, LAD21CD,
                                 colnames(df_lv)[grepl('as|HE', colnames(df_lv))]) %>%
  pivot_longer(cols = starts_with(c('HE', 'as')),
               names_to = c(".value", "time"),
               names_pattern = "(\\w+)(\\d+)") 
df_hlm$time = as.numeric(df_hlm$time)

## subset for hlm
temp = df_lv %>% group_by(LAD21CD) %>% slice_sample(prop = 0.2)
lsoa_sample = temp %>% group_by(LAD21CD, lsoa11) %>% 
  dplyr::summarise(n = n()) %>% ungroup()
#table(lsoa_sample$LAD21CD)

# to wide format
LongToWide = function(dat,
                      spending_name = 'as',
                      hlm = F,
                      sample = lsoa_sample$lsoa11){
  
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
                                c('HE', spending_name)))) %>%
  mutate(time = year - (min(year)-1))
  
  if (hlm == F){
    out = out %>% tidyr::pivot_wider(id_cols = all_of(c('lsoa11', 'LAD21CD')),
                     names_from = time, 
                     values_from = all_of(c('HE', spending_name)),
                     names_sep = '') 
  } else{
    out = out %>% 
      dplyr::filter(lsoa11 %in% sample)
  }
  
  out = as.data.frame(out)

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
simulate_models = function(model_fit_type,
                           max_samples = NULL,
                           max_params = n_params,
                           data_base = df_lv){
  

  simfit = list()
  for (i in 1:max_params){
    
    df_list = vector("list", length = 100)
    
    csv_files_n = unique(csv_files_unique)[i]
    csv_files_n = csv_files[grepl(csv_files_n, csv_files)]
    
    # loop through each .csv file
    for (file in csv_files_n) {
      file_name = basename(file)
      last_number = as.numeric(sub(".*_(\\d+)\\.Rdata", "\\1", file_name))
      load(file)
      df_list[last_number] = list(out)
    }
    
    # max_samples
    if (is.null(max_samples)){
      df_list =  Filter(Negate(is.null), df_list)
    }else{
      df_list =  Filter(Negate(is.null), df_list)[1:max_samples]
    }
    
    if (!model_fit_type == 'hlm'){
      
      wdf_list = lapply(df_list, LongToWide, sample = NULL)
      
      syntax_sim = RC_GCLM_syntax(model = model_fit_type,
                                  endogeneous = c('HE', 'as'),
                                  control = NULL)
      fit_sim = sem(syntax_sim,
                    data = data_base,
                    estimator = "mlr",
                    cluster = 'LAD21CD',
                    orthogonal = T)
      multicore_logical = T
      
      }else{
      
      wdf_list = lapply(df_list, function(dat) LongToWide(dat, hlm = T))
      df_list = NULL
      
      fit_sim = function(dat){
        
        model = lme4::lmer(HE ~ as + time + (1|LAD21CD) + (1|lsoa11),
                           data = dat,
                           control = lmerControl(calc.derivs = FALSE))
        out_list = list(coef = summary(model)[["coefficients"]][,'Estimate'],
                        se = summary(model)[["coefficients"]][,'Std. Error'],
                        converged = TRUE)
        
        return(out_list)
      }
      
      multicore_logical = F

    }
    
    tic()
    # simulation
    simfit[[i]] = sim(seed = 12345,
                      multicore = multicore_logical,
                      numProc = 8,
                      rawData = wdf_list,
                      model = fit_sim,
                      silent = T) 
    print(i)
    toc()
    gc()

  }
  
  return(simfit)
}


## applying the simulation function
tic()
simfit_rcclpm = simulate_models(model_fit_type = 'reclpm')
toc()

tic()
simfit_rcgclm = simulate_models(model_fit_type = 'regclm', max_samples = 5)
toc()

tic()
simfit_hlm = simulate_models(model_fit_type = 'hlm', max_samples = 3)
toc()

tic()
simfit_hlm2 = simulate_models(model_fit_type = 'hlm', max_samples = 3)
toc()

## naive estimates

## rcclpm
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

## rcgclm
syntax_rcgclm = RC_GCLM_syntax(model = 'regclm',
                               endogeneous = c('HE', 'as'),
                               control = NULL)
naive_rcgclm = sem(syntax_rcgclm,
                   data = df_lv,
                   estimator = "mlr",
                   cluster = 'LAD21CD',
                   orthogonal = T)
naive_rcgclm_sum = tidy(naive_rcgclm) %>% filter(label %in% c('b_HElo', 'd_HElo')) %>%
  slice(1:2) %>% select(label, estimate, std.error) %>%
  mutate(label = ifelse(label == 'b_HElo', 'long', 'short'))

## hlm
naive_hlm = lmer(HE ~ as + time + (1|LAD21CD) + (1|lsoa11), 
                 data = df_hlm %>% filter(lsoa11 %in% lsoa_sample$lsoa11))
naive_hlm_sum = as.data.frame(coef(summary(naive_hlm))) %>%
  dplyr::select(estimate = Estimate , std.error = `Std. Error`) %>%
  slice(2)  %>% mutate(label = 'total')
summary(naive_hlm)

naive_hlm = lmer(HE ~ as + (1|LAD21CD), 
                 data = df_hlm %>% filter(lsoa11 %in% lsoa_sample$lsoa11))

performance::icc(naive_hlm, by_group = T)

naive_hlm_coef = coef(naive_hlm)$LAD21CD
naive_hlm_coef$LAD21CD = rownames(naive_hlm_coef)
temp_hlm = df_hlm %>% left_join(naive_hlm_coef, by = 'LAD21CD')
cor(temp_hlm$HE, temp_hlm$`(Intercept)`)



df_hlm_lad = df_hlm  %>% #filter(lsoa11 %in% lsoa_sample$lsoa11)%>%
  group_by(LAD21CD,time) %>% 
  dplyr::summarise(as_lad = mean(as),
                   #lo_lad = mean(lo),
                   var_as_lad = var(as),
                   HE_lad = mean(HE),
                   time = mean(time))
cor(df_hlm_lad$HE_lad, df_hlm_lad$as_lad)
cor(df_hlm_lad$HE_lad, df_hlm_lad$lo_lad)

cor(df_hlm_lad$as_lad[df_hlm_lad$time==1],
    df_hlm_lad$as_lad[df_hlm_lad$time==2])

cor(df_hlm$HE[df_hlm$time==4], df_hlm$as[df_hlm$time==3])

df_hlm_lsoa = df_hlm  %>% 
  group_by(lsoa11) %>% 
  dplyr::summarise(cor_lo = cor(lo, HE),
                   cor_as = cor(as, HE))
mean(df_hlm_lsoa$cor_lo)
mean(df_hlm_lsoa$cor_as)

#within_var = var_components[var_components$grp == 'LAD21CD', "vcov"]
#between_var = var_components[var_components$grp == 'Residual', "vcov"]
#between_variances <- as.data.frame(tapply(df_test_1$as, df_test_1$LAD21CD, var))

## function to extract parameters for plotting
extract_heatmap_data = function(lst_data,
                                params,
                                param_name,
                                param_comb_names = unique(csv_files_unique),
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
    mutate(`bias, %` = (estimate - `Estimate Average`)/estimate)%>%
    mutate_if(is.numeric, ~ round(., 4))

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
                                  naive_model = naive_hlm_sum)

## icc
icc_list = vector("list", length = n_params)
for (i in 1:n_params){
  csv_files_n = unique(csv_files_unique)[i]
  csv_files_n = csv_files[grepl(csv_files_n, csv_files)]
  csv_files_n = csv_files_n[1]
  load(csv_files_n)
  icc_list[i] = list(out)

}
icc_list = lapply(icc_list, function(dat) LongToWide(dat, hlm = T))
icc_list_out = list()
icc_list_out = lapply(icc_list, function(dat){
  performance::icc(lme4::lmer(as ~ 1 + (1|lsoa11), data = dat))

}) 
icc_df = do.call(rbind.data.frame, icc_list_out)
icc_df$vec_names = gsub("df_", "", unique(csv_files_unique))
icc_df %<>%
  separate(vec_names, into = c("corr", "autocorr"), sep = "_") %>%
  mutate(across(everything(), as.numeric))
sumtab_hlm = sumtab_hlm %>% left_join(icc_df)


## inspecting
sumtab_rcgclm_long = sumtab_rcgclm %>% filter(label == 'long') %>% arrange(bias)
sumtab_rcgclm_short = sumtab_rcgclm %>% filter(label == 'short') %>% arrange(bias)
summary(sumtab_rcgclm_long$bias)
summary(sumtab_rcgclm_short$bias)

lad_level = df_lv %>% group_by(LAD21CD) %>%
  dplyr::summarise(as1_lad = mean(as1),
            as2_lad = mean(as2),
            as3_lad = mean(as3),
            HE1_lad = mean(HE1),
            HE2_lad = mean(HE2),
            HE3_lad = mean(HE3))
cor(lad_level$as1_lad, lad_level$HE1_lad)
cor(lad_level$as2_lad, lad_level$HE2_lad)
cor(lad_level$as3_lad, lad_level$HE3_lad)
var(lad_level$as3_lad)

## plotting

library(gridExtra)
library(cowplot)
library(hrbrthemes)

# rcgclm 
ggplot(sumtab_rcgclm_long, aes(corr, autocorr, fill = bias)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white') +
  theme_ipsum()

ggplot(sumtab_rcgclm_short, aes(corr, autocorr, fill = bias)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white') +
  theme_ipsum()

# rcclpm
ggplot(sumtab_hlm, aes(corr, autocorr, fill = `bias, %`)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white') +
  theme_ipsum()

ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/simulated_hlm.jpeg",
       width = 20, height = 10, units = 'cm') 


# combine both plots using grid.arrange()
combined_plot1 = grid.arrange(long, short, ncol = 2)
combined_plot2 = grid.arrange(total, ncol = 1)

# plots with legend
plot1_legend = ggplot(sumtab_rcgclm, aes(corr, autocorr, fill = long)) + 
  geom_tile()+
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.04, 0.04)) +
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank())

plot2_legend = ggplot(sumtab_rcclpm, aes(corr, autocorr, fill = total)) + 
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


