# Paper 1.R

library(readxl)
library(plyr); library(dplyr)
library(magrittr)
library(imputeTS)
library(panelr)
library(IMD)
library(glue)
library(tidyverse)
library(lavaan)
library(semTable)
library(viridis)
library(ggplot2)
library(data.table)
library(tictoc)
library(doParallel)
library(officer)

options(max.print=1900)

## setting directory
USERNAME = Sys.getenv("USERNAME")
DIR = '/YandexDisk/PhD Research/health-ses-policies2'
path = paste0('C:/Users/', USERNAME, DIR)
setwd(paste0(path, '/data'))
source(paste0(path, '/code/functions.R'))

# pre-processing

# loading the data
df = readRDS('df.rds')
children_data = read.csv('spending_data_ch.csv')

# remove suffix '_total' from names
names(children_data) = gsub('_total', '', names(children_data))
policy_names_ch = c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s')

# joining with the main df
df = df %>% filter(year > 2013) %>%
  dplyr::left_join(children_data %>% 
                        dplyr::select(LAD21CD,year, all_of(policy_names_ch)), by = c('LAD21CD', 'year'))

## collapse some categories
df %<>%
  dplyr::mutate(ca = c2s, 
                cb = c6s + c8s,
                cc = c1s + c3s + c4s + c5s + c7s,
                ot = law_order + infrastructure + env + social_care_adult
                )

policy_names_ch = c(policy_names_ch,
                    'ca', 'cb', 'cc')

# in prices of 2020
df %<>%
  dplyr::mutate(across(all_of(policy_names_ch), ~ . /def*100 ))

policy_names_ch = c(policy_names_ch,'ot',
                    policy_names_6[-2])

# replace negative values with 0
df %<>%
  dplyr::mutate(across(all_of(policy_names_ch), ~ ifelse(. < 1, 1, .)))

#test = df %>% filter(LAD21CD == 'E09000008') %>% 
#  select(c('name', 'year', 'LAD21CD', 
#           paste0(policy_names_ch))) %>% distinct()


# outliers
df = df %>% filter(!LAD21CD %in% c('E06000039', 'E08000028'))

##
dep_vec = c('inc_dep', 
            'empl_dep',
            'edu_dep',
            'crime_dep',
            'housing_dep',
            'envir_dep',
            'yinc_dep',
            'oinc_dep',
            'lsoa_ses_score')

df = df %>% group_by(LAD21CD, year) %>%
  dplyr::mutate(pop_prop = pop_census11/sum(pop_census11)) %>%
  mutate_at(vars(all_of(dep_vec)), list(~ sum(. * pop_prop))) %>%
  select(lsoa11,
         year, 
         LAD21CD,
         pop_prop,
         all_of(dep_vec),
         everything()) %>% ungroup()

# 1 - wealthy
specify_probs = function(df, probs, vars = dep_vec) {
  for (i in seq_along(probs)) {
    for (j in seq_along(dep_vec)){
      
      quantile_val = quantile(df[[dep_vec[j]]], probs = unlist(probs[i]))
      df[[paste0(dep_vec[j], i)]] = ifelse(df[,dep_vec[j]]  <= quantile_val[1], 1,
                                           ifelse(df[,dep_vec[j]]  >= quantile_val[2], 2, 0))
    }
    
  }
  
  return(df)
}
df = specify_probs(df, probs = list(c(0.5, 0.5),
                                    c(0.4, 0.6),
                                    c(0.3, 0.7)))
#table(df$lsoa_ses_score1)
#table(df$lsoa_ses_score2)
#table(df$lsoa_ses_score3)

## a dataset for descriptive stat
df_before_scaling = df
df_before_scaling %<>%
  mutate_at(vars(all_of(health_vars)), ~ -.)

# Z-scores for health
for (i in c("antidep_rate", "est_qof_dep", "prop_ibesa")){
  df[, i] = scale(df[, i])
}

# scale for controls
for (i in control_names[!control_names %in% c('public_health_mean',
                                              'inc_mean',
                                              'rural',
                                              'London',
                                              'SD',
                                              'MD')]){
  df[, i] = scale(df[, i])
}

# a dataframe for the sensitivity analysis
#df_for_outliers = df

# normalize and log for spending
for (i in c(policy_names_ch,
            control_names[control_names %in% c('public_health_mean',
                                               'inc_mean')])){
  df[, i] = scale(log(df[, i]))
}

# flip the sign
df = df %>%
  mutate_at(vars(all_of(health_vars)), ~ -.)

test = df[df$ca < -4, c('name', 'year', 'LAD21CD', 'c2s', 'c4s')] %>%
  distinct()

# final dataset - wide format
df$time = df$time-1
#summary(df_lv)


# key vectors with names
lsoa_group = c('lsoa_ses_score1', 'lsoa_ses_score2', 'lsoa_ses_score3')
dvs = c('z_mh_rate')

ivs = c('ca', 'cb', 'cc',
        'social_care_adult', 
        'healthcare',
        'env',
        'law_order',
        'infrastructure',
        c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s'),'ot'
)
ivs_map = c('ca', 'cb', 'cc', 
            'as', 'hc', 'en', 'lo', 'fr',
            c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s'), 'ot')

df_lv_1 = lavaan_df(dv = 'z_mh_rate',
                    deprivation_cat = 'lsoa_ses_score1',
                    df = df,
                    ivs = ivs,
                    ivs_map = ivs_map,
                    max_time = 6)

df_lv_2 = lavaan_df(dv = 'z_mh_rate',
                    deprivation_cat = 'lsoa_ses_score2',
                    df = df,
                    ivs = ivs,
                    ivs_map = ivs_map,
                    max_time = 6) %>% 
  filter(lsoa_ses_score2 > 0)

df_lv_3 = lavaan_df(dv = 'z_mh_rate',
                    deprivation_cat = 'lsoa_ses_score3',
                    df = df,
                    ivs = ivs,
                    ivs_map = ivs_map,
                    max_time = 6) %>% 
  filter(lsoa_ses_score3 > 0)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------
setwd(paste0(path, '/output/paper3'))

# free models

group_free_syntax = RC_GCLM_syntax(model = 'regclm',
                                   cor = F,
                                   max_time = 6,
                                   endogeneous = c('HE', 'ca', 'cb', 'cc', 'ot', 'hc'),
                                   reverse = c('HE', 'ca', 'cb', 'cc', 'ot', 'hc'),
                                   control = control_names[-c(3,12)],
                                   multiple  = T)

tic()
cluster = makeCluster(10) 
registerDoParallel(cluster)

free_models = foreach(dv = dvs, .combine='c',
                      .packages = c('lavaan', 'tidyverse',
                                    'dplyr', 'tidyr',
                                    'magrittr')) %dopar% {
                                      
                                      # Data preparation 
                                      df_lv_1 = lavaan_df(dv = 'z_mh_rate',
                                                          deprivation_cat = 'lsoa_ses_score1',
                                                          df = df,
                                                          ivs = ivs,
                                                          ivs_map = ivs_map,
                                                          max_time = 6)
                                      
                                      df_lv_2 = lavaan_df(dv = 'z_mh_rate',
                                                          deprivation_cat = 'lsoa_ses_score2',
                                                          df = df,
                                                          ivs = ivs,
                                                          ivs_map = ivs_map,
                                                          max_time = 6) %>% 
                                        filter(lsoa_ses_score2 > 0)
                                      
                                      df_lv_3 = lavaan_df(dv = 'z_mh_rate',
                                                          deprivation_cat = 'lsoa_ses_score3',
                                                          df = df,
                                                          ivs = ivs,
                                                          ivs_map = ivs_map,
                                                          max_time = 6) %>% 
                                        filter(lsoa_ses_score3 > 0)
                                      
                                      # Modeling for each deprivation category
                                      group_free_fit_dep_1 = sem(group_free_syntax, data = df_lv_1, estimator = "mlr", orthogonal = T, cluster = 'LAD21CD', group = 'lsoa_ses_score1')
                                      gc()
                                      group_free_fit_dep_2 = sem(group_free_syntax, data = df_lv_2, estimator = "mlr", orthogonal = T, cluster = 'LAD21CD', group = 'lsoa_ses_score2')
                                      gc()
                                      group_free_fit_dep_3 = sem(group_free_syntax, data = df_lv_3, estimator = "mlr", orthogonal = T, cluster = 'LAD21CD', group = 'lsoa_ses_score3')
                                      gc()
                                      
                                      # Return a list of models for the current dv
                                      list(group_free_fit_dep_1, group_free_fit_dep_2, group_free_fit_dep_3)
                                    }

stopCluster(cluster)
toc()
saveRDS(free_models, 'free_models_ch.rds')

# constrained models

equality_params = c('b_HEca1,b_HEca2',
                    'b_HEcb1,b_HEcb2',
                    'b_HEcc1,b_HEcc2',
                    
                    'd_HEca1,d_HEca2',
                    'd_HEcb1,d_HEcb2',
                    'd_HEcc1,d_HEcc2')

syntax = list()
for (i in seq_along(equality_params)){
  syntax[[i]] = RC_GCLM_syntax(model = 'regclm',
                               cor = F,
                               max_time = 6,
                               endogeneous = c('HE', 'ca', 'cb', 'cc', 'ot', 'hc'),
                               reverse = c('HE', 'ca', 'cb', 'cc', 'ot', 'hc'),
                               control = control_names[-c(3,12)],
                               multiple  = T,
                               group_equality = equality_params[i]
  )
}


groupSEM = function(comb){
  
  dvar = comb[[1]]
  lsoa = comb[[2]]
  synt = comb[[3]]
  synt_n = comb[[4]]
  
  list_result = list()
  
  df_lv = lavaan_df(dv = dvar,
                    deprivation_cat = lsoa,
                    df = df,
                    ivs = ivs,
                    ivs_map = ivs_map,
                    max_time = 6)
  df_lv %<>% filter((!!as.name(lsoa)) > 0)
  list_result[[1]] = sem(synt,
                         data = df_lv,
                         estimator = "mlr",
                         orthogonal = T,
                         cluster = 'LAD21CD',
                         group = lsoa)
  gc()
  
  # save as .rds
  #saveRDS(list_result,
  #               paste0('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/',
  #                      dvar, '_', lsoa, '_', synt_n, '.rds'))
  
  return(list_result)
  
}

# only hospital admissions
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
        paste0(dvs_temp, '_', 'constrained_models_ch', '.rds'))

# read
tic()
free_models <- readRDS("free_models_ch.rds")
z_mh_rate_constrained_models_ch <- readRDS("z_mh_rate_constrained_models_ch.rds")
toc()
gc()

# # ----------------------------------------------------------------------

# Output

# 1. Sample Description

stationary = c(control_names[-c(3,12)], 'lsoa_ses_score1')
nonstationary = c('z_mh_rate', 
                  'ca', 'cb', 'cc', 'ot', 
                  'healthcare')
vars_used = c(nonstationary, stationary)

df_before_scaling$imd_year = case_when(df_before_scaling$year == 2014 &
                                         df_before_scaling$lsoa_ses_score1 == 1 ~ '_1',
                                       df_before_scaling$year == 2014 &
                                         df_before_scaling$lsoa_ses_score1 == 2 ~ '_3',
                                       df_before_scaling$year == 2019 &
                                         df_before_scaling$lsoa_ses_score1 == 1 ~ '_2',
                                       df_before_scaling$year == 2019 &
                                         df_before_scaling$lsoa_ses_score1 == 2 ~ '_4',
                                       TRUE ~ 'other')
table(df_before_scaling$imd_year)

# Z-scores for health
df_before_scalingZ = df_before_scaling
#for (i in c("antidep_rate", "est_qof_dep", "prop_ibesa")){
#  df_before_scalingZ[, i] = scale(df_before_scaling[, i])
#}

sumstat_dep1 = summarize_data(dat = df_before_scalingZ %>% filter(!imd_year == 'other'),
                              .stationary = stationary[-11],
                              group = 'imd_year',
                              rownames = nm_out_ch,
                              quant = F,
                              stat = list('Mean' = mean,
                                          'SD' = sd#,
                                          #'Q25' = function(x) quantile(x, probs = 0.25),
                                          #'Q75' = function(x) quantile(x, probs = 0.75)
                              )
)
for (i in c(2, 4, 10)) {
  sumstat_dep1 = tibble::add_row(sumstat_dep1, .before = i)
}
sumstat_dep1 = tibble::add_column(sumstat_dep1, "Var1" = '', .after = 5)
sumstat_dep1 = tibble::add_column(sumstat_dep1, "Var2" = '', .after = 10)

section_subnames = c('Health Indicators', 'Spending, £ per capita', 'Controls')
sumstat_dep1 = sumstat_dep1 %>%
  dplyr::mutate(across(1, ~replace(.x, is.na(.x), section_subnames))) %>%
  dplyr::mutate_all(~ ifelse(is.na(.), "", .)) 

#colnames(sumstat_dep1) = c('', 'Top 50%', '', '', 'Bottom 50%', '', '')
colnames(sumstat_dep1) = c('',
                           '2014 Top 50%', '', '2019 Top 50%', '',
                           'SD or Percentage Change Top 50%', 
                           '2014 Bottom 50%', '', '2019 Bottom 50%', '',
                           'SD or Percentage Change Top 50%')


# Other Outputs

section_names = c('Autoregressive Effects',
                  'Cross-Lagged Effects: Mental Health -> Spending',
                  'Cross-Lagged Effects: Spending -> Mental Health',
                  'Fit Measures (Scaled)')

# main summary function
mgc_modelling_outputs = function(lst_constrained){
  # 3. Regression Table
  group_free_tab = CoefsExtract(models = c('group_free_fit_dep_1',
                                           'group_free_fit_dep_2',
                                           'group_free_fit_dep_3'),
                                end = paste0(c('HE', 'ca', 'cb', 'cc', 'ot', 'hc'), '1'),
                                impulses = paste0(c('e_HE', 'e_ca', 'e_cb', 'e_cc', 'e_ot', 'e_hc'), '1')) %>%
    dplyr::select(everything(),type = type_1, -type_2)
  gc()
  
  fit_measures_gf = cbind.data.frame(est.std.x_long_2 = fitmeasures(group_free_fit_dep_1, measures),
                                     est.std.y_long_2 = fitmeasures(group_free_fit_dep_2, measures),
                                     est.std_long_2 = fitmeasures(group_free_fit_dep_3, measures),
                                     
                                     est.std.x_short_2 = fitmeasures(group_free_fit_dep_1, measures),
                                     est.std.y_short_2 = fitmeasures(group_free_fit_dep_2, measures),
                                     est.std_short_2 = fitmeasures(group_free_fit_dep_3, measures))
  fit_measures_gf[] = lapply(fit_measures_gf, sprintf, fmt = "%.3f")
  gf_models_coefs = TableEffects(dat = group_free_tab,
                                 .end_new = end_new_ch,
                                 fit_measures = fit_measures_gf,
                                 param_range = 17:29,
                                 section_name_rows = c(1, 7, 12, 17),
                                 patterns = c("~HE|HE~#",
                                              "~ca|ca~#",
                                              "~cb|cb~#",
                                              "~cc|cc~#",
                                              "~ot|ot~#",
                                              "~hc|hc~#")
  )
  gf_models_coefs[9:13,1] = end_new_ch[2:6]
  
  # long-run table
  gf_models_coefs_long = gf_models_coefs %>%
    dplyr::select(id,
                  contains('long')) %>%
    dplyr::select(id,
                  contains('.x'),
                  contains('.y'),
                  everything())
  col_gf = c('', 
             'Model 1', '',
             'Model 2', '',
             'Model 3', '')
  gf_models_coefs_long = SubHead(CiSplit(gf_models_coefs_long),
                                 sub_head = c('Top', 'Bottom'),
                                 sub_head_add = c('50%', '50%', 
                                                  '40%', '40%',
                                                  '30%', '30%'),
                                 n = 3,
                                 colnames = col_gf)
  
  # short-run table
  gf_models_coefs_short = gf_models_coefs %>%
    dplyr::select(id,
                  contains('short')) %>%
    dplyr::select(id,
                  contains('.x'),
                  contains('.y'),
                  everything())
  gf_models_coefs_short = SubHead(CiSplit(gf_models_coefs_short),
                                  sub_head = c('Top', 'Bottom'),
                                  sub_head_add = c('50%', '50%', 
                                                   '40%', '40%',
                                                   '30%', '30%'),
                                  n = 3,
                                  colnames = col_gf)
  
  # 4. Anova Results
  anova_table = function(list_range, 
                         free_model,
                         constrained = lst_constrained){
    
    # if error in the following return NA
    anova_list = lapply(constrained[list_range], function(model) {
      tryCatch({
        as.data.frame(anova(free_model, model))
      }, error = function(e) {
        error_mod = data.frame('Df' = c(NA,NA),
                               'AIC' = c(NA,NA),
                               'BIC' = c(NA,NA),
                               'Chisq' = c(NA,NA),
                               'Chisq diff' = c(NA,NA),
                               'Df diff' = c(NA,NA),
                               'Pr(>Chisq)' =c(NA,NA))
        colnames(error_mod) = c('Df', 'AIC', 'BIC', 'Chisq', 'Chisq diff', 'Df diff', 'Pr(>Chisq)')
        rownames(error_mod) = c('free_model', 'model')
        return(error_mod)
      })
    })
    
    out = rbind.data.frame(anova_list[[1]][1,],
                           do.call(rbind, lapply(anova_list, function(df) df[2, ])))
    out = out[-1,]
    out[] = lapply(out, sprintf, fmt = "%.3f")
    out$`Constrained Spending` = c(rep(descriptives_names_ch[2:4], 2))
    out$Dynamic = c(#'Free Model', 
      rep('Long-Run', 3),
      rep('Short-Run', 3))
    out %<>% dplyr::select(Dynamic, `Constrained Spending`,
                           `Chisq diff`, `Pr(>Chisq)`)%>%
      mutate(Sig = sig_fun(`Pr(>Chisq)`))
    out[out == 'NA'|out == 'l'] = ''
    rownames(out) = NULL
    
    return(out)
  }
  
  anova_1 = anova_table(list_range = 1:6,
                        free_model = group_free_fit_dep_1)
  anova_2 = anova_table(list_range = 7:12,
                        free_model = group_free_fit_dep_2)
  anova_3 = anova_table(list_range = 13:18,
                        free_model = group_free_fit_dep_3)
  anova_all = anova_1 %>%
    left_join(anova_2, by = c('Constrained Spending', 'Dynamic'))%>%
    left_join(anova_3, by = c('Constrained Spending', 'Dynamic'))
  
  
  # 5. Substantive
  
  ratio_policy = c()
  df_before_scaling = as.data.frame(df_before_scaling)
  for (i in policy_names_6_ch){
    ratio_policy = c(ratio_policy, 10/((exp(sd(log(df_before_scaling[,i])))-1)*100)
    )
    
  }
  id_ratio_policy = c("HE~ca", "HE~cb", "HE~bc", "HE~ot","HE~hc")
  ratio_df = cbind.data.frame('id' = id_ratio_policy, 'ratio' = ratio_policy)
  
  # transforming
  pct_coefs = CoefsExtract(models = c('group_free_fit_dep_1',
                                      'group_free_fit_dep_2',
                                      'group_free_fit_dep_3'),
                           standardized = F,
                           controls = NULL,
                           df_transform = ratio_df,
                           end = paste0(c('HE', 'ca', 'cb', 'cc', 'ot', 'hc'), '1'),
                           impulses = paste0(c('e_HE', 'e_ca', 'e_cb', 'e_cc', 'e_ot', 'e_hc'), '1')) %>% 
    filter(type_1 == 'c_policy') %>%
    dplyr::select(everything(),type = type_1, -type_2)
  
  
  pct_coefs$id = c('Children Social Care 1',
                   'Children Social Care 2',
                   'Children Social Care 3',
                   'Other Spending',
                   #'Adult Social Care',
                   'Healthcare')
  gf_pct_coefs = TableEffects(dat = pct_coefs,
                              fit_measures = fit_measures_gf,
                              param_range = 6:18,
                              subsections = F)
  
  # long-run table
  gf_pct_coefs_long = gf_pct_coefs %>%
    dplyr::select(id,
                  contains('long')) %>%
    dplyr::select(id,
                  contains('.x'),
                  contains('.y'),
                  everything())
  
  colnames(gf_pct_coefs_long)[1] = 'Constrained Spending'
  
  anova_long = anova_all %>% filter(Dynamic == 'Long-Run') %>%
    dplyr::select(c(`Constrained Spending`,
                    grep('Sig', colnames(anova_all)),
                    grep('Chisq diff', colnames(anova_all))))
  
  gf_pct_coefs_long = gf_pct_coefs_long %>% left_join(anova_long, by = 'Constrained Spending')
  
  #gf_pct_coefs_long = merge(gf_pct_coefs_long, anova_long, by = 'Constrained Spending',
  #           no.dups = F, all = T, sort = F)
  
  gf_pct_coefs_long = gf_pct_coefs_long[,c(1:3,11,8,
                                           4:5,12,9,
                                           6:7,13,10)]%>%
    add_row('Constrained Spending' = 'Long-Run', .before = 1)
  
  gf_models_coefs_long = SubHead(CiSplit(gf_pct_coefs_long),
                                 sub_head = c('Top', 'Bottom','Chisq diff', 'Chisq Sig'),
                                 sub_head_add = c('50%', '50%', '', '',
                                                  '40%', '40%', '', '',
                                                  '30%', '30%', '', ''),
                                 n = 3,
                                 colnames = c('', 
                                              'Model 1', '', '', '',
                                              'Model 2', '', '', '',
                                              'Model 3', '', '', '')) 
  
  # short-run table
  gf_pct_coefs_short = gf_pct_coefs %>%
    dplyr::select(id,
                  contains('short')) %>%
    dplyr::select(id,
                  contains('.x'),
                  contains('.y'),
                  everything())
  
  colnames(gf_pct_coefs_short)[1] = 'Constrained Spending'
  
  anova_short = anova_all %>% filter(Dynamic == 'Short-Run') %>%
    dplyr::select(c(`Constrained Spending`,
                    grep('Sig', colnames(anova_all)),
                    grep('Chisq diff', colnames(anova_all))))
  
  gf_pct_coefs_short = gf_pct_coefs_short %>% left_join(anova_short, by = 'Constrained Spending')
  
  gf_pct_coefs_short = gf_pct_coefs_short[,c(1:3,11,8,
                                             4:5,12,9,
                                             6:7,13,10)]%>%
    add_row('Constrained Spending' = 'Short-Run', .before = 1)%>%
    add_row('Constrained Spending' = 'Fit Measures (Scaled)', .before = 8)
  
  gf_models_coefs_short = SubHead(CiSplit(gf_pct_coefs_short),
                                  sub_head = c('Top', 'Bottom','Chisq diff', 'Chisq Sig'),
                                  sub_head_add = c('50%', '50%', '', '',
                                                   '40%', '40%', '', '',
                                                   '30%', '30%', '', ''),
                                  n = 3,
                                  colnames = c('', 
                                               'Model 1', '', '', '',
                                               'Model 2', '', '', '',
                                               'Model 3', '', '', ''))
  
  long_short_tab = rbind(gf_models_coefs_long[1:12,],
                         gf_models_coefs_short[-1,])
  
  
  
  return(list(as.data.frame(long_short_tab),
              as.data.frame(gf_models_coefs_long),
              as.data.frame(gf_models_coefs_short),
              as.data.frame(anova_all)))
  
}

## Out
# treating empty colnames in officer
replace_empty_colnames = function(df) {
  colnames = names(df)
  empty_cols = colnames[colnames == ""]
  if (length(empty_cols) > 0) {
    colnames[colnames == ""] = "RRR"
    colnames(df) = colnames
  }
  return(df)
}

# Combine every 3 sublists into one and add it to the nested list
nested_free_models <- list()
for (i in seq(1, length(free_models), by = 3)) {
  nested_free_models[[length(nested_free_models) + 1]] <- 
    list(free_models[[i]], free_models[[i+1]], free_models[[i+2]])
}

#
list_names = c('z_mh_rate_constrained_models_ch')
tic()
for (i in 1:length(dvs)){
  
  assign("group_free_fit_dep_1", nested_free_models[[i]][[1]], envir = .GlobalEnv)
  assign("group_free_fit_dep_2", nested_free_models[[i]][[2]], envir = .GlobalEnv)
  assign("group_free_fit_dep_3", nested_free_models[[i]][[3]], envir = .GlobalEnv)
  
  out_dvs = mgc_modelling_outputs(lst_constrained = readRDS(paste0(list_names[i], ".rds")))
  
  # Create a Word document
  doc = read_docx()
  out_dvs = lapply(out_dvs, replace_empty_colnames)
  
  # Loop over the list of data frames and add them to the document
  for (j in seq_along(out_dvs)) {
    doc = doc %>% 
      body_add_table(out_dvs[[j]])    
    # Add a page break after each table
    if (j < length(out_dvs)) {
      doc = doc %>% 
        body_add_break()
    }
  }
  
  # Save the document
  target_name = paste0("paper3_tabs_", dvs[i], '.docx')
  print(doc,
        target = target_name)
  
  print(i)
  
}
toc()
gc()
beepr::beep()


# # ----------------------------------------------------------------------

library(ggpubr)

lsoa_group = c('lsoa_ses_score1', 'lsoa_ses_score2', 'lsoa_ses_score3')
vars_original_names = c('z_mh_rate',
                        'ca', 'cb', 'cc')
all_vars = vars_original_names

df_plot = df_before_scaling %>% select(year,
                                       lsoa11,
                                       LAD21CD,
                                       all_of(c(all_vars,
                                                lsoa_group))) 

# Mental indices comparison by lsoa_dep

for (i in lsoa_group) {
  
  num = as.numeric(unlist(str_extract_all(i, "\\d+")))
  if (num == 1){
    df_plot <- df_plot %>%
      mutate(!!sym(i) := factor(!!sym(i),
                                levels = 1:2,
                                labels = c('Top 50%',
                                           'Bottom 50%')))
    
  } else if(num == 2) {
    df_plot <- df_plot %>%
      mutate(!!sym(i) := factor(!!sym(i),
                                levels = 1:2,
                                labels = c('Top 40%',
                                           'Bottom 40%')))
  }else if(num == 3) {
    df_plot <- df_plot %>%
      mutate(!!sym(i) := factor(!!sym(i),
                                levels = 1:2,
                                labels = c('Top 30%',
                                           'Bottom 30%')))
  }
  
  
}

list_panel = list()
n = 0
for (j in lsoa_group){
  n = n + 1
  list_panel[[n]] = df_plot %>% 
    group_by(year, !!sym(j)) %>%
    dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = z_mh_rate:cc) %>%
    ungroup() %>% dplyr::rename(dep_colour = !!sym(j)) %>%
    mutate(dep_cat = sub('[[:digit:]]+', '', sym(j)))%>%
    mutate(dep_linetype = j)
}

panel_df = do.call(rbind.data.frame, list_panel) %>% 
  filter(!is.na(dep_colour))

panel_df$name = factor(panel_df$name,
                       levels = vars_original_names,
                       labels = vars_original_names)
#panel_df %<>% filter(name %in% 'SAMHI')

cols = c('Top 50%' = "#4A90E2", 
         'Top 40%' = "#0000CC",
         'Top 30%' = "#000055",
         'Bottom 50%' = "#C9302C",
         'Bottom 40%' = "#8A2525",
         'Bottom 30%' = "#550000")

cols = c('Top 50%' = "#4A90E2", 
         'Top 40%' = "#4A90E2",
         'Top 30%' = "#4A90E2",
         'Bottom 50%' = "#8A2525",
         'Bottom 40%' = "#8A2525",
         'Bottom 30%' = "#8A2525")

linetypes = c('Top 50%' = "dotted", 
              'Top 40%' = "dashed",
              'Top 30%' = "solid",
              'Bottom 50%' = "dotted",
              'Bottom 40%' = "dashed",
              'Bottom 30%' = "solid")

# Spending comparison 
panel_df$dep_colour = as.factor(panel_df$dep_colour)
panel_df  %>% 
  filter(name %in% c('z_mh_rate', 'ca', 'cb', 'cc')) %>%
  ggplot(aes(year, value, colour = dep_colour, linetype = dep_colour)) +
  scale_x_continuous(name = NULL, 
                     breaks = 2014:2019)+ 
  scale_y_continuous(name = 'Spending, £ per capita'#, limits = c(100, 1400)
  ) + 
  theme(axis.text = element_text(size = 24)) +
  facet_wrap(~ name, scales = 'free')   +
  geom_smooth(method = loess,
              linewidth = 0.5,
              se = T,
              formula = y ~ x,
              level = 0.9,
              fill = 'lightgrey')+
  theme_pubclean() +
  # add lines
  #geom_line(aes(group = LAD21CD), color = "lightblue") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = linetypes)  + 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom')

###
all_vars = c('z_mh_rate',
             'ca', 'cb', 'cc')
panel_df = df_before_scaling %>%
  group_by(LAD21CD, year) %>%
  dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE)))
panel = panel_data(panel_df, id = LAD21CD, wave = year)

list_plots = list()
for (i in 1:length(all_vars)){
  list_plots[[i]] = panel  %>% 
    ggplot(aes(year, !!sym(all_vars[i]))) +
    scale_x_continuous(name = NULL, 
                       breaks = 2013:2019)+ 
    scale_y_continuous(name = 'Spending, £ per capita')+
    geom_line(aes(group = LAD21CD), color = "lightblue") +
    geom_smooth(method = loess, se = F, fullrange = T, color="darkred") +
    theme_pubclean() + 
    theme(axis.text = element_text(size = 24)) + 
    theme(axis.title.y = element_text(size = 24)) 
}

# policies
ggarrange(list_plots[[1]],
          list_plots[[2]],
          list_plots[[3]],
          list_plots[[4]],
          #labels = c('Adult Social Care',
          #           'Children Social Care',
          #           'Healthcare'          ),
          ncol = 4, nrow = 1,
          font.label = list(size = 30), align ='hv') +
  ylab("Common Y-Axis Label")




