# Paper 2.R

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

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
options(max.print=50)

# pre-processing

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df.rds')

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
table(df$lsoa_ses_score1)
table(df$lsoa_ses_score2)
table(df$lsoa_ses_score3)

# a dataset for descriptive stat
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
                                              'MD',
                                              'lsoa_ses_score')]){
  df[, i] = scale(df[, i])
}

# normalize and log for spending
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean',
                                               'inc_mean')])){
  df[, i] = scale(log(df[, i]))
}

# flip the sign
df = df %>%
  mutate_at(vars(all_of(health_vars)), ~ -.)
# save as rds
#saveRDS(df, 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_map.rds')

# key vectors with names
lsoa_group = c('lsoa_ses_score1', 'lsoa_ses_score2', 'lsoa_ses_score3')
dvs = c('samhi_index', 
        'prop_ibesa',
        'est_qof_dep', 
        'antidep_rate',
        'z_mh_rate')

# final dataset - wide format

dep_vec_3 = paste0(rep(dep_vec, each = 3), 1:3)
df_lv_1 = lavaan_df(dv = 'samhi_index',
                    deprivation_cat = 'lsoa_ses_score1',
                    df = df)
##df_lv_1 = as.data.frame(na.omit(df_lv_1))
#colnames(df_lv_1)
df_lv_2 = lavaan_df(dv = 'samhi_index',
                    deprivation_cat = 'lsoa_ses_score2',
                    df = df) %>% 
  filter(lsoa_ses_score2 > 0)
df_lv_3 = lavaan_df(dv = 'samhi_index',
                    deprivation_cat = 'lsoa_ses_score3',
                    df = df) %>% 
  filter(lsoa_ses_score3 > 0)
# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

# free models

tic()
cluster = makeCluster(10) 
registerDoParallel(cluster)

free_models = foreach(dv = dvs, .combine='c',
                      .packages = c('lavaan', 'tidyverse',
                                    'dplyr', 'tidyr',
                                    'magrittr')) %dopar% {
                                      
                                      # Data preparation 
                                      df_lv_1 = lavaan_df(dv = dv, deprivation_cat = 'lsoa_ses_score1', df = df)
                                      df_lv_2 = lavaan_df(dv = dv, deprivation_cat = 'lsoa_ses_score2', df = df) %>%
                                        filter(lsoa_ses_score2 > 0)
                                      df_lv_3 = lavaan_df(dv = dv, deprivation_cat = 'lsoa_ses_score3', df = df) %>%
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


# constrained models

equality_params = c('b_HEas1,b_HEas2',
                    'b_HEcs1,b_HEcs2',
                    'b_HEhc1,b_HEhc2',
                    'b_HEen1,b_HEen2',
                    'b_HElo1,b_HElo2',
                    'b_HEfr1,b_HEfr2',
                    
                    'd_HEas1,d_HEas2',
                    'd_HEcs1,d_HEcs2',
                    'd_HEhc1,d_HEhc2',
                    'd_HEen1,d_HEen2',
                    'd_HElo1,d_HElo2',
                    'd_HEfr1,d_HEfr2')

syntax = list()
for (i in seq_along(equality_params)){
  syntax[[i]] = RC_GCLM_syntax(multiple = T,
                               control = control_names[-c(3,12)],
                               group_equality = equality_params[i]
                               )
  
  # syntax[[i]] = RC_GCLM_syntax(model = 'regclm',
  #                              endogeneous = c('HE', 'as', 'cs', 'hc'),
  #                              impulses = F,
  #                              past_states = F,
  #                              cor = T)
  
}


groupSEM = function(comb){
  
  dvar = comb[[1]]
  lsoa = comb[[2]]
  synt = comb[[3]]
  synt_n = comb[[4]]
  
  list_result = list()
  
  df_lv = lavaan_df(dv = dvar,
                    deprivation_cat = lsoa,
                    df = df)
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


dvs_temp = dvs[3]

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

## read
#samhi_index_constrained_models <- readRDS("samhi_index_constrained_models.rds")
#antidep_rate_constrained_models <- readRDS("antidep_rate_constrained_models.rds")
#est_qof_dep_constrained_models <- readRDS("est_qof_dep_constrained_models.rds")
#prop_ibesa_constrained_models <- readRDS("prop_ibesa_constrained_models.rds")
#z_mh_rate_constrained_models <- readRDS("z_mh_rate_constrained_models.rds")
#
## save constrained models as .rds
#saveRDS(list(samhi_index_constrained_models,
#             prop_ibesa_constrained_models,
#             est_qof_dep_constrained_models,
#             antidep_rate_constrained_models,
#             z_mh_rate_constrained_models),
#         paste0('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/',
#                'constrained_models', '.rds'))
#
## remove separate all lists with constrained models
#rm(samhi_index_constrained_models,
#   prop_ibesa_constrained_models,
#   est_qof_dep_constrained_models,
#   antidep_rate_constrained_models,
#   z_mh_rate_constrained_models)
#gc()

# read
setwd('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2')
tic()
free_models <- readRDS("free_models.rds")
#constrained_models <- readRDS("constrained_models.rds")
toc()
gc()

# Combine every 3 sublists into one and add it to the nested list
nested_free_models <- list()
for (i in seq(1, length(free_models), by = 3)) {
  nested_free_models[[length(nested_free_models) + 1]] <- 
    list(free_models[[i]], free_models[[i+1]], free_models[[i+2]])
}
rm(free_models)

# # ----------------------------------------------------------------------

# Output

# 1. Sample Description

stationary = c(control_names[-c(3,12)], 'lsoa_ses_score1')
nonstationary = c('samhi_index',
                  'prop_ibesa',
                  'est_qof_dep',
                  'antidep_rate',
                  'z_mh_rate',
                  
                  'social_care_adult',
                  'social_care_children',
                  'healthcare',
                  'env',
                  'law_order',
                  'infrastructure')
vars_used = c(nonstationary, stationary)

sumstat_dep = list()
for (k in 1:length(lsoa_group)){
  
  lsoa_ses_score = lsoa_group[k]
  
  df_before_scaling$imd_year = case_when(df_before_scaling$year == 2013 &
                                           df_before_scaling[,lsoa_ses_score] == 1 ~ '_1',
                                         df_before_scaling$year == 2013 &
                                           df_before_scaling[,lsoa_ses_score] == 2 ~ '_3',
                                         df_before_scaling$year == 2019 &
                                           df_before_scaling[,lsoa_ses_score] == 1 ~ '_2',
                                         df_before_scaling$year == 2019 &
                                           df_before_scaling[,lsoa_ses_score] == 2 ~ '_4',
                                         TRUE ~ 'other')
  #table(df_before_scaling$imd_year)
  
  # Z-scores for health
  df_before_scalingZ = df_before_scaling
  for (i in c("antidep_rate", "est_qof_dep", "prop_ibesa")){
    df_before_scalingZ[, i] = scale(df_before_scaling[, i])
  }
  
  sumstat_dep[[k]] = summarize_data(dat = df_before_scalingZ %>%
                                      filter(!imd_year == 'other'),
                                    .stationary = stationary[-11],
                                    group = 'imd_year',
                                    rownames = nm_out[-c(14,23)],
                                    quant = F,
                                    stat = list('Mean' = mean,
                                                'SD' = sd#,
                                                #'Q25' = function(x) quantile(x, probs = 0.25),
                                                #'Q75' = function(x) quantile(x, probs = 0.75)
                                    )
  )
  for (i in c(2,8,15)) {
    sumstat_dep[[k]] = tibble::add_row(sumstat_dep[[k]], .before = i)
  }
  sumstat_dep[[k]] = tibble::add_column(sumstat_dep[[k]], "Var1" = '', .after = 5)
  sumstat_dep[[k]] = tibble::add_column(sumstat_dep[[k]], "Var2" = '', .after = 10)
  
  section_subnames = c('Health Indicators', 'Spending, £ per capita', 'Controls')
  sumstat_dep[[k]] = sumstat_dep[[k]] %>%
    dplyr::mutate(across(1, ~replace(.x, is.na(.x), section_subnames))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), "", .)) 
}


#colnames(sumstat_dep1) = c('', 'Top 50%', '', '', 'Bottom 50%', '', '')
colnames(sumstat_dep[[1]]) = c('',
                           '2013 Top 50%', '', '2019 Top 50%', '',
                           'SD or Percentage Change Top 50%', 
                           '2013 Bottom 50%', '', '2019 Bottom 50%', '',
                           'SD or Percentage Change Top 50%')
colnames(sumstat_dep[[2]]) = c('',
                               '2013 Top 40%', '', '2019 Top 40%', '',
                               'SD or Percentage Change Top 40%', 
                               '2013 Bottom 40%', '', '2019 Bottom 40%', '',
                               'SD or Percentage Change Top 40%')
colnames(sumstat_dep[[3]]) = c('',
                               '2013 Top 30%', '', '2019 Top 30%', '',
                               'SD or Percentage Change Top 30%', 
                               '2013 Bottom 30%', '', '2019 Bottom 30%', '',
                               'SD or Percentage Change Top 30%')

#quantile(df_before_scaling$SD[df_before_scaling$lsoa_ses_score1==2])
#sd(df_before_scaling$nonwhite[df_before_scaling$lsoa_ses_score1==1])
#sd(df_before_scaling$nonwhite[df_before_scaling$lsoa_ses_score1==2])

# temporal distance
df_imd_dist = df_before_scaling %>% 
  filter(year %in% c(2013, 2019)) %>%
  dplyr::select(c(all_of(nonstationary), LAD21CD, year, lsoa_ses_score1))  %>%
  dplyr::group_by(LAD21CD, year,lsoa_ses_score1) %>%
  dplyr::mutate(across(all_of(nonstationary), mean, na.rm = T)) %>%
  dplyr::group_by(year,LAD21CD,lsoa_ses_score1) %>%
  sample_n(1) %>%
  pivot_wider(id_cols = LAD21CD, names_from = c(year,lsoa_ses_score1), 
              values_from = all_of(nonstationary)) %>%
  dplyr::mutate(samhi_index_prop = (samhi_index_2019 - samhi_index_2013)/samhi_index_2013,
                prop_ibesa_prop = (prop_ibesa_2019 - prop_ibesa_2013)/prop_ibesa_2013,
                est_qof_dep_prop = (est_qof_dep_2019 - est_qof_dep_2013)/est_qof_dep_2013,
                antidep_rate_prop = (antidep_rate_2019 - antidep_rate_2013)/antidep_rate_2013,
                z_mh_rate_prop = (z_mh_rate_2019 - z_mh_rate_2013)/z_mh_rate_2013,
                social_care_adult_prop = (social_care_adult_2019 - social_care_adult_2013)/social_care_adult_2013,
                social_care_children_prop = (social_care_children_2019 - social_care_children_2013)/social_care_children_2013,
                healthcare_prop = (healthcare_2019 - healthcare_2013)/healthcare_2013,
                env_prop = (env_2019 - env_2013)/env_2013,
                law_order_prop = (law_order_2019 - law_order_2013)/law_order_2013,
                infrastructure_prop = (infrastructure_2019 - infrastructure_2013)/infrastructure_2013)
  
# lm
df_lad <- df_before_scaling %>%
  dplyr::select(c(nonstationary, LAD21CD,
                  year, lsoa_ses_score1)) %>%
  group_by(LAD21CD, year, lsoa_ses_score1) %>%
  summarise(across(.cols = all_of(nonstationary), .fns = ~mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
  filter(year %in% c(2013, 2019))

lm1 = lm(samhi_index ~ factor(year)*factor(lsoa_ses_score1), data = df_lad)
summary(lm1)


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
                                           'group_free_fit_dep_3')) %>%
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
                                 fit_measures = fit_measures_gf,
                                 param_range = 20:32,
                                 section_name_rows = c(1, 8, 14, 20)
  )
  gf_models_coefs[10:15,1] = end_new[2:7]
  
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
    out$`Constrained Spending` = c(rep(descriptives_names[6:11], 2))
    out$Dynamic = c(#'Free Model', 
      rep('Long-Run', 6),
      rep('Short-Run', 6))
    out %<>% dplyr::select(Dynamic, `Constrained Spending`,
                           `Chisq diff`, `Pr(>Chisq)`)%>%
      mutate(Sig = sig_fun(`Pr(>Chisq)`))
    out[out == 'NA'|out == 'l'] = ''
    rownames(out) = NULL
    
    return(out)
  }
  
  anova_1 = anova_table(list_range = 1:12,
                        free_model = group_free_fit_dep_1)
  anova_2 = anova_table(list_range = 13:24,
                        free_model = group_free_fit_dep_2)
  anova_3 = anova_table(list_range = 25:36,
                        free_model = group_free_fit_dep_3)
  anova_all = anova_1 %>%
    left_join(anova_2, by = c('Constrained Spending', 'Dynamic'))%>%
    left_join(anova_3, by = c('Constrained Spending', 'Dynamic'))
  
  
  # 5. Substantive
  
  ratio_policy = c()
  df_before_scaling = as.data.frame(df_before_scaling)
  for (i in policy_names_6){
    ratio_policy = c(ratio_policy, 10/((exp(sd(log(df_before_scaling[,i])))-1)*100)
    )
    
  }
  id_ratio_policy = c("HE~as", "HE~cs", "HE~hc", "HE~en", "HE~lo", "HE~fr")
  ratio_df = cbind.data.frame('id' = id_ratio_policy, 'ratio' = ratio_policy)
  
  # transforming
  pct_coefs = CoefsExtract(models = c('group_free_fit_dep_1',
                                      'group_free_fit_dep_2',
                                      'group_free_fit_dep_3'),
                           standardized = F,
                           controls = NULL,
                           df_transform = ratio_df) %>% 
    filter(type_1 == 'c_policy') %>%
    dplyr::select(everything(),type = type_1, -type_2)
  
  
  pct_coefs$id = c('Adult Social Care',
                   'Children Social Care',
                   'Healthcare',
                   'Environment',
                   'Law and Order',
                   'Infrastructure')
  gf_pct_coefs = TableEffects(dat = pct_coefs,
                              fit_measures = fit_measures_gf,
                              param_range = 7:19,
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
  
  gf_pct_coefs_short = gf_pct_coefs_short %>% left_join(anova_long, by = 'Constrained Spending')
  
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
  
  long_short_tab = rbind(gf_models_coefs_long[1:14,],
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


#
list_names = c('samhi_index_constrained_models',
               'antidep_rate_constrained_models',
               'est_qof_dep_constrained_models',
               'prop_ibesa_constrained_models',
               'z_mh_rate_constrained_models')
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
  target_name = paste0("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/paper2_tabs_",
                       dvs[i], '.docx')
  print(doc,
        target = target_name)
  
  print(i)
  
}
toc()
gc()
beepr::beep()




# # ----------------------------------------------------------------------

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
library(ggpubr)

vars_original_names = c('samhi_index',
                        'prop_ibesa',
                        'antidep_rate',
                        'est_qof_dep',
                        'z_mh_rate',
                        'social_care_adult',
                        'social_care_children',
                        'healthcare',
                        'env',
                        'law_order',
                        'infrastructure')
all_vars = c(health_vars,
             policy_names_6)

df_plot = df_before_scaling %>% select(year,
                                       lsoa11,
                                       LAD21CD,
                                       all_of(c(all_vars,
                                                dep_vec_3,
                                                lsoa_group))) 

# Z-scores for health domains
for (i in c("antidep_rate", "est_qof_dep", "prop_ibesa")){
  df_plot[, i] = scale(df_plot[, i])
}

# Mental indices comparison by lsoa_dep

for (i in lsoa_group) {
  
  num = as.numeric(unlist(str_extract_all(i, "\\d+")))
  if (num == 1){
    df_plot <- df_plot %>%
      dplyr::mutate(!!sym(i) := factor(!!sym(i),
                                levels = 1:2,
                                labels = c('Top 50%',
                                           'Bottom 50%')))
    
  } else if(num == 2) {
    df_plot <- df_plot %>%
      dplyr::mutate(!!sym(i) := factor(!!sym(i),
                                levels = 1:2,
                                labels = c('Top 40%',
                                           'Bottom 40%')))
  }else if(num == 3) {
    df_plot <- df_plot %>%
      dplyr::mutate(!!sym(i) := factor(!!sym(i),
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
    group_by(LAD21CD, year, !!sym(j)) %>%
    dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = samhi_index:infrastructure) %>%
    ungroup() %>% dplyr::rename(dep_colour = !!sym(j)) %>%
    dplyr::mutate(dep_cat = sub('[[:digit:]]+', '', sym(j)))%>%
    dplyr::mutate(dep_linetype = j)
}

panel_df = do.call(rbind.data.frame, list_panel) %>% 
  filter(!is.na(dep_colour))

panel_df$name = factor(panel_df$name,
                       levels = vars_original_names,
                       labels = nm_out[1:11])
panel_df %>%
  dplyr::filter(name %in% nm_out[1:5])   %>%
  ggplot(aes(year, value, 
             colour = factor(dep_colour),
             linetype = factor(dep_colour))) +
  scale_x_continuous(name = NULL, 
                     breaks = 2013:2019)+ 
  theme(axis.text = element_text(size = 24)) +
  facet_wrap(~ name, scales = 'free_x') +
  geom_smooth(method = loess,
              linewidth = 0.5,
              se = T,
              formula = y ~ x,
              level = 0.9,
              fill = 'lightgrey')  +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = linetypes) +
  theme_pubclean()+ 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = 'Z-Standardised Scores'#,
                     #limits = c(-2, 2)
                     ) #+ 
 #scale_fill_manual(values = c(rep("#0072B2", 3), rep("#CC79A7", 3))) +
 #scale_colour_manual(values = c(rep("#0072B2", 3), rep("#CC79A7", 3)))
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/mhealth_smoothplots.svg",
       width = 25, height = 18, units = 'cm')


# Spending comparison 
panel_df  %>% 
  filter(name %in% nm_out[6:11]) %>%
  ggplot(aes(year, value, colour = dep_colour, linetype = dep_colour)) +
  scale_x_continuous(name = NULL, 
                     breaks = 2013:2019)+ 
  scale_y_continuous(name = 'Spending, £ per capita'#, limits = c(100, 1400)
                     ) + 
  theme(axis.text = element_text(size = 24))  +
  geom_smooth(method = loess,
              linewidth = 0.5,
              se = T,
              formula = y ~ x,
              level = 0.9,
              fill = 'lightgrey') +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = linetypes)  +
  facet_wrap(~ name, scales = 'free') +
  theme_pubclean() + 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom') # + 
  #scale_fill_manual(values = c("#0072B2", "#CC79A7")) +
  #scale_colour_manual(values = c("#0072B2", "#CC79A7"))
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2/spending_smoothplots.svg",
       width = 25, height = 18, units = 'cm')
