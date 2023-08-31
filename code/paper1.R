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
library(data.table)
library(tidyr)
library(broom)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.R')
options(max.print=3900)

# pre-processing

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')

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
                                              'MD')]){
  df[, i] = scale(df[, i])
}

# a dataframe for the sensitivity analysis
df_for_outliers = df

# normalize and log for spending
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean',
                                               'inc_mean')])){
  df[, i] = scale(log(df[, i]))
}

# flip the sign
df = df %>%
  mutate_at(vars(all_of(health_vars)), ~ -.)

# final dataset - wide format
df_lv = lavaan_df(dv = 'samhi_index',
                  df = df)
df_lv = as.data.frame(na.omit(df_lv))
summary(df_lv)

# saving
saveRDS(df_lv, 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df_lv.RDS')

#par(mfrow=c(2,2))
# quick dist 
# hist(df_lv$HE1)
# hist(df$antidep_rate, breaks = 30)
# hist(df$samhi_index, breaks = 30)
# hist(df$z_mh_rate, breaks = 30)
# hist(df$est_qof_dep, breaks = 30)
# hist(df$prop_ibesa, breaks = 30)
# 
# hist(df$social_care_adult, breaks = 30)
# hist(df$social_care_children, breaks = 30)
# hist(df$healthcare, breaks = 30)
# hist(df$env, breaks = 30)
# hist(df$law_order, breaks = 30)
# hist(df$infrastructure, breaks = 30)
# hist(df$public_health, breaks = 30)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

# Series of models

# 1. Random Curve Only
only_growth_syntax = RC_GCLM_syntax(model = 'regclm',
                                    impulses = F,
                                    past_states = F,
                                    cor = T)
only_growth_fit = sem(only_growth_syntax,
                      data = df_lv, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD')
fm_only_growth_fit = fitmeasures(only_growth_fit, measures)
#summary(only_growth_fit, std=T, ci = T)
gc()

# 1. RI-CLPM
riclpm_syntax = RC_GCLM_syntax(model = 'reclpm',
                               no_slopes = no_slopes,
                               cor = T, resid_stationary = F)
riclpm_fit = sem(riclpm_syntax,
                 data = df_lv,
                 estimator = "mlr",
                 orthogonal = T,
                 cluster = 'LAD21CD')
fm_riclpm_fit = fitmeasures(riclpm_fit, measures)
#summary(riclpm_fit, std=T, ci = T)
gc()

# # 2. RC-CLPM
# rcclpm_syntax = RC_GCLM_syntax(model = 'reclpm')
# rcclpm_fit = sem(rcclpm_syntax,
#                  data = df_lv,
#                  estimator = "mlr",
#                  orthogonal = T,
#                  cluster = 'LAD21CD')
# beepr::beep()
# fm_rcclpm_fit = fitmeasures(rcclpm_fit, measures)
# # summary(rcclpm_fit, std=T, ci = T)
# 
# # 3. RI-GCLM
# rigclm_syntax = RC_GCLM_syntax(model = 'regclm',
#                                no_slopes = no_slopes)
# rigclm_fit = sem(rigclm_syntax,
#                  data = df_lv,
#                  estimator = "mlr",
#                  orthogonal = T,
#                  cluster = 'LAD21CD'
# )
# beepr::beep()
# fm_rigclm_fit = fitmeasures(rigclm_fit, measures)
# #summary(rigclm_fit, std=T, ci = T)
# gc()


# 4. RC-GCLM
rcgclm_syntax = RC_GCLM_syntax(model = 'regclm',
                               cor = T)
rcgclm_fit = sem(rcgclm_syntax,
                 data = df_lv, 
                 estimator = "mlr",
                 orthogonal = T, 
                 cluster = 'LAD21CD'
                 )
beepr::beep()
#summary(rcgclm_fit, std=T, ci = T)
fm_rcgclm_fit = fitmeasures(rcgclm_fit, measures)
gc()

anova(riclpm_fit, rcgclm_fit)
anova(only_growth_fit, rcgclm_fit)

# 5. Health only
rcgclm_syntax_h = RC_GCLM_syntax(model = 'regclm',
                                 endogeneous = c('HE', 'as', 'cs', 'hc'),
                                 cor = T)
rcgclm_fit_h = sem(rcgclm_syntax_h,
                   data = df_lv, 
                   estimator = "mlr",
                   orthogonal = T, 
                   cluster = 'LAD21CD')
beepr::beep()
fm_rcgclm_fit_h = fitmeasures(rcgclm_fit_h, measures)
#summary(rcgclm_fit_h, std=T, ci = T)
gc()

# ---------------------------------------------------------------------
# ------------------------------- Sensitivity -------------------------
# ---------------------------------------------------------------------

df_noLondon = df_for_outliers

# filter first
df_noLondon %<>% filter(!class == 'L')

# normalize and log for spending
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean',
                                               'inc_mean')])){
  df_noLondon[, i] = scale(log(df_noLondon[, i]))
}

# flip the sign
df_noLondon %<>% mutate_at(vars(all_of(health_vars)), ~ -.)

# final dataset - wide format
df_noLondon_lv = lavaan_df(dv = 'samhi_index',
                              df = df_noLondon)
df_noLondon_lv = as.data.frame(na.omit(df_noLondon_lv))
summary(df_noLondon_lv)


# 1. Without London
rcgclm_syntax_noLondon = RC_GCLM_syntax(model = 'regclm',
                                        control = control_names[!control_names %in% 'London'])
rcgclm_L_fit = sem(rcgclm_syntax_noLondon,
                   data = df_noLondon_lv, 
                   estimator = "mlr",
                   orthogonal = T, 
                   cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_L_fit, std=T, ci = T)
fm_rcgclm_L_fit = fitmeasures(rcgclm_L_fit, measures)
gc()

# 2. Without outliers: 3*IQR

remove_outliers = function(x, k) {
  qnt = quantile(x, probs = c(.25, .75), na.rm = T)
  H = k*IQR(x, na.rm = T)
  x[x > (qnt[2] + H)] = NA
  x[x < (qnt[1] - H)] = NA
  x
}

df_outliers_3 = df_for_outliers %>% group_by(year) %>%
  mutate_at(vars(all_of(c(policy_names_6,
                          'samhi_index'))),
            ~remove_outliers(., k = 3)) %>%
  na.omit() %>% ungroup()

# normalize and log for spending
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean',
                                               'inc_mean')])){
  df_outliers_3[, i] = scale(log(df_outliers_3[, i]))
}

# flip the sign
df_outliers_3 %<>% mutate_at(vars(all_of(health_vars)), ~ -.)


df_outliers_3_lv = lavaan_df(dv = 'samhi_index',
                               df = df_outliers_3)
df_outliers_3_lv = as.data.frame(na.omit(df_outliers_3_lv))
#summary(df_outliers_3_lv)

rcgclm_outliers_3_fit = sem(rcgclm_syntax,
                 data = df_outliers_3_lv, 
                 estimator = "mlr",
                 orthogonal = T, 
                 cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_outliers_3_fit, std=T, ci = T)
fm_rcgclm_outliers_3_fit = fitmeasures(rcgclm_outliers_3_fit, measures)
gc()

# 3. Without outliers: 1.5*IQR

df_outliers_1.5 = df_for_outliers %>% group_by(year) %>%
  mutate_at(vars(all_of(c(policy_names_6,
                          'samhi_index'))),
            ~remove_outliers(., k = 1.5)) %>%
  na.omit() %>% ungroup()

# normalize and log for spending
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean',
                                               'inc_mean')])){
  df_outliers_1.5[, i] = scale(log(df_outliers_1.5[, i]))
}

# flip the sign
df_outliers_1.5 %<>% mutate_at(vars(all_of(health_vars)), ~ -.)


df_outliers_1.5_lv = lavaan_df(dv = 'samhi_index',
                               df = df_outliers_1.5)
df_outliers_1.5_lv = as.data.frame(na.omit(df_outliers_1.5_lv))
#summary(df_outliers_1.5_lv)

rcgclm_outliers_1.5_fit = sem(rcgclm_syntax_noLondon,
                            data = df_outliers_1.5_lv, 
                            estimator = "mlr",
                            orthogonal = T, 
                            cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_outliers_1.5_fit, std=T, ci = T)
fm_rcgclm_outliers_1.5_fit = fitmeasures(rcgclm_outliers_1.5_fit, measures)
gc()

# ---------------------------------------------------------------------
# -------------------------------- Indices-- --------------------------
# ---------------------------------------------------------------------

mental_sub_syntax = RC_GCLM_syntax(model = 'regclm')

# 1. IBESA (%)

df_lv_sub1 = lavaan_df(dv = 'prop_ibesa',
                                  df = df)
mental_sub1_fit = sem(mental_sub_syntax,
                      data = df_lv_sub1, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
#summary(mental_sub1_fit, std=T, ci = T)
fm_sub1_fit = fitmeasures(mental_sub1_fit, measures)
gc()

# 2. QOF – Depression (%)
df_lv_sub2 = lavaan_df(dv = 'est_qof_dep',
                                  df = df)
mental_sub2_fit = sem(mental_sub_syntax,
                      data = df_lv_sub2, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
#summary(mental_sub2_fit, std=T, ci = T)
fm_sub2_fit = fitmeasures(mental_sub2_fit, measures)
gc()

# 3. ADQ of antidepressants per person

df_lv_sub3 = lavaan_df(dv = 'antidep_rate',
                                  df = df)
mental_sub3_fit = sem(mental_sub_syntax,
                      data = df_lv_sub3, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
fm_sub3_fit = fitmeasures(mental_sub3_fit, measures)
#summary(mental_sub3_fit, std=T, ci = T)
gc()

# 4. Hospital admissions (z-scores)

df_lv_sub4 = lavaan_df(dv = 'z_mh_rate',
                                  df = df)
mental_sub4_fit = sem(mental_sub_syntax,
                      data = df_lv_sub4, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
beepr::beep()
#summary(mental_sub4_fit, std=T, ci = T)
fm_sub4_fit = fitmeasures(mental_sub4_fit, measures)
gc()

# # ----------------------------------------------------------------------
# # ----------------------------- Output ---------------------------------
# # ----------------------------------------------------------------------

# 1. Sample description

stationary = control_names
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

sumstat_fin = summarize_data(dat = df_before_scaling,
                                         quant = F,
                             stat = list('Mean' = mean,
                                         'SD' = sd#,
                                         #'Q25' = function(x) quantile(x, 0.25),
                                         #'Q75' = function(x) quantile(x, 0.75)
                                         )
                             )


# # ----------------------------------------------------------------------

# 2. Correlations

cor_tab = as.data.frame(cor(df_before_scaling[,vars_used]))
cor_tab[] = lapply(cor_tab, sprintf, fmt = "%.2f")
cor_tab[lower.tri(cor_tab, diag=F)] = ''
cor_tab = as.data.frame(cor_tab)
colnames(cor_tab) = 1:ncol(cor_tab)
rownames(cor_tab) = nm_out

row_column_numbering = function(dat){
  dat %<>% rownames_to_column(var = ' ')
  dat = cbind.data.frame(1:nrow(dat), dat)
  colnames(dat) = c('','', 1:(ncol(dat)-2))
  return(dat)
}
cor_tab = row_column_numbering(cor_tab)

# # ----------------------------------------------------------------------

# 3.1 Correlations - Random effects - main model

effects_all = CoefsExtract(models = c('only_growth_fit',
                                      'riclpm_fit',
                                      'rcgclm_fit',
                                      'rcgclm_fit_h'),
             growth = c(paste0('i', endogeneous),
                        paste0('s', endogeneous)))

# keeping covariances only
i_cor = effects_all %>% filter(type == 'i_cor' & nchar(id) > 6)
i_cor = i_cor[,c('id', 'est.std.x.x_long')]
i_cor = i_cor %>% tidyr::separate('id', c('X', 'Y'), sep =':=')

# creating a cor matrix
growth = c(paste0('i', endogeneous), 
           paste0('s', endogeneous))
growthcor = as.data.frame(matrix(nrow = length(growth),
                                 ncol = length(growth),
       dimnames = list(growth, growth)))
for (row in growth){
  for (col in growth){
    if (any(i_cor$X == row & i_cor$Y == col)){
      growthcor[row, col] = i_cor[i_cor$X == row & i_cor$Y == col, 
                                'est.std.x.x_long']
    }
    
  }
}
growthcor[lower.tri(growthcor, diag = F)] <- ''
growthcor[is.na(growthcor)] = '1.000'

# col and row names
all_nam = c('Intercept Mental Health',
            'Intercept Adult Social Care',
            'Intercept Children Social Care',
            'Intercept Healthcare',
            'Intercept Environment',
            'Intercept Law and Order',
            'Intercept Infrastructure',
            'Slope Mental Health',
            'Slope Adult Social Care',
            'Slope Children Social Care',
            'Slope Healthcare',
            'Slope Environment',
            'Slope Law and Order',
            'Slope Infrastructure'
)
dimnames(growthcor) = list(all_nam, 1:length(all_nam))

# split into 3 tables

int_int = growthcor[1:7,1:7]
int_slope = growthcor[1:7,8:14]
slope_slope = growthcor[8:14,8:14]

int_int = row_column_numbering(int_int)
int_slope = row_column_numbering(int_slope)
colnames(int_slope) = c('', '', all_nam[grepl('Slope', all_nam)])
slope_slope = row_column_numbering(slope_slope)

# # ----------------------------------------------------------------------

# 3.2 Correlations - impulses - all models 

end_new = c('Mental Health',
            'Adult Social Care',
            'Children Social Care',
            'Healthcare',
            'Environment',
            'Law and Order', 
            'Infrastructure')

# extracting impulses

d_impulse_cov = effects_all %>% filter(type == 'i_cor' & nchar(id) == 6) %>% 
  dplyr:: select(id, ends_with('long'))
d_impulse_cov = d_impulse_cov %>% tidyr::separate('id', c('X', 'Y'), sep =':=')

pars_vec = c("est.std.x_long",
             "est.std.y_long",
             "est.std.x.x_long",
             "est.std.y.y_long")

# cleaning the table

impulse_cor = d_impulse_cov
col = c('X', 'Y')
impulse_cor[, col] = apply(impulse_cor[, col], 2, function(x) end_new[match(x, endogeneous)])
impulse_cor = impulse_cor[order(match(impulse_cor$X, end_new)), ]
impulse_cor[impulse_cor == 'NA'] = ''
colnames(impulse_cor) = c('', '', 'Model 1.1', 'Model 1.2', 'Model 1.3', 'Model 1.4')
impulse_cor = CiSplit(impulse_cor)

# # ----------------------------------------------------------------------

# 4.1 Regression table - main

parameters = c('Number of Parameters',
               'Chi-Squared',
               'Degrees of Freedom',
               'CFI',
               'TLI',
               'SMRM',
               'RMSEA',
               'RMSEA Lower Bound',
               'RMSEA Upper Bound',
               'AGFI',
               'AIC',
               'BIC',
               'Log-Likelihood')

section_names = c('Autoregressive Effects',
                  'Cross-Lagged Effects: Mental Health -> Spending',
                  'Cross-Lagged Effects: Spending -> Mental Health',
                  'Random Intercepts',
                  'Random Slopes',
                  'Fit Measures (Scaled)')
fit_measures_seq = cbind.data.frame(est.std.x_long = fm_only_growth_fit,
                                    est.std.y_long = fm_riclpm_fit,
                                    est.std.x.x_long = fm_rcgclm_fit,
                                    est.std.y.y_long = fm_rcgclm_fit_h)

fit_measures_seq[] = lapply(fit_measures_seq, sprintf, fmt = "%.3f")

# create tables with effects and fit measures

TableEffects = function(dat = effects_all,
                        .end_new = end_new,
                        .parameters = parameters,
                        .section_names = section_names,
                        fit_measures = NULL) {
  
  patterns = c("~HE|HE~#",
                "~as|as~#",
               "~cs|cs~#",
               "~hc|hc~#",
                "~en|en~#",
               "~lo|lo~#",
               "~fr|fr~#")

  dat = dat %>%
    filter(!type %in% c('d_impulse_cov',
                        'e_growth_cov',
                        'g_other_policies',
                        'h_controls',
                        'i_cor')) %>%
    mutate(id = reduce(patterns, function(x, y) if_else(grepl(y, x),
                                                        .end_new[match(y, patterns)], x), 
                       .init = id)) %>%
    rbind.fill(., fit_measures %>% rownames_to_column("id") %>% 
                 mutate(id = str_remove(id, ".scaled"))) %>%
    mutate(across(1, ~replace(.x, 34:46, .parameters)))
  
  # subsections in a table
  for (i in rev(c(1, 8, 14, 20, 27, 34))) {
    dat = tibble::add_row(dat, .before = i)
  }
  
  dat = dat %>%
    mutate(across(1, ~replace(.x, is.na(.x), .section_names)))%>%
    mutate_all(~ ifelse(is.na(.), "", .)) %>%
    select(-type)
  
  
  return(dat)
}

seq_models_coefs = TableEffects(fit_measures = fit_measures_seq)
seq_models_coefs[,c(3,5)] = NULL # remove empty short-run for only_growth_fit and reclpm_fit
seq_models_coefs[10:15,1] = end_new[2:7]

# # ----------------------------------------------------------------------

# 4.2. Regression table - indices

indices_models_coefs_ = CoefsExtract(models = c('mental_sub1_fit',
                                                'mental_sub2_fit',
                                                'mental_sub3_fit',
                                                'mental_sub4_fit'),
                                     growth = c(paste0('i', endogeneous),
                                                paste0('s', endogeneous)))
fit_measures_indices = cbind.data.frame(est.std.x_long = fm_sub1_fit,
                                        est.std.y_long = fm_sub2_fit,
                                        est.std.x.x_long = fm_sub3_fit,
                                        est.std.y.y_long = fm_sub4_fit)
fit_measures_indices[] = lapply(fit_measures_indices, sprintf, fmt = "%.3f")

indices_models_coefs = TableEffects(dat = indices_models_coefs_,
                                    fit_measures = fit_measures_indices)

indices_models_coefs[10:15,1] = end_new[2:7]

# # ----------------------------------------------------------------------

# 4.3 Regression table - sensitivity (plus the main model)

sensitivity_models_coefs_ = CoefsExtract(models = c('rcgclm_fit',
                                                    'rcgclm_L_fit',
                                                    'rcgclm_outliers_3_fit',
                                                    'rcgclm_outliers_1.5_fit'),
                                         growth = c(paste0('i', endogeneous),
                                                 paste0('s', endogeneous)))
fit_measures_sensitivity = cbind.data.frame(est.std.x_long = fm_rcgclm_fit,
                                            est.std.y_long = fm_rcgclm_L_fit,
                                            est.std.x.x_long = fm_rcgclm_outliers_3_fit,
                                            est.std.y.y_long = fm_rcgclm_outliers_1.5_fit)
fit_measures_sensitivity[] = lapply(fit_measures_sensitivity, sprintf, fmt = "%.3f")

sensitivity_models_coefs = TableEffects(dat = sensitivity_models_coefs_,
                                        fit_measures = fit_measures_sensitivity)
sensitivity_models_coefs[10:15,1] = end_new[2:7]

# # ----------------------------------------------------------------------

# 5. Main model - effects of spendings on each other
effects_main_std = CoefsExtract('rcgclm_fit',
                                standardized = T)
other_policies = effects_main_std %>% filter(type %in% c('g_other_policies',
                                                      'a_auto') &
                                            !grepl('HE', id))
other_policies_long = other_policies[,c('id', 'est.std_long')]
other_policies_short = other_policies[,c('id', 'est.std_short')]
#other_policies$est.std.x.x_long = gsub("\\[.*?\\]", '', other_policies$est.std.x.x_long)

# applying the function
other_policies_matrix_long = MatrixEffects(dat = other_policies_long,
                                cor_name = 'id',
                                colnames = endogeneous[-1],
                                rownames = endogeneous[-1],
                                pars = 'est.std_long',
                                cor = F,
                                sep = '~') 
other_policies_matrix_short = MatrixEffects(dat = other_policies_short,
                                           cor_name = 'id',
                                           colnames = endogeneous[-1],
                                           rownames = endogeneous[-1],
                                           pars = 'est.std_short',
                                           cor = F,
                                           sep = '~') 
dimnames(other_policies_matrix_long) = list(nm_out[6:11], nm_out[6:11])
dimnames(other_policies_matrix_short) = list(nm_out[6:11], nm_out[6:11])

# # ----------------------------------------------------------------------

# 6. Main model - controls

controls = effects_all %>% filter(type == 'h_controls')
controls = controls[,c('id', 'est.std.x.x_long')]
#controls$est.std.x.x_long = gsub("\\[.*?\\]", '', controls$est.std.x.x_long)

# applying the function
controls_matrix = MatrixEffects(dat = controls,
                                cor_name = 'id',
                                rownames = gsub('[[:digit:]]+', '', control_names),
                                pars = 'est.std.x.x_long',
                                cor = F,
                                sep = '~') 
dimnames(controls_matrix) = list(nm_out[12:23], rep(nm_out[c(1,6:11)],2))
controls_inter_mat = controls_matrix[,1:7]
controls_slope_mat = controls_matrix[,8:14]

# # ----------------------------------------------------------------------

# 7. Substantive (pct) effects from all models

# obtaining a normalising ratio to shift from the log_norm to log interpretation

# ratio_health = c()
# for (i in policy_names_6){
#   ranges_ratio = (max(df[,i]) - min(df[,i])) / (max(log(df_before_scaling[,i])) - min(log(df_before_scaling[,i])))
#   ranges_ratio = 1/ranges_ratio
#   ratio_health = c(ratio_health, ranges_ratio)
# 
# }
# ratio_policy = c()
# for (i in policy_names_6){
#   ranges_ratio = (max(df[,i]) - min(df[,i])) / (max(log(df_before_scaling[,i])) - min(log(df_before_scaling[,i])))
#   ratio_policy = c(ratio_policy, log(1.1)*ranges_ratio)
#   
# }
# id_ratio_policy = c("HE~as", "HE~cs", "HE~hc", "HE~en", "HE~lo", "HE~fr")
# id_ratio_health = c("as~HE", "cs~HE", "hc~HE", "en~HE", "lo~HE", "fr~HE")
# id_ratio = c(id_ratio_health, id_ratio_policy)
# ratio = c(ratio_health, ratio_policy)
# ratio_df = cbind.data.frame('id' = id_ratio, ratio)

ratio_policy = c()
df_before_scaling = as.data.frame(df_before_scaling)
for (i in policy_names_6){
  ratio_policy = c(ratio_policy, 10/((exp(sd(log(df_before_scaling[,i])))-1)*100)
                   )
  
}
id_ratio_policy = c("HE~as", "HE~cs", "HE~hc", "HE~en", "HE~lo", "HE~fr")
ratio_df = cbind.data.frame('id' = id_ratio_policy, 'ratio' = ratio_policy)

# transforming
pct_coef = CoefsExtract(models = c('rcgclm_fit',
                                   'mental_sub1_fit',
                                   'mental_sub2_fit',
                                   'mental_sub3_fit',
                                   'mental_sub4_fit'),
                        standardized = F,
                        controls = NULL,
                        df_transform = ratio_df) %>% 
  filter(type == 'c_policy') %>%
  select(-type)


pct_coef$id = c('Adult Social Care',
                'Children Social Care',
                'Healthcare',
                'Environment',
                'Law and Order',
                'Infrastructure')

# # ----------------------------------------------------------------------

# 8. Final Tables

col_seq = c('', 
            'Model 1.1', 
            'Model 1.2',
            'Model 1.3',
            '',
            'Model 1.4',
            '')

col_ind = c('',
            'Model 2.1', '',
            'Model 2.2', '',
            'Model 2.3', '',
            'Model 2.4', '')

col_sens = c('', 
             'Model 1.3', '',
             'Model 3.1', '',
             'Model 3.2', '',
             'Model 3.3', '')


col_pct = c('', 
            'Model 1.3', '',
            'Model 2.1', '',
            'Model 2.2', '',
            'Model 2.3', '',
            'Model 2.4', '')

# a function to create a head with the long-run and short-run indication

SubHead = function(tab, which_null = NULL, n, colnames){
  
  head = c('', rep(c('Long-run', 'Short-run'), n))
  
  if (!is.null(which_null)){
    head = head[-which_null]
  }
  
  tab = rbind.data.frame(head, tab)
  colnames(tab) = colnames
  
  return(tab)
}

# Main text

seq_models_coefs = SubHead(CiSplit(seq_models_coefs),
                           which_null = c(3,5),
                           n = 4,
                           colnames = col_seq)
seq_models_coefs[1,2:3] = ''

pct_coef = SubHead(CiSplit(pct_coef),
                   n = 5,
                   colnames = col_pct)

# Appendices

sumstat_fin

impulse_cor = CiSplit(impulse_cor)
int_int = CiSplit(int_int, rownm = F)
int_slope = CiSplit(int_slope, rownm = F)
slope_slope = CiSplit(slope_slope, rownm = F)

controls_inter_mat = CiSplit(controls_inter_mat, rownm = T)
controls_slope_mat = CiSplit(controls_slope_mat, rownm = T)

other_policies_matrix_long = CiSplit(other_policies_matrix_long, rownm = T)
other_policies_matrix_short = CiSplit(other_policies_matrix_short, rownm = T)
Long = c('Long-run', rep('', 6))
Short = c('Short-run', rep('', 6))
other_policies_matrix = rbind.data.frame(Long,
                                         other_policies_matrix_long,
                                         Short,
                                         other_policies_matrix_short)

indices_models_coefs = SubHead(CiSplit(indices_models_coefs),
                               n = 4,
                               colnames = col_ind)
sensitivity_models_coefs = SubHead(CiSplit(sensitivity_models_coefs),
                                   n = 4,
                                   colnames = col_sens)

cor_tab

# writing to word
library(officer)

# Create a list of data frames
df_list = list(
  
  # main
  
  seq_models_coefs,
  pct_coef,
  
  # Appendices
  
  sumstat_fin,
  impulse_cor,
  indices_models_coefs,
  
  # for the main model
  
  int_int,
  int_slope,
  slope_slope,
  controls_inter_mat,
  controls_slope_mat,
  other_policies_matrix,
  
  cor_tab,
  
  sensitivity_models_coefs
)

# treating empty colnames in officer

replace_empty_colnames <- function(df) {
  colnames <- names(df)
  empty_cols <- colnames[colnames == ""]
  if (length(empty_cols) > 0) {
    colnames[colnames == ""] <- "RRR"
    colnames(df) <- colnames
  }
  return(df)
}
df_list <- lapply(df_list, replace_empty_colnames)


# Create a Word document
doc <- read_docx()

# Loop over the list of data frames and add them to the document
for (i in seq_along(df_list)) {
  doc <- doc %>% 
    body_add_table(df_list[[i]])
  
  # Add a page break after each table
  if (i < length(df_list)) {
    doc <- doc %>% 
      body_add_break()
  }
}

# Save the document

print(doc, target = "C:/Users/ru21406/YandexDisk/PhD Research/Literature review/Paper1_tabs.docx")




# # ----------------------------------------------------------------------

# 9. Descriptive Plots

library(ggpubr)

# Z-scores for health domains
for (i in c("antidep_rate", "est_qof_dep", "prop_ibesa")){
  df_before_scaling[, i] = scale(df_before_scaling[, i])
}

all_vars = c(health_vars,
             policy_names_6)
panel_df = df_before_scaling %>%
  group_by(LAD21CD, year) %>%
  dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE)))
panel = panel_data(panel_df, id = LAD21CD, wave = year)

list_plots = list()
for (i in 1:5){
  list_plots[[i]] = panel  %>% 
    ggplot(aes(year, !!sym(all_vars[i]))) +
    scale_x_continuous(name = NULL, 
                       breaks = 2013:2019)+ 
    scale_y_continuous(name = NULL, limits = c(-3.5, 3))  +
    geom_line(aes(group = LAD21CD), color = "lightblue") +
    geom_smooth(method = loess, se = F, fullrange = T, color="darkred") +
    theme_pubclean() + 
    theme(axis.text = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24))
}
for (i in 6:length(all_vars)){
  list_plots[[i]] = panel  %>% 
    ggplot(aes(year, !!sym(all_vars[i]))) +
    scale_x_continuous(name = NULL, 
                       breaks = 2013:2019)+ 
    scale_y_continuous(name = NULL)  +
    geom_line(aes(group = LAD21CD), color = "lightblue") +
    geom_smooth(method = loess, se = F, fullrange = T, color="darkred") +
    theme_pubclean() + 
    theme(axis.text = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24))
}

# health variables
ggarrange(list_plots[[1]],
          list_plots[[5]],
          list_plots[[4]],
          list_plots[[3]],
          list_plots[[2]],
          labels = c('           SAMHI',
                     'Incapacity Benefits',
                     'Depression',
                     'Antidepressants',
                     'Hospital Admission'
          ),
          ncol = 3, nrow = 2,
          font.label = list(size = 30), align ='hv')
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/Literature review/mhealth_lineplots.jpeg",
       width = 60, height = 40, units = 'cm')

# policies
ggarrange(list_plots[[6]],
          list_plots[[7]],
          list_plots[[8]],
          list_plots[[9]],
          list_plots[[10]],
          list_plots[[11]],
          labels = c('Adult Social Care',
                     'Children Social Care',
                     'Healthcare',
                     'Environment',
                     'Law and Order',
                     'Infrastructure'
          ),
          ncol = 3, nrow = 2,
          font.label = list(size = 30), align ='hv')
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/Literature review/spending_lineplots.jpeg",
       width = 60, height = 40, units = 'cm')




