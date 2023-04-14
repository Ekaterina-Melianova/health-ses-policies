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
library(data.table)
library(tidyr)
library(broom)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.R')
options(max.print=3900)

# pre-processing

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')

# class
df$London = ifelse(df$class == 'L', 1, 0)
#df$MD = ifelse(df$class == 'MD', 1, 0)
df$SD = ifelse(df$class == 'SD', 1, 0)

# out
# rm = c('E07000211', 'E06000019',
#        'E06000039', 'E09000018',
#        'E06000012',
#        'E06000036'
#        )

# df %<>% filter(!LAD21CD %in% rm)# %>% filter(year > 2013)
# df$time = df$year - (min(df$year)-1)
# table(df$time)

# a dataset for descriptive stat
df_before_scaling = df
df_before_scaling$z_mh_rate = -df_before_scaling$z_mh_rate
df_before_scaling$antidep_rate = -df_before_scaling$antidep_rate
df_before_scaling$est_qof_dep = -df_before_scaling$est_qof_dep
df_before_scaling$prop_ibesa = -df_before_scaling$prop_ibesa
df_before_scaling$samhi_index = -df_before_scaling$samhi_index



# normalising from 0 to 10 to facilitate the convergence of SEM models
for (i in health_vars[-c(1:2)]){
  df[, i] = scale(df[, i])
}

for (i in c(#health_vars,
  control_names[!control_names %in% c('public_health_mean',
                                      'inc_mean',
                                      'rural',
                                      'London',
                                      'SD')]
  
)){
  df[, i] = scale(df[, i])
}
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean', 
                                               'inc_mean')])){
  df[, i] = normalize(log(df[, i]))*10
}

# flipping the sign
df$z_mh_rate = -df$z_mh_rate
df$antidep_rate = -df$antidep_rate
df$est_qof_dep = -df$est_qof_dep
df$prop_ibesa = -df$prop_ibesa
df$samhi_index = -df$samhi_index

# final dataset - wide format
df_lavaan_mental = lavaan_df(dv = 'samhi_index',
                             df = df)
df_lavaan_mental = as.data.frame(na.omit(df_lavaan_mental))
summary(df_lavaan_mental)

#par(mfrow=c(2,2))
# quick dist 
hist(df_lavaan_mental$HE1)
hist(df$antidep_rate, breaks = 30)
hist(df$samhi_index, breaks = 30)
hist(df$z_mh_rate, breaks = 30)
hist(df$est_qof_dep, breaks = 30)
hist(df$prop_ibesa, breaks = 30)

hist(df$social_care_adult, breaks = 30)
hist(df$social_care_children, breaks = 30)
hist(df$healthcare, breaks = 30)
hist(df$env, breaks = 30)
hist(df$law_order, breaks = 30)
hist(df$infrastructure, breaks = 30)
hist(df$public_health, breaks = 30)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

# Series of models

# 1. RI-CLPM
riclpm_syntax = RC_GCLM_syntax(model = 'reclpm',
                               no_slopes = no_slopes)
riclpm_fit = sem(riclpm_syntax,
                 data = df_lavaan_mental, 
                 estimator = "mlr",
                 orthogonal = T, 
                 cluster = 'LAD21CD'
)
fm_riclpm_fit = fitmeasures(riclpm_fit, measures)
#summary(riclpm_fit, standardized=T)

# # 2. RC-CLPM
# rcclpm_syntax = RC_GCLM_syntax(model = 'reclpm')
# rcclpm_fit = sem(rcclpm_syntax,
#                  data = df_lavaan_mental,
#                  estimator = "mlr",
#                  orthogonal = T,
#                  cluster = 'LAD21CD'
# )
# beepr::beep()
# fm_rcclpm_fit = fitmeasures(rcclpm_fit, measures)
# # summary(rcclpm_fit, standardized=T)
# 
# # 3. RI-GCLM
# rigclm_syntax = RC_GCLM_syntax(model = 'regclm',
#                                no_slopes = no_slopes)
# rigclm_fit = sem(rigclm_syntax,
#                  data = df_lavaan_mental, 
#                  estimator = "mlr",
#                  orthogonal = T, 
#                  cluster = 'LAD21CD'
# )
# beepr::beep()
# fm_rigclm_fit = fitmeasures(rigclm_fit, measures)
# #summary(rigclm_fit, standardized=T)
# gc()


# 4. RC-GCLM
rcgclm_syntax = RC_GCLM_syntax(model = 'regclm')
rcgclm_fit = sem(rcgclm_syntax,
                 data = df_lavaan_mental, 
                 estimator = "mlr",
                 orthogonal = T, 
                 cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_fit, standardized=T)
fm_rcgclm_fit = fitmeasures(rcgclm_fit, measures)
gc()

# 5. Health only
rcgclm_syntax_h = RC_GCLM_syntax(model = 'regclm',
                                 endogeneous = c('HE', 'as', 'cs', 'hc')
                                 )
rcgclm_fit_h = sem(rcgclm_syntax_h,
                   data = df_lavaan_mental, 
                   estimator = "mlr",
                   orthogonal = T, 
                   cluster = 'LAD21CD'
)
beepr::beep()
fm_rcgclm_fit_h = fitmeasures(rcgclm_fit_h, measures)
#summary(rcgclm_fit_h, standardized=T)

# ---------------------------------------------------------------------
# ------------------------------- Robustness --------------------------
# ---------------------------------------------------------------------
# 1. Without London
rcgclm_syntax_L = RC_GCLM_syntax(model = 'regclm',
                                 control = control_names[!control_names %in% 'London'])
rcgclm_L_fit = sem(rcgclm_syntax_L,
                   data = df_lavaan_mental[!df_lavaan_mental$class == 'L',], 
                   estimator = "mlr",
                   orthogonal = T, 
                   cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_L_fit, standardized=T)
fm_rcgclm_L_fit = fitmeasures(rcgclm_L_fit, measures)

# 2. Without outliers: 3*IQR

remove_outliers = function(x, k) {
  qnt = quantile(x, probs = c(.25, .75), na.rm = T)
  H = k*IQR(x, na.rm = T)
  x[x > (qnt[2] + H)] = NA
  x[x < (qnt[1] - H)] = NA
  x
}

df_outliers_3 = df %>% group_by(year) %>%
  mutate_at(vars(all_of(c(policy_names_6,
                          'samhi_index'))),
            ~remove_outliers(., k = 3)) %>%
  na.omit()
df_lavaan_outliers_3 = lavaan_df(dv = 'samhi_index',
                               df = df_outliers_3)
df_lavaan_outliers_3 = as.data.frame(na.omit(df_lavaan_outliers_3))
#summary(df_lavaan_outliers_3)

rcgclm_outliers_3_fit = sem(rcgclm_syntax,
                 data = df_lavaan_outliers_3, 
                 estimator = "mlr",
                 orthogonal = T, 
                 cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_outliers_3_fit, standardized=T)
fm_rcgclm_outliers_3_fit = fitmeasures(rcgclm_outliers_3_fit, measures)

# 3. Without outliers: 1.5*IQR

df_outliers_1.5 = df %>% group_by(year) %>%
  mutate_at(vars(all_of(c(policy_names_6,
                          'samhi_index'))),
            ~remove_outliers(., k = 1.5)) %>%
  na.omit()
df_lavaan_outliers_1.5 = lavaan_df(dv = 'samhi_index',
                                 df = df_outliers_1.5)
df_lavaan_outliers_1.5 = as.data.frame(na.omit(df_lavaan_outliers_1.5))
#summary(df_lavaan_outliers_1.5)

rcgclm_outliers_1.5_fit = sem(rcgclm_syntax_L,
                            data = df_lavaan_outliers_1.5, 
                            estimator = "mlr",
                            orthogonal = T, 
                            cluster = 'LAD21CD'
)
beepr::beep()
#summary(rcgclm_outliers_1.5_fit, standardized=T)
fm_rcgclm_outliers_1.5_fit = fitmeasures(rcgclm_outliers_1.5_fit, measures)

# ----------------------------------------------------------------------

# Indices

mental_sub_syntax = RC_GCLM_syntax(model = 'regclm')

# 1. IBESA (%)

df_lavaan_mental_sub1 = lavaan_df(dv = 'prop_ibesa',
                                  df = df)
mental_sub1_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub1, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
#summary(mental_sub1_fit, standardized=T)

fm_mental_sub1_fit = fitmeasures(mental_sub1_fit, measures)

# 2. QOF – Depression (%)
df_lavaan_mental_sub2 = lavaan_df(dv = 'est_qof_dep',
                                  df = df)
mental_sub2_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub2, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
#summary(mental_sub2_fit, standardized=T)
fm_mental_sub2_fit = fitmeasures(mental_sub2_fit, measures)

# 3. ADQ of antidepressants per person

df_lavaan_mental_sub3 = lavaan_df(dv = 'antidep_rate',
                                  df = df)
mental_sub3_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub3, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
fm_mental_sub3_fit = fitmeasures(mental_sub3_fit, measures)
#summary(mental_sub3_fit, standardized=T)
#hist(df$antidep_rate)

# 4. Hospital admissions (z-scores)

df_lavaan_mental_sub4 = lavaan_df(dv = 'z_mh_rate',
                                  df = df)
mental_sub4_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub4, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
beepr::beep()
#summary(mental_sub4_fit, standardized=T)
fm_mental_sub4_fit = fitmeasures(mental_sub4_fit, measures)

# # ----------------------------------------------------------------------
# # ----------------------------------------------------------------------
# # ----------------------------------------------------------------------

# Output

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

sumstat_fin = summarize_data(dat = df_before_scaling, stat = list('Mean' = mean,
                                                                  'SD' = sd,
                                                                  'Min' = min,
                                                                  'Max' = max))
# # ----------------------------------------------------------------------

# 2. Correlations

cor_tab = signif(cor(df_before_scaling[,vars_used]), 2)
cor_tab[lower.tri(cor_tab, diag=F)] = ''
cor_tab = as.data.frame(cor_tab)
colnames(cor_tab) = 1:ncol(cor_tab)
rownames(cor_tab) = nm_out

# # ----------------------------------------------------------------------

# 3. Random effects correlations - main model

effects_all = CoefsExtract(models = c('riclpm_fit', 'rcgclm_fit', 'rcgclm_fit_h'),
             growth = c(paste0('i', endogeneous), 
                        paste0('s', endogeneous)))

# keeping covariances only
d_growth_cov = effects_all %>% filter(type == 'd_growth_cov') %>% 
  dplyr:: select(id, ends_with('long'))
d_growth_cov = d_growth_cov[,c('id', 'est.std.y_long')]
#d_growth_cov$est.std.y_long = gsub("\\[.*?\\]", '', d_growth_cov$est.std.y_long)

# applying the function
growthcortab_m3 = MatrixEffects(dat = d_growth_cov,
                                cor_name = 'id',
                                pars = 'est.std.y_long') 

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
dimnames(growthcortab_m3) = list(all_nam, 1:length(all_nam))

# # ----------------------------------------------------------------------

# 4.1 Regression table - main

end_new = c('Mental Health',
            'Adult Social Care',
            'Children Social Care',
            'Healthcare',
            'Environment',
            'Law and Order', 
            'Infrastructure')
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
                  'Cross-Lagged Effects of Mental Health on Spending',
                  'Cross-Lagged Effects of Spending on Mental Health',
                  'Random Intercepts',
                  'Random Slopes',
                  'Fit Measures')
fit_measures_seq = round(cbind.data.frame(est.std.x_long = fm_riclpm_fit,
                                          est.std.y_long = fm_rcgclm_fit,
                                          est.std_long = fm_rcgclm_fit_h), 3)

# create tables with effects and fit measures

TableEffects = function(dat = effects_all,
                         .end_new = end_new,
                         .parameters = parameters,
                         .section_names = section_names,
                         fit_measures = NULL) {
  
  patterns = c("~HE|HE~#",
                "~as|as~#", "~cs|cs~#", "~hc|hc~#",
                "~en|en~#", "~lo|lo~#", "~fr|fr~#")
  
  dat = dat %>%
    #filter(!grepl("~~", id)) %>%
    filter(!type %in% c('d_growth_cov', 'f_other_policies', 'g_controls')) %>%
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
seq_models_coefs[,3] = NULL # remove empty short-run for reclpm_fit
seq_models_coefs[10:15,1] = end_new[2:7]

# # ----------------------------------------------------------------------

# 4.2. Regression table - indices

indices_models_coefs_ = CoefsExtract(models = c('mental_sub1_fit',
                                                'mental_sub2_fit',
                                                'mental_sub3_fit',
                                                'mental_sub4_fit'),
                                     growth = c(paste0('i', endogeneous),
                                                paste0('s', endogeneous)))
fit_measures_indices = round(cbind.data.frame(est.std.x_long = fm_mental_sub1_fit,
                                      est.std.y_long = fm_mental_sub2_fit,
                                      est.std.x.x_long = fm_mental_sub3_fit,
                                      est.std.y.y_long = fm_mental_sub4_fit),2)

indices_models_coefs = TableEffects(dat = indices_models_coefs_,
                                    fit_measures = fit_measures_indices)

indices_models_coefs[10:15,1] = end_new[2:7]

# # ----------------------------------------------------------------------

# 4.3 Regression table - sensitivity

sensitivity_models_coefs_ = CoefsExtract(models = c('rcgclm_L_fit', 
                                                    'rcgclm_outliers_3_fit',
                                                    'rcgclm_outliers_1.5_fit'),
                                     growth = c(paste0('i', endogeneous),
                                                paste0('s', endogeneous)))
fit_measures_sensitivity = round(cbind.data.frame(est.std.x_long = fm_rcgclm_L_fit,
                                              est.std.y_long = fm_rcgclm_outliers_3_fit,
                                              est.std_long = fm_rcgclm_outliers_1.5_fit),2)

sensitivity_models_coefs = TableEffects(dat = sensitivity_models_coefs_,
                                    fit_measures = fit_measures_sensitivity)

sensitivity_models_coefs[10:15,1] = end_new[2:7]

# # ----------------------------------------------------------------------

# 5. Main model - effects of spendings on each other

f_other_policies = effects_all %>% filter(type %in% c('f_other_policies',
                                                      'a_auto') &
                                            !grepl('HE', id))
f_other_policies_long = f_other_policies[,c('id', 'est.std.y_long')]
f_other_policies_short = f_other_policies[,c('id', 'est.std.y_short')]
#f_other_policies$est.std.y_long = gsub("\\[.*?\\]", '', f_other_policies$est.std.y_long)

# applying the function
other_policies_matrix_long = MatrixEffects(dat = f_other_policies_long,
                                cor_name = 'id',
                                colnames = endogeneous[-1],
                                rownames = endogeneous[-1],
                                pars = 'est.std.y_long',
                                cor = F,
                                sep = '~') 
other_policies_matrix_short = MatrixEffects(dat = f_other_policies_short,
                                           cor_name = 'id',
                                           colnames = endogeneous[-1],
                                           rownames = endogeneous[-1],
                                           pars = 'est.std.y_short',
                                           cor = F,
                                           sep = '~') 
dimnames(other_policies_matrix_long) = list(nm_out[6:11], nm_out[6:11])
dimnames(other_policies_matrix_short) = list(nm_out[6:11], nm_out[6:11])

# # ----------------------------------------------------------------------

# 6. Main model - controls

g_controls = effects_all %>% filter(type == 'g_controls')
g_controls = g_controls[,c('id', 'est.std.y_long')]
#g_controls$est.std.y_long = gsub("\\[.*?\\]", '', g_controls$est.std.y_long)

# applying the function
controls_matrix = MatrixEffects(dat = g_controls,
                                cor_name = 'id',
                                rownames = gsub('[[:digit:]]+', '', control_names),
                                pars = 'est.std.y_long',
                                cor = F,
                                sep = '~') 
dimnames(controls_matrix) = list(nm_out[12:22], rep(nm_out[c(1,6:11)],2))
controls_inter_mat = controls_matrix[,1:7]
controls_slope_mat = controls_matrix[,8:14]

# # ----------------------------------------------------------------------

# 7. Substantive (pct) effects from all models

# obtaining a normalising ratio to shift from the log_norm to log interpretation

ratio = c()
for (i in policy_names_6){
  ratio = c(ratio, log(1.1)*(max(df[,i]) - min(df[,i])) / (max(log(df_before_scaling[,i])) - min(log(df_before_scaling[,i]))))

}
id_ratio = c("HE~as", "HE~cs", "HE~hc", "HE~en", "HE~lo", "HE~fr")
ratio_df = cbind.data.frame('id' = id_ratio, ratio)

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
            '',
            'Model 1.3')

col_ind = c('',
            'Model 2.1', '',
            'Model 2.2', '',
            'Model 2.3', '',
            'Model 2.4', '')

col_sens = c('', 
             'Model 3.1', '',
             'Model 3.2', '',
             'Model 3.3', '')


col_pct = c('', 
            'Model 1.2', '',
            'Model 2.1', '',
            'Model 2.2', '',
            'Model 2.3', '',
            'Model 2.4', '')

SubHead = function(tab, which_null = NULL, n, colnames){
  
  head = c('', rep(c('Long-run', 'Short-run'), n))
  
  if (!is.null(which_null)){
    head= head[-which_null]
  }
  
  tab = rbind.data.frame(head, tab)
  colnames(tab) = colnames
  
  return(tab)
}

# Main text

sumstat_fin

seq_models_coefs = SubHead(CiSplit(seq_models_coefs),
                           which_null = 3,
                           n = 3,
                           colnames = col_seq)
indices_models_coefs = SubHead(CiSplit(indices_models_coefs),
                               n = 4,
                               colnames = col_ind)

pct_coef = SubHead(CiSplit(pct_coef),
                   n = 5,
                   colnames = col_pct)

# Appendicies

cor_tab
growthcortab_m3 = CiSplit(growthcortab_m3, rownm = T)
sensitivity_models_coefs = CiSplit(sensitivity_models_coefs)

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

# # ----------------------------------------------------------------------

# 9. Descriptive Plots

library(ggpubr)

all_vars = c(health_vars,
             policy_names_6)
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
    scale_y_continuous(name = NULL
    ) + 
    theme(axis.text = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24)) +
    geom_line(aes(group = LAD21CD), color = "lightblue") +
    geom_smooth(method = loess, se = F, fullrange = T, color="darkred") +
    theme_pubclean()
}

# health variables
ggarrange(list_plots[[5]],
          list_plots[[4]],
          list_plots[[3]],
          list_plots[[2]],
          list_plots[[1]],
          labels = c('Incapacity Benefits Rate',
                     'Depression Rate',
                     'Antidepressants Rate',
                     'Hospital Attendances Score',
                     'Mental Health Index'
          ),
          ncol = 3, nrow = 2,
          font.label = list(size = 14), align ='h')
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/output/mhealth_lineplots.jpeg")

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
          font.label = list(size = 14))
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/output/spending_lineplots.jpeg")




