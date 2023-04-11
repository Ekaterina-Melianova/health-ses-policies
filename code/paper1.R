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
for (i in c(health_vars,
            control_names[!control_names %in% c('public_health_mean',
                                                'inc_mean',
                                                'rural',
                                                'London',
                                                'SD')]
            
)){
  df[, i] = normalize(df[, i])*10
}
for (i in c(policy_names_6,
            control_names[control_names %in% c('public_health_mean', 
                                               'inc_mean')])){
  df[, i] = normalize(log(df[, i]))*10
}

# Flipping the sign
df$z_mh_rate = -df$z_mh_rate
df$antidep_rate = -df$antidep_rate
df$est_qof_dep = -df$est_qof_dep
df$prop_ibesa = -df$prop_ibesa
df$samhi_index = -df$samhi_index

# class
df$London = ifelse(df$class == 'L', 1, 0)
#df$MD = ifelse(df$class == 'MD', 1, 0)
df$SD = ifelse(df$class == 'SD', 1, 0)


# final dataset - wide format
df_lavaan_mental = lavaan_df(dv = 'samhi_index',
                             df = df)
df_lavaan_mental = as.data.frame(na.omit(df_lavaan_mental))
summary(df_lavaan_mental)

#par(mfrow=c(2,2))
# quick dist 
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

# Sequential

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

# 2. RC-CLPM
rcclpm_syntax = RC_GCLM_syntax(model = 'reclpm')
rcclpm_fit = sem(rcclpm_syntax,
                 data = df_lavaan_mental,
                 estimator = "mlr",
                 orthogonal = T,
                 cluster = 'LAD21CD'
)
beepr::beep()
fm_rcclpm_fit = fitmeasures(rcclpm_fit, measures)
# summary(rcclpm_fit, standardized=T)

# 3. RI-GCLM
regclm_syntax = RC_GCLM_syntax(model = 'regclm',
                               no_slopes = no_slopes)
regclm_fit = sem(regclm_syntax,
                 data = df_lavaan_mental, 
                 estimator = "mlr",
                 orthogonal = T, 
                 cluster = 'LAD21CD'
)
beepr::beep()
fm_regclm_fit = fitmeasures(regclm_fit, measures)
#summary(regclm_fit, standardized=T)
gc()


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

# 2. Without Outliers: 3*IQR

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

# 3. Without Outliers: 1.5*IQR

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

# 3. Different time lag

# df_2lag = df %>% filter(year %in% c(2013, 2015, 2017, 2019)) %>%
#   mutate(time2 = case_when(time == 1 ~ 1,
#                            time == 3 ~ 2,
#                            time == 5 ~ 3,
#                            time == 7 ~ 4))
# table(df_2lag$time2)
# 
# df_lavaan_mental2 = lavaan_df(dv = 'samhi_index',
#                              df = df_2lag,
#                              time = 'time2')
# df_lavaan_mental2 = as.data.frame(na.omit(df_lavaan_mental2))
# #summary(df_lavaan_mental2)
# 
# rcgclm_syntax2 = RC_GCLM_syntax(model = 'regclm',
#                                 max_time = 4,
#                                 no_slopes = c('sen ',
#                                               '~~ sen'))
# rcgclm_fit2 = sem(rcgclm_syntax2,
#                  data = df_lavaan_mental2, 
#                  estimator = "mlr",
#                  orthogonal = T, 
#                  cluster = 'LAD21CD'
# )
# beepr::beep()
# #summary(rcgclm_fit2, standardized=T)
# fm_rcgclm_fit2 = fitmeasures(rcgclm_fit2, measures)

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

# Output

# 1. Sample Description
stationary = c('lsoa_ses_score', 'pop_census11', 'nonwhite', 'females', 'older', 'rural', 'n')
nonstationary = c('samhi_index', 'prop_ibesa', 'est_qof_dep',
                  'antidep_rate',   'z_mh_rate', 
                  'social_care_adult', 'social_care_children', 'healthcare',
                  'env', 'law_order', 'infrastructure')
vars_used = c(nonstationary, stationary)

sumstat_fin = summarize_data(dat = df_before_scaling, stat = list('Mean' = mean,
                                                                  'SD' = sd,
                                                                  'Min' = min,
                                                                  'Max' = max))
# 2. Correlations

cor = signif(cor(df_before_scaling[,vars_used]), 2)
cor[lower.tri(cor, diag=F)] = ''
colnames(cor) = 1:ncol(cor)
rownames(cor) = nm_out[-1]

# 3. Random Effects Correlations (1+)

effects_all = CoefsExtract(models = c('only_growth_fit',
                                   'growth_impulses_fit',
                                   'growth_impulses_pastst_fit'),
             growth = c(paste0('i', endogeneous), 
                        paste0('s', endogeneous)))

# keeping covariances only
temp = effects_all %>% filter(grepl('~~', id)) %>% 
  dplyr:: select(id, ends_with('long'))

# applying the function
growthcortab_m3 = GrowthCorTable(dat = temp,
                                 cor_name = 'id',
                                 pars = 'est.std_long') 


# 4.1 Regression Table - Main
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
fit_measures_seq = round(cbind.data.frame(est.std.x_long = fm_only_growth_fit,
                                      est.std.y_long = fm_growth_impulses_fit,
                                      est.std_long = fm_growth_impulses_pastst_fit),2)

# create tables with effects and fit measures

TableEffects = function(dat = effects_all,
                         .end_new = end_new,
                         .parameters = parameters,
                         .section_names = section_names,
                         fit_measures = NULL) {
  
  patterns = c("~HE|HE~#",
                "~as|as~#", "~cs|cs~#", "~hc|hc~#",
                "~en|en~#", "~lo|lo~#", "~ir|ir~#")
  
  dat = dat %>%
    filter(!grepl("~~", id)) %>%
    mutate(id = reduce(patterns, function(x, y) if_else(grepl(y, x),
                                                        .end_new[match(y, patterns)], x), 
                       .init = id)) %>%
    rbind.fill(., fit_measures %>% rownames_to_column("id") %>% 
                 mutate(id = str_remove(id, ".scaled"))) %>%
    mutate(across(1, ~replace(.x, 30:42, .parameters)))
  
  # subsections in a table
  for (i in rev(c(1, 8, 11, 17, 24, 31))) {
    dat = tibble::add_row(dat, .before = i)
  }
  
  dat = dat %>%
    mutate(across(1, ~replace(.x, is.na(.x), .section_names)))%>%
    mutate_all(~ ifelse(is.na(.), "", .))
  
  
  return(dat)
}

seq_models_coefs = TableEffects(fit_measures = fit_measures_seq)
seq_models_coefs[,3] = NULL
colnames(seq_models_coefs) = c('', 'Growth Curve Only', 'RE-CLPM', '', 'RE-GCLM')
seq_models_coefs[10:12,1] = end_new[2:4]

#saving
#write.table(seq_models_coefs, file = "seq_models_coefs.txt",
#            sep = ",", quote = FALSE, row.names = F)

# 4.2. Regression Table - Indices
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

colnames(indices_models_coefs) = c('', 'IBESA', '',
                               'Depression', '',
                               'Antidepressants', '',
                               'Hospital Admissions', '')

indices_models_coefs[10:12,1] = end_new[2:4]

# saving
#write.table(indices_models_coefs, file = "indices_models_coefs.txt",
#            sep = ",", quote = FALSE, row.names = F)

# 4.3 Regression Table - Sensitivity




# 5. Substantive effects from all models

rng = range(log(df_before_scaling[,'social_care_adult']))
exp(0.018*((rng[2] - rng[1])/10 + rng[2])) - 1
0.018*((rng[2] - rng[1])/10 + rng[2])/10


n = ncol(indices_models_coefs)
cross_only = cbind(seq_models_coefs[13:18,c(1,4:5)],
                   indices_models_coefs[13:18,2:n])
sgnif = cross_only
coefs = cross_only
sterr = cross_only

# coefs
coefs[,2:ncol(cross_only)] = apply(coefs[,2:ncol(coefs)], 2, 
                         FUN = function(x) {
                           as.numeric(str_replace_all(x, c("\\*\\**\\**|\\^" = '',
                                                           '\\[.*\\]' = '')))
                           }
                         )
std_dev = apply(df_before_scaling[,policy_names_6], 2, FUN = sd)*10
coefs = cbind.data.frame(coefs, std_dev)
coefs[,2:ncol(coefs)] = coefs[,2:ncol(coefs)]/coefs[,'std_dev']

# standard errors
sterr[,2:ncol(sterr)] = as.numeric(apply(sterr[,2:ncol(sterr)], 2, 
                                   FUN = function(x) {
                                     gsub("[^\\[\\]]*\\[([^\\[\\]]+)\\][^\\[\\]]*", "\\1", x, perl=TRUE)
                                   }
))
sterr = cbind.data.frame(sterr, std_dev)
sterr[,2:ncol(sterr)] = sterr[,2:ncol(sterr)]/sterr[,'std_dev']

# merging with sign stars
for (i in 1:nrow(coefs)){
  for (j in 2:ncol(coefs)){
    coefs[i,j] = paste0(sprintf('%.3f', as.numeric(coefs[i,j])),
                        gsub("[^*^]+", "", sgnif[i,j]),
                        ' [',
                        sprintf('%.3f', as.numeric(sterr[i,j])),
                        ']')
    
    
  }
}

coefs_substantive = coefs[,1:(ncol(coefs)-1)]
colnames(coefs_substantive) = c('', 'SAMHI', '',
                                'IBESA', '',
                                'Depression', '',
                                'Antidepressants', '',
                                'Hospital Admissions', '')
head = rbind.data.frame(c('', rep(c('Long-run', 'Short-run'), 5)))
colnames(head) = colnames(coefs_substantive)
coefs_substantive = rbind(head, coefs_substantive)


#### code for table cleaning (optional)

# Create example data frame
ddf <- data.frame(ID = 1:3, col1 = c("0.014*^ [0.006]", "0.023*^ [0.004]", "0.011*^ [0.008]"), col2 = c("0.012*^ [0.003]", "0.021*^ [0.005]", "0.017*^ [0.006]"))

ddf = seq_models_coefs
# Extract values in brackets and create new data frame
new_df <- data.frame(matrix(nrow = nrow(ddf)*2, ncol = ncol(ddf)))
new_df[,1] <- ddf[,1] # copy IDs from original data frame
for (i in 1:nrow(ddf)) {
  for (j in 2:ncol(ddf)) {
    x <- ddf[i,j]
    v1 <- gsub(".*\\[(.*?)\\].*", "\\1", x) # extract value in brackets
    v2 <- gsub("\\[.*\\]", "", x) # remove value in brackets
    new_df[(i-1)*2+1,j] <- v2 # add value to first row
    new_df[(i-1)*2+2,j] <- v1 # add value to second row
  }
}
####

# 6. (Optional) Effects of Controls

#
#
#

# 7. Descriptive Plots
library(ggpubr)

all_vars = c(health_vars,
             policy_names_6)
panel_df = df_before_scaling %>% #filter(class=='L') %>%
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




