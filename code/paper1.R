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
source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.R')
options(max.print=3900)

# pre-processing
measures = c('npar',
             'chisq.scaled',
             'df.scaled',
             'cfi.scaled',
             'tli.scaled',
             'srmr',
             'rmsea.scaled',
             'rmsea.ci.lower.scaled',
             'rmsea.ci.upper.scaled',
             'agfi',
             'aic',
             'bic',
             'logl')

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')
df_before_scaling = df

## deprivation data - from the IMD R package

## 0 - lower class (1-4 decile)
## 1 - middle class (5-7 decile)
## 2 - upper class (8-10 decile)

# LSOA deprivation groups
#df = df[!is.na(df$Income_Score), ]
#df$lsoa_dep = ifelse(df$Income_decile <= 4, 1,
#                     ifelse(df$Income_decile >= 7, 2, 0))

## table(df$lsoa_dep)
##     0      1      2 
## 57915 116469 114039 

# LAD deprivation groups
#df$lad_dep = ifelse(df$Income_Score <= quantile(df$Income_Score, probs = 0.4)[[1]], 1,
#                    ifelse(df$Income_Score >= quantile(df$Income_Score, probs = 0.7)[[1]], 2, 0))

## table(df$lad_dep)
##      0      1      2 
##  83412 115785  89226 

# #removing outliers
# df = df %>% filter(health > 0 &
#                      healthcare > 0 &
#                      social > 0 &
#                      law_order > 0 &
#                      infrastructure > 0)
# summary(df)

# normalising from 0 to 10 to facilitate the convergence of SEM models
for (i in c(policy_names_6,
            health_vars,
            control_names[-length(control_names)]
            
)){
  df[, i] = normalize(df[, i])*10
}

# Flipping the sign
df$z_mh_rate = -df$z_mh_rate
df$antidep_rate = -df$antidep_rate
df$est_qof_dep = -df$est_qof_dep
df$prop_ibesa = -df$prop_ibesa
df$samhi_index = -df$samhi_index

#par(mfrow=c(2,2))
# quick dist 
hist(df$antidep_rate, breaks = 30, main = 'Meantal Health Index, normalised', )
hist(df$health, breaks = 30, main = 'Environment, Culture & Planning, \n normalised logarithm of £10,000 per capita')
hist(df$healthcare, breaks = 30)
hist(df$env, breaks = 30)
hist(df$education, breaks = 30)
hist(df$law_order, breaks = 30)
hist(df$infrastructure, breaks = 30)
hist(df$samhi_index, breaks = 30)
hist(df$antidep_rate, breaks = 30)


# final dataset - wide format
df_lavaan_mental = lavaan_df(HE = 'samhi_index',
                             df = df,
                             max_time = 7)
df_lavaan_mental = as.data.frame(na.omit(df_lavaan_mental))
summary(df_lavaan_mental)
sd(df_lavaan_mental)
std_dev = apply(df[,c(policy_names_6, health_vars)], 2, FUN = sd)
apply(df_before_scaling[,c(policy_names_6, health_vars)], 2, FUN = sd)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

# Sequential models

# 1. Random Curves
only_growth_syntax = RC_GCLM_syntax(model = 'gclm',
                                    impulses = F,
                                    past_states = F)
only_growth_fit = sem(only_growth_syntax,
                      data = df_lavaan_mental, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
fm_only_growth_fit = fitmeasures(only_growth_fit, measures)
summary(only_growth_fit, standardized=T)


# 2. Adding Impulses
growth_impulses_syntax = RC_GCLM_syntax(model = 'gclm',
                                        impulses = T,
                                        past_states = F)
growth_impulses_fit = sem(growth_impulses_syntax,
                          data = df_lavaan_mental, 
                          estimator = "mlr",
                          orthogonal = T, 
                          cluster = 'LAD21CD'
)
fm_growth_impulses_fit = fitmeasures(growth_impulses_fit, measures)
summary(growth_impulses_fit, standardized=T)

# 3. Adding Past States == FULL MODEL
growth_impulses_pastst_syntax = RC_GCLM_syntax(model = 'gclm',
                                               impulses = T,
                                               past_states = T)
growth_impulses_pastst_fit = sem(growth_impulses_pastst_syntax,
                                 data = df_lavaan_mental, 
                                 estimator = "mlr",
                                 orthogonal = T, 
                                 cluster = 'LAD21CD'
)
beepr::beep()
summary(growth_impulses_pastst_fit, standardized=T)
fm_growth_impulses_pastst_fit = fitmeasures(growth_impulses_pastst_fit, measures)
gc()
lavResiduals(growth_impulses_pastst_fit)

# ----------------------------------------------------------------------
# Indices

mental_sub_syntax = RC_GCLM_syntax(model = 'gclm')

# 1. IBESA (%)
df_lavaan_mental_sub1 = lavaan_df(HE = 'prop_ibesa',
                                df = df)
mental_sub1_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub1, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
summary(mental_sub1_fit, standardized=T)
fm_mental_sub1_fit = fitmeasures(mental_sub1_fit, measures)

# 2. QOF – Depression (%)
df_lavaan_mental_sub2 = lavaan_df(HE = 'est_qof_dep',
                                  df = df)
#df_lavaan_mental_sub2 %<>% filter(!lsoa11 %in% out)
mental_sub2_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub2, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
summary(mental_sub2_fit, standardized=T)
fm_mental_sub2_fit = fitmeasures(mental_sub2_fit, measures)

# 3. ADQ of antidepressants per person
df_lavaan_mental_sub3 = lavaan_df(HE = 'antidep_rate',
                                  df = df)
mental_sub3_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub3, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
fm_mental_sub3_fit = fitmeasures(mental_sub3_fit, measures)
summary(mental_sub3_fit, standardized=T)

# 4. Hospital admissions (z-scores)
df_lavaan_mental_sub4 = lavaan_df(HE = 'z_mh_rate',
                                  df = df)
mental_sub4_fit = sem(mental_sub_syntax,
                      data = df_lavaan_mental_sub4, 
                      estimator = "mlr",
                      orthogonal = T, 
                      cluster = 'LAD21CD'
)
beepr::beep()
summary(mental_sub4_fit, standardized=T)
fm_mental_sub4_fit = fitmeasures(mental_sub4_fit, measures)

# # ----------------------------------------------------------------------
# 
# # cross-lagged for comparison
# clpm_syntax = RC_GCLM_syntax(model = 'clpm', 
#                              cross = T)
# clpm_fit = sem(clpm_syntax,
#                data = df_lavaan_mental, 
#                estimator = "mlr",
#                orthogonal = T, 
#                cluster = 'LAD21CD'
# )
# fitmeasures(clpm_fit, measures)
# summary(clpm_fit, standardized=T)
# 
# # ----------------------------------------------------------------------
# 
# ## social care only
# hpolicy_syntax = RC_GCLM_syntax(endogeneous = c('HE', 'sc'), 
#                                 model = 'gclm',
#                                 cross = T)
# hpolicy_fit = sem(hpolicy_syntax,
#                   data = df_lavaan_mental, 
#                   estimator = "mlr",
#                   orthogonal = T, 
#                   cluster = 'LAD21CD'
# )
# fitmeasures(hpolicy_fit, measures)
# # comparing models
# summary(hpolicy_fit, standardized=T)
# # ----------------------------------------------------------------------

# Output

# 1. Sample Description
stationary = c('lsoa_ses_score', 'pop_census11', 'nonwhite', 'females', 'older', 'rural', 'n')
nonstationary = c('samhi_index', 'prop_ibesa', 'est_qof_dep', 'antidep_rate',
              'z_mh_rate', 'health', 'healthcare', 'education', 'env', 'law_order',
              'infrastructure')
vars_used = c(nonstationary, stationary)
sumstat = df_before_scaling %>%
  
  # Select and rename five variables 
  dplyr::select(
   all_of(nonstationary), year
  ) %>%
  group_by(year) %>%
  
  # Find the mean, st. dev., min, and max for each variable 
  summarise(across(everything(),
                   list(mean = mean, sd = sd),
                   .names = "{.col}__{.fn}"))

sumstat =  as.data.frame(t(sumstat[-1]))
colnames(sumstat) = 2013:2019
sumstat$id = rownames(sumstat)
rownames(sumstat) = NULL

sumstat %<>%
  separate(id, into = c("variable", "stat"), sep = "__") %>%
  pivot_wider(id_cols = variable, names_from = stat, values_from = '2013':'2019')

overall = df_before_scaling %>%
  dplyr::select(all_of(vars_used)) %>% 
  summarise(across(everything(),
                   list(mean = mean, sd = sd),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(names_to = 'key',
               values_to = 'value',
               cols = everything())%>% 
  separate(key, into = c("variable", "stat"), sep = "__") %>%
  pivot_wider(id_cols = variable, names_from = stat, values_from = value)

sumstat_fin = sumstat %>% full_join(overall) %>%
  mutate_each(list(round(., 2)), -variable)
sumstat_fin = as.data.frame(apply(sumstat_fin, 2, function(x) ifelse(is.na(x), '', x)))
colnames(sumstat_fin) = c('', '2013', '', '2014', '', '2015', '', '2016', '',
                          '2017', '', '2018', '', '2019', '', 'Total', '' )
stat_names = as.data.frame(matrix(nrow = 1, ncol=ncol(sumstat_fin)))
colnames(stat_names) = colnames(sumstat_fin)
stat_names[1,] = c('', rep(c('Mean', 'SD'), 8))
sumstat_fin = rbind.data.frame(stat_names, sumstat_fin)
nm = c('', 'Mental Health Index',
       'Incapacity Benefits Rate',
       'Depression Rate',
       'Antidepressants Rate',
       'Hospital Attendances Score',
       'Public Health & Social Care',
       'Healthcare',
       'Education',
       'Environment',
       'Law and Order',
       'Infrastructure',
       'IMD (inc., educ., empl. domains)',
       'LSOA population size',
       'Non-white, LSOA prop.',
       'Females, LSOA prop.',
       'Older, LSOA prop.',
       'Rural, prop. of rural LSOAs',
       'Number of LSOAs in a LAD')
sumstat_fin[,1] = nm

# 2. Correlations

cor = signif(cor(df_before_scaling[,vars_used]), 2)
cor[lower.tri(cor, diag=F)] = ''
colnames(cor) = 1:ncol(cor)
cor[,1] = nm[-1]
rownames(cor) = nm[-1]

# stargazer(cor, 
#           summary = FALSE, 
#           type = 'text', 
#           initial.zero = FALSE, 
#           digits=2)



# 3. Random Effects Correlations (1+)

effects_all = CoefsExtract(models = c('only_growth_fit',
                                   'growth_impulses_fit',
                                   'growth_impulses_pastst_fit'),
             growth = c(paste0('i', endogeneous), paste0('s', endogeneous)))


temp = effects_all %>% filter(grepl('~~', id)) %>% 
  dplyr:: select(id, ends_with('long'))


# a function to extract growth curve cor tables
GrowthCorTable = function(dat, cor_name = NULL, pars = NULL){

  dat = dat[c(cor_name, pars)]
  
  sgn = dat
  sgn %<>% tidyr::separate(cor_name, c('X', 'Y'), '~~')
  
  dat[,pars] = as.numeric(sub('\\*\\**\\**|\\^', '', dat[,pars]))
  dat %<>% tidyr::separate(cor_name, c('X', 'Y'), '~~')
  pivot_initial = as.data.frame(with(dat,tapply(get(pars), list(Y,X), FUN = identity)))
  for (row in 1:nrow(pivot_initial)){
    for (col in 1:ncol(pivot_initial)){
      pivot_initial[row,col] = ifelse(is.na(pivot_initial[row,col]),
                                      pivot_initial[col,row],
                                      pivot_initial[row,col])
    }
  }
  # pivot_initial = lessR::corReorder(R=as.matrix(pivot_initial),
  #                                   vars = c(iHE, ihe, ihc, ied, ien,
  #                                            ilo, iir, sHE, she,
  #                                            shc, sed, sen, slo, sir))
  pivot_initial[lower.tri(pivot_initial, diag = F)] = ''
  #pivot = apply(pivot_initial, 2, function(x) ifelse(is.na(x), '', x))
  pivot = cov2cor(getCov(as.matrix(pivot_initial, lower=F)))
  
  pivot = as.data.frame(round(pivot, digits = 2))
  fun = function(x) {
    x = ifelse(x<0, paste0('-', substr(as.character(x), 3, 5)), 
               ifelse(x>0 & !x==1, substr(as.character(x), 2, 4), 
                      ifelse(x==1, '1.00', '.00')))
    x = ifelse(x == 0.0, paste0(x, '0'), x)
    return(x)
  }
  pivot = as.data.frame(as.matrix(sapply(pivot, fun)))
  pivot[lower.tri(pivot, diag = F)] <- ''
  dimnames(pivot) = list(growth, growth)
  
  for (i in growth){
    for (j in growth){
      if (!i==j){
        pivot[i,j] = paste0(pivot[i,j], str_replace_all(sgn[sgn$X==i&sgn$Y==j,pars], "[:digit:]|-|\\.", ""))
      }
    }
  }
  
  
  # col and row names
  all_nam = c('Intercept Mental Health',
              'Intercept Public Health & Social Care',
              'Intercept Healthcare',
              'Intercept Education',
              'Intercept Environment',
              'Intercept Law and Order',
              'Intercept Infrastructure',
              'Slope Mental Health',
              'Slope Public Health & Social Care',
              'Slope Healthcare',
              'Slope Education',
              'Slope Environment',
              'Slope Law and Order',
              'Slope Infrastructure'
  )
  dimnames(pivot) = list(all_nam, all_nam)
  
  return(pivot)
}


# applying the function
growthcortab_m3 = GrowthCorTable(dat=temp, cor_name = 'id', pars = 'est.std_long') 


# 4. Regression Table (1)
end_new = c('Mental Health',
                      'Healthcare',
                      'Social Care and Public Health',
                      'Education',
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

TableEffects = function(dat = effects_all, 
                        .end_new = end_new,
                        .parameters = parameters,
                        .section_names = section_names,
                        fit_measures = NULL){
  
  # filtering effects of interest
  dat %<>% filter(!grepl('~~', id))
  
  # renaming
  dat[,'id'] = ifelse(grepl('~HE|HE~#', dat[,'id']), .end_new[1], dat[,'id'])
  dat[,'id'] = ifelse(grepl('~hc|hc~#', dat[,'id']), .end_new[2], dat[,'id'])
  dat[,'id'] = ifelse(grepl('~he|he~#', dat[,'id']), .end_new[3], dat[,'id'])
  dat[,'id'] = ifelse(grepl('~ed|ed~#', dat[,'id']), .end_new[4], dat[,'id'])
  dat[,'id'] = ifelse(grepl('~en|en~#', dat[,'id']), .end_new[5], dat[,'id'])
  dat[,'id'] = ifelse(grepl('~lo|lo~#', dat[,'id']), .end_new[6], dat[,'id'])
  dat[,'id'] = ifelse(grepl('~ir|ir~#', dat[,'id']), .end_new[7], dat[,'id'])
  #dat$id = ifelse(grepl('~~', dat$id), dat$id,
  #                      sapply(dat$id, FUN = function(x) substr(x,3,nchar(x))))
  
  # cleaning
  fit_measures = cbind.data.frame(id = rownames(fit_measures),fit_measures)
  fit_measures$id = sub('.scaled', '', fit_measures$id)
  dat = rbind.fill(dat, fit_measures)
  dat[is.na(dat)] = ''
  dat[1:15,4] = dat[1:15,5]
  dat[30:42,1] = .parameters
  
  # subsections in a table
  rows = c(1, 8, 10, 16, 23, 30)
  
  for (i in rev(rows)) {
    dat = tibble::add_row(dat, .before = i)
  }
  
  dat[is.na(dat[,1]),1] = .section_names
  dat = apply(dat, 2, function(x) ifelse(is.na(x), '', x))
  dat = as.data.frame(dat)
  
  return(dat)
}

seq_models_coefs = TableEffects(fit_measures = fit_measures_seq)
seq_models_coefs[,c(3,5)] = NULL
colnames(seq_models_coefs) = c('', 'Growth Curve Only', 'RE-CLPM', '', 'RE-GCLM')

#saving
write.table(seq_models_coefs, file = "seq_models_coefs.txt",
            sep = ",", quote = FALSE, row.names = F)

# 5. Regression Table (2)
indices_models_coefs_ = CoefsExtract(models = c('mental_sub1_fit',
                                                'mental_sub2_fit',
                                                'mental_sub3_fit',
                                                'mental_sub4_fit'),
                                     growth = c(paste0('i', endogeneous), paste0('s', endogeneous)))
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

# saving
write.table(indices_models_coefs, file = "indices_models_coefs.txt",
            sep = ",", quote = FALSE, row.names = F)


# 6. Substantive effects from all models
sgnif = cross_only

n = ncol(indices_models_coefs)
cross_only = cbind(seq_models_coefs[13:18,c(1,4:5)],
                   indices_models_coefs[13:18,2:n])


cross_only[,2:n] = apply(cross_only[,2:n], 2, 
      FUN = function(x) as.numeric(sub('\\*\\**\\**|\\^', '', x)))


for (i in growth){
  for (j in growth){
    if (!i==j){
      pivot[i,j] = paste0(pivot[i,j], str_replace_all(sgn[sgn$X==i&sgn$Y==j,pars], "[:digit:]|-|\\.", ""))
    }
  }
}

# 7. (Optional) Effects of Controls




































































