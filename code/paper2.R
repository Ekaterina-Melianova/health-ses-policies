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

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.R')
options(max.print=3400)

# pre-processing

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')

# LAD deprivation groups
df = df %>% group_by(LAD21CD, year) %>%
  dplyr::mutate(pop_prop = pop_census11/sum(pop_census11),
                lsoa_ses_score_w = pop_prop*lsoa_ses_score,
                lad_ses_score = sum(lsoa_ses_score_w)) %>%
  select(lsoa11, year, LAD21CD,
         pop_prop, lsoa_ses_score_w,
         lad_ses_score, lsoa_ses_score, everything()) %>% ungroup()


specify_probs = function(df, probs) {
  for (i in seq_along(probs)) {
    quantile_val = quantile(df$lad_ses_score, probs = unlist(probs[i]))
    
    df[[paste0("lsoa_dep_", i)]] = ifelse(df$lad_ses_score  <= quantile_val[1], 1,
                                           ifelse(df$lad_ses_score  >= quantile_val[2], 2, 0))
  }
  
  return(df)
}
df = specify_probs(df, probs = list(c(0.5, 0.5), c(0.4, 0.6), c(0.3, 0.7)))
table(df$lsoa_dep_1)

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



# final dataset - wide format
df_lv = lavaan_df(dv = 'samhi_index',
                  deprivation_cat = 'lsoa_dep_1',
                  df = df)
df_lv = as.data.frame(na.omit(df_lv))
summary(df_lv)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

# LSOA deprived vs. not deprived

# a. free model
group_free_syntax = RC_GCLM_syntax(multiple = T,
                                   control = control_names[-3])
group_free_fit = sem(group_free_syntax,
                            data = df_lv, 
                            estimator = "mlr",
                            orthogonal = T, 
                            cluster = 'LAD21CD',
                            group = 'lsoa_dep'
)
beepr::beep()
gc()
fm_group_free = fitmeasures(group_free_fit, measures)
group_free_std = broom::tidy(group_free_fit)


# running constrained models

equality_params = c('b_HEhc1,b_HEhc2',
                    'b_HEas1,b_HEas2',
                    'b_HEcs1,b_HEcs2',
                    'b_HElo1,b_HElo2',
                    'b_HEen1,b_HEen2',
                    'b_HEfr1,b_HEfr2',
                    
                    'd_HEhc1,d_HEhc2',
                    'd_HEas1,d_HEas2',
                    'd_HEcs1,d_HEcs2',
                    'd_HElo1,d_HElo2',
                    'd_HEen1,d_HEen2',
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

lsoa_group = c('lsoa_dep_1', 'lsoa_dep_2', 'lsoa_dep_3')

list_combined <- list()
i <- 1
for (lsoa in lsoa_group){
  
  for (synt in syntax){
    
    list_combined[[i]] <- list(lsoa, synt)
    i <- i + 1
    
  }
}

groupSEM = function(comb){
  
  lsoa <- comb[[1]]
  synt <- comb[[2]]
  
  list_result = list()
  
  df_lv = lavaan_df(dv = 'samhi_index',
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
  
  #list_result[[2]] = as.data.frame(anova(list_result[[1]], group_free_fit))
  
  return(list_result)
  
}

# running

#syntax = syntax[c(5,6)]
#lsoa_group = lsoa_group[2:3]

list_results_sens = list()

cluster = makeCluster(12) 
registerDoParallel(cluster)

tic()
list_results = foreach(comb = list_combined,
                       .packages = c('lavaan', 'tidyverse',
                                     'dplyr', 'tidyr',
                                     'magrittr'),
                       .combine = c) %dopar% {
                         groupSEM(comb)
                       }

toc()
stopCluster(cluster)





# # ----------------------------------------------------------------------

# Output

# 1. Sample Description

stationary = c(control_names[-3], 'lsoa_dep')
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

sumstat_dep1 = summarize_data(dat = df_before_scaling[df_before_scaling$lsoa_dep == 1,],
                              rownames = c(nm_out[-14], 'IMD binary'), quant = F)
sumstat_dep2 = summarize_data(dat = df_before_scaling[df_before_scaling$lsoa_dep == 2,],
                              rownames = c(nm_out[-14], 'IMD binary'), quant = F)

# 
# # 2. Correlations
# cor = signif(cor(df_before_scaling[,vars_used]), 2)
# cor[lower.tri(cor, diag=F)] = ''
# colnames(cor) = 1:ncol(cor)
# rownames(cor) = nm_out[-1]
# rownames(cor)[12] = 'LSOA Income and Employment Deprivation'

# 3. Regression Table
group_free_tab = CoefsExtract(models = 'group_free_fit')

# 4. Anova Results
anova_all = rbind.data.frame(anova_list[[1]][1,],
  do.call(rbind, lapply(anova_list, function(df) df[2, ])))

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
pct_coef = CoefsExtract(models = 'group_free_fit',
                        standardized = F,
                        controls = NULL,
                        df_transform = ratio_df) %>% 
  filter(type_1 == 'c_policy') %>%
  select(-c(type_1, type_2))


pct_coef$id = c('Adult Social Care',
                'Children Social Care',
                'Healthcare',
                'Environment',
                'Law and Order',
                'Infrastructure')

# # ----------------------------------------------------------------------

# 5. Descriptive Plots
all_vars = c(health_vars,
             policy_names_6)
df_before_scaling$lsoa_dep = factor(df_before_scaling$lsoa_dep,
                           levels = 1:2,
                           labels = c('LSOA Income and Employment Deprivation < 50%',
                                      'LSOA Income and Employment Deprivation > 50%'))
vars_original_names = c('samhi_index',
                        'prop_ibesa',
                        'antidep_rate',
                        'est_qof_dep',
                        'z_mh_rate',
                        'health',
                        'healthcare',
                        'education',
                        'env',
                        'law_order',
                        'infrastructure')

# Mental indices comparison by lsoa_dep
panel_df1 = df_before_scaling %>% 
  group_by(year, lsoa_dep) %>%
  dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = samhi_index:infrastructure) %>%
  ungroup() 
panel_df1$name = factor(panel_df1$name,
                       levels = vars_original_names,
                       labels = nm_out[2:12])

panel_df1  %>% 
  filter(name %in% nm_out[2:6]) %>%
  ggplot(aes(year, value)) +
  scale_x_continuous(name = NULL, 
                     breaks = 2013:2019)+ 
  scale_y_continuous(name = NULL
  ) + 
  theme(axis.text = element_text(size = 24))  +
  geom_smooth(aes(color = lsoa_dep, fill = lsoa_dep),
              method = loess, se = T, fullrange = T,
              formula = y ~ x) + 
  facet_wrap(~ name, scales = 'free') +
  theme_pubclean() + 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom') + 
  scale_fill_manual(values = c("#0072B2", "#CC79A7")) +
  scale_colour_manual(values = c("#0072B2", "#CC79A7"))


# Spending comparison by lsoa_dep
panel_df2 = df_before_scaling %>% 
  distinct(LAD21CD, year, .keep_all = TRUE) %>%
  group_by(year, lsoa_dep) %>%
  dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = samhi_index:infrastructure) %>%
  ungroup() 
panel_df2$name = factor(panel_df2$name,
                        levels = vars_original_names,
                        labels = nm_out[2:12])

panel_df2  %>% 
  filter(!name %in% nm_out[2:6]) %>%
  ggplot(aes(year, value)) +
  scale_x_continuous(name = NULL, 
                     breaks = 2013:2019)+ 
  scale_y_continuous(name = NULL
  ) + 
  theme(axis.text = element_text(size = 24))  +
  geom_smooth(aes(color = lsoa_dep, fill = lsoa_dep),
              method = loess, se = T, fullrange = T,
              formula = y ~ x) + 
  facet_wrap(~ name, scales = 'free') +
  theme_pubclean() + 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom')  + 
  scale_fill_manual(values = c("#0072B2", "#CC79A7")) +
  scale_colour_manual(values = c("#0072B2", "#CC79A7"))
