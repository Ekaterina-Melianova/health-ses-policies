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

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/code/functions.R')
options(max.print=3400)

# pre-processing

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')

## deprivation 

# LAD deprivation groups
df %<>% group_by(LAD21CD) %>%
  #dplyr::mutate(lad_ses_score = mean(lsoa_ses_score)) %>%
  ungroup() %>%
  dplyr::mutate(lsoa_dep = ifelse(lsoa_ses_score < quantile(lsoa_ses_score, probs = 0.5)[[1]], 1,
                                 ifelse(lsoa_ses_score >= quantile(lsoa_ses_score, probs = 0.5)[[1]], 2, 0)))
table(df$lsoa_dep)
df = df %>% filter(lsoa_dep > 0)

# a dataset for descriptives
df_before_scaling = df
df_before_scaling$z_mh_rate = -df_before_scaling$z_mh_rate
df_before_scaling$antidep_rate = -df_before_scaling$antidep_rate
df_before_scaling$est_qof_dep = -df_before_scaling$est_qof_dep
df_before_scaling$prop_ibesa = -df_before_scaling$prop_ibesa
df_before_scaling$samhi_index = -df_before_scaling$samhi_index

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
hist(df$antidep_rate[df$lsoa_dep==1], breaks = 30)
hist(df$health, breaks = 30)
hist(df$healthcare[df$lsoa_dep==1], breaks = 30)
hist(df$env[df$lsoa_dep==1], breaks = 30)
hist(df$education[df$lsoa_dep==1], breaks = 30)
hist(df$law_order[df$lsoa_dep==1], breaks = 30)
hist(df$infrastructure[df$lsoa_dep==1], breaks = 30)
hist(df$samhi_index, breaks = 30)
hist(df$antidep_rate, breaks = 30)

# final dataset - wide format
df_lavaan_mental = lavaan_df(dv = 'samhi_index',
                             df = df,
                             max_time = 7, 
                             deprivation_cat = 'lsoa_dep')
df_lavaan_mental = as.data.frame(na.omit(df_lavaan_mental))
summary(df_lavaan_mental)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

# LSOA deprived vs. not deprived

# a. free model
group_free_syntax = RC_GCLM_syntax(multiple = T,
                                   control = control_names[-1])
group_free_fit = sem(group_free_syntax,
                            data = df_lavaan_mental, 
                            estimator = "mlr",
                            orthogonal = T, 
                            cluster = 'LAD21CD',
                            group = 'lsoa_dep'
)
beepr::beep()
#summary(group_free_fit, fit.measures=T, standardized=T)
gc()
fm_group_free = fitmeasures(group_free_fit, measures)
group_free_std = standardizedSolution(group_free_fit)

# b. fixed for social care/public health
group_SCfixed_syntax = RC_GCLM_syntax(multiple = T,
                                      control = control_names[-1],
                                      group_equality = 'b_HEhe1,b_HEhe2')
group_SCfixed_fit = sem(group_SCfixed_syntax,
                               data = df_lavaan_mental, 
                               estimator = "mlr",
                               orthogonal = T, 
                               cluster = 'LAD21CD',
                               group = 'lsoa_dep'
)
beepr::beep()
#summary(group_SCfixed_fit, fit.measures=T, standardized=T)
gc()

# compare
anova_SC = as.data.frame(anova(group_SCfixed_fit, group_free_fit))

# c. fixed for healthcare
group_HCfixed_syntax = RC_GCLM_syntax(multiple = T,
                                      control = control_names[-1] ,
                                      group_equality = 'b_HEhc1,b_HEhc2')
group_HCfixed_fit = sem(group_HCfixed_syntax,
                               data = df_lavaan_mental, 
                               estimator = "mlr",
                               orthogonal = T, 
                               cluster = 'LAD21CD',
                               group = 'lsoa_dep'
)
beepr::beep()
#summary(group_HCfixed_fit, fit.measures=T, standardized=T)
gc()

# compare
anova_HC = as.data.frame(anova(group_HCfixed_fit, group_free_fit))

# d. fixed for education
group_EDfixed_syntax = RC_GCLM_syntax(multiple = T,
                                      control = control_names[-1] ,
                                      group_equality = 'b_HEed1,b_HEed2')
group_EDfixed_fit = sem(group_EDfixed_syntax,
                        data = df_lavaan_mental, 
                        estimator = "mlr",
                        orthogonal = T, 
                        cluster = 'LAD21CD',
                        group = 'lsoa_dep'
)
beepr::beep()
#summary(group_EDfixed_fit, fit.measures=T, standardized=T)
gc()

# compare
# non-significant
anova_ED = as.data.frame(anova(group_EDfixed_fit, group_free_fit))

# e. fixed for environment
group_ENfixed_syntax = RC_GCLM_syntax(multiple = T,
                                      control = control_names[-1],
                                      group_equality = 'b_HEen1,b_HEen2')
group_ENfixed_fit = sem(group_ENfixed_syntax,
                        data = df_lavaan_mental, 
                        estimator = "mlr",
                        orthogonal = T, 
                        cluster = 'LAD21CD',
                        group = 'lsoa_dep'
)
beepr::beep()
#summary(group_ENfixed_fit, fit.measures=T, standardized=T)

gc()

# compare
anova_EN = as.data.frame(anova(group_ENfixed_fit, group_free_fit))

# f. fixed for law & order
group_LOfixed_syntax = RC_GCLM_syntax(multiple = T,
                                      control = control_names[-1] ,
                                      group_equality = 'b_HElo1,b_HElo2')
group_LOfixed_fit = sem(group_LOfixed_syntax,
                        data = df_lavaan_mental, 
                        estimator = "mlr",
                        orthogonal = T, 
                        cluster = 'LAD21CD',
                        group = 'lsoa_dep'
)
beepr::beep()
#summary(group_LOfixed_fit, fit.measures=T, standardized=T)
gc()

# compare
anova_LO = as.data.frame(anova(group_LOfixed_fit, group_free_fit))

# g. fixed for infrastructure
group_IRfixed_syntax = RC_GCLM_syntax(multiple = T,
                                      control = control_names[-1] ,
                                      group_equality = 'b_HEir1,b_HEir2')
group_IRfixed_fit = sem(group_IRfixed_syntax,
                        data = df_lavaan_mental, 
                        estimator = "mlr",
                        orthogonal = T, 
                        cluster = 'LAD21CD',
                        group = 'lsoa_dep'
)
beepr::beep()
#summary(group_IRfixed_fit, fit.measures=T, standardized=T)
gc()

# compare
anova_IR = as.data.frame(anova(group_IRfixed_fit, group_free_fit))

# # ----------------------------------------------------------------------

# Output

# 1. Sample Description

stationary = c('pop_census11', 'nonwhite', 'females', 'older', 'rural', 'n')
nonstationary = c('samhi_index', 'prop_ibesa', 'est_qof_dep', 'antidep_rate',
                  'z_mh_rate', 'health', 'healthcare', 'education', 'env', 'law_order',
                  'infrastructure')
vars_used = c(nonstationary, stationary)

# sumstat = df_before_scaling %>%
#   
#   # Select and rename five variables 
#   dplyr::select(
#     all_of(nonstationary), lsoa_dep, year
#   ) %>%
#   group_by(lsoa_dep, year) %>%
#   
#   # Find the mean and sd for each variable 
#   summarise(across(everything(),
#                    list(mean = mean, sd = sd),
#                    .names = "{.col}__{.fn}"))
# 
# sumstat =  as.data.frame(t(sumstat[-c(1,2)]))
# sumstat$id = rownames(sumstat)
# rownames(sumstat) = NULL
# 
# sumstat %<>%
#   separate(id, into = c("variable", "stat"), sep = "__") %>%
#   pivot_wider(id_cols = variable, names_from = stat, values_from = 'V1':'V14')
# 
# overall = df_before_scaling %>%
#   group_by(lsoa_dep) %>%
#   dplyr::select(all_of(vars_used)) %>%
#   summarise(across(everything(),
#                    list(mean = mean, sd = sd),
#                    .names = "{.col}__{.fn}"
#                    )) %>%
#   pivot_longer(names_to = 'key',
#                values_to = 'value',
#                cols = samhi_index__mean:n__sd)%>%
#   separate(key, into = c("variable", "stat"), sep = "__") %>%
#   pivot_wider(id_cols = c(variable, lsoa_dep), names_from = stat, values_from = value)
# 
# sumstat %<>%
#  mutate(across(where(is.numeric), round, 2)) 
# 
# # head
# substat_head = rbind.data.frame(
#   c('', c(rbind(c(2013:2019, 'Total'), rep('', 8)))),
#   c('', rep(c('Mean', 'SD'), 8))
# )
# colnames(substat_head) = 1:ncol(substat_head)
# 
# # separate into 2 tabs
# 
# sumstat_dep1 = sumstat[c(1, 2:15)] %>%
#   full_join(overall[1:17, c('variable', 'mean', 'sd')]) %>%
#     mutate(across(where(is.numeric), round, 2))
# sumstat_dep1 = as.data.frame(apply(sumstat_dep1, 2, function(x) ifelse(is.na(x), '', x)))
# colnames(sumstat_dep1) = 1:ncol(sumstat_dep1)
# sumstat_dep1 = rbind.data.frame(substat_head, sumstat_dep1)
# 
# sumstat_dep2 = sumstat[c(1, 16:ncol(sumstat))] %>% 
#   full_join(overall[18:34, c('variable', 'mean', 'sd')]) %>%
#   mutate(across(where(is.numeric), round, 2))
# sumstat_dep2 = as.data.frame(apply(sumstat_dep2, 2, function(x) ifelse(is.na(x), '', x)))
# colnames(sumstat_dep2) = 1:ncol(sumstat_dep2)
# sumstat_dep2 = rbind.data.frame(substat_head, sumstat_dep2)

sumstat_dep1 = summarize_data(dat = df_before_scaling[df_before_scaling$lsoa_dep == 1,],
                              rownames = nm_out[-c(1,13)])
sumstat_dep2 = summarize_data(dat = df_before_scaling[df_before_scaling$lsoa_dep == 2,],
                              rownames = nm_out[-c(1,13)])


# bracket sd (optional)
sd_modify = function(dat){
  for (i in seq(3,17,2)){
    for(j in 3:nrow(dat)){
      if(!dat[j,i]==''){
        dat[j,i] = paste0('[',dat[j,i],']')
      }
    }
  }
  return(dat)
}
sumstat_dep1 = sd_modify(sumstat_dep1)
sumstat_dep2 = sd_modify(sumstat_dep2)

sumstat_dep1[,1] = c('', nm_out[-13])
sumstat_dep2[,1] = c('', nm_out[-13])

# 2. Correlations
cor = signif(cor(df_before_scaling[,vars_used]), 2)
cor[lower.tri(cor, diag=F)] = ''
colnames(cor) = 1:ncol(cor)
#cor[,1] = nm_out[-1]
rownames(cor) = nm_out[-1]
rownames(cor)[12] = 'LSOA Income and Employment Deprivation'

# 3. Regression Table
library(data.table)
group_free_tab = CoefsExtract(models = 'group_free_fit')

# 4. Anova Results
anova_all = rbind(anova_SC, anova_HC[2,], anova_ED[2,], 
                  anova_EN[2,], anova_LO[2,], anova_IR[2,])


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
