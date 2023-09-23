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

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
options(max.print=340)

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



# final dataset - wide format

dep_vec_3 = paste0(rep(dep_vec, each = 3), 1:3)
df_lv_1 = lavaan_df(dv = 'samhi_index',
                    deprivation_cat = 'lsoa_ses_score1',
                    df = df)
#df_lv_1 = as.data.frame(na.omit(df_lv_1))
colnames(df_lv_1)
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

# LSOA deprived vs. not deprived

# a. free model
group_free_syntax = RC_GCLM_syntax(multiple = T,
                                   control = control_names[-c(3,12)])
group_free_fit_dep_1 = sem(group_free_syntax,
                           data = df_lv_1, 
                           estimator = "mlr",
                           orthogonal = T, 
                           cluster = 'LAD21CD',
                           group = 'lsoa_ses_score1')
beepr::beep()
gc()
fm_group_free_dep_1 = fitmeasures(group_free_fit_dep_1, measures)
#group_free_std = broom::tidy(group_free_fit_dep_1)

group_free_fit_dep_2 = sem(group_free_syntax,
                           data = df_lv_2, 
                           estimator = "mlr",
                           orthogonal = T, 
                           cluster = 'LAD21CD',
                           group = 'lsoa_ses_score2')
beepr::beep()
gc()
fm_group_free_dep_2 = fitmeasures(group_free_fit_dep_2, measures)

group_free_fit_dep_3 = sem(group_free_syntax,
                           data = df_lv_3, 
                           estimator = "mlr",
                           orthogonal = T, 
                           cluster = 'LAD21CD',
                           group = 'lsoa_ses_score3')
beepr::beep()
gc()
fm_group_free_dep_3 = fitmeasures(group_free_fit_dep_3, measures)


# running constrained models

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

lsoa_group = c('lsoa_ses_score1', 'lsoa_ses_score2', 'lsoa_ses_score3')

list_combined = list()
i = 1
for (lsoa in lsoa_group){
  
  for (synt in syntax){
    
    list_combined[[i]] = list(lsoa, synt)
    i = i + 1
    
  }
}

groupSEM = function(comb){
  
  lsoa = comb[[1]]
  synt = comb[[2]]
  
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

sumstat_dep1 = summarize_data(dat = df_before_scaling,
                              group = 'lsoa_ses_score1',
                              rownames = nm_out[-c(14,23)],
                              quant = T,
                              stat = list('Mean' = mean,
                                          'SD' = sd,
                                          'Q25' = function(x) quantile(x, probs = 0.25),
                                          'Q75' = function(x) quantile(x, probs = 0.75)))
colnames(sumstat_dep1) = c('', 'Top 50%', '', '', 'Bottom 50%', '', '')

# 3. Regression Table

section_names = c('Autoregressive Effects',
                  'Cross-Lagged Effects: Mental Health -> Spending',
                  'Cross-Lagged Effects: Spending -> Mental Health',
                  'Fit Measures (Scaled)')

group_free_tab = CoefsExtract(models = c('group_free_fit_dep_1',
                                         'group_free_fit_dep_2',
                                         'group_free_fit_dep_3')) %>%
  dplyr::select(everything(),type = type_1, -type_2)

fit_measures_gf = cbind.data.frame(est.std.x_long_2 = fm_group_free_dep_1,
                                   est.std.y_long_2 = fm_group_free_dep_2,
                                   est.std_long_2 = fm_group_free_dep_3,
                                   
                                   est.std.x_short_2 = fm_group_free_dep_1,
                                   est.std.y_short_2 = fm_group_free_dep_2,
                                   est.std_short_2 = fm_group_free_dep_3)
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
                       free_model){
  
  anova_list = lapply(list_results[list_range], 
                      function(model) anova(free_model, model))
  
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
#lavTestLRT(group_free_fit_dep_3, list_results[[26]],
#           method = 'satorra.2000')


aov(df$samhi_index ~ df$year)

df_diff = df %>% dplyr::select(LAD21CD, year, samhi_index, lsoa_dep_1) %>%
  group_by(LAD21CD, year) %>%
  summarise(lad_samhi_index = mean(samhi_index),
            lsoa_dep_1 = mean(lsoa_dep_1)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(year), 
              names_from = lsoa_dep_1,
              values_from = lad_samhi_index)

mean(df$samhi_index[df$year == 2013 & df$lsoa_dep_1 == 1])
mean(df$samhi_index[df$year == 2013 & df$lsoa_dep_1 == 2])
mean(df$samhi_index[df$year == 2019 & df$lsoa_dep_1 == 1])
mean(df$samhi_index[df$year == 2019 & df$lsoa_dep_1 == 2])

t.test(mean(samhi_dep1_13 - samhi_dep2_13),
       mean(samhi_dep1_19 - samhi_dep2_19))



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
  #left_join(anova_all, by = d) %>%
  dplyr::select(id,
                contains('.x'),
                contains('.y'),
                everything())
gf_models_coefs_long = SubHead(CiSplit(gf_pct_coefs_long),
                               sub_head = c('Top', 'Bottom'),
                               sub_head_add = c('50%', '50%', 
                                                '40%', '40%',
                                                '30%', '30%'),
                               n = 3,
                               colnames = col_gf)

# short-run table
gf_pct_coefs_short = gf_pct_coefs %>%
  dplyr::select(id,
                contains('short')) %>%
  dplyr::select(id,
                contains('.x'),
                contains('.y'),
                everything())
gf_models_coefs_short = SubHead(CiSplit(gf_pct_coefs_short),
                                sub_head = c('Top', 'Bottom'),
                                sub_head_add = c('50%', '50%', 
                                                 '40%', '40%',
                                                 '30%', '30%'),
                                n = 3,
                                colnames = col_gf)

# # ----------------------------------------------------------------------

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
    pivot_longer(cols = samhi_index:infrastructure) %>%
    ungroup() %>% dplyr::rename(dep_colour = !!sym(j)) %>%
    mutate(dep_cat = sub('[[:digit:]]+', '', sym(j)))%>%
    mutate(dep_linetype = j)
}

panel_df = do.call(rbind.data.frame, list_panel) %>% 
  filter(!is.na(dep_colour))

panel_df$name = factor(panel_df$name,
                       levels = vars_original_names,
                       labels = nm_out[1:11])
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

panel_df %>%
  filter(name %in% nm_out[1:5])  %>%
  ggplot(aes(year, value, colour = dep_colour, linetype = dep_colour)) +
  scale_x_continuous(name = NULL, 
                     breaks = 2013:2019)+ 
  scale_y_continuous(name = 'Z-Standardised Scores', limits = c(-1.5, 1.5)) + 
  theme(axis.text = element_text(size = 24)) +
  geom_smooth(method = loess,
              linewidth = 0.5,
              se = T,
              formula = y ~ x,
              level = 0.9,
              fill = 'lightgrey') +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = linetypes) +
  facet_wrap(~ name, scales = 'free') +
  theme_pubclean()+ 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom') #+ 
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
