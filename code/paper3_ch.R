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

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
options(max.print=1900)

# pre-processing

# loading the data
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df.rds')

# 
children_data = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/Data/spending_data_ch.csv')
# remove suffix '_total' from names
names(children_data) = gsub('_total', '', names(children_data))

policy_names_ch = c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s')

# joining with the main df
df = df %>% filter(year > 2013) %>%
  dplyr::left_join(children_data %>% 
                        dplyr::select(LAD21CD,year, all_of(policy_names_ch)), by = c('LAD21CD', 'year'))
#colnames(df)
#test = df %>%
#  group_by(LAD21CD,year) %>%
#  # summarise number of missings at c1s:c7s variables
#  summarise_at(vars(c1s:c7s), ~sum(is.na(.))) %>%
#  filter(year>2013)


## collapse some categories
df %<>%
  dplyr::mutate(c1 = c2s, 
                c2 = c6s + c8s,
                c3 = c1s + c3s + c4s + c5s + c7s,
                ot = law_order + infrastructure + env
                )

policy_names_ch = c(policy_names_ch,
                    'c1', 'c2', 'c3')

# in prices of 2020
df %<>%
  dplyr::mutate(across(all_of(policy_names_ch), ~ . /def*100 ))
#summary(df[, policy_names_ch])
#hist(df$c1)
#hist(df$c2)
#hist(df$c3)

policy_names_ch = c(policy_names_ch,'ot',
                    policy_names_6[-2])

# replace negative values with 0
df %<>%
  dplyr::mutate(across(all_of(policy_names_ch), ~ ifelse(. < 1, 1, .)))# %>%
  #filter(c1 > 10 & c2 > 10 & c3 > 10)

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

test = df[df$c1 < -4, c('name', 'year', 'LAD21CD', 'c2s', 'c4s')] %>%
  distinct()
#test = df[df$c4s < -5, c('name','LAD21CD')]
#test = df %>% filter(LAD21CD == 'E06000039')
hist(df$c1)
hist(df$c2)
hist(df$c3)
hist(df$ot)
hist(df$healthcare)
hist(df$social_care_adult)

#hist(df$c5s)
#hist(df$c7s)
#hist(df$c1s)
#df = df %>% filter(c1 > -8)
#df = df %>% filter(c2 > -8)
#df = df %>% filter(c3 > -8)
#df %>% filter(c1 < -8) %>% select(LAD21CD) %>% distinct()
#df = df %>% filter(c1 > 2)
#df = df %>% filter(c2 > 2)
#df = df %>% filter(c3 > 2)

# outliers
#df = df %>% filter(LAD21CD != c('E06000039', 'E08000028'))
#
# average all numeric variables by LAD21CD and year and select only fisrt row of character variabls
#df_ = df %>%
#  group_by(LAD21CD, year) %>%
#    dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE),
#            across(where(is.character), first)) %>%
#  ungroup()
#
#df_$time = df_$time-1



# final dataset - wide format
df$time = df$time-1
#df$z_mh_rate = scale(df$z_mh_rate)
df_lv = lavaan_df(dv = 'z_mh_rate',
                  df = df,
                  deprivation_cat = 'lsoa_ses_score1',
                  ivs = c('c1', 'c2', 'c3',
                          'social_care_adult', 
                          'healthcare',
                          'env',
                          'law_order',
                          'infrastructure',
                          c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s'),'ot'
                          ),
                  ivs_map = c('c1', 'c2', 'c3', 
                              'as', 'hc', 'en', 'lo', 'fr',
                              c('c1s', 'c2s', 'c3s', 'c4s', 'c5s', 'c6s', 'c7s', 'c8s'), 'ot'),
                  max_time = 6)
df_lv = as.data.frame(na.omit(df_lv))
summary(df_lv)

# ----------------------------------------------------------------------
# ------------------------------ MODELLING -----------------------------
# ----------------------------------------------------------------------

## compare
group_free_syntax = RC_GCLM_syntax(model = 'regclm',
                                   cor = F,
                                   max_time = 6,
                                   endogeneous = c('HE', 'c1', 'c2', 'c3', 'ot', 'hc', 'as'),
                                   reverse = c('HE', 'c1', 'c2', 'c3', 'ot', 'hc', 'as'),
                                   control = control_names[-c(3,12)],
                                   multiple  = T)
group_free_fit_dep_1 = sem(group_free_syntax,
                           data = df_lv,
                           estimator = "mlr",
                           orthogonal = T,
                           cluster = 'LAD21CD',
                           group = 'lsoa_ses_score1')
fitmeasures(group_free_fit_dep_1, measures)

gc()
beepr::beep()
summary(group_free_fit_dep_1, std=T, ci = F)
m_lv = broom::tidy(group_free_fit_dep_1) %>%
  filter(grepl('b_HE|d_HE', label)) %>%
  dplyr::mutate(p.value = round(p.value,3)) %>%
  dplyr::select(label, estimate, p.value, group) 
res = cbind(m_lv[1:12,], m_lv[61:72,])
res = cbind(m_lv[1:8,], m_lv[41:48,])
res = cbind(m_lv[1:14,], m_lv[71:84,])


group_free_syntax_ = RC_GCLM_syntax(model = 'regclm',
                                   cor = F,
                                   max_time = 6,
                                   endogeneous = c('HE', 'c1', 'c2', 'c3', 'ot', 'hc', 'as'),
                                   reverse = c('HE', 'c1', 'c2', 'c3', 'ot', 'hc', 'as'),
                                   control = control_names[-c(3,12)],
                                   multiple  = T)
group_free_fit_dep_1_ = sem(group_free_syntax_,
                           data = df_lv,
                           estimator = "mlr",
                           orthogonal = T,
                           cluster = 'LAD21CD',
                           group = 'lsoa_ses_score1')
fitmeasures(group_free_fit_dep_1_, measures)

gc()
beepr::beep()
summary(group_free_fit_dep_1_, std=T, ci = F)
m_lv = broom::tidy(group_free_fit_dep_1_) %>%
  filter(grepl('b_HE|d_HE', label)) %>%
  dplyr::mutate(p.value = round(p.value,3)) %>%
  dplyr::select(label, estimate, p.value, group) 
res = cbind(m_lv[1:14,], m_lv[71:84,])



# 3. Regression Table
group_free_tab = CoefsExtract(models = 'group_free_fit_dep_1') %>%
  dplyr::select(everything(),type = type_1, -type_2)
gc()

fit_measures_gf = cbind.data.frame(est.std.x_long_2 = fitmeasures(group_free_fit_dep_1, measures),
                                   est.std.x_short_2 = fitmeasures(group_free_fit_dep_1, measures))
fit_measures_gf[] = lapply(fit_measures_gf, sprintf, fmt = "%.3f")
gf_models_coefs = TableEffects(dat = group_free_tab,
                               fit_measures = fit_measures_gf,
                               param_range = 20:32,
                               section_name_rows = c(1, 8, 14, 20)
)
gf_models_coefs[10:15,1] = end_new[2:7]


# # ----------------------------------------------------------------------

library(ggpubr)

lsoa_group = c('lsoa_ses_score1', 'lsoa_ses_score2', 'lsoa_ses_score3')
vars_original_names = c('samhi_index',
                        'prop_ibesa',
                        'antidep_rate',
                        'est_qof_dep',
                        'z_mh_rate',
                        'c1', 'c2', 'c3')
all_vars = c(health_vars,
             vars_original_names)

df_plot = df_before_scaling %>% select(year,
                                       lsoa11,
                                       LAD21CD,
                                       all_of(c(all_vars,
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
    pivot_longer(cols = samhi_index:c3) %>%
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
  filter(name %in% c('c1', 'c2', 'c3')) %>%
  ggplot(aes(year, value, colour = dep_colour, linetype = dep_colour)) +
  scale_x_continuous(name = NULL, 
                     breaks = 2014:2019)+ 
  scale_y_continuous(name = 'Spending, £ per capita'#, limits = c(100, 1400)
  ) + 
  theme(axis.text = element_text(size = 24))  +
  geom_smooth(method = loess,
              linewidth = 0.5,
              se = T,
              formula = y ~ x,
              level = 0.9,
              fill = 'lightgrey') +
  # add lines
  #geom_line(aes(group = LAD21CD), color = "lightblue") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = linetypes)  +
  facet_wrap(~ name, scales = 'free') +
  theme_pubclean() + 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom')

#
all_vars = c(health_vars,
             'c1', 'c2', 'c3')
panel_df = df_before_scaling %>%
  group_by(LAD21CD, year) %>%
  dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE)))
panel = panel_data(panel_df, id = LAD21CD, wave = year)

list_plots = list()
for (i in 6:length(all_vars)){
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
ggarrange(list_plots[[6]],
          list_plots[[7]],
          list_plots[[8]],
          #labels = c('Adult Social Care',
          #           'Children Social Care',
          #           'Healthcare'          ),
          ncol = 3, nrow = 1,
          font.label = list(size = 30), align ='hv') +
  ylab("Common Y-Axis Label")




