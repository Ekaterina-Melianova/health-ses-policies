
library(sjlabelled)
library(haven)
library(readxl)
library(panelr)
library(tidyverse)
library(vroom)
library(performance)

wave_codes = letters[1:11][-2]
wave_codes_youth = letters[1:11][seq(1, 11, 2)]
wave_codes_child = letters[1:11][seq(2, 11, 2)][-1]
list_df_waves = list()
wd = 'C:/Users/ru21406/YandexDisk/PhD Research/Data/Survey/'

for (i in 1:length(wave_codes)){
  
  wave_code = wave_codes[i]
  
  # Load the data

    if (wave_code %in% wave_codes_youth){
    setwd(paste0(wd, '6614spss_main/UKDA-6614-spss/spss/spss25/ukhls'))
    names_selected = c('pidp','_hidp', '_sex', '_dvage', '_country',
                     '_ypsdqes_dv',
                     '_ypsdqcp_dv',
                     '_ypsdqha_dv',
                     '_ypsdqpp_dv',
                     '_ypsdqps_dv',
                     '_ypsdqtd_dv')
  df_youth =  read_sav(paste0(wave_code, "_youth.sav")) %>%
    select(ends_with(names_selected)) %>%
    rename_at(vars(all_of(colnames(.))), ~ sub('_', '', names_selected))
  df_youth = remove_all_labels(df_youth)
  df_young = df_youth
    }
  
  if (wave_code %in% wave_codes_child){
    setwd(paste0(wd, '6614spss_main/UKDA-6614-spss/spss/spss25/ukhls'))
    names_selected_ = c('pidp', '_hidp', '_sex', '_dvage', '_country',
                       '_chsdqes_dv',
                       '_chsdqcp_dv',
                       '_chsdqha_dv',
                       '_chsdqpp_dv',
                       '_chsdqps_dv',
                       '_chsdqtd_dv')
    df_child =  read_sav(paste0(wave_code, "_child.sav")) %>%
      select(ends_with(names_selected_)) %>%
      rename_at(vars(all_of(colnames(.))), ~ sub('_', '', names_selected))
    df_child = remove_all_labels(df_child)
    df_young = df_child
  }
  
  # LA indication
  setwd(paste0(wd, '6670tab_lsoa/UKDA-6670-tab/tab/ukhls'))
  df_lsoa = vroom(paste0(wave_code, "_lsoa01_protect.tab"))
  col = colnames(df_lsoa)
  df_lsoa = df_lsoa %>% rename_at(vars(all_of(colnames(.))), 
                                  ~ sub(paste0(wave_codes[i], '_'), '', col))
  
  
  # Join datasets
  
  df_merged = df_young %>%
    left_join(df_lsoa)
  
  # Add wave variable
  df_merged$wave = wave_code
  
  # Select only England
  df_merged = df_merged %>% filter(country == 1)
  df_merged = as.data.frame(df_merged)
  
  # Add to the list
  list_df_waves[[i]] = df_merged
  print(wave_code)
}

youth = do.call(plyr::rbind.fill, list_df_waves)

# --- ADD CONSTANT VARIABLES
setwd(paste0(wd, '6614tab_main/UKDA-6614-tab/tab/ukhls'))
df_xwaveid = vroom("xwaveid.tab") %>%
  select(pidp, psu, strata, quarter)
youth = youth %>% left_join(df_xwaveid)

# sampling month
youth$year <- ifelse(youth$wave == 'a' & youth$quarter %in% 1:4, 2009,
                        ifelse(youth$wave == 'a' & youth$quarter %in% 5:8|
                                 youth$wave == 'b' & youth$quarter %in% 1:4, 2010,
                               ifelse(youth$wave == 'b' & youth$quarter %in% 5:8|
                                        youth$wave == 'c' & youth$quarter %in% 1:4, 2011,
                                      ifelse(youth$wave == 'c' & youth$quarter %in% 5:8|
                                               youth$wave == 'd' & youth$quarter %in% 1:4, 2012,
                                             ifelse(youth$wave == 'd' & youth$quarter %in% 5:8|
                                                      youth$wave == 'e' & youth$quarter %in% 1:4, 2013,
                                                    ifelse(youth$wave == 'e' & youth$quarter %in% 5:8|
                                                             youth$wave == 'f' & youth$quarter %in% 1:4, 2014,
                                                           ifelse(youth$wave == 'f' & youth$quarter %in% 5:8|
                                                                    youth$wave == 'g' & youth$quarter %in% 1:4, 2015,
                                                                  ifelse(youth$wave == 'g' & youth$quarter %in% 5:8|
                                                                           youth$wave == 'h' & youth$quarter %in% 1:4, 2016,
                                                                         ifelse(youth$wave == 'h' & youth$quarter %in% 5:8|
                                                                                  youth$wave == 'i' & youth$quarter %in% 1:4, 2017,
                                                                                ifelse(youth$wave == 'i' & youth$quarter %in% 5:8|
                                                                                         youth$wave == 'j' & youth$quarter %in% 1:4, 2018,
                                                                                       ifelse(youth$wave == 'j' & youth$quarter %in% 5:8|
                                                                                                youth$wave == 'k' & youth$quarter %in% 1:4, 2019,
                                                                                              ifelse(youth$wave == 'k' & youth$quarter %in% 5:8, 2020, NA))))))))))))


table(youth$year)

# LSOA matching with LAD
setwd('C:/Users/ru21406/YandexDisk/PhD Research/Data')
lad21 = read_excel('OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.xlsx', sheet = 1)
lsoa01 = read.csv2('lsoa01.csv', sep = ',')
names(lsoa01)[1] = 'LSOA01CD'
la_all_codes = read.csv('la_all_codes.csv')

# selecting relevant vars
lad21 = lad21 %>% select(LAD21CD, LSOA11CD) %>%
  distinct(.keep_all = T)
lsoa01 = lsoa01 %>% select(LSOA11CD, LSOA01CD)
la_all_codes = la_all_codes %>% select(LADCD, UTLACD)

match_all = lad21 %>%
  left_join(lsoa01) %>%
  left_join(la_all_codes, by = c('LAD21CD' = 'LADCD'), .keep = T)
table(is.na(match_all)) # 4 lsoas ?
match_all = match_all %>% distinct(LSOA01CD, .keep_all = T)

# Add to the main dataset
youth = youth %>% dplyr::rename(LSOA01CD = lsoa01)
youth = youth %>% 
  left_join(match_all, by = 'LSOA01CD')

# Filtering
youth = youth[youth$year >= 2013,]

# checking n of times a person appeared in the survey
youth = youth %>%
  group_by(pidp) %>%
  mutate(n = n())

# adding general policy data
RXS = readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/RXS.RData'))

# fixing Northamptonshire (North + West) in the survey dataset
youth[youth$UTLACD %in% c('E06000062', 'E06000061'), 'UTLACD'] = 'E10000021'

# merging policies
youth = youth %>%
  left_join(RXS, by = c('UTLACD', 'year'))
summary(youth$population)

# filtering
table(youth$wave)
youth = youth[youth$dvage >=10 & youth$wave %in% wave_codes_youth, ]
youth = youth[!youth$ypsdqes_dv == -9,]
table(youth$ypsdqes_dv)

# rename
youth = youth %>% rename(emotional = 'ypsdqes_dv',
                         conduct = 'ypsdqcp_dv',
                         hyperact = 'ypsdqha_dv',
                         peer = 'ypsdqpp_dv',
                         prosocial = 'ypsdqps_dv',
                         total_difficulties = 'ypsdqtd_dv'
  
)

# inspecting for separate persons
ts.plot(youth[youth$pidp == '546050897', 'emotional'],
        col="mediumseagreen",lwd=4, ylab= "Mood")

# exploring the panel 
youth_panel <- panel_data(youth, id = pidp, wave = year)
youth_panel <- youth_panel %>% 
  mutate(
    emotional_mean = mean(emotional),  # this is the person-level mean (based on the time variation)
    emotional_lag = lag(emotional),
    education_lag = lag(education),
    children_social_care_lag = lag(children_social_care),
    adult_social_care_lag = lag(adult_social_care),
    public_health_lag = lag(public_health),
    housing_lag = lag(housing),
    cultural_lag = lag(cultural),
    environmental_lag = lag(environmental),
    planning_development_lag = lag(planning_development),
    police_lag = lag(police),
    fire_rescue_lag = lag(fire_rescue)
    
  )

youth_panel %>% 
  line_plot(emotional, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)
youth_panel %>% 
  line_plot(education, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)
youth_panel %>% 
  line_plot(emotional, 
            overlay = FALSE,
            subset.ids = filter(youth_panel, n == 8)$pidp, 
            add.mean = TRUE)

youth_panel %>% 
  line_plot(education, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)
# MLM

model.1 = lmer(emotional ~ 1 + scale(dvage) + scale(education_lag) + scale(children_social_care_lag) + sex + (1 | pidp) + (1 | UTLACD), youth_panel)
icc(model.1)
summary(model.1)

model.2 = lmer(conduct ~ 1 + scale(education_lag) + scale(children_social_care_lag) + sex + (1 | pidp) + (1 | UTLACD), youth_panel)
summary(model.2)

model.3 = lmer(hyperact ~ 1 + scale(education_lag) + scale(children_social_care_lag) + sex + (1 | pidp) + (1 | UTLACD), youth_panel)
summary(model.3)

model.4 = lmer(peer ~ 1 + scale(education_lag) + scale(children_social_care_lag) + sex + (1 | pidp) + (1 | UTLACD), youth_panel)
summary(model.4)

model.5 = lmer(prosocial ~ 1 + scale(education_lag) + scale(children_social_care_lag) + sex + (1 | pidp) + (1 | UTLACD), youth_panel)
summary(model.5)

model.6 = lmer(total_difficulties ~ 1 + scale(education_lag) + scale(children_social_care_lag) + sex + (1 | UTLACD), youth_panel)
summary(model.6)

