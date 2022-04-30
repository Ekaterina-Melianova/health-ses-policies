
library(ctsem)
library(ctnet)
library(ctsemOMX)
library(tidyverse)
library(plyr); library(dplyr)
library(data.table)
library(panelr)
library(performance)
library(Amelia)
library(lavaan)
library(pdfetch)
library(nomisr)
library(imputeTS)
library(fingertipsR)
library(viridis)
library(ggplot2)
library(feisr)
#library(pracma)
library(moderndive) 

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/prelims/functions.r')

# policies
RSX_RG = readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/RSX_RG.RData'))

# HEALTH INDICATORS 

# 93351 Average weekly earnings
# 92964 Income deprivation
# 91126 Unemployment (model-based)
# 92313 Percentage of people in employment
# 91125 19-24 year olds not in education, employment or training
# 93203 16-17 year olds not in education, employment or training (NEET) or whose activity is not known
# 93350 Gender pay gap (by workplace location)
# 93352 Households with problem debt
# 93759 Fuel poverty (low income, low energy efficiency methodology)
# 92628 Employment deprivation: score
# 92793 Housing affordability ratio
# 90669 Quality of indoor living environment: IMD score

# 41001 Suicide rate
# 22301	Self-reported wellbeing - people with a low satisfaction score
# 22302	Self-reported wellbeing - people with a low worthwhile score
# 22303	Self-reported wellbeing - people with a low happiness score
# 22304	Self-reported wellbeing - people with a high anxiety score
# 92616 Self-reported well-being - high satisfaction score: % of respondents
# 92617 Self-reported well-being - high happiness score: % of respondents
# 848 Depression: Recorded prevalence (aged 18+)
# 93495 Estimated prevalence of common mental disorders: % of population aged 16 & over
# 93496 Estimated prevalence of common mental disorders: % of population aged 65 & over
# 90408 Long-term health problem or disability: % of population
# 358 Long-term mental health problems (GP Patient Survey): % of respondents

# 11601 Utilisation of outdoor space for exercise/health reasons
# 93014 Percentage of physically active adults
# 212 Stroke: QOF prevalence (all ages)
# 219 Hypertension: QOF prevalence (all ages)
# 273 CHD: QOF prevalence (all ages)
# 93088 Percentage of adults (aged 18+) classified as overweight or obese

# 92309 Supporting information - % population aged under 18
# 92310 Supporting information - % population aged 65+
# 92860 Supporting information - % population from ethnic minorities

# well-being, depression, suicide, earnings
health_raw = fingertipsR::fingertips_data(IndicatorID = c('93351', '91126', '41001', '848',
                                             '22301', '273', '93088'),
                         AreaTypeID = 'All')
table(health_raw$IndicatorID)

# filtering
health = health_raw %>% filter(Sex == 'Persons' #& Age %in% c('18+ yrs', '16+ yrs')
                               ) %>%
  select(UTLACD = AreaCode, IndicatorName, year = Timeperiod, Value, AreaName) %>%
  filter(grepl('E', UTLACD) & nchar(UTLACD) == 9)
unique(health$IndicatorName)

#mis = health_raw %>% filter(IndicatorID == 93088 & grepl('Tyneside', AreaName))

# renaming
names = c('CHD',
          'depression',
          'satisfaction',
          'suicide',
          'unemploy',
          'obesity',
          'earnings_weekly')
names = cbind.data.frame(IndicatorName = unique(health$IndicatorName), names)
health = health %>%
  left_join(names) %>%
  select(-IndicatorName)

# year format
table(health$year)
health$year = as.numeric(substr(health$year, 1, 4)) 
health = health %>% filter(year %in% 2013:2020)

# unique cases
health = unique(health)

# wide format
split_list <- split(health, health$names)
for (i in seq_along(split_list)){
  colnames(split_list[[i]])[colnames(split_list[[i]]) == 'Value'] =
    unique(split_list[[i]][, 'names'])
  
  split_list[[i]] = split_list[[i]] %>% select(-names)
  
  #if (i %in% seq_along(split_list)[-length(split_list)]){
  #  split_list[[i]] = split_list[[i]] %>% select(-AreaName)
  #}
}

health_df = purrr::reduce(split_list, full_join, by = c('year', 'UTLACD'))
AreaName_vars = names(health_df)[grepl('AreaName', names(health_df))]
health_df = cbind(health_df, AreaName = apply(health_df[,AreaName_vars], 1, max, na.rm=T))
health_df = health_df %>% select(!!-AreaName_vars)

#h = split_list[[4]]
# Deprivation Index
deprivation_2015 = deprivation_decile('202', 2015)
deprivation_2019 = deprivation_decile('202', 2019)

# regional gross disposable household income (GDHI) from ONS
#gross_inc = nomis_get_data(id = "NM_185_1",
#                      time = 2013:2021,
#                      select = c('DATE', 'GEOGRAPHY_CODE',
#                                 'MEASURE_NAME', 'OBS_VALUE',
#                                 'GEOGRAPHY_TYPE')) %>%
#  filter(grepl('local authorities', GEOGRAPHY_TYPE))
#1
#
#gross_inc = gross_inc %>% filter(grepl('E', GEOGRAPHY_CODE)) %>%
#  filter(MEASURE_NAME == 'Gross Disposable Household Income - GDHI (£m)')
#gross_inc = gross_inc %>% rename(year = DATE, UTLACD = GEOGRAPHY_CODE, 
#                                 gross_inc = OBS_VALUE) %>%
#  select(year, UTLACD, gross_inc)

# fixing Bournemouth, Christchurch and Poole UA
RSX_RG[RSX_RG$la == 'Bournemouth UA', 'UTLACD'] = 'E06000028'
RSX_RG[RSX_RG$la == 'Poole UA', 'UTLACD'] = 'E06000029'

#
health_df[health_df$UTLACD %in% c('E07000049', 'E07000050'), 'UTLACD'] = 'E07000052'
health_df = health_df %>%
  group_by(year, UTLACD) %>%
  mutate(d = mean(depression, na.rm = T),
         s = mean(suicide, na.rm = T),
         u = mean(unemploy, na.rm = T),
         e = mean(earnings_weekly, na.rm = T),
         sa = mean(satisfaction, na.rm = T),
         c = mean(CHD, na.rm = T),
         o = mean(obesity, na.rm = T))

keys = cbind.data.frame(names = names$names, 'new' = c( 'c', 'd', 'sa', 's', 'u', 'o', 'e'))
health_df = as.data.frame(health_df)
for (i in 1:nrow(keys)){
  health_df[, keys[i,'names']] = ifelse(!is.nan(health_df[,keys[i,'new']]),
                                        health_df[,keys[i,'new']],
                                        health_df[, keys[i,'names']])
}

# leaving only one (West) Dorset
health_df = health_df %>% 
  filter(!AreaName %in% c('East Dorset', 'North Dorset'))

#j = 2013
#i = 'depression'
dr = health_df
for (i in names[,2]){
  for (j in 2013:2020){
      dr[dr$AreaName == 'Dorset' & dr$year == j, i] = 
        ifelse(is.na(dr[dr$AreaName == 'Dorset' & dr$year == j, i]) &
               !any(is.na(dr[dr$AreaName == 'West Dorset' & dr$year == j, i])),
               dr[dr$AreaName == 'West Dorset' & dr$year == j, i],
               ifelse(!any(is.na(dr[dr$AreaName == 'Dorset (Cty)' & dr$year == j, i])),
               dr[dr$AreaName == 'Dorset (Cty)' & dr$year == j, i],
               dr[dr$AreaName == 'Dorset' & dr$year == j, i]))
  }
}

for (i in names[,2]){
  for (j in 2013:2020){
    dr[dr$AreaName == 'Dorset' & dr$year == j, i] = 
      ifelse(is.na(dr[dr$AreaName == 'Dorset' & dr$year == j, i]),
             dr[dr$AreaName == 'NHS Dorset CCG' & dr$year == j, i],
               dr[dr$AreaName == 'Dorset' & dr$year == j, i])
  }
}

dr = health_df
for (i in names[,2]){
  for (j in 2013:2020){
    dr[dr$AreaName == 'Dorset' & dr$year == j, i] = 
      ifelse(is.na(dr[dr$AreaName == 'Dorset' & dr$year == j, i]) &
               !length(dr[dr$AreaName == 'West Dorset' & dr$year == j, i]) == 0,
               dr[dr$AreaName == 'West Dorset' & dr$year == j, i],
             dr[dr$AreaName == 'Dorset' & dr$year == j, i])
    
    dr[dr$AreaName == 'Dorset' & dr$year == j, i] =
             ifelse(is.na(dr[dr$AreaName == 'Dorset' & dr$year == j, i]) ,
                    dr[dr$AreaName == 'Dorset (Cty)' & dr$year == j, i],
                    dr[dr$AreaName == 'Dorset' & dr$year == j, i])
  }
}

health_df = dr
health_df = health_df %>% 
  #filter(!AreaName %in% c('West Dorset', 'Dorset (Cty)')) %>%
  select(-(keys[,2]))

# joining with policies
df = RSX_RG %>%
  left_join(health_df) %>%
  left_join(deprivation_2019, by = c('UTLACD' = 'AreaCode'), keep = T)

# Buckinghamshire score is missing? remove for now
df = df %>% filter(!UTLACD == 'E06000060')

# fixing IMDscore for Bournemouth, Christchurch and Poole 
df[df$UTLACD %in% c('E06000029', 'E06000028'), 'IMDscore'] =
  df[df$UTLACD == 'E06000058', 'IMDscore'][1]

# City of London combined with Hackney
# Isles of Scilly and Cornwall 
# remove for now
df = df[!df$UTLACD %in% c('E06000053', 'E09000001'),]
#test = health_raw[health_raw$IndicatorName == 'Suicide rate' & health_raw$AreaName == 'Blackpool',]

# the rest missing (recent 2019-2020 data primarily) replace with moving average
#df[, names$names] = na_ma(df[, names$names], weighting = 'simple')

# deflate earning
df$earnings_weekly = df$earnings_weekly*df$def/100


# exploring the panel 
panel_df =  panel_data(df, id = UTLACD, wave = year)

panel_df %>% 
  line_plot(depression, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(CHD, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(obesity, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(satisfaction, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(suicide, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(earnings_weekly, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(unemploy, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(skills_funding, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_df %>% 
  line_plot(public_health, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

#------------------------------------- CTSEM ----------------------------------#

# subsetting 
df$time = df$year - 2013
df = df %>% dplyr::rename(income = earnings_weekly, id = UTLACD)

df = df[, c('id', 'time',
            'skills_funding',
            'public_health',
            'satisfaction',
            'depression',
            'CHD',
            'income',
            'IMDscore')]

# Scaling
df[, c('skills_funding', 'public_health', 'satisfaction', 'depression', 'CHD', 'income', 'IMDscore')] =
  scale(df[, c('skills_funding', 'public_health', 'satisfaction', 'depression', 'CHD', 'income', 'IMDscore')])
summary(df)

# de-trending
vars_to_detrend = names(df[, !names(df) %in% c('time', 'id', 'IMDscore')])

# detrending function
detrended = list()
DetrendPanel = function(vars = NULL, id = id, time = time, data = df){
  for (i in vars){
      detrended[[which(vars == i)]] = left_join(get_regression_points(lm(as.formula(paste(i, '~ id*time')), data = df)) %>% 
                    select(id, time, residual),
                  unique(get_regression_points(lm(as.formula(paste(i, '~ id')), data = df)) %>% 
                           select(id, grep('hat', colnames(.))))) %>% 
        rowwise() %>% 
    dplyr::mutate(!!paste0(i, '_det') :=  sum(c(!!as.name(paste0(i, '_hat')), residual))) %>%
    select(c(id, time, !!paste0(i, '_det')))
  }
  
  out = purrr::reduce(detrended, full_join)
  
  return(out)
}

# applying the function
df_det = DetrendPanel(vars = vars_to_detrend)

# merging with the rest of the dataset
df = df %>% select(id, time, IMDscore) %>% full_join(df_det)

# outliers
id_skills = df[df$time == 7, c('skills_funding_det', 'id')] %>% arrange(skills_funding_det) %>%
  slice((nrow(.)-2):nrow(.)) %>% select(id)
df = df %>% filter(!id %in% id_skills$id)

# Model
nlatent = 4
nmanifest = 4
tpoints = length(unique(df$time))
latentNames = paste0("eta", 1:nlatent)
manifestNames = c('skills_funding_det', 'public_health_det',
                  'income_det', 'CHD_det')

A <- matrix(c( "pskill_drift", 0, 'inc_pskill', 0,
               0, 'phealth_drift', 0, 'health_phealth',
               'pskill_inc', 0, 'inc_drift', 'health_inc',
               0, 'phealth_health', 'inc_health', 'health_drift'),
            nrow = nlatent, ncol = nlatent)

fullm = ctModel(Tpoints = tpoints, 
                n.latent = nlatent,
                manifestNames = manifestNames,
                n.manifest = nmanifest,
                TIpredNames = 'IMDscore',
                LAMBDA = diag(nlatent),
                DRIFT = A,
                DIFFUSION = "auto",
                #CINT = matrix(c('int1', 'int2', 'int3', 'int4'), nrow = nlatent, ncol = 1),
                MANIFESTMEANS = matrix(0, nrow = nlatent, ncol = 1),
                MANIFESTVAR = matrix(0, nrow = nlatent, ncol = nlatent),
                TRAITVAR = "auto",
                type = "stanct" 
)

pars_tab = fullm$pars
ctModelLatex(fullm)

# running the model
start <- Sys.time()
set.seed(1234)
simfit <- ctStanFit(datalong = df, ctstanmodel = fullm,
                    cores = 16#, plot = 10
                    )
print(runtime <- Sys.time() - start)

# summary and posterior
ctsem_results <- summary(simfit)
#ctsem_results$popmeans
drift_est <- ctStanContinuousPars(simfit)$DRIFT
post <- ctsem::ctExtract(simfit)
post_drift <- post$pop_DRIFT

#summary(simfit, verbose = T)["TIPREDEFFECT"]
#summary(simfit, verbose = T)["discreteTIPREDEFFECT"]
#summary(simfit, verbose = T)["asymTIPREDEFFECT"]
#summary(simfit, verbose = T)["addedTIPREDVAR"]


### impulse response
#ctStanDiscretePars(simfit, plot = T, indices = 'CR', cores = 16)
dtpars = ctStanDiscretePars(simfit, cores = 16, times = dts,nsamples = 500)
odtpars = ctStanDiscretePars(simfit, observational = T, cores = 16, times = dts, nsamples = 500)

dt = rbind(data.table(Shock = 'Independent',as.data.table(dtpars)),
         data.table(Shock = 'Correlated',as.data.table(odtpars)))

dt$Shock = paste0(dt$Shock,' shock')
dt[,Shock := factor(Shock,levels = c('Independent shock','Correlated shock'))]
dt$col = paste0('Shock to ',dt$col)

setnames(dt, old = c('value','Time interval','row'),
         new = c('Expected change','Years since shock','Variable'))

dt[,`Years since shock` := as.numeric(`Years since shock`)]

#pdf('dtplot.pdf', height = 5)
ggplot(dt, aes(y = `Expected change`,
               x = `Years since shock`,
               colour = Variable,
               fill = Variable)) +
  stat_summary(fun.data = mean_sdl, geom = "ribbon", alpha = 0.5) +
  geom_hline(yintercept = 0, colour = 'darkgrey') +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  facet_grid(cols = vars(col), rows = vars(Shock)) +
  theme_bw()
#dev.off()

## depression separately
dt_sub = dt[dt$col == 'Shock to eta3',]
dt_sub = dt_sub %>% filter(Variable %in% c('eta3', 'eta4'))

dt_sub = dt[dt$col == 'Shock to eta2',]
dt_sub = dt_sub %>% filter(Variable %in% c('eta2', 'eta4'))

ggplot(dt_sub,aes(y = `Expected change`, x = `Years since shock`, 
              colour = Variable, fill = Variable)) +
  stat_summary(fun.data = mean_sdl, geom = "ribbon", alpha = 0.5) +
  geom_hline(yintercept = 0, colour = 'darkgrey') +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  facet_grid(cols = vars(col), rows = vars(Shock)) +
  theme_bw()

##
ctKalman(simfit, plot=TRUE, #smoothed (conditioned on all time points) latent states.
         kalmanvec = c('etasmooth'), timestep=1)

### LaTex model
ctModelLatex(fullm)

### funding -> income -> health
IE_CI <- sapply(dts,function(dt){
  getCIs(post_drift,simplify=TRUE, FUN=ctnet::IE,  dt = dt, IV = 1, DV = 4, M = 3)
}, simplify = "array")

plot.new()
plot.window(xlim = c(0,7), ylim = c(-0.01, 0.01))
axis(1); axis(2)
abline(h = 0)
title("Path-Specific Effects across Time-Intervals", xlab = "Time-Interval", ylab = "Coefficient")
#lines(dts, TE_CI[2,], col = "purple", lty = 1)
#lines(dts, TE_CI[1,], col = "purple", lty = 2); lines(dts, TE_CI[3,], col = "purple", lty = 2)
#lines(dts, DE_CI[2,], col = "orange", lty = 1)
#lines(dts, DE_CI[1,], col = "orange", lty = 2); lines(dts, DE_CI[3,], col = "orange", lty = 2)
lines(dts, IE_CI[2,], col = "darkgreen", lty = 1)
lines(dts, IE_CI[1,], col = "darkgreen", lty = 2); lines(dts, IE_CI[3,], col = "darkgreen", lty = 2)


### lagged effects
dts = seq(0, 7)
phidt_CI <- sapply(dts, function(dt){
  getCIs(post_drift, simplify = T, FUN = expm::expm, const = dt)
}, simplify = "array")

phidt_CI <- plotPhi(posterior = post_drift, dts = dts, plot = F)

#plotPhi(CI_obj = phidt_CI, dts = dts,  index = "AR")
#plotPhi(CI_obj = phidt_CI, dts = dts,  index = "CL")

### public_health -> health
plotPhi(CI_obj = phidt_CI, dts = dts,  index = matrix(c(2,4), nrow = 1))
### health -> public_health
plotPhi(CI_obj = phidt_CI, dts = dts,  index = matrix(c(4,2), nrow = 1))
### skills_funding -> health
plotPhi(CI_obj = phidt_CI, dts = dts,  index = matrix(c(1,4), nrow = 1))
### income -> health
plotPhi(CI_obj = phidt_CI, dts = dts,  index = matrix(c(3,4), nrow = 1))
### skills_funding -> income
plotPhi(CI_obj = phidt_CI, dts = dts,  index = matrix(c(1,3), nrow = 1)) 

