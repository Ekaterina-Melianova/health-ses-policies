
library(ctsem)
library(ctnet)
library(ctsemOMX)
library(tidyverse)
library(panelr)
library(performance)
library(Amelia)
library(lavaan)

setwd('C:\\Users\\ru21406\\YandexDisk\\PhD Research\\Data')

# main df
df  = readRDS("df.rds")
# names(df)

# policies
RSX_RG = readRDS(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/PhD Research/Data/Financial/RSX_RG.RData'))

# selecting vars
GHQ = c('scghqa', 'scghqb', 'scghqc',
        'scghqd', 'scghqe', 'scghqf',
        'scghqg', 'scghqh', 'scghqi',
        'scghqj', 'scghqk', 'scghql')
sub = df %>% select(pidp, LAD21CD, la, wave, year,
                    sex, employ, marstat, urban_dv, ethn_dv,
                 skills_funding, public_health, SRH,
                 dvage, fimnnet_dv, def, n, GHQ) %>%
  ungroup() %>% rename(income = fimnnet_dv, id = pidp)
sub = as.data.frame(sub)

# deflating income
sub$income = sub$income*sub$def/100
sub = sub %>% select(-def)
# exploring the data

# health
table(sub$SRH)
#sub = sub[!sub$SRH == -8,]

# age
summary(sub[sub$dvage > 0, 'dvage'])
hist(sub[sub$dvage > 0, 'dvage'])

# income
hist(sub[sub$income > 0 & sub$income < 7000, 'income'])

# participation over the years
head(table(sub$id, sub$year), 20)

# exploring the policy panel 
panel_RSX_RG =  panel_data(RSX_RG, id = LAD, wave = year)

panel_RSX_RG %>% 
  filter(!LAD == c('E09000001', 'E06000053')) %>%
  filter(skills_funding < 50) %>%
  line_plot(skills_funding, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_RSX_RG %>% 
  line_plot(education, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_RSX_RG %>% 
  line_plot(public_health, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)
# main df
sample = sub %>% slice_sample(n = nrow(sub)/length(unique(sub$year)))
panel_sample = panel_data(sample, id = id, wave = year)

panel_sample %>% 
  line_plot(income, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_sample %>% 
  line_plot(SRH, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_sample %>% 
  filter(scghqi > 0) %>%
  line_plot(scghqi, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

panel_sample %>% 
  filter(scghql > 0) %>%
  line_plot(scghql, 
            add.mean = TRUE,
            mean.function = "loess", 
            alpha = 0.2)

# inspecting for separate persons
panel = panel_data(sub, id = id, wave = year)

ts.plot(panel[panel$id == '884010211', 'income'],
        col = "mediumseagreen", lwd = 4)
ts.plot(panel[panel$id == '884010211', 'SRH'],
        col = "mediumseagreen", lwd = 4)

# lags
panel = panel %>% 
  mutate(public_health_lag = lag(public_health),
         skills_funding_lag = lag(skills_funding))

# MLM
model.1 = lmer(scghqi ~ 1 + scale(dvage, center = F) + scale(skills_funding_lag) + scale(public_health_lag) + sex + (1 | id) + (1 | LAD21CD), panel)
icc(model.1)
summary(model.1)

model.2 = lmer(SRH ~ 1 + scale(dvage, center = F) + scale(skills_funding_lag) + scale(public_health_lag) + sex + (1 | id) + (1 | LAD21CD), panel)
summary(model.2)

# missing values

# filter inapplicable and impute the rest
socdem = c('sex', 'employ', 'marstat', 'ethn_dv', 'urban_dv', 'dvage')
vars_selected = c(GHQ, socdem, 'SRH')
ids = c('id', 'LAD21CD', 'la', 'wave')
sub = sub %>% filter(across(all_of(vars_selected), function(x) !x == -8))
for (i in vars){
  sub[, i] = ifelse(sub[, i] < 0, NA, sub[, i])
}
table(sub$scghql)
table(sub$scghqa)
table(sub$scghqb, sub$wave)

# imputation
a.out <- amelia(sub, m = 5,
                idvars = ids,
                noms = socdem[-length(socdem)],
                ords = c(GHQ, 'SRH', 'dvage'))
dt <- a.out$imputations$imp3

summary(dt)
table(dt$scghqc)

# reverse coding
# table(dt$scghqa)
# vars_to_recode = c('scghqa', 'scghqc', 'scghqd', 'scghqg', 'scghqh', 'scghql')
# keys = rep(1, length(GHQ))
# keys[which(GHQ %in% vars_to_recode)] = -1
# dt[, GHQ] = psych::reverse.code(keys, dt[,GHQ])
# table(dt$scghqa)

# constructing a measure of psychological distress
model = '
GHQ =~ scghqa + scghqb + scghqc + scghqd + scghqe + scghqf + scghqg + scghqh + scghqi + scghqj + 1*scghqk + scghql
'
fit = cfa(model, cluster = 'id', data = dt)
summary(fit, fit.measures = T, standardized = T)

model = '
GHQ =~ scghqb + scghqe + scghqf + 1*scghqi + scghqj + scghqk
'
fit = cfa(model, cluster = 'id', data = dt, estimator = 'ML')
summary(fit, fit.measures = T, standardized = T)

# predict GHQ values
dt$GHQ = lavPredict(fit)

# look at the distribution
hist(dt[dt$year == 2013, 'GHQ'])
hist(dt[dt$year == 2014, 'GHQ'])
hist(dt[dt$year == 2015, 'GHQ'])
hist(dt[dt$year == 2016, 'GHQ'])
hist(dt[dt$year == 2017, 'GHQ'])
hist(dt[dt$year == 2018, 'GHQ'])
hist(dt[dt$year == 2019, 'GHQ'])
hist(dt[dt$year == 2020, 'GHQ'])



















