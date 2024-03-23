

#-------------------------------------------------------------------------------
## rcclpm
#-------------------------------------------------------------------------------

library(MASS)

model_number = 4
pars_grid_current = pars_grid_list[[model_number]]
nvar = 14
model_type = 'cross-lag'
n_per_cluster=50
i=134
dataframe = gen_data(N2=50)

# Function to generate correlated Z within each cluster ID
generate_correlated_Z <- function(cluster_data) {
  cluster_data <- cluster_data %>%
    group_by(ID) %>%
    mutate(
      Z = X1 + Y1 + rnorm(1, mean = 0, sd = 0.5)
    )
  return(cluster_data)
}

# Generate the confounder Z
dataframe <- generate_correlated_Z(dataframe)
cor(dataframe[dataframe$ID == 1, 'X1'], dataframe[dataframe$ID == 1, 'Z'])
cor(dataframe[dataframe$ID == 1, 'Y1'], dataframe[dataframe$ID == 1, 'Z'])

temp2 = dataframe %>%
  group_by(ID) %>%
  mutate(across(starts_with('X'), 
                ~mean(.x), 
                .names = "{.col}"))

temp3 = dataframe %>% group_by(ID) %>% summarise_all(.funs = mean)

# generate a random variable for dataframe with correlation 0.4
dataframe$control = dataframe$X1 * 0.2 + sqrt(1 - 0.2^2) * rnorm(100)
cor(dataframe$X1, dataframe$control)

m_est_rcgclm = RC_GCLM_syntax(endogeneous = c('Y', 'X'),
                              model = 'recgclm',
                              max_time = 7,
                              control = 'Z')

fit_Xmean = sem(m_est_rcgclm,
                data = temp2, 
                estimator = "mlr",
                orthogonal = T, 
                cluster = 'ID')
#summary(fit_Xmean)
tidy(fit_Xmean) %>% 
  filter(label %in% c('b_XX', 'b_XY', 'b_YX', 'b_YY'#,
                     # 'd_XX', 'd_XY', 'd_YX', 'd_YY'
                      )) %>%
  group_by(label) %>%
  slice(1) %>%
  pull(estimate)

fit_XYmean = sem(m_est_rcgclm,
                data = temp3, 
                estimator = "mlr",
                orthogonal = T)
#summary(fit_XYmean)
tidy(fit_XYmean) %>% 
  filter(label %in% c('b_XX', 'b_XY', 'b_YX', 'b_YY'#,
                      #'d_XX', 'd_XY', 'd_YX', 'd_YY'
                      ))  %>%
  group_by(label) %>%
  slice(1) %>%
  pull(estimate)

#-------------------------------------------------------------------------------

model_number = 1
pars_grid_current = pars_grid_list[[model_number]]
nvar = 2
model_type = 'lm'
library(MASS)
n_per_cluster=100
i=134
dataframe = gen_data(N2=50)

# Function to generate correlated Z within each cluster ID
generate_correlated_Z <- function(cluster_data) {
  n=50
  cluster_data <- cluster_data %>%
    group_by(ID) %>%
    mutate(
      Z = X + 
        Y + rnorm(n, mean = 0, sd = 0.5)
    )
  return(cluster_data)
}

# Generate the confounder Z
dataframe <- generate_correlated_Z(dataframe)
cor(dataframe[dataframe$ID == 1, 'X'], dataframe[dataframe$ID == 1, 'Z'])
cor(dataframe[dataframe$ID == 1, 'Y'], dataframe[dataframe$ID == 1, 'Z'])

temp2 = dataframe %>%
  group_by(ID) %>%
  mutate(across(starts_with('X'), 
                ~mean(.x), 
                .names = "{.col}_mean"))

temp2.1 = dataframe %>%
  group_by(ID) %>%
  mutate(across(starts_with('X'), 
                ~mean(.x), 
                .names = "{.col}_mean")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(X_demeaned = X - X_mean)
summary(temp2.1)

temp3 = dataframe %>% group_by(ID) %>% summarise_all(.funs = mean)

summary(m.z0 <- lme4::lmer(Y ~ X + (1|ID), temp2.1))
summary(m.z1 <- lme4::lmer(Y ~ X + Z + (1|ID), temp2.1))
summary(m.z2 <- lm(Y ~ X + Z, temp3))

summary(mx <- lme4::lmer(X ~ 1 + (1|ID), dataframe))
icc = performance::icc(mx)[[1]]
#icc = 0.4

summary(m1 <- lme4::lmer(Y ~ X + (1|ID), temp2.1))
summary(m2 <- lme4::lmer(Y ~ X_mean + (1|ID), temp2.1))
summary(m2.z <- lme4::lmer(Y ~ X_mean + Z + (1|ID), temp2.1))
summary(m7.z <- lm(Y ~ X + Z , temp3))

summary(m2.1 <- lme4::lmer(Y ~ X_mean + X_demeaned + (1|ID), temp2.1))
summary(lme4::lmer(Y ~  X_mean + (1|ID), temp2.1))
summary(lme4::lmer(Y ~  X + (1|ID), temp2.1))

summary(m3 <- lm(Y ~ I(X-mean(X)), temp2.1))
summary(m4 <- lm(Y ~ X_mean , temp2.1))
summary(m5 <- lm(Y ~ X , temp3))
summary(m4.1 <- lm(Y ~ X_mean + X_demeaned, temp2.1))
summary(lm(Y ~ X_mean, temp2.1))
summary(m6<-lm(Y ~ X , temp2.1))

library(sandwich)
library(lmtest)
library(lavaan.survey)

install.packages("C:/Users/ru21406/Downloads/lavaan.survey_1.1.3.1.tar.gz", type = "source", repos = NULL)
cl_vcov_mat <- vcovCL(m4, cluster = ~ID)
coeftest(m4, vcov = cl_vcov_mat)
#coefci(m4, vcov = cl_vcov_mat)

cl_vcov_mat <- vcovCL(m4.1, cluster = ~ID)
coeftest(m4.1, vcov = cl_vcov_mat)
#coefci(m4.1, vcov = cl_vcov_mat
#

m_est_lm = '
Y ~ meanY*1 + b_YX*X

#Y ~~ varY*Y
#X ~~ varX*X

'
fit = sem(m_est_lm,
          estimator = 'mlr',
          data = temp2.1,
          cluster = 'ID')
summary(fit, fit.measures = F)
myDesign <- survey::svydesign( ids =~ID,weights = ~1,data = temp2.1)
fit.svy <- lavaan.survey(fit, survey.design = myDesign)
summary(fit.svy)

wg_size = 50
lavOptions("cluster")
out2L <- sim(seed = 12345,
             nRep = 70, 
             n = 50, 
             generate = gen_data,
             model = m_est_lm,
             se = 'robust',
             cluster = "ID", 
             lavaanfun = "sem")
look1 = cbind.data.frame(cilower=out2L@cilower[,'b_YX'], 
                        ciupper=out2L@ciupper[,'b_YX']) %>%
  mutate(coverage = ifelse(cilower < 0.6 & ciupper > 0.6, 1, 0))
summary(look1$coverage)
look1_ = cbind.data.frame(cilower=out2L@cilower[,'b_YX'], 
                         ciupper=out2L@ciupper[,'b_YX']) %>%
  mutate(coverage = ifelse(cilower < 0.6 & ciupper > 0.6, 1, 0))
summary(look1_$coverage)

wg_size = 20
out2L2 <- sim(seed = 12345,
             nRep = 70, 
             n = 50, 
             generate = gen_data,
             model = m_est_lm,
             cluster = "ID", 
             lavaanfun = "sem")
look2 = cbind.data.frame(cilower=out2L2@cilower[,'b_YX'], 
                         ciupper=out2L2@ciupper[,'b_YX']) %>%
  mutate(coverage = ifelse(cilower < 0.6 & ciupper > 0.6, 1, 0))
summary(look2$coverage)









k=1/(VarCorr(m1)$ID[1])/(1/(VarCorr(m1)$ID[1]) + 1/attr(VarCorr(m1), "sc")^2 )



k = icc + (1-icc)/n
(m1@beta[2]-m2.1@beta[3])/(m2.1@beta[2]-m2.1@beta[3])
(m1@beta[2]-m2@beta[3])/(m2@beta[2]-m2@beta[3])

m1@beta[2]
m2.1@beta[2]*k + (1-k)*m2.1@beta[3]
m2.1@beta[2]*icc + (1-icc)*m2.1@beta[3]

coef(m3)[2]
coef(m4.1)[2]*k + (1-k)*coef(m4.1)[3]
coef(m4.1)[2]*icc + (1-icc)*coef(m4.1)[3]


n=2500
m1@beta[2] - m2@beta[2]
icc*(m2@beta[2] - m2@beta[3])
n*(icc-1)/n*(m2@beta[2] - m2@beta[3])



coef(m3)[2] - coef(m4)[2]
icc*(coef(m4)[2] - coef(m4)[3])
49*(icc-1)/50*(coef(m4)[2] - coef(m4)[3])





summary(lm(Y ~ X, dataframe))
summary(m0 <- lm(Y ~ X, temp2))
summary(lm(Y ~ X, temp3))

summary(lme4::lmer(Y ~ X + (1|ID), dataframe))

summary(lme4::lmer(Y ~ X_mean + (1|ID), temp2))

# eq 4
summary(m <- lme4::lmer(Y ~ X_mean + X_demeaned + (1|ID), temp2.1))

summary(mx <- lme4::lmer(X ~ 1 + (1|ID), temp2.1))
icc = performance::icc(mx)[[1]]
reliability <- VarCorr(mx)$ID[1]  / (VarCorr(mx)$ID[1] + attr(VarCorr(mx), "sc")^2  / n_per_cluster)
reliability*(m@beta[2]) + (1-reliability)*m@beta[3]

coef(m2)[2]
coef(m0)[2]
m@beta[2]

eta = icc/reliability
eta*(m@beta[2] + m@beta[3]) + (1-eta)*m@beta[3]





