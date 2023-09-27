library(MASS)

par_tab = list(
  c('b_HEHE',5),
  c('b_HEas',1),
  c('b_asHE','(-5)'),
  c('b_asas',20),
  c('covar_iHE.ias','(-21)'),
  c('covar_iHE.sHE',1),
  c('covar_iHE.sas','(-0.5)'),
  c('covar_ias.sHE','(-0.5)'),
  c('covar_ias.sas','(-1)'),
  c('covar_sHE.sas','(-0.1)'),
  c('d_HEHE',20),
  c('d_HEas','(-1)'),
  c('d_asHE',1),
  c('d_asas',1),
  c('ecov_HEas',0.1),
  c('evarHE',1),
  c('evaras',10),
  c('mean_i_HE','mean_i_HE'),
  c('mean_i_as','mean_i_as'),
  c('mean_s_HE','(-20)'),
  c('mean_s_as','(-5)'),
  c('var_iHE',55),
  c('var_ias',41),
  c('var_sHE',0.5),
  c('var_sas',0.5))

syntax_sim = RC_GCLM_syntax(endogeneous = c('HE', 'as'),
                            control = NULL)
fit_init = sem(syntax_sim,
               data = df_lv,
               estimator = "mlr",
               cluster = 'LAD21CD',
               orthogonal = T)
summary(fit_init)
param_init = parameterestimates(fit_init) %>% 
  filter(!label == '') %>%
  group_by(label) %>%
  slice(1) %>%
  dplyr::select(label, est) %>%
  mutate(est = round(est, 4))

par_tab = list(
  c('b_HEHE',0.05),
  c('b_HEas',0.01),
  c('b_asHE','(-0.05)'),
  c('b_asas',0.2),
  c('covar_iHE.ias','covar_iHE.ias'),
  c('covar_iHE.sHE',0.01),
  c('covar_iHE.sas','(-0.005)'),
  c('covar_ias.sHE','(-0.005)'),
  c('covar_ias.sas','(-0.01)'),
  c('covar_sHE.sas','(-0.001)'),
  c('d_HEHE',0.2),
  c('d_HEas','(-0.01)'),
  c('d_asHE',0.01),
  c('d_asas',0.01),
  c('ecov_HEas',0.001),
  c('evarHE',0.01),
  c('evaras',0.1),
  c('mean_i_HE','mean_i_HE'),
  c('mean_i_as','mean_i_as'),
  c('mean_s_HE','(-0.2)'),
  c('mean_s_as','(-0.05)'),
  c('var_iHE','var_iHE'),
  c('var_ias','var_ias'),
  c('var_sHE',0.005),
  c('var_sas',0.005))



par_tab = do.call(rbind.data.frame, par_tab)
colnames(par_tab) = c('label', 'par')

par_tab_list = list()
cov_vec = seq(-0.5, 0.5, 0.1)

pars = numeric(0)
for (i in seq_along(cov_vec)){
  means = as.data.frame(mvrnorm(n = 100,
                 mu = c(0, 0),
                 Sigma = matrix(c(1, cov_vec[i],
                                  cov_vec[i], 1),
                                nrow = 2)),
                 empirical = T)
  var_ias = var(means[,1])
  var_iHE = var(means[,2])
  covar_iHE.ias = cov(means)[1,2]
  pars_ = t(rbind(var_ias, var_iHE, covar_iHE.ias))
  pars = rbind.data.frame(pars, pars_)
  pars %<>% arrange(covar)
}

covar_iHE.ias = seq(-0.3, 0.3, 0.1)

par_tab$par[par_tab$label == 'var_ias'] = cov(means)[1,1]
par_tab$par[par_tab$label == 'var_iHE'] = cov(means)[2,2]
par_tab$par[par_tab$label == 'covar_iHE.ias'] = cov(means)[1,2]

for (i in 1:nrow(means)){
  par_tab$par[par_tab$label == 'mean_i_as'] = means[i,1]
  par_tab$par[par_tab$label == 'mean_i_HE'] = means[i,2]
  par_tab_list[[i]] = par_tab
}


synt_list = list()
for (synt in seq_along(par_tab_list)){
  
  restricted_pars = par_tab_list[[synt]]
  synt_list[[synt]] = syntax_sim
  
  for (i in 1:nrow(restricted_pars)){
    synt_list[[synt]] = gsub(restricted_pars[i,1],
                             restricted_pars[i,2],
                             synt_list[[synt]]) %>%
    glue_collapse("\n")
  }
  
  synt_list[[synt]] = gsub('\\+ -', '- ', synt_list[[synt]])
}

Output = list()
n_samples = length(synt_list)
N = 100
seed_vec = seq(1, 100, 1)
for (n_samples in 1:n_samples){
  Output[[n_samples]] = sim(20, 
                            model = synt_list[[n_samples]],
                            dataOnly = T,
                            seed = seed_vec[n_samples],
                            n = N,
                            generate = synt_list[[n_samples]],
                            estimator = "mlr",
                            orthogonal = T)
}

lst_test = list()
for (i in 1:length(Output[[1]])){
  lst_test[[i]] = cbind.data.frame(do.call(rbind.data.frame,
                                lapply(Output, function(lst) lst[[i]])),
                                LAD21CD = rep(1:N, each = n_samples))
}
fit_sim = sem(syntax_sim,
              data = df_lv,
              estimator = "mlr",
              cluster = 'LAD21CD',
              orthogonal = T)
test = sim(seed = 12345, 
           model = fit_sim,
           rawData = lst_test)
sumtab = summaryParam(test)






temp1 = cbind.data.frame(do.call(rbind, Output),
                         LAD21CD = rep(1:100, each = n_samples))
temp2 = temp1 %>%
  group_by(LAD21CD) %>%
  mutate(across(starts_with('as'), 
                function(x) mean(x)))

temp3 = temp1 %>% group_by(LAD21CD) %>% summarise_all(.funs = mean)

cor(temp1$as1, temp1$HE1)
within_cor = temp1 %>%
  group_by(LAD21CD) %>%
  summarise(cor = cor(as1, HE1))
mean(within_cor$cor)

cor(temp2$as1, temp2$HE1)
cor(temp3$as1, temp3$HE1)
cor(temp2)
cor(temp1)
cor(temp1$as2,temp1$as3)
cor(temp1$as4[temp1$LAD21CD == 1],
    temp1$as7[temp1$LAD21CD == 1])
cor(temp2$as3,temp2$as4)
cor(temp2$HE3,temp2$HE4)

summary(temp1)
summary(temp2)

cor(temp1)
cor(temp2)
cor(temp3)

fit_temp1 = sem(syntax_sim,
                data = temp1,
                estimator = "mlr",
                cluster = 'LAD21CD',
                orthogonal = T
                )
summary(fit_temp1)
varTable(fit_temp1)

fit_temp2 = sem(syntax_sim,
                data = temp2,
                estimator = "mlr",
                cluster = 'LAD21CD',
                orthogonal = T)
varTable(fit_temp2)
summary(fit_temp2)

fit_temp3 = sem(syntax_sim,
                data = temp3,
                estimator = "mlr",
                orthogonal = T)
summary(fit_temp3)

m1_par = parameterestimates(fit_temp1) %>% 
  filter(!label == '') %>%
  group_by(label) %>%
  slice(1) %>%
  dplyr::select(label, est1 = est) %>%
  mutate(est1 = round(est1, 4))
m2_par = parameterestimates(fit_temp2) %>% 
  filter(!label == '') %>%
  group_by(label) %>%
  slice(1) %>%
  dplyr::select(label, est2 = est) %>%
  mutate(est2 = round(est2, 4))
m3_par = parameterestimates(fit_temp3) %>% 
  filter(!label == '') %>%
  group_by(label) %>%
  slice(1) %>%
  dplyr::select(label, est3 = est) %>%
  mutate(est3 = round(est3, 4))


tab_m3 = m1_par %>% left_join(m2_par) %>% left_join(m3_par)

