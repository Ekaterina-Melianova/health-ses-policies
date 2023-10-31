#paper3.R

# libraries
library(readxl)
library(plyr); library(dplyr)
library(magrittr)
library(glue)
library(tidyverse)
library(lavaan)
library(data.table)
library(tidyr)
library(broom)

library(parallel)
library(doParallel)
library(tictoc)
library(simsem)

library(gridExtra)
library(cowplot)
library(hrbrthemes)
library(RColorBrewer)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_lv.RDS')
df_lv %<>% dplyr::rename(!!!setNames(c('LAD21CD', paste0("as", 1:7)),
                              c('ID', paste0("SP", 1:7)))) %>%
  dplyr::select('ID', starts_with('SP'), starts_with('HE'))

# functions

ToLong = function(dat,
                  all_ids = c("ID", "id")){
  
  setDT(dat)
  
  # Generate id column
  dat[, id := .I]
  
  # Identify columns to melt
  melt_cols = setdiff(names(dat), all_ids)
  
  dat = melt(dat, 
             id.vars = all_ids,
             measure.vars = melt_cols,
             variable.name = "time",
             value.name = "value")
  
  # Split the time column into value and time parts using regex
  dat[, c("var_name", "time") := tstrsplit(time, "(?<=\\D)(?=\\d)", perl=TRUE)]
  
  # Extract numeric part from 'time' and convert to numeric
  dat[, time := as.numeric(sub("\\D", "", time))]
  
  dat = dcast(dat, ... ~ var_name, value.var = "value")
  
  
  return(dat)
}

aggregate_x = function(data) {
  dt1 = setDT(data)
  cols = grep("^S", names(dt1))
  dt1[, (cols) := lapply(.SD, mean), by = "ID", .SDcols = cols]
  return(dt1)
}

summarise_simultation = function(true, missp, pars_grid_current){
  summary_out = list()
  
  #converged = unlist(lapply(1:nrow(pars_grid_current), 
  #                          function(i) !any(is.na(true[[35]]@coef))))
  
  #for (i in which(converged == T)){
  for (i in seq_along(true)){
    missp[[i]]@paramValue = as.data.frame(t(colMeans(true[[i]]@coef, na.rm=T)))
    summary_out[[i]] = summaryParam(missp[[i]],
                                    detail = TRUE,
                                    digits = 3) %>%
      rownames_to_column('reg_pars')
  }
  
  summary_out = cbind(do.call(rbind.data.frame, summary_out),
                      #pars_grid_current[which(converged == T),] %>%
                        pars_grid_current %>%
                        slice(rep(1:n(),
                                  each = ncol(true[[1]]@coef))))
  
  
  
  return(summary_out)
}

#icc_range_ = c(0.1, 0.3, 0.5, 0.7, 0.9)
icc_range_ = c(0.05, 0.35, 0.65, 0.85)
#icc_range_ = c(0.1, 0.5,  0.9)
create_L1_L2 = function(par_tab,
                        m_est,
                        icc_range = icc_range_,
                        evarSP,
                        var_iSP,
                        var_sSP,
                        varSP,
                        evarHE,
                        var_iHE,
                        var_sHE,
                        varHE) {
  
  # Loop through parameter table and replace values in the model
  m_L1 = m_est
  for (i in 1:nrow(par_tab)) {
    m_L1 = gsub(par_tab[i,1], par_tab[i,2], m_L1) %>%
      glue_collapse("\n")
  }
  
  m_L1 = gsub('evarSP', evarSP, m_L1)
  m_L1 = gsub('var_iSP', var_iSP, m_L1)
  m_L1 = gsub('var_sSP', var_sSP, m_L1)
  
  m_L1 = gsub('evarHE', evarHE, m_L1)
  m_L1 = gsub('var_iHE', var_iHE, m_L1)
  m_L1 = gsub('var_sHE', var_sHE, m_L1)
  
  m_L1 = gsub('varSP', varSP, m_L1)
  m_L1 = gsub('varHE', varHE, m_L1)
  
  
  withinSP = sum(evarSP, var_iSP, var_sSP, varSP, na.rm = T)
  withinHE = sum(evarHE, var_iHE, var_sHE, varHE, na.rm = T)
  
  betweenSP = c()
  for (i in seq_along(icc_range)){
    betweenSP[i] = withinSP*icc_range[i]/(1-icc_range[i])
  }
  betweenHE = withinHE*0.35/(1-0.35)
  
  # Loop through variable vector and create a list of models with replaced values
  
  
  m_L2 = lapply(betweenSP, function(x) {
    m_L2 = m_est
    for (i in 1:nrow(par_tab)) {
      m_L2 = gsub(par_tab[i,1], par_tab[i,2], m_L2) %>%
        glue_collapse("\n")
    }  
      
    m_L2 = gsub('evarSP', x*evarSP/withinSP, m_L2)
    m_L2 = gsub('var_iSP', x*var_iSP/withinSP, m_L2)
    m_L2 = gsub('var_sSP', x*var_sSP/withinSP, m_L2)
    m_L2 = gsub('varSP', x*varSP/withinSP, m_L2)
    
    m_L2 = gsub('evarHE', betweenHE*evarHE/withinHE, m_L2)
    m_L2 = gsub('var_iHE', betweenHE*var_iHE/withinHE, m_L2)
    m_L2 = gsub('var_sHE', betweenHE*var_sHE/withinHE, m_L2)
    m_L2 = gsub('varHE', betweenHE*varHE/withinHE, m_L2)
    return(m_L2)
  })
  
  out = list(m_est = m_est,
             m_L1 = m_L1,
             m_L2 = m_L2)
  return(out)
}

gen_data = function(N2) {
  
  NperC = rep(wg_size, N2)
  model_L1 = pars_grid_current[i, 'model_L1']
  model_L2 = pars_grid_current[i, 'model_L2']
  par_value = pars_grid_current[i, 'L1']
  
  k1 = paste0('(', par_value[1], ')')
  k2 = paste0('(', par_value[1], ')')
  
  popL1 = gsub('k', k1, model_L1) %>%
    glue_collapse("\n")
  popL2 = gsub('k', k2, model_L2) %>%
    glue_collapse("\n")
  
  
  ## model-implied covariance matrices
  
  if (model_type == 'growth'){
    
    Sigma1 = fitted(lavaan::growth(popL1,estimator = 'mlr'))$cov
    Sigma2 = fitted(lavaan::growth(popL2, estimator = 'mlr'))$cov
    
  } else if(model_type == 'lm'){
    
    Sigma1 = fitted(lavaan::sem(popL1,estimator = 'mlr'))$cov
    Sigma2 = fitted(lavaan::sem(popL2, estimator = 'mlr'))$cov
  } else{
    
    Sigma1 = fitted(lavaan::sem(popL1,
                                orthogonal = T,
                                estimator = 'mlr'))$cov
    Sigma2 = fitted(lavaan::sem(popL2,
                                orthogonal = T,
                                estimator = 'mlr'))$cov
  }
  
  ## make sure order of names is consistent
  Sigma2 = Sigma2[rownames(Sigma1), colnames(Sigma1)]
  
  clusterList = lapply(1:N2, function(x) {
    L2comp = MASS::mvrnorm(n = 1, mu = rep(0, nvar), Sigma = Sigma2)
    dat2 = t(L2comp) %x% rep(1, NperC[x]) # apply to each L1 component
    dat1 = MASS::mvrnorm(n = NperC[x], mu = rep(0, nvar), Sigma = Sigma1, empirical = T)
    
    ## combine components
    dat = data.frame(dat1 + dat2, ID = rep(x, NperC[x]))
    dat
  })
  
  return(do.call(rbind, clusterList))
}  

find_uncoverged_params = function(out_list){
  converge_list = list()
  for (i in seq_along(out_list)){
    tryCatch({
      converge_list[[i]] = summaryConverge(out_list[[i]])
    }, error = function(x) {
      x[[i]] = 'error'
    })
  }
  uncoverged = which(unlist(lapply(converge_list, function(x) !is.null(x))))
  unconverged_params = pars_grid_current[uncoverged, 1:2]
  
  return(unconverged_params)
}

# Define the models

{
m_est_lm = '
HE ~ meanHE*1 + b_HESP*SP

HE ~~ varHE*HE
SP ~~ varSP*SP

'

m_est_growth = '
iHE =~ 1*HE1 + 1*HE2 + 1*HE3 + 1*HE4 + 1*HE5
sHE =~ 0*HE1 + 1*HE2 + 2*HE3 + 3*HE4 + 4*HE5

iHE ~ b_HESP*SP
sHE ~ SP

iHE ~~ cov_iHE.sHE*sHE
iHE ~~ var_iHE*iHE
sHE ~~ var_sHE*sHE

iHE ~ mean_i_HE*1
sHE ~ mean_s_HE*1

HE1 ~~ varHE*HE1
HE2 ~~ varHE*HE2
HE3 ~~ varHE*HE3
HE4 ~~ varHE*HE4
HE5 ~~ varHE*HE5

SP ~~ varSP*SP

HE1 + HE2 + HE3 + HE4 + HE5 ~ 0*1

'

}

m_est_rcclpm = RC_GCLM_syntax(endogeneous = c('HE', 'SP'),
                              control = NULL,
                              model = 'reclpm',
                              max_time = 5)
m_est_rcgclm = RC_GCLM_syntax(endogeneous = c('HE', 'SP'),
                              control = NULL,
                              model = 'recgclm',
                              max_time = 5)


par_tab_rcgclm_long = data.frame(
  label = c('b_HEHE', 'b_HESP', 'b_SPHE', 'b_SPSP',
            'cov_iHE.iSP', 'cov_iHE.sHE', 'cov_iHE.sSP', 'cov_iSP.sHE', 'cov_iSP.sSP', 'cov_sHE.sSP',
            'd_HEHE', 'd_HESP', 'd_SPHE', 'd_SPSP',
            'ecov_HESP', 'evarHE', 'evarSP',
            'mean_i_HE', 'mean_i_SP', 'mean_s_HE', 'mean_s_SP',
            'var_iHE', 'var_iSP', 'var_sHE', 'var_sSP'),
  par = c(0.03, 'k', '(-0.06)', '(-0.2)', 
          '(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.25)', '(-0.004)', '(-0.06)', '(-0.2)',
          0.002, 'evarHE', 'evarSP',
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iHE', 'var_iSP', 'var_sHE', 'var_sSP')
)

par_tab_rcgclm_short = data.frame(
  label = c('b_HEHE', 'b_HESP', 'b_SPHE', 'b_SPSP',
            'cov_iHE.iSP', 'cov_iHE.sHE', 'cov_iHE.sSP', 'cov_iSP.sHE', 'cov_iSP.sSP', 'cov_sHE.sSP',
            'd_HEHE', 'd_HESP', 'd_SPHE', 'd_SPSP',
            'ecov_HESP', 'evarHE', 'evarSP',
            'mean_i_HE', 'mean_i_SP', 'mean_s_HE', 'mean_s_SP',
            'var_iHE', 'var_iSP', 'var_sHE', 'var_sSP'),
  par = c(0.03, 0.006, '(-0.06)', '(-0.2)', 
          '(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.25)', 'k', '(-0.06)', '(-0.2)',
          0.002, 'evarHE', 'evarSP',
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iHE', 'var_iSP', 'var_sHE', 'var_sSP')
)

par_tab_rcclpm = data.frame(
  label = c('cov_iHE.iSP', 'cov_iHE.sHE', 'cov_iHE.sSP', 'cov_iSP.sHE', 'cov_iSP.sSP', 'cov_sHE.sSP', 
            'd_HEHE', 'd_HESP', 'd_SPHE', 'd_SPSP',
            'ecov_HESP', 'evarHE', 'evarSP',
            'mean_i_HE', 'mean_i_SP', 'mean_s_HE', 'mean_s_SP', 
            'var_iHE', 'var_iSP', 'var_sHE', 'var_sSP'),
  par = c('(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.5)', 'k', '(0.06)', '(0.2)',
          0.003, 'evarHE', 'evarSP', 
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iHE', 'var_iSP', 'var_sHE', 'var_sSP')
)

par_tab_lm = data.frame(
  label = c('b_HESP', 'varHE', 'meanHE', 'meanSP', 'varSP'),
  par = c('k', 'var_HE', 0.4, '(-0.07)', 'varSP')
)

par_tab_growth = data.frame(
  label = c('b_HESP', 
            'cov_iHE.sHE', 'var_iHE', 'var_sHE',
            'mean_i_HE', 'mean_s_HE',
            'varSP'),
  par = c('k',
          0.03, 'var_iHE', 'var_sHE',
          0.4, -0.2,
          'varSP')
)

growth_lm = create_L1_L2(par_tab=par_tab_lm,
                         m_est=m_est_lm,
                         evarSP = NA,
                         var_iSP = NA,
                         var_sSP = NA,
                         varSP = 0.8,
                         evarHE = NA,
                         var_iHE = NA,
                         var_sHE = NA,
                         varHE = NA)
growth_models = create_L1_L2(par_tab=par_tab_growth,
                             m_est=m_est_growth,
                             evarSP = NA,
                             var_iSP = NA,
                             var_sSP = NA,
                             varSP = 0.8,
                             evarHE = NA,
                             var_iHE = 0.6,
                             var_sHE = 0.07,
                             varHE = 0.02)
rcclpm_models = create_L1_L2(par_tab=par_tab_rcclpm,
                             m_est=m_est_rcclpm,
                             evarSP = 0.05,
                             var_iSP = 0.4,
                             var_sSP = 0.05,
                             varSP = NA,
                             evarHE = 0.05,
                             var_iHE = 0.6,
                             var_sHE = 0.05,
                             varHE = NA)
rcgclm_models_long = create_L1_L2(par_tab=par_tab_rcgclm_long,
                                  m_est=m_est_rcgclm,
                                  evarSP = 0.05,
                                  var_iSP = 0.4,
                                  var_sSP = 0.05,
                                  varSP = NA,
                                  evarHE = 0.05,
                                  var_iHE = 0.6,
                                  var_sHE = 0.05,
                                  varHE = NA)
rcgclm_models_short = create_L1_L2(par_tab_rcgclm_short,
                                   m_est_rcgclm,
                                   evarSP = 0.05,
                                   var_iSP = 0.4,
                                   var_sSP = 0.05,
                                   varSP = NA,
                                   evarHE = 0.05,
                                   var_iHE = 0.6,
                                   var_sHE = 0.05,
                                   varHE = NA)
all_m = list(growth_lm, growth_models, rcclpm_models, rcgclm_models_long, rcgclm_models_short)
names(all_m) = c('growth_lm', 'growth_models', 'rcclpm', 'rcgclm_long', 'rcgclm_short')

# params for upper and lower levels
pars_grid_list = list()
pars = seq(-0.5, 0.5, 0.1)
pars_grid = expand.grid(L1 = pars, icc = icc_range_)

for (i in seq_along(all_m)){
  mod = names(all_m)[i]
  
  pars_grid_list[[i]] = pars_grid
  pars_grid_list[[i]]$model_est = all_m[[i]][['m_est']]
  pars_grid_list[[i]]$model_L1 = all_m[[i]][['m_L1']]
  pars_grid_list[[i]]$model_L2 = rep(all_m[[i]][['m_L2']], each = length(pars))
  
  if(mod %in% c('model_growth', 'model_lm')){
    pars_grid_list[[i]]$orthogonal = F
  } else {
    pars_grid_list[[i]]$orthogonal = T
  }
  
  if(mod %in% 'model_growth'){
    pars_grid_list[[i]]$lavaanfun = 'growth'
  } else {
    pars_grid_list[[i]]$lavaanfun = 'sem'
  }
}

#-------------------------------------------------------------------------------
#-----------------------------------Simulation----------------------------------
#-------------------------------------------------------------------------------

# Progress bar setup
pb = txtProgressBar(max = nrow(pars_grid_current), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress = progress)
wg_size = 50
nRep = 200
n = 50

# lm
model_number = 1
pars_grid_current = pars_grid_list[[model_number]]
nvar = 2

tic()
cl = makeCluster(14)
registerDoSNOW(cl)                     
true_out_lm = foreach(i = 1:nrow(pars_grid_current),
                      .packages = c("simsem","lavaan","dplyr","glue","broom"), 
                      .options.snow = opts#,
                      #.export = c('nvar', 'wg_size', 'gen_data', 'n', 'nRep')
                      ) %dopar% {
                        
                        simsem::sim(seed = 12345,
                                    nRep = nRep,
                                    n = n,
                                    generate = gen_data, 
                                    model = pars_grid_current[i, 'model_est'], 
                                    lavaanfun = 'sem', 
                                    orthogonal = F,
                                    estimator = 'mlr', 
                                    cluster = "ID")
                      }
missp_out_lm = foreach(i = 1:nrow(pars_grid_current),
                      .packages = c("simsem","lavaan","dplyr","glue","broom","data.table"), 
                      .options.snow = opts#,
                      #.export = c('nvar', 'wg_size', 'gen_data', 'n', 'nRep')
                      ) %dopar% {
                        
                        simsem::sim(seed = 12345,
                                    nRep = nRep,
                                    n = n,
                                    generate = gen_data, 
                                    model = pars_grid_current[i, 'model_est'], 
                                    lavaanfun = 'sem', 
                                    orthogonal = F,
                                    estimator = 'mlr', 
                                    cluster = "ID",
                                    datafun = aggregate_x)
                      }
close(pb)
stopCluster(cl)
beepr::beep()
toc()

#find_uncoverged_params(true_out_lm)
#find_uncoverged_params(missp_out_lm)

# growth
model_number = 2
pars_grid_current = pars_grid_list[[model_number]]
nvar = 6

tic()
cl = makeCluster(14)
registerDoSNOW(cl)                     
true_out_growth = foreach(i = 1:nrow(pars_grid_current),
                      .packages = c("simsem","lavaan","dplyr","glue","broom"), 
                      .options.snow = opts) %dopar% {
                        
                        simsem::sim(seed = 12345,
                                    nRep = nRep,
                                    n = n,
                                    generate = gen_data, 
                                    model = pars_grid_current[i, 'model_est'], 
                                    lavaanfun = 'growth', 
                                    estimator = 'mlr', 
                                    cluster = "ID")
                      }
missp_out_growth = foreach(i = 1:nrow(pars_grid_current),
                       .packages = c("simsem","lavaan","dplyr","glue","broom","data.table"), 
                       .options.snow = opts) %dopar% {
                         
                         simsem::sim(seed = 12345,
                                     nRep = nRep,
                                     n = n,
                                     generate = gen_data, 
                                     model = pars_grid_current[i, 'model_est'], 
                                     lavaanfun = 'growth', 
                                     estimator = 'mlr', 
                                     cluster = "ID",
                                     datafun = aggregate_x)
                       }
close(pb)
stopCluster(cl)
beepr::beep()
toc()

#find_uncoverged_params(true_out_growth)
#find_uncoverged_params(missp_out_growth)

# rcclpm
model_number = 3
pars_grid_current = pars_grid_list[[model_number]]
nvar = 10

tic()
cl = makeCluster(14)
registerDoSNOW(cl)                     
true_out_rcclpm = foreach(i = 1:nrow(pars_grid_current),
                      .packages = c("simsem","lavaan","dplyr","glue","broom"), 
                      .options.snow = opts) %dopar% {
                        
                        simsem::sim(seed = 12345,
                                    nRep = nRep,
                                    n = n,
                                    generate = gen_data, 
                                    model = pars_grid_current[i, 'model_est'], 
                                    lavaanfun = 'sem', 
                                    orthogonal = T,
                                    estimator = 'mlr', 
                                    cluster = "ID")
                      }
missp_out_rcclpm = foreach(i = 1:nrow(pars_grid_current),
                       .packages = c("simsem","lavaan","dplyr","glue","broom","data.table"), 
                       .options.snow = opts) %dopar% {
                         
                         simsem::sim(seed = 12345,
                                     nRep = nRep,
                                     n = n,
                                     generate = gen_data, 
                                     model = pars_grid_current[i, 'model_est'], 
                                     lavaanfun = 'sem', 
                                     orthogonal = T,
                                     estimator = 'mlr', 
                                     cluster = "ID",
                                     datafun = aggregate_x)
                       }
close(pb)
stopCluster(cl)
beepr::beep()
toc()

#find_uncoverged_params(true_out_rcclpm)
#find_uncoverged_params(missp_out_rcclpm)


# rcgclm long
model_number = 4
pars_grid_current = pars_grid_list[[model_number]]
nvar = 10

tic()
cl = makeCluster(14)
registerDoSNOW(cl)                     
true_out_rcgclm_long = foreach(i = 1:nrow(pars_grid_current),
                          .packages = c("simsem","lavaan","dplyr","glue","broom"), 
                          .options.snow = opts) %dopar% {
                            
                            simsem::sim(seed = 12345,
                                        nRep = nRep,
                                        n = n,
                                        generate = gen_data, 
                                        model = pars_grid_current[i, 'model_est'], 
                                        lavaanfun = 'sem', 
                                        orthogonal = T,
                                        estimator = 'mlr', 
                                        cluster = "ID")
                          }
missp_out_rcgclm_long = foreach(i = 1:nrow(pars_grid_current),
                           .packages = c("simsem","lavaan","dplyr","glue","broom","data.table"), 
                           .options.snow = opts) %dopar% {
                             
                             simsem::sim(seed = 12345,
                                         nRep = nRep,
                                         n = n,
                                         generate = gen_data, 
                                         model = pars_grid_current[i, 'model_est'], 
                                         lavaanfun = 'sem', 
                                         orthogonal = T,
                                         estimator = 'mlr', 
                                         cluster = "ID",
                                         datafun = aggregate_x)
                           }
close(pb)
stopCluster(cl)
beepr::beep()
toc()


#find_uncoverged_params(true_out_rcgclm_long)
#find_uncoverged_params(missp_out_rcgclm_long)


# rcgclm short
model_number = 5
pars_grid_current = pars_grid_list[[model_number]]
nvar = 10

tic()
cl = makeCluster(14)
registerDoSNOW(cl)                     
true_out_rcgclm_short = foreach(i = 1:nrow(pars_grid_current),
                               .packages = c("simsem","lavaan","dplyr","glue","broom"), 
                               .options.snow = opts) %dopar% {
                                 
                                 simsem::sim(seed = 12345,
                                             nRep = nRep,
                                             n = n,
                                             generate = gen_data, 
                                             model = pars_grid_current[i, 'model_est'], 
                                             lavaanfun = 'sem', 
                                             orthogonal = T,
                                             estimator = 'mlr', 
                                             cluster = "ID")
                               }
missp_out_rcgclm_short = foreach(i = 1:nrow(pars_grid_current),
                                .packages = c("simsem","lavaan","dplyr","glue","broom","data.table"), 
                                .options.snow = opts) %dopar% {
                                  
                                  simsem::sim(seed = 12345,
                                              nRep = nRep,
                                              n = n,
                                              generate = gen_data, 
                                              model = pars_grid_current[i, 'model_est'], 
                                              lavaanfun = 'sem', 
                                              orthogonal = T,
                                              estimator = 'mlr', 
                                              cluster = "ID",
                                              datafun = aggregate_x)
                                }
close(pb)
stopCluster(cl)
beepr::beep()
toc()

#find_uncoverged_params(true_out_rcgclm_short)
#find_uncoverged_params(missp_out_rcgclm_short)

# Summarizing the results

sim_res_lm = summarise_simultation(true_out_lm,
                                   missp_out_lm,
                                   pars_grid_list[[1]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'b_HESP', 'k', reg_pars),
                model = 'lm')

sim_res_growth = summarise_simultation(true_out_growth,
                                       missp_out_growth,
                                       pars_grid_list[[2]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'b_HESP', 'k', reg_pars),
                model = 'growth')

sim_res_rcclpm = summarise_simultation(true_out_rcclpm,
                                       missp_out_rcclpm,
                                       pars_grid_current = pars_grid_list[[3]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'd_HESP <- (e_HE2~e_SP1)', 'k', reg_pars),
                model = 'rcclpm')

sim_res_rcgclm_long = summarise_simultation(true=true_out_rcgclm_long,
                                            missp=missp_out_rcgclm_long,
                                            pars_grid_list[[4]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'b_HESP <- (HE3~SP2)', 'k', reg_pars),
                model = 'rcgclm_long')


sim_res_rcgclm_short = summarise_simultation(true_out_rcgclm_short,
                                             missp_out_rcgclm_short,
                                             pars_grid_list[[5]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'd_HESP <- (HE2~e_SP1)', 'k', reg_pars),
                model = 'rcgclm_short')


sim_res_all = rbind.data.frame(sim_res_lm,
                               sim_res_growth,
                               sim_res_rcclpm,
                               sim_res_rcgclm_long,
                               sim_res_rcgclm_short
                               ) %>%
  filter(reg_pars == 'k')

#sim_res_all$rel_bias_log = ifelse(sim_res_all[,'Rel Bias'] < 0,
#                                   -log1p(-sim_res_all[,'Rel Bias']),
#                                   log1p(sim_res_all[,'Rel Bias']))

ggplot(sim_res_all %>% dplyr::filter(reg_pars == 'k'),
       aes(x = as.factor(round(L1, 2)), y = as.factor(round(icc, 2)), 
           fill = `Average Bias`)) + 
  geom_tile(color = "black", size = 0.3) +
  
  # Add geom_text to overlay the bias value on each tile
  geom_text(aes(label = sprintf("%.2f", `Average Bias`)), size = 4, color = "black") +
  
  scale_fill_gradient2(low = brewer.pal(11, "RdBu")[1], 
                       high = brewer.pal(11, "RdBu")[11], 
                       mid = "white", 
                       midpoint = 0,
                       name = "Average Bias"#, 
                       #breaks = c(-log(10), 0, log(10)), 
                       #labels = c("-10", "0", "10")
  ) +
  facet_wrap(~ model#, labeller = as_labeller(facet_titles)
             ) +
  labs(x = "Effect Size", y = "ICC") +
  theme_ipsum_tw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 24))#+
#labs(title = title)

ggplot(sim_res_all %>% dplyr::filter(reg_pars == 'k' & !L1 == 0),
       aes(x = as.factor(round(L1, 2)), y = as.factor(round(icc, 2)), 
           fill = `Rel Bias`)) + 
  geom_tile(color = "black", size = 0.3) +
  
  # Add geom_text to overlay the bias value on each tile
  geom_text(aes(label = sprintf("%.2f", `Rel Bias`)), size = 4, color = "black") +
  
  scale_fill_gradient2(low = brewer.pal(11, "RdBu")[1], 
                       high = brewer.pal(11, "RdBu")[11], 
                       mid = "white", 
                       midpoint = 0,
                       name = "Relative Bias"#, 
                       #breaks = c(-log(10), 0, log(10)), 
                       #labels = c("-10", "0", "10")
  ) +
  facet_wrap(~ model#, labeller = as_labeller(facet_titles)
  ) +
  labs(x = "Effect Size", y = "ICC") +
  theme_ipsum_tw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 24))#+
#labs(title = title)


ggplot(sim_res_all %>% dplyr::filter(reg_pars == 'k'),
       aes(x = as.factor(round(L1, 2)), y = as.factor(round(icc, 2)), 
           fill = Coverage)) + 
  geom_tile(color = "black", size = 0.3) +
  
  # Add geom_text to overlay the bias value on each tile
  geom_text(aes(label = sprintf("%.2f", `Coverage`)), size = 4, color = "black") +
  
  scale_fill_gradient2(low = brewer.pal(11, "RdBu")[1], 
                       high = brewer.pal(11, "RdBu")[11], 
                       mid = "white", 
                       midpoint = 0,
                       name = "Coverage"#, 
                       #breaks = c(-log(10), 0, log(10)), 
                       #labels = c("-10", "0", "10")
  ) +
  facet_wrap(~ model#, labeller = as_labeller(facet_titles)
  ) +
  labs(x = "Effect Size", y = "ICC") +
  theme_ipsum_tw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 24))#+
#labs(title = title)




