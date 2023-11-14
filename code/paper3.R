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
library(foreach)
library(doSNOW)

library(gridExtra)
library(cowplot)
library(hrbrthemes)
library(RColorBrewer)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_lv.RDS')
df_lv %<>% dplyr::rename(!!!setNames(c('LAD21CD', paste0("as", 1:7), paste0("HE", 1:7)),
                              c('ID', paste0("X", 1:7), paste0("Y", 1:7)))) %>%
  dplyr::select('ID', starts_with('X'), starts_with('Y'))

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
  cols = grep("^X", names(dt1))
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
    
    true_params = summaryParam(true[[i]]) %>% 
      dplyr::select(power_true = 'Power (Not equal 0)')
    
    summary_main = summaryParam(missp[[i]],
                                detail = TRUE,
                                digits = 3) %>%
      rownames_to_column('reg_pars') %>%
      dplyr::mutate(SE_ratio = `Average SE`/`Estimate SD`)
    
      fit_ind_true = as.data.frame(t(colMeans(inspect(true[[i]], 'fit')[,c(
        "chisq.scaled", 'pvalue.scaled', 'baseline.pvalue.scaled',
                                                    "aic",
                                                    "bic",
                                                    "rmsea.scaled",
                                                    "cfi.scaled", 
                                                    "tli.scaled", 
                                                    "srmr",
                                                    'aic',
                                                    'bic')])))
      colnames(fit_ind_true) = paste0('true_', colnames(fit_ind_true))
      fit_ind_missp = as.data.frame(t(colMeans(inspect(missp[[i]], 'fit')[,c(
        "chisq.scaled", 'pvalue.scaled', 'baseline.pvalue.scaled',
        "aic",
        "bic",
        "rmsea.scaled",
        "cfi.scaled", 
        "tli.scaled", 
        "srmr",
        'aic',
        'bic')])))
      colnames(fit_ind_missp) = paste0('missp_', colnames(fit_ind_missp))
      fin_ind_all = cbind(fit_ind_true, fit_ind_missp)
      
      summary_out[[i]] = cbind(summary_main, 
                               true_params,
                               fin_ind_all %>%
                                 slice(rep(1:n(), nrow(summary_main))))

  }
  
  summary_out = cbind(do.call(rbind.data.frame, summary_out),
                      #pars_grid_current[which(converged == T),] %>%
                        pars_grid_current %>%
                        slice(rep(1:n(),
                                  each = ncol(true[[1]]@coef))))
  
  
  
  return(summary_out)
}


sim_res_lm = summarise_simultation(true_out_lm,
                                   missp_out_lm,
                                   pars_grid_list[[1]]) %>%
dplyr::mutate(reg_pars = if_else(reg_pars == 'b_YX', 'k', reg_pars),
                   model = 'lm')

#
#icc_range_ = c(0.05, 0.25, 0.45, 0.65, 0.85)
icc_range_ = c(0.1, 0.4, 0.7)

create_L1_L2 = function(par_tab,
                        m_est,
                        icc_range = icc_range_,
                        evarX,
                        var_iX,
                        var_sX,
                        varX,
                        evarY,
                        var_iY,
                        var_sY,
                        varY) {
  
  # Loop through parameter table and replace values in the model
  m_L1 = m_est
  for (i in 1:nrow(par_tab)) {
    m_L1 = gsub(par_tab[i,1], par_tab[i,2], m_L1) %>%
      glue_collapse("\n")
  }
  
  m_L1 = gsub('evarX', evarX, m_L1)
  m_L1 = gsub('var_iX', var_iX, m_L1)
  m_L1 = gsub('var_sX', var_sX, m_L1)
  
  m_L1 = gsub('evarY', evarY, m_L1)
  m_L1 = gsub('var_iY', var_iY, m_L1)
  m_L1 = gsub('var_sY', var_sY, m_L1)
  
  m_L1 = gsub('varX', varX, m_L1)
  m_L1 = gsub('varY', varY, m_L1)
  
  
  withinX = sum(evarX, var_iX, var_sX, varX, na.rm = T)
  withinY = sum(evarY, var_iY, var_sY, varY, na.rm = T)
  
  betweenX = c()
  for (i in seq_along(icc_range)){
    betweenX[i] = withinX*icc_range[i]/(1-icc_range[i])
  }
  betweenY = withinY*0.35/(1-0.35)
  
  # Loop through variable vector and create a list of models with replaced values
  
  
  m_L2 = lapply(betweenX, function(x) {
    m_L2 = m_est
    for (i in 1:nrow(par_tab)) {
      m_L2 = gsub(par_tab[i,1], par_tab[i,2], m_L2) %>%
        glue_collapse("\n")
    }  
      
    m_L2 = gsub('evarX', x*evarX/withinX, m_L2)
    m_L2 = gsub('var_iX', x*var_iX/withinX, m_L2)
    m_L2 = gsub('var_sX', x*var_sX/withinX, m_L2)
    m_L2 = gsub('varX', x*varX/withinX, m_L2)
    
    m_L2 = gsub('evarY', betweenY*evarY/withinY, m_L2)
    m_L2 = gsub('var_iY', betweenY*var_iY/withinY, m_L2)
    m_L2 = gsub('var_sY', betweenY*var_sY/withinY, m_L2)
    m_L2 = gsub('varY', betweenY*varY/withinY, m_L2)
    return(m_L2)
  })
  
  out = list(m_est = m_est,
             m_L1 = m_L1,
             m_L2 = m_L2)
  return(out)
}

find_uncoverged_params = function(out_list){
  converge_list = list()
  for (i in seq_along(out_list)){
    tryCatch({
      converge_list[[i]] = 100*(1-(summaryConverge(out_list[[i]])$Converged['num.nonconverged']/1000))
    }, error = function(e) {
      i = 'error'
    })
  }
  #uncoverged_which = which(unlist(lapply(converge_list, function(x) !is.null(x))))
  #unconverged_params = pars_grid_current[uncoverged_which, 1:2]
  uncoverged = unlist(converge_list)
  n_uncoverged = length(uncoverged)
  mean_uncoverged = mean(uncoverged)
  
  return(c(n_uncoverged, mean_uncoverged))
}

# Define the models

{
m_est_lm = '
Y ~ meanY*1 + b_YX*X
#X ~ meanX*1

Y ~~ varY*Y
X ~~ varX*X

'

m_est_growth = '
iY =~ 1*Y1 + 1*Y2 + 1*Y3 + 1*Y4 + 1*Y5 + 1*Y6+ 1*Y7
sY =~ 0*Y1 + 1*Y2 + 2*Y3 + 3*Y4 + 4*Y5 + 5*Y6+ 6*Y7

iY ~ b_YX*X
sY ~ b2_YX*X

iY ~~ cov_iY.sY*sY
iY ~~ var_iY*iY
sY ~~ var_sY*sY

iY ~ mean_i_Y*1
sY ~ mean_s_Y*1

Y1 ~~ varY*Y1
Y2 ~~ varY*Y2
Y3 ~~ varY*Y3
Y4 ~~ varY*Y4
Y5 ~~ varY*Y5
Y6 ~~ varY*Y6
Y7 ~~ varY*Y7

X ~~ varX*X

Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 ~ 0*1

'

}

m_est_rcclpm = RC_GCLM_syntax(endogeneous = c('Y', 'X'),
                              control = NULL,
                              model = 'reclpm',
                              max_time = 7)
m_est_rcgclm = RC_GCLM_syntax(endogeneous = c('Y', 'X'),
                              control = NULL,
                              model = 'recgclm',
                              max_time = 7)


par_tab_rcgclm_long = data.frame(
  label = c('b_YY', 'b_YX', 'b_XY', 'b_XX',
            'cov_iY.iX', 'cov_iY.sY', 'cov_iY.sX', 'cov_iX.sY', 'cov_iX.sX', 'cov_sY.sX',
            'd_YY', 'd_YX', 'd_XY', 'd_XX',
            'ecov_YX', 'evarY', 'evarX',
            'mean_i_Y', 'mean_i_X', 'mean_s_Y', 'mean_s_X',
            'var_iY', 'var_iX', 'var_sY', 'var_sX'),
  par = c(0.03, 'k', '(-0.06)', '(-0.2)', 
          '(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.25)', '(-0.004)', '(-0.06)', '(-0.2)',
          0.002, 'evarY', 'evarX',
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iY', 'var_iX', 'var_sY', 'var_sX')
)

par_tab_rcgclm_short = data.frame(
  label = c('b_YY', 'b_YX', 'b_XY', 'b_XX',
            'cov_iY.iX', 'cov_iY.sY', 'cov_iY.sX', 'cov_iX.sY', 'cov_iX.sX', 'cov_sY.sX',
            'd_YY', 'd_YX', 'd_XY', 'd_XX',
            'ecov_YX', 'evarY', 'evarX',
            'mean_i_Y', 'mean_i_X', 'mean_s_Y', 'mean_s_X',
            'var_iY', 'var_iX', 'var_sY', 'var_sX'),
  par = c(0.03, 0.006, '(-0.06)', '(-0.2)', 
          '(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.25)', 'k', '(-0.06)', '(-0.2)',
          0.002, 'evarY', 'evarX',
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iY', 'var_iX', 'var_sY', 'var_sX')
)

par_tab_rcclpm = data.frame(
  label = c('cov_iY.iX', 'cov_iY.sY', 'cov_iY.sX', 'cov_iX.sY', 'cov_iX.sX', 'cov_sY.sX', 
            'd_YY', 'd_YX', 'd_XY', 'd_XX',
            'ecov_YX', 'evarY', 'evarX',
            'mean_i_Y', 'mean_i_X', 'mean_s_Y', 'mean_s_X', 
            'var_iY', 'var_iX', 'var_sY', 'var_sX'),
  par = c('(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.5)', 'k', '(0.06)', '(0.2)',
          0.003, 'evarY', 'evarX', 
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iY', 'var_iX', 'var_sY', 'var_sX')
)

par_tab_lm = data.frame(
  label = c('b_YX', 'varY', 'meanY', 'meanX', 'varX'),
  par = c('k', 'varY', 0.4, '(-0.07)', 'varX')
)

par_tab_growth = data.frame(
  label = c('b_YX', 'b2_YX',
            'cov_iY.sY',
            'var_iY', 'var_sY', 'varY',
            'mean_i_Y', 'mean_s_Y',
            'varX'),
  par = c('k', 0.01, 
          0.03,
          'var_iY', 'var_sY', 'varY',
          0.4, -0.2,
          'varX')
)

lm_models = create_L1_L2(par_tab=par_tab_lm,
                         m_est=m_est_lm,
                         evarX = NA,
                         var_iX = NA,
                         var_sX = NA,
                         varX = 0.8,
                         evarY = NA,
                         var_iY = NA,
                         var_sY = NA,
                         varY = 0.6)
growth_models = create_L1_L2(par_tab=par_tab_growth,
                             m_est=m_est_growth,
                             evarX = NA,
                             var_iX = NA,
                             var_sX = NA,
                             varX = 0.8,
                             evarY = NA,
                             var_iY = 0.6,
                             var_sY = 0.07,
                             varY = 0.02)
rcclpm_models = create_L1_L2(par_tab=par_tab_rcclpm,
                             m_est=m_est_rcclpm,
                             evarX = 0.05,
                             var_iX = 0.4,
                             var_sX = 0.05,
                             varX = NA,
                             evarY = 0.05,
                             var_iY = 0.6,
                             var_sY = 0.05,
                             varY = NA)
rcgclm_models_long = create_L1_L2(par_tab=par_tab_rcgclm_long,
                                  m_est=m_est_rcgclm,
                                  evarX = 0.05,
                                  var_iX = 0.4,
                                  var_sX = 0.05,
                                  varX = NA,
                                  evarY = 0.05,
                                  var_iY = 0.6,
                                  var_sY = 0.05,
                                  varY = NA)
rcgclm_models_short = create_L1_L2(par_tab_rcgclm_short,
                                   m_est_rcgclm,
                                   evarX = 0.05,
                                   var_iX = 0.4,
                                   var_sX = 0.05,
                                   varX = NA,
                                   evarY = 0.05,
                                   var_iY = 0.6,
                                   var_sY = 0.05,
                                   varY = NA)
all_m = list(lm_models, growth_models, rcclpm_models, rcgclm_models_long, rcgclm_models_short)
names(all_m) = c('lm', 'growth', 'rcclpm', 'rcgclm_long', 'rcgclm_short')

# params for upper and lower levels
pars_grid_list = list()
pars = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
pars_grid = expand.grid(L1 = pars, L2 = pars, icc = icc_range_)

for (i in seq_along(all_m)){
  mod = names(all_m)[i]
  
  pars_grid_list[[i]] = pars_grid
  pars_grid_list[[i]]$model_est = all_m[[i]][['m_est']]
  pars_grid_list[[i]]$model_L1 = all_m[[i]][['m_L1']]
  pars_grid_list[[i]]$model_L2 = rep(all_m[[i]][['m_L2']], each = length(pars)^2)
  
  if(mod %in% c('growth', 'lm')){
    pars_grid_list[[i]]$orthogonal = F
  } else {
    pars_grid_list[[i]]$orthogonal = T
  }
  
  if(mod %in% 'growth'){
    pars_grid_list[[i]]$lavaanfun = 'growth'
  } else {
    pars_grid_list[[i]]$lavaanfun = 'sem'
  }
}

#-------------------------------------------------------------------------------
#-----------------------------------Simulation----------------------------------
#-------------------------------------------------------------------------------


# Progress bar setup
pb = txtProgressBar(max = nrow(pars_grid_list[[1]]), style = 3)
opts = list(progress = function(n) setTxtProgressBar(pb, n))
wg_size = 50
nRep = 1000
n = 50

gen_data = function(N2) {
  
  NperC = rep(wg_size, N2)
  model_L1 = pars_grid_current[i, 'model_L1']
  model_L2 = pars_grid_current[i, 'model_L2']
  par_value_L1 = pars_grid_current[i, 'L1']
  par_value_L2 = pars_grid_current[i, 'L2']
  
  k1 = paste0('(', par_value_L1, ')')
  k2 = paste0('(', par_value_L2, ')')
  
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

# lm
model_number = 1
pars_grid_current = pars_grid_list[[model_number]]
nvar = 2
model_type = 'lm'

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
nvar = 8
model_type = 'growth'

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
nvar = 14
model_type = 'cross-lag'

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
nvar = 14
model_type = 'cross-lag'

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
nvar = 14
model_type = 'cross-lag'

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
                                .packages = c("simsem","lavaan","dplyr","glue",
                                              "broom","data.table"), 
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

# unconverged - only rcgclm
find_uncoverged_params(missp_out_rcgclm_long)
find_uncoverged_params(missp_out_rcgclm_short)
find_uncoverged_params(true_out_rcgclm_long)
find_uncoverged_params(true_out_rcgclm_short)

# saving result lists

getwd()
wd = 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper3'
setwd(wd)
saveRDS(true_out_lm, 'true_lm_7.rds')
saveRDS(missp_out_lm, 'missp_lm_7.rds')
saveRDS(true_out_growth, 'true_growth_7.rds')
saveRDS(missp_out_growth, 'missp_growth_7.rds')

saveRDS(true_out_rcclpm, 'true_rcclpm_7.rds')
saveRDS(missp_out_rcclpm, 'missp_rcclpm_7.rds')

saveRDS(true_out_rcgclm_long, 'true_rcgclm_long_7.rds')
saveRDS(missp_out_rcgclm_long, 'missp_rcgclm_long_7.rds')
saveRDS(true_out_rcgclm_short, 'true_rcgclm_short_7.rds')
saveRDS(missp_out_rcgclm_short, 'missp_rcgclm_short_7.rds')

true_out_lm = readRDS('true_lm_7.rds')
missp_out_lm = readRDS('missp_lm_7.rds')
true_out_growth = readRDS('true_growth_7.rds')
missp_out_growth = readRDS('missp_growth_7.rds')
true_out_rcclpm = readRDS('true_rcclpm_7.rds')
missp_out_rcclpm = readRDS('missp_rcclpm_7.rds')
true_out_rcgclm_long = readRDS('true_rcgclm_long_7.rds')
missp_out_rcgclm_long = readRDS('missp_rcgclm_long_7.rds')
true_out_rcgclm_short = readRDS('true_rcgclm_short_7.rds')
missp_out_rcgclm_short = readRDS('missp_rcgclm_short_7.rds')

# Summarizing results

sim_res_lm = summarise_simultation(true_out_lm,
                                   missp_out_lm,
                                   pars_grid_list[[1]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'b_YX', 'k', reg_pars),
                model = 'lm')

sim_res_growth = summarise_simultation(true_out_growth,
                                       missp_out_growth,
                                       pars_grid_list[[2]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'b_YX', 'k', reg_pars),
                model = 'growth')

sim_res_rcclpm = summarise_simultation(true_out_rcclpm,
                                       missp_out_rcclpm,
                                       pars_grid_current = pars_grid_list[[3]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'd_YX <- (e_Y2~e_X1)', 'k', reg_pars),
                model = 'rcclpm')

sim_res_rcgclm_long = summarise_simultation(true=true_out_rcgclm_long,
                                            missp=missp_out_rcgclm_long,
                                            pars_grid_list[[4]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'b_HESP <- (HE3~SP2)', 'k', reg_pars),
                model = 'rcgclm_long')


sim_res_rcgclm_short = summarise_simultation(true_out_rcgclm_short,
                                             missp_out_rcgclm_short,
                                             pars_grid_list[[5]]) %>%
  dplyr::mutate(reg_pars = if_else(reg_pars == 'd_YX <- (Y2~e_X1)', 'k', reg_pars),
                model = 'rcgclm_short')


sim_res_all = rbind.data.frame(sim_res_lm,
                               sim_res_growth,
                               sim_res_rcclpm,
                               sim_res_rcgclm_long,
                               sim_res_rcgclm_short
                               ) %>%
  filter(reg_pars == 'k') %>%
  mutate(bw_diff = L1 - L2,
         rmse = sqrt((`Average Bias`)^2))

# Define a function to perform the repeated analysis and eta-squared calculation
perform_anova = function(response, data = sim_res_all, fit = FALSE) {
  
  response_quoted = if (grepl(" ", response)) paste("`", response, "`", sep = "") else response
  
  terms = c('model', 'bw_diff', 'icc')
  interaction_terms = paste(terms, collapse = "*")
  
  # Create the model formula with all interactions
  #formula = as.formula(paste(response_quoted, "~",
  #                           paste0(interaction_terms, '+', 'abs(L1)', '+', 'abs(L2)')))
  data %<>% mutate(L1 = abs(L1),
                   L2 = abs(L2),
                   bw_diff = abs(bw_diff),
                  `Average Bias`= abs(`Average Bias`)) %>%
    dplyr::rename(ICC = icc, 
                  AbsDiff = bw_diff,
                  AbsL1 = L1,
                  AbsL2 = L2,
                  Method = model)
  data$ICC = as.factor(data$ICC)
  formula = paste0(response_quoted, '~ Method*AbsDiff*ICC*AbsL1*AbsL2')
  
  if (fit){
    lm_fit = lm(formula, data = data %<>% filter(!Method == 'lm'))
  } else{
    lm_fit = lm(formula, data = data)
  }
  
  aov_fit = anova(lm_fit)
  
  # Calculate the total sum of squares once
  total_sum_sq = sum(aov_fit$"Sum Sq")
  
  # Add the percentage of explained variance
  #eta = cbind(aov_fit, PctExp = round(aov_fit$"Sum Sq" / total_sum_sq * 100, 3))
  eta = cbind.data.frame(rownames(aov_fit),  round(aov_fit$"Sum Sq" / total_sum_sq * 100, 2))[-length(aov_fit$"Sum Sq"),]
  colnames(eta) = c('Term', response)
  
  return(eta)
}

criteria_fitind = c('true_rmsea.scaled', 'missp_rmsea.scaled', 
                    'true_srmr', 'missp_srmr', 
                    'true_chisq.scaled', 'missp_chisq.scaled', 
                    'true_aic', 'missp_aic', 
                    'true_bic', 'missp_bic',
                    'true_cfi.scaled',  'missp_cfi.scaled', 
                    'true_tli.scaled',  'missp_tli.scaled')
eta_fitind = purrr::reduce(lapply(criteria_fitind, 
                                  function(x) perform_anova(x, fit = T)), 
                           dplyr::left_join, by = 'Term')
colnames(eta_fitind) = c('Term', 
                         'True RMSEA', 'Missp RMSEA', 
                         'True SRMR', 'Missp SRMR',
                         'True Chisq', 'Missp Chisq',
                         'True AIC', 'Missp AIC',
                         'True BIC', 'Missp BIC',
                         'True CFI', 'Missp CFI',
                         'True TLI', 'Missp TLI')
eta_fitind$Term = gsub(':', ' : ', eta_fitind$Term)
criteria_main = c('Average Bias',
                    'Rel SE Bias',
                    'Coverage', 
                    'Power (Not equal 0)')
eta_main = purrr::reduce(lapply(criteria_main, perform_anova), 
                           dplyr::left_join, by = 'Term')
colnames(eta_main) = c('Term', 
                       'Average Bias',
                       'Relative SE Bias',
                       'Coverage', 
                       'Power')
eta_main$Term = gsub(':', ' : ', eta_main$Term)

summary(sim_res_all$`Rel SE Bias`)
summary(sim_res_all$Coverage)
summary(abs(sim_res_all$`Average Bias`))

# writing to word
library(officer)

# Create a list of data frames
df_list = list(eta_main,eta_fitind)

# Create a Word document
doc = read_docx()

# Loop over the list of data frames and add them to the document
for (i in seq_along(df_list)) {
  doc = doc %>% 
    body_add_table(df_list[[i]])
  
  # Add a page break after each table
  if (i < length(df_list)) {
    doc = doc %>% 
      body_add_break()
  }
}

# Save the document

print(doc, target = "paper3_tabs.docx")

# ---------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------

sim_res_all$model = factor(sim_res_all$model,
                           levels = c('lm', 'growth', 'rcclpm', 'rcgclm_long', 'rcgclm_short'),
                           labels = c('Linear Regression',
                                      'Growth Curve',
                                      'RC-CLPM',
                                      'RC-GCLM Long-Run',
                                      'RC-GCLM Short-Run'))
sim_res_all$icc = factor(sim_res_all$icc,
                         levels = c(0.1, 0.4, 0.7),
                         labels = c('ICC = 0.1',
                                    'ICC = 0.4',
                                    'ICC = 0.7'))


library(ggpubr)
wd = 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper3/'
library(RColorBrewer)

# Average Bias
ggplot(sim_res_all, aes(x = abs(bw_diff), y = abs(`Average Bias`),
                        color = icc)) + 
  scale_linewidth_manual(values = c(0.3, 0.8, 1.4)) +
  scale_color_manual(values = brewer.pal(3, "Set1"), 
                     labels = c(bquote(ICC[x] ~ '= 0.1'),
                                bquote(ICC[x] ~ '= 0.4'),
                                bquote(ICC[x] ~ '= 0.7'))) +
  geom_point(size = 1) + 
  scale_y_continuous(name = 'Absolute Bias') + 
  scale_x_continuous(name = 'Absolute Difference between L1 and L2 Effects') + 
  geom_smooth(method = "loess",  se = F, linewidth = 0.7) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal()+ 
  theme(axis.title = element_text(size = 18, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'bottom',
        panel.spacing = unit(2, "lines"))
ggsave(paste0(wd, "average_bias.svg"), width = 40, height = 10, units = 'cm')

# Rel SE Bias
re_se_df = sim_res_all %>% 
  dplyr::select(L1, L2, `Rel SE Bias`,`SE_ratio`, icc, model) %>%
  pivot_longer(cols = c(L1, L2), names_to = 'L',
               values_to = 'L_value') 
ggplot(re_se_df %>% filter(L == 'L1'), 
       aes(x = abs(L_value), y = `Rel SE Bias`, color = icc))  +
  scale_color_manual(values = brewer.pal(3, "Set1"), 
                     labels = c(bquote(ICC[x] ~ '= 0.1'),
                                bquote(ICC[x] ~ '= 0.4'),
                                bquote(ICC[x] ~ '= 0.7')))+ 
  #scale_linewidth_manual(values = c(0.3, 0.8, 1.4)) + 
  geom_point(size = 1) + 
  scale_y_continuous(name = 'Relative SE Bias'#, limits  = c(0,0.5)
  ) + 
  scale_x_continuous(name = 'Absolute L1 Effect') + 
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 18, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'bottom',
        panel.spacing = unit(2, "lines"))
ggsave(paste0(wd, "rel_se_bias_L1.svg"), width = 40, height = 10, units = 'cm')

ggplot(re_se_df %>% filter(L == 'L2'), 
       aes(x = abs(L_value), y = `Rel SE Bias`, color = icc)) +
  scale_color_manual(values = brewer.pal(3, "Set1"), 
                     labels = c(bquote(ICC[x] ~ '= 0.1'),
                                bquote(ICC[x] ~ '= 0.4'),
                                bquote(ICC[x] ~ '= 0.7')))+ 
  #scale_linewidth_manual(values = c(0.3, 0.8, 1.4)) + 
  geom_point(size = 1) + 
  scale_y_continuous(name = 'Relative SE Bias') + 
  scale_x_continuous(name = 'Absolute L2 Effect') + 
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 18, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'bottom',
        panel.spacing = unit(2, "lines"))
ggsave(paste0(wd, "rel_se_bias_L2.svg"), width = 40, height = 10, units = 'cm')

# Coverage
ggplot(sim_res_all, aes(x = abs(bw_diff), y = Coverage, color = icc))+
  scale_color_manual(values = brewer.pal(3, "Set1"), 
                     labels = c(bquote(ICC[x] ~ '= 0.1'),
                                bquote(ICC[x] ~ '= 0.4'),
                                bquote(ICC[x] ~ '= 0.7'))) + 
  #scale_linewidth_manual(values = c(0.3, 0.8, 1.4)) + 
  geom_point(size = 1) + 
  scale_y_continuous(name = 'Coverage') + 
  scale_x_continuous(name = 'Absolute Difference between L1 and L2 Effects') + 
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 18, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'bottom',
        panel.spacing = unit(2, "lines"))
ggsave(paste0(wd, "coverage.svg"), width = 40, height = 10, units = 'cm')

# fit ind
ind_df = sim_res_all %>%
  filter(model != 'Linear Regression') %>%
  group_by(model, icc) %>%
  summarise(across(all_of(criteria_fitind), mean)) %>% 
  group_by(model,icc) %>%
  slice(1)  %>%
  pivot_longer(cols = all_of(criteria_fitind), 
               names_to = 'index',
               values_to = 'value')

ind_plot_names = c('True Model RMSEA', 'Misspecified Model RMSEA', 
                   'True Model SRMR', 'Misspecified Model SRMR',
                   'True Model Chisq', 'Misspecified Model Chisq',
                   'True Model AIC', 'Misspecified Model AIC',
                   'True Model BIC', 'Misspecified Model BIC',
                   'True Model CFI', 'Misspecified Model CFI',
                   'True Model TLI', 'Misspecified Model TLI')
ind_df$index = factor(ind_df$index,
                      levels = criteria_fitind,
                      labels = ind_plot_names)

ind_vec = c('RMSEA', 'SRMR', 'Chisq', 'AIC', 'BIC', 'CFI', 'TLI')
ind_plot_list = list()
for (i in seq_along(ind_vec)){
  ind_df_current = ind_df %>% filter(grepl(ind_vec[i], index))
  ind_plot_list[[i]] = ggplot(ind_df_current,
                         aes(y = value, x = model, fill = icc))  +
    scale_fill_brewer(palette  = 6, 
                      labels = c(bquote(ICC[x] ~ '= 0.1'),
                                 bquote(ICC[x] ~ '= 0.4'),
                                 bquote(ICC[x] ~ '= 0.7'))) + 
    geom_bar(aes(y = value, x = model),
             stat = "identity",
             position = "dodge", width = 0.6,
             show.legend = T) +
    scale_y_continuous(name = NULL) + 
    scale_x_discrete(name = NULL) + 
    theme_minimal() + 
    theme(axis.title = element_text(size = 18, face = 'bold'),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18, face = 'bold'),
          legend.title = element_blank(),
          strip.text = element_text(size = 22, face = 'bold'),
          legend.position = 'bottom',
          legend.text = element_text(size = 28),
          panel.spacing = unit(2, "lines"),
          legend.key.size = unit(2, "lines")) +
    facet_wrap(~ index, scales = 'free', nrow = 1) + 
    coord_cartesian(ylim = c(0, max(ind_df_current$value))) +
    theme(strip.background = element_blank())
}

ggarrange(ind_plot_list[[1]],
          ind_plot_list[[2]],
          ind_plot_list[[3]],
          ind_plot_list[[4]],
          ind_plot_list[[5]],
          ind_plot_list[[6]],
          ind_plot_list[[7]],
          ncol = 1, nrow = 7, 
          common.legend = T,
          legend = 'bottom')

ggsave(paste0(wd, "fit_ind.svg"), width = 60, height = 40, units = 'cm')


##### power

power_df = sim_res_all %>% dplyr::select(L1, L2, 
                                         `Estimate Average`,
                                         `Average Param`, Coverage,
                                         bw_diff, icc, model,
                                         `Average CI Width`,
                                         `Not Cover Below`,
                                         `Not Cover Above`,
                                         power_missp = `Power (Not equal 0)`,
                                      power_true) %>%
  #mutate(power_diff = power_missp - power_true)%>%
  filter(L1==L2) %>%
  group_by(model, icc) %>%
  summarise(across(c(power_missp, power_true, power_diff), mean)) %>% 
  group_by(model,icc) %>%
  slice(1)  %>%
  pivot_longer(cols = c(power_missp, power_true, power_diff), 
               names_to = 'index',
               values_to = 'value')

ggplot(power_df,
       aes(y = value, x = model, fill = icc))  +
  scale_fill_brewer(palette  = 6) + 
  geom_bar(aes(y = value, x = model),
           stat = "identity", position = "dodge") +
  scale_y_continuous(name = NULL) + 
  scale_x_discrete(name = NULL)+
  facet_wrap(~ index, scales = 'free', nrow = 7) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 18, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'bottom') 

