
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

library(parallel)
library(doParallel)
library(tictoc)
library(simsem)

library(gridExtra)
library(cowplot)
library(hrbrthemes)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
options(max.print=3900)
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

## extracting parameters 
flattenList <- function(x, depth = 0, name = NULL, lab = F) {
  res <- list()
  for (i in seq_along(x)) {
    el <- x[[i]]
    if (lab == T){
      el$label = rownames(el)
      rownames(el) = NULL
    }
    newName <- if (is.null(name)) names(x)[i] else paste(name, names(x)[i], sep = "_")
    if (is.list(el) && depth < 1) {
      res <- c(res, flattenList(el, depth + 1, newName, lab = F))
    } else {
      res[[newName]] <- el
    }
  }
  return(res)
}

t = 7

generate_matrix <- function(diag_val, dim = t) {
  # Initialize an empty matrix
  mat <- matrix(0, nrow=dim, ncol=dim)
  
  # Fill in the matrix
  for(i in 1:dim) {
    for(j in 1:dim) {
      mat[i, j] <- diag_val + (i - j) * rnorm(100, mean = 0.01, sd = 0.01)[i]
    }
  }
  
  # Return the matrix
  return(mat)
}

generate_autocorr_mat <- function(n = t, k) {
  # Initialize an n x n matrix filled with zeros
  mat <- matrix(0, ncol=n, nrow=n)
  
  # Loop through each row and column
  for(i in 1:n) {
    for(j in 1:n) {
      if(i == j) {
        mat[i, j] <- 1
      } else {
        # Calculate the autocorrelation based on distance
        # You might adjust this formula to get the exact diminishing pattern you want
        mat[i, j] <- 1 - k* abs(i - j)
      }
    }
  }
  
  return(mat)
}

fit_sim_hlm = function(dat){
  
  model = lme4::lmer(HE ~ SP + time + (1|Group) + (1|id),
                     data = dat, REML = FALSE#,
                     #control = lmerControl(calc.derivs = FALSE)
                     )
  out_list = list(coef = summary(model)[["coefficients"]][,'Estimate'],
                  se = summary(model)[["coefficients"]][,'Std. Error'],
                  converged = TRUE)
  
  return(out_list)
}

ToWide = function(dat){
  
  dat =  dat %>%
    ungroup() %>% 
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -c(Group, id),
                 names_to = c(".value", "time"),
                 names_pattern = "(\\w+)(\\d+)")
  dat$time = as.numeric(dat$time)
  
  return(dat)
  
}


# define correlation matrices

bg_value = seq(-0.8, 0.8, 0.2)
wg_value = seq(-0.8, 0.8, 0.2)

bw_grid = expand.grid(wg_value, bg_value)
colnames(bw_grid) = c('Within', 'Between')
bw_grid %<>% mutate(bwratio = as.character(Within/Between)) %>%
  group_by(bwratio) %>% #slice(1) %>%
  mutate(bwratio = round(as.numeric(bwratio),1)) %>%
  as.data.frame %>% group_by(bwratio) %>%
  mutate(id_ratio = 1:n()) %>% ungroup() %>%
  as.data.frame

eta_range = seq(0, 0.9, 0.1)
eta_list = list()
for (i in seq_along(eta_range)){
  eta_list[[i]] = rep(eta_range[i], 2*t)
}


list_wb_comb = list()
for(i in 1:nrow(bw_grid)){
  
  # between group
  xx_bg = generate_autocorr_mat(k = 0.05)
  
  yy_bg = generate_autocorr_mat(k = 0.05)
  
  xy_bg = matrix(data = bw_grid[i,2], nrow = t, ncol = t)
  #xy_bg = generate_matrix(bw_grid[i,2])
  yx_bg = t(xy_bg)
  
  rbg = rbind(cbind(xx_bg, xy_bg), cbind(yx_bg, yy_bg))
  
  # within group
  xx_wg = generate_autocorr_mat(k = 0.03)
  
  yy_wg = generate_autocorr_mat(k = 0.03)
  
  xy_wg = matrix(data = bw_grid[i,1], nrow = t, ncol = t)
  #xy_wg = generate_matrix(bw_grid[i,1])
  yx_wg = t(xy_wg)
  
  rwg = rbind(cbind(xx_wg, xy_wg), cbind(yx_wg, yy_wg))
  
  list_wb_comb[[i]] = list(rbg, rwg)
}

# generating data

no_cores = detectCores() - 4
cl = makeCluster(no_cores)
registerDoParallel(cl)

n_samples = 100
n_comb = length(list_wb_comb)
seed_vec = 1:n_samples

tic()
data_sim = foreach(i = 1:n_comb,
                   .combine = 'c',
                   .packages = c('psych',
                                 'dplyr')) %dopar% {
  
  data_list = vector(mode = 'list', length = length(eta_range))
  for (lst in seq_along(data_list)){
    data_list[[lst]] = vector(mode = 'list', length = n_samples)
  }
  
  for (et in seq_along(eta_list)) {
    for (j in 1:n_samples){
      
      set.seed(seed_vec[j])
      
      data_list[[et]][[j]] = as.data.frame(
        psych::sim.multilevel(
          nvar = 2*t,
          ncases = 1000,
          ngroups = 50,
          rbg = list_wb_comb[[i]][[1]],
          rwg = list_wb_comb[[i]][[2]],
          eta = eta_list[[et]])$xy)
      colnames(data_list[[et]][[j]]) = c('Group', paste0('HE', 1:t),
                                       paste0('SP', 1:t))
        
      }
    names(data_list) = eta_range
    
  }
  list(data_list)
}
toc()
stopCluster(cl)

# data with aggregated S variables
tic()
no_cores = detectCores() - 4
cl = makeCluster(no_cores)
registerDoParallel(cl)

data_sim_X_aggreg = foreach(i = seq_along(data_sim),
                            .combine = 'c',
                            .packages = 'dplyr') %dopar% {
  
  # loop through each element of the nested list and apply the following function
  out = lapply(data_sim[[i]], function(lst1){
    lapply(lst1, function(lst2){
      
      # group by 'Group' and calculate the mean of all columns starting with 'S'
      lst2 %>% 
        group_by(Group) %>% 
        mutate(across(starts_with('S'), mean))
    })
  })
  list(out)
}
stopCluster(cl)
toc()

#View(data_sim[[1]][[2]][[1]])
#View(data_sim_X_aggreg[[5]][[3]][[1]])

# long format
tic()
data_sim_longf <- map(data_sim, ~ map(.x, ~ map(.x, ToWide)))
toc()
#View(data_sim_longf[[5]][[3]][[1]])

tic()
data_sim_X_aggreg_longf <- map(data_sim_X_aggreg, ~ map(.x, ~ map(.x, ToWide)))
toc()
#View(data_sim_X_aggreg_longf[[5]][[3]][[1]])

gc()

## running simulations

syntax_sim = RC_GCLM_syntax(endogeneous = c('HE', 'SP'),
                            control = NULL)
#fit_sim = sem(syntax_sim,
#              data = data_sim[[1]][["-0.7"]][[1]],
#              estimator = "mlr",
#              cluster = 'Group',
#              orthogonal = T)
#summary(fit_sim)


# true model
result_true = data_sim_longf

tic()
for (cm in seq_along(data_sim_longf)){
  
  n_sim = 0
  
  for (sym_data in data_sim_longf[[cm]]){
    n_sim = n_sim + 1
    result_true[[cm]][[n_sim]] = simsem::sim(seed = 12345, 
                                             #model = fit_sim, 
                                             model = fit_sim_hlm,
                                             rawData = sym_data,
                                             multicore = T,
                                             numProc = 14)
    gc()
    cat("n_sim:", n_sim, "\n")
  }
  cat("cm:", cm, "\n")
}

toc()
beepr::beep()
#View(result_true[[1]])
#summaryParam(result_true[[3]][[1]])
# misspecified model

result_missp = data_sim_X_aggreg_longf

tic()
for (cm in seq_along(data_sim_X_aggreg_longf)){
  
  n_sim = 0
  
  for (sym_data in data_sim_X_aggreg_longf[[cm]]){
    n_sim = n_sim + 1
    result_missp[[cm]][[n_sim]] = simsem::sim(seed = 12345, 
                                              #model = fit_sim, 
                                              model = fit_sim_hlm,
                                              rawData = sym_data,
                                              multicore = T,
                                              numProc = 14)
    gc()
    cat("n_sim:", n_sim, "\n")
  }
  cat("cm:", cm, "\n")
}

toc()
beepr::beep()
#View(summaryParam(result_missp[[2]][[1]]))

## extracting params
reg_pars = c('b_HESP <- (HE2~SP1)',
             'b_SPHE <- (SP2~HE1)',
             'b_HEHE <- (HE2~HE1)',
             'b_SPSP <- (SP2~SP1)',
             
             'd_HESP <- (HE2~e_SP1)',
             'd_SPHE <- (SP2~e_HE1)',
             'd_HEHE <- (HE2~e_HE1)',
             'd_SPSP <- (SP2~e_SP1)')
effect_names = c('long_cross_SP',
                 'long_cross_HE',
                 'long_auto_HE',
                 'long_auto_SP',
                 
                 'short_cross_SP',
                 'short_cross_HE',
                 'short_auto_HE',
                 'short_auto_SP')
effect_names_df = cbind.data.frame(reg_pars, effect_names)

params_true_ = lapply(result_true, function(lst) lapply(lst, summaryParam))
params_true = flattenList(params_true_)
params_true = lapply(params_true, function(lst){
  lst = lst[reg_pars, ] %>%
    rownames_to_column('reg_pars') %>%
    left_join(effect_names_df, by = 'reg_pars')
    
})
params_true = do.call(rbind.data.frame, params_true)
colnames(params_true)[-1] = paste0(colnames(params_true)[-1], '.true')

params_missp_ = lapply(result_missp, function(lst) lapply(lst, summaryParam))
params_missp = flattenList(params_missp_)
params_missp = lapply(params_missp, function(lst){
  lst = lst[reg_pars, ] %>%
    rownames_to_column('reg_pars') %>%
    left_join(effect_names_df, by = 'reg_pars')
  
})
params_missp = do.call(rbind.data.frame, params_missp)
colnames(params_missp)[-1] = paste0(colnames(params_missp)[-1], '.missp')
  
params_all = cbind.data.frame(params_true, params_missp[,-1],
                              bw_grid %>%
                                slice(rep(1:n(),
                                          each=length(reg_pars)*length(eta_range)))) %>%
  rownames_to_column('eta_npar') %>%
  separate(eta_npar, into = c("eta", "npar"), sep = "\\.(?=[^.]+$)")
params_all$eta = as.numeric(params_all$eta)

params_all %<>% 
  mutate(bias = (`Estimate Average.missp` - `Estimate Average.true`)/`Estimate Average.true`)

## plotting

ggplot(params_all %>%
         filter(effect_names.true == 'long_cross_SP' ),
       aes(as.factor(Between), as.factor(Within), fill = bias)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-60,60)
  )  +
  facet_wrap(~ eta)+
  theme_ipsum() 
