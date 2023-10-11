
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


library(furrr)
library(dplyr)
library(progressr)

#source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
#options(max.print=3900)
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

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
                     data = dat, REML = FALSE)
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

n_samp = 1000
eta_range = seq(0, 0.9, 0.1)
eta_list = list()
for (i in seq_along(eta_range)){
  eta_list[[i]] = rep(eta_range[i], 2*t)
}
no_cores = 10

simulate_multilevel_data <- function(n_comb, 
                                     n_samples, 
                                     t,
                                     n_groups,
                                     n_cases,
                                     eta_list,
                                     list_wb_comb,
                                     seed_vec,
                                     aggregate = FALSE,
                                     hlm = FALSE) {
  
  plan(multicore, workers = no_cores)
  
  # Using progress
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    data_list <- future_map(1:n_comb,
                            .progress = T,
                            .options = furrr_options(seed = TRUE),
                            function(i) {
                              
                              dat = vector(mode = 'list', length = length(eta_list))
                              
                              for (et in seq_along(eta_list)) {
                                dat[[et]] = lapply(1:n_samples, function(j) {
                                  
                                  set.seed(seed_vec[j])
                                  
                                  sample_data = as.data.frame(psych::sim.multilevel(
                                    nvar = 2*t,
                                    ncases = n_cases,
                                    ngroups = n_groups,
                                    rbg = list_wb_comb[[i]][[1]],
                                    rwg = list_wb_comb[[i]][[2]],
                                    eta = eta_list[[et]])$xy)
                                  colnames(sample_data) = c('Group', paste0('HE', 1:t), paste0('SP', 1:t))
                                  
                                  sample_data
                                })
                              }
                              
                              names(dat) = eta_range
                              p(sprintf("Combination %g", i)) 
                              
                              return(dat)
                            })
  }) 
  
  data_list
}
## APPLYING

data_sim = simulate_multilevel_data(n_comb = length(list_wb_comb), 
                                n_samples = n_samp,
                                seed_vec = 1:n_samp, 
                                t = 7,
                                n_groups = 50,
                                n_cases = 2500,
                                eta_list = eta_list,
                                list_wb_comb = list_wb_comb)


transform_multilevel_data <- function(list_data,
                                      aggregate = FALSE,
                                      hlm = FALSE) {
  
  plan(multicore, workers = no_cores)
  n_comb = length(list_data)
  
  # Using progress
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    data_list <- future_map(1:n_comb, .progress = T, .options = furrr_options(seed = TRUE), function(i) {
      
      dat = list_data[[i]]
      n_samples = length(dat[[1]])
      
      for (et in seq_along(dat)) {
        dat[[et]] = lapply(1:n_samples, function(j) {
          
          sample_data = dat[[et]][[j]]
          
          if (aggregate) {
            sample_data = sample_data %>% 
              group_by(Group) %>% 
              mutate(across(starts_with('S'), mean))
          }
          
          if (hlm) {
            sample_data = ToWide(sample_data)
          }
          
          sample_data
        })
      }
      
      names(dat) = eta_range
      p(sprintf("Combination %g", i)) 
      
      return(dat)
    })
  }) 
  
  data_list
}



true_data = transform_multilevel_data(list_data = data_sim,
                                      aggregate = F,
                                      hlm = T)

missp_data = transform_multilevel_data(list_data = data_sim,
                                       aggregate = T,
                                       hlm = T)

