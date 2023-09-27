
fit_sim = sem(syntax_sim,
              data = df_lv,
              estimator = "mlr",
              cluster = 'LAD21CD',
              orthogonal = T)


cov_vec_ = cov_vec
par_tab_ = par_tab
syntax_sim_ = syntax_sim

simWB = function(cov_vec = cov_vec_[1:2],
                 par_tab = par_tab_,
                 syntax_sim = syntax_sim_){
  
  fin = list()
  fin = vector(mode = 'list', length = length(cov_vec))
  for (i in seq_along(fin)){
    fin[[i]] = vector(mode = 'list', length = 3)
  }

  for (K in seq_along(cov_vec)){
    par_tab_list = list()
    
    means = as.data.frame(mvrnorm(n = 100,
                                  mu = c(0, 0),
                                  Sigma = matrix(c(1, cov_vec[K],
                                                   cov_vec[K], 1),
                                                 nrow = 2)),
                          empirical = T)
    par_tab$par[par_tab$label == 'var_ias'] = cov(means)[1,1]
    par_tab$par[par_tab$label == 'var_iHE'] = cov(means)[2,2]
    
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
      
    }
    
    synt_list_fin = list()
    for (N_COR in seq_along(cov_vec)){
      synt_list_fin[[N_COR]] = lapply(synt_list,
                                  function(x) gsub('NA', cov_vec[N_COR], x))
    }
    
    
    Output = vector(mode = 'list', length = length(cov_vec))
    for (i in seq_along(Output)){
      Output[[i]] = vector(mode = 'list', length = 100)
    }
    n_samples = length(synt_list)
    N = 100
    seed_vec = seq(1, N, 1)
    for (N_COR in seq_along(cov_vec)){
      for (n_samples in 1:n_samples){
        Output[[N_COR]][[n_samples]] = sim(2, 
                                           model = synt_list_fin[[N_COR]][[n_samples]],
                                           dataOnly = T,
                                           seed = seed_vec[n_samples],
                                           n = N,
                                           generate = synt_list_fin[[N_COR]][[n_samples]],
                                           estimator = "mlr",
                                           orthogonal = T)
      }
    }

    lst_X_aggregated = vector(mode = 'list', length = length(Output))
    for (i in seq_along(lst_X_aggregated)){
      lst_X_aggregated[[i]] = vector(mode = 'list', length = 2)
    }
    lst_X_original = lst_X_aggregated
    for (j in 1:length(Output)){
          for (i in 1:length(Output[[1]][[1]])){
            lst_X_original[[j]][[i]] = do.call(rbind.data.frame,
                                               lapply(Output[[j]], function(lst) lst[[i]]))
            lst_X_aggregated[[j]][[i]] = cbind.data.frame(
              lst_X_original[[j]][[i]],
              LAD21CD = rep(1:N, each = n_samples)) %>%
        group_by(LAD21CD) %>%
        mutate(across(starts_with('as'), 
                      function(x) mean(x)))
      }
    }
    
    result = list()
    
    n_sim = 1
    for (sym_data in lst_X_aggregated){
          result[[n_sim]] = sim(seed = 12345, 
                                model = fit_sim,
                                rawData = sym_data,
                                multicore = T,
                                numProc = 8)
          n_sim = n_sim + 1
    }

    fin[[K]][[1]] = result
    fin[[K]][[2]] = lst_X_original
    fin[[K]][[3]] = lst_X_aggregated

    
  }
  
  return(fin)
  
}

tic()
sim_result = simWB(cov_vec = cov_vec_)
toc()
gc()

sumtab = summaryParam(sim_result[[1]][[1]][[1]])
sumtab2 = summaryParam(sim_result[[1]][[1]][[4]])

true_data = list() 

for (i in 1:length(sim_result)) {
  for (j in 1:length(sim_result[[1]][[2]])) {
    true_data[[length(true_data) + 1]] <- test[[i]][[2]][[j]][[1]]
  }
}
