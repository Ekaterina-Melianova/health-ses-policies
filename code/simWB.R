
fit_sim = sem(syntax_sim,
              data = df_lv,
              estimator = "mlr",
              cluster = 'LAD21CD',
              orthogonal = T)

cov_vec = seq(-0.5, 0.5, 0.1)

cov_vec_ = cov_vec
par_tab_ = par_tab
syntax_sim_ = syntax_sim

simWB = function(cov_vec = cov_vec_,
                 par_tab = par_tab_,
                 syntax_sim = syntax_sim_){
  
  fin = list()
  fin = vector(mode = 'list', length = length(cov_vec))
  for (i in seq_along(fin)){
    fin[[i]] = vector(mode = 'list', length = 4)
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
    
    n_samples = length(synt_list)
    N = 50
    seed_vec = seq(1, N, 1)
    draws = 100
    
    Output = vector(mode = 'list', length = length(cov_vec))
    for (i in seq_along(Output)){
      Output[[i]] = vector(mode = 'list', length = n_samples)
    }
    
    for (N_COR in seq_along(cov_vec)){
      for (n_samples in 1:n_samples){
        Output[[N_COR]][[n_samples]] = sim(draws, 
                                           model = synt_list_fin[[N_COR]][[n_samples]],
                                           dataOnly = T,
                                           seed = seed_vec[n_samples],
                                           n = N,
                                           generate = synt_list_fin[[N_COR]][[n_samples]],
                                           estimator = "mlr",
                                           orthogonal = T,
                                           multicore = T,
                                           numProc = 8)
      }
    }

    lst_X_aggregated = vector(mode = 'list', length = length(Output))
    for (i in seq_along(lst_X_aggregated)){
      lst_X_aggregated[[i]] = vector(mode = 'list', length = n_samples)
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
          cat("n_sim:", n_sim, "\n")
    }

    fin[[K]][[1]] = result
    fin[[K]][[2]] = lst_X_original
    fin[[K]][[3]] = lst_X_aggregated
    fin[[K]][[4]] = cov(means)
    
    
    gc()

    cat("K:", K, "\n")
  }
  
  return(fin)
  
}

tic()
sim_result = simWB(cov_vec = cov_vec_)
toc()
gc()


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


true_data = vector(mode = 'list', length = length(sim_result))
for (i in seq_along(true_data)){
  true_data[[i]] = vector(mode = 'list', length = length(sim_result))
}

for (i in 1:length(sim_result)) {
  for (j in 1:length(sim_result[[1]][[2]])) {
    true_data[[i]][[j]] <- sim_result[[i]][[2]][[j]][[1]]
  }
}

true_models = true_data
for (i in 1:length(true_data)) {
  for (j in 1:length(true_data[[1]])) {
    true_data[[i]][[j]] = cbind.data.frame(true_data[[i]][[j]], 
                                           LAD21CD = rep(1:100, each = 30))
    true_models[[i]][[j]] = sem(syntax_sim,
                                 data = true_data[[i]][[j]],
                                 estimator = "mlr",
                                 cluster = 'LAD21CD',
                                 orthogonal = T)
    
    true_models[[i]][[j]] = tidy( true_models[[i]][[j]]) %>% 
      filter(label %in% c('b_HEas', 'd_HEas')) %>%
      slice(1:2) %>% dplyr::select(label, estimate, std.error) %>%
      mutate(label = ifelse(label == 'b_HEas', 'long', 'short'))
    
    print(j)
  }
  print(i)
}

names(true_models) = cov_vec_
for (i in seq_along(true_models)){
  names(true_models[[i]]) = cov_vec_
}
true_models = flattenList(true_models, lab = F)
true_models_df = do.call(rbind.data.frame, true_models)
true_models_df = cbind(true_models_df, vec_names = rownames(true_models_df))
true_models_df$vec_names = substr(true_models_df$vec_names, 1,
                                  nchar(true_models_df$vec_names)-2)
true_models_df %<>%
  separate(vec_names, into = c("Between", "Within"), sep = "_")
true_models_df$Between = as.numeric(true_models_df$Between)
true_models_df$Within = as.numeric(true_models_df$Within)
rownames(true_models_df) = NULL


## function to extract parameters for plotting
extract_heatmap_data = function(lst_data,
                                params,
                                param_name,
                                param_comb_names,
                                naive_model){
  
  
  for (i in seq_along(lst_data)){
    names(lst_data[[i]][[1]]) = param_comb_names
  }
  
  extract_params = function(lst,
                            params2 = params){
    sumtab = summaryParam(lst)
    sumtab = sumtab[params2,]
    return(sumtab)
  }
  lst_data = lapply(lst_data, function(x) x[[1]])
  
  sumtab_list = list()
  for (i in seq_along(lst_data)){
    sumtab_list[[i]] = lapply(lst_data[[i]], extract_params)
  }
  names(sumtab_list) = param_comb_names
  
  sumtab_list = flattenList(sumtab_list)
  
  sumtab_df = do.call(rbind.data.frame, sumtab_list)
  sumtab_df = cbind(sumtab_df, vec_names = rownames(sumtab_df))
  sumtab_df = sumtab_df %>%
    tidyr::separate(vec_names, into = c("Between", "Within"), sep = "_")%>%
    mutate(label = gsub(".*?\\.([a-zA-Z_]+) <- (.*)", "\\1 <- \\2", 
                        sumtab_df$vec_names))
  sumtab_df$label = setNames(param_name,params)[sumtab_df$label]
  sumtab_df$Between = as.numeric(sumtab_df$Between)
  sumtab_df$Within = substr(sumtab_df$Within, 1,
                               nchar(sumtab_df$Within)-2)
  sumtab_df$Within = as.numeric(sumtab_df$Within)
  rownames(sumtab_df) = NULL
  
  sumtab_df$label = ifelse(is.na(sumtab_df$label), param_name, sumtab_df$label)
  
  ## adding the naive model parameters and compute bias
  sumtab_df %<>% left_join(naive_model) %>%
    mutate(`bias, %` = (estimate - `Estimate Average`)/estimate)%>%
    #mutate(`bias, %` = (`Estimate Average` - estimate)/`Estimate Average`)%>%
    mutate_if(is.numeric, ~ round(., 4))
  
  return(sumtab_df)
}
## applying the function
sumtab_rcgclm = extract_heatmap_data(lst_data = sim_result,
                                     params = c('b_HEas <- (HE2~as1)',
                                                'd_HEas <- (HE2~e_as1)'),
                                     param_name = c('long', 'short'),
                                     naive_model = true_models_df,
                                     param_comb_names = cov_vec_)

## inspecting
sumtab_rcgclm_long = sumtab_rcgclm %>% filter(label == 'long') %>% arrange(`bias, %`)
sumtab_rcgclm_short = sumtab_rcgclm %>% filter(label == 'short') %>% arrange(`bias, %`)

## plotting

library(gridExtra)
library(cowplot)
library(hrbrthemes)

# rcgclm 
ggplot(sumtab_rcgclm_long, aes(Within, Between, fill = `bias, %`)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.7, 0.7)) +
  hrbrthemes::theme_ipsum()

ggplot(sumtab_rcgclm_short, aes(Within, Between, fill = `bias, %`)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-0.7, 0.7)) +
  theme_ipsum()

