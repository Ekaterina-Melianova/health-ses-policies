
library(gridExtra)
library(cowplot)
library(hrbrthemes)

ToWide = function(dat){
  
  dat =  dat %>%
    ungroup() %>% 
    mutate(lsoa11 = 1:n()) %>%
    pivot_longer(cols = -c(LAD21CD, lsoa11),
                 names_to = c(".value", "time"),
                 names_pattern = "(\\w+)(\\d+)")
  dat$time = as.numeric(dat$time)
  
  return(dat)
  
}

syntax_sim = RC_GCLM_syntax(endogeneous = c('HE', 'as'),
                            control = NULL)
fit_sim = sem(syntax_sim,
              data = df_lv,
              estimator = "mlr",
              cluster = 'LAD21CD',
              orthogonal = T)

fit_sim_hlm = function(dat){
  
  model = lme4::lmer(HE ~ as + time + (1|LAD21CD) + (1|lsoa11),
                     data = dat,
                     control = lmerControl(calc.derivs = FALSE))
  out_list = list(coef = summary(model)[["coefficients"]][,'Estimate'],
                  se = summary(model)[["coefficients"]][,'Std. Error'],
                  converged = TRUE)
  
  return(out_list)
}

    

#cov_vec = seq(-0.5, 0.5, 0.1)
#cov_vec = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
#cov_vec = seq(-0.9, 0.9, 0.1)

cov_vec = c(-0.9, -0.7, -0.5, -0.3, 0, 0.3, 0.5, 0.7, 0.9)


cov_vec_ = cov_vec
par_tab_ = par_tab
syntax_sim_ = syntax_sim

simWB = function(cov_vec = cov_vec_,
                 par_tab = par_tab_,
                 syntax_sim = syntax_sim_,
                 type = 'clpm'){
  
  fin = list()
  fin = vector(mode = 'list', length = length(cov_vec))
  for (i in seq_along(fin)){
    fin[[i]] = vector(mode = 'list', length = 4)
  }
  
  
  for (K in seq_along(cov_vec)){
    
    #cov_vec = cov_vec[7:8]
    n_groups = 5
    N = 50
    #seed_vec = seq(1, length(cov_vec), 1)
    
    
    par_tab = list(
      c('cov_iHE.ias','NA'),
      c('mean_i_HE','mean_i_HE'),
      c('mean_i_as','mean_i_as'),
      c('var_iHE','var_iHE'),
      c('var_ias','var_ias'))
    
    par_tab = do.call(rbind.data.frame, par_tab)
    colnames(par_tab) = c('label', 'par')
    

    par_tab_list = list()
    set.seed(12345)
    v1 = rnorm(n_groups, mean = 0, sd = 1)
    v2 = rnorm_pre(v1, mu = 0, sd = 1, r = cov_vec[K], empirical = T)
    means = cbind.data.frame(v1,v2)
    
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
    
    # adding within cor
    synt_list_fin = list()
    for (N_COR in seq_along(cov_vec)){
      synt_list_fin[[N_COR]] = lapply(synt_list,
                                  function(x) gsub('NA', cov_vec[N_COR], x))
    }
    
    # true long-run effect for data generation
    synt_list_generate = synt_list_fin
    for (N_COR in seq_along(cov_vec)){
      synt_list_generate[[N_COR]] = lapply(synt_list_fin[[N_COR]],
                                           function(x) gsub('b_HEas', '(-0.2)', x))
      }


    # gen data
    Output = vector(mode = 'list', length = length(cov_vec))
    for (i in seq_along(Output)){
      Output[[i]] = vector(mode = 'list', length = n_groups)
    }
    
    draws = 6
    seed_vec = seq(1, n_groups, 1)
    
    # generate data with given between and within cors 
    for (N_COR in seq_along(cov_vec)){
      for (n_group in 1:n_groups){
        Output[[N_COR]][[n_group]] = simsem::sim(draws, 
                                                 model = synt_list_fin[[N_COR]][[n_group]],
                                                 dataOnly = T,
                                                 seed = seed_vec[n_group],
                                                 n = N,
                                                 generate = synt_list_generate[[N_COR]][[n_group]],
                                                 estimator = "mlr",
                                                 orthogonal = T,
                                                 multicore = T,
                                                 numProc = 12
                                         )
      }
    }
    
   
    insp = summaryParam(Output[[1]][[1]])
    
    #
    # est true effects
    

    lst_X_aggregated = vector(mode = 'list', length = length(Output))
    for (i in seq_along(lst_X_aggregated)){
      lst_X_aggregated[[i]] = vector(mode = 'list', length = draws)
    }
    lst_X_original = lst_X_aggregated
    lst_XY_aggregated = lst_X_aggregated
    for (j in 1:length(Output)){
          for (i in 1:length(Output[[1]][[1]])){
            lst_X_original[[j]][[i]] = cbind.data.frame(do.call(rbind.data.frame,
                                               lapply(Output[[j]],
                                                      function(lst) lst[[i]])),
                                             LAD21CD = rep(1:n_groups, each = N))
            
            lst_X_aggregated[[j]][[i]] = 
              lst_X_original[[j]][[i]] %>%
        group_by(LAD21CD) %>%
        mutate(across(starts_with('as'), 
                      function(x) mean(x)))
            
           lst_XY_aggregated[[j]][[i]] = lst_X_original[[j]][[i]] %>%
             group_by(LAD21CD) %>% dplyr::summarise_all(.funs = mean)
      }
    }
    #cor(lst_X_aggregated[[1]][[3]])
    
    
    # simulating
    
    result = list()
    

      n_sim = 1
      for (sym_data in lst_X_aggregated){
        result[[n_sim]] = simsem::sim(seed = 12345, 
                                      model = fit_sim,
                                      rawData = sym_data,
                                      multicore = T,
                                      numProc = 12)
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
sim_result_hlm = simWB(type = 'hlm')
toc()
gc()

# mean_s_HE = -0.2, long = 0.1, short = -0.1, cov_vec = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
tic()
sim_result2 = simWB()
toc()
gc()

# mean_s_HE = -0.2, long = 0.1, short = 0.1, cov_vec = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
tic()
sim_result3 = simWB()
toc()
gc()

# mean_s_HE = -0.2, long = 0.2, short = 0.2, cov_vec = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
tic()
sim_result4 = simWB()
toc()
gc()

# mean_s_HE = -0.2, long = 0.2, short = 0.2, cov_vec = c(-0.9, -0.7, -0.5, -0.3, 0, 0.3, 0.5, 0.7, 0.9)
tic()
sim_result5 = simWB()
toc()
gc()

# mean_s_HE = 0.2, long = 0.2, short = 0.2, cov_vec = c(-0.9, -0.7, -0.5, -0.3, 0, 0.3, 0.5, 0.7, 0.9)
tic()
sim_result6 = simWB()
toc()
gc()


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



## function to extract parameters for plotting
extract_heatmap_data = function(lst_data,
                                params,
                                param_name,
                                param_comb_names,
                                long_val,
                                short_val){
  
  
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
  sumtab_df = sumtab_df %>% 
    mutate(`bias, %` = ifelse(label == 'long',
                              (`Estimate Average` - long_val)/long_val,
                              (`Estimate Average` - short_val)/short_val)) %>%
    mutate_if(is.numeric, ~ round(., 4))
  
  return(sumtab_df)
}
## applying the function
sumtab_rcgclm = extract_heatmap_data(lst_data = sim_result6,
                                     params = c('b_HEas <- (HE2~as1)',
                                                'd_HEas <- (HE2~e_as1)'),
                                     param_name = c('long', 'short'),
                                     param_comb_names = cov_vec,
                                     long_val = 0.2,
                                     short_val = 0.2)
sumtab_rcgclm = extract_heatmap_data(lst_data = sim_result4,
                                     params = c('b_HEHE <- (HE2~HE1)',
                                                'd_HEHE <- (HE2~e_HE1)'),
                                     param_name = c('long', 'short'),
                                     param_comb_names = cov_vec,
                                     long_val = 0.05,
                                     short_val = 0.2)
sumtab_rcgclm = extract_heatmap_data(lst_data = sim_result4,
                                     params = c('b_asas <- (as2~as1)',
                                                'd_asas <- (as2~e_as1)'),
                                     param_name = c('long', 'short'),
                                     param_comb_names = cov_vec,
                                     long_val = 0.2,
                                     short_val = 0.01)
sumtab_rcgclm$Between = as.factor(round(sumtab_rcgclm$Between, 2))
sumtab_rcgclm$Within = as.factor(round(sumtab_rcgclm$Within, 2))

## inspecting
sumtab_rcgclm_long = sumtab_rcgclm %>% filter(label == 'long') %>% arrange(`bias, %`)
sumtab_rcgclm_short = sumtab_rcgclm %>% filter(label == 'short') %>% arrange(`bias, %`)
summary(sumtab_rcgclm_long$`bias, %`)
summary(sumtab_rcgclm_short$`bias, %`)

## plotting

# rcgclm 
ggplot(sumtab_rcgclm_long, aes(Within, Between, fill = `bias, %`)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white') +
theme_ipsum()

ggplot(sumtab_rcgclm_short, aes(Within, Between, fill = `bias, %`)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white'#,
                       #limits = c(-1, 1)
                       )+
  theme_ipsum()

