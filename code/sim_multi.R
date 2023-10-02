

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


# define cor matrices

t = 7
autocor = 0.98
bg_value = seq(-0.9, 0.9, 0.2)
wg_value = seq(-0.9, 0.9, 0.2)
bw_grid = expand.grid(bg_value, wg_value)
eta = rep(.5, 2*t)
eta = c(c(0.5, 0.52, 0.6, 0.61, 0.71, 0.77, 0.8),
        c(0.3, 0.35, 0.49, 0.45, 0.51, 0.5, 0.69))
eta = test_lv$etawg 
eta = c(c(0.57, 0.56, 0.55, 0.54, 0.53, 0.52, 0.51),
        c(0.55, 0.54, 0.53, 0.52, 0.51, 0.50, 0.49))
  
list_wb_comb = list()
for(i in 1:nrow(bw_grid)){
  
  # between group
  xx_bg = diag(1, t)
  xx_bg[upper.tri(xx_bg)] = autocor
  xx_bg[lower.tri(xx_bg)] = autocor
  
  yy_bg = diag(1, t)
  yy_bg[upper.tri(yy_bg)] = autocor
  yy_bg[lower.tri(yy_bg)] = autocor
  
  xy_bg = matrix(data = bw_grid[i,2], nrow = t, ncol = t)
  #xy_bg = generate_matrix(bw_grid[i,2])
  yx_bg = t(xy_bg)
  
  rbg = rbind(cbind(xx_bg, xy_bg), cbind(yx_bg, yy_bg))
  
  # within group
  xx_wg = diag(1, t)
  xx_wg[upper.tri(xx_wg)] = autocor
  xx_wg[lower.tri(xx_wg)] = autocor
  
  yy_wg = diag(1, t)
  yy_wg[upper.tri(yy_wg)] = autocor
  yy_wg[lower.tri(yy_wg)] = autocor
  
  xy_wg = matrix(data = bw_grid[i,1], nrow = t, ncol = t)
  #xy_wg = generate_matrix(bw_grid[i,1])
  yx_wg = t(xy_wg)
  
  rwg = rbind(cbind(xx_wg, xy_wg), cbind(yx_wg, yy_wg))
  
  list_wb_comb[[i]] = list(rbg, rwg)
}
list_wb_comb[[i]]

# generating data

no_cores = detectCores() - 4
cl = makeCluster(no_cores)
registerDoParallel(cl)

#n_comb = 5
n_samples = 100
n_comb = length(list_wb_comb)
seed_vec = 1:n_samples
data_sim = vector(mode = 'list', length = n_comb)

tic()
data_sim = foreach(i=1:n_comb, .combine='c', .packages='psych') %dopar% {
  
  data_list = vector(mode = 'list', length = n_samples)
  
  for (j in 1:n_samples) {
    set.seed(seed_vec[j])
    data_list[[j]] = as.data.frame(psych::sim.multilevel(nvar = 2*t,
                                                         ncases = 3000,
                                                         ngroups = 100,
                                                         rbg = list_wb_comb[[i]][[1]],
                                                         rwg = list_wb_comb[[i]][[2]],
                                                         eta = eta)$xy)
    colnames(data_list[[j]]) = c('Group', paste0('HE', 1:7), paste0('SP', 1:7))
  }
  
  list(data_list)
}
toc()
stopCluster(cl)

tic()
# data with aggregated S variables
data_sim_X_aggreg = data_sim
for (i in seq_along(data_sim)){
  data_sim_X_aggreg[[i]] = lapply(data_sim[[i]], function(lst){
       lst %>% group_by(Group) %>% mutate(across(starts_with('S'),
                                                 function(x) mean(x)))
    })
}
toc()

#tic()
## data with aggregated H and S variables
#data_sim_XY_aggreg = data_sim
#for (i in seq_along(data_sim)){
#  data_sim_XY_aggreg[[i]] = lapply(data_sim[[i]], function(lst){
#    lst %>% group_by(Group) %>% summarise_all(.funs = mean)
#  })
#}
#toc()
gc()

## running simulations

syntax_sim = RC_GCLM_syntax(endogeneous = c('HE', 'SP'),
                            control = NULL)
#fit_sim = sem(syntax_sim,
#              data = data_sim[[80]][[1]],
#              estimator = "mlr",
#              cluster = 'Group',
#              orthogonal = T)
summary(fit_sim)


# true model
result_true = list()

tic()
n_sim = 0
for (sym_data in data_sim){
  
  n_sim = n_sim + 1
  
  result_true[[n_sim]] = simsem::sim(seed = 12345, 
                                     model = fit_sim,
                                     rawData = sym_data,
                                     multicore = T,
                                     numProc = 12)
  gc()
  cat("n_sim:", n_sim, "\n")
}
toc()
beepr::beep()

# misspecified model

result_missp = list()

tic()
n_sim = 0
for (sym_data in data_sim_X_aggreg){
  
  n_sim = n_sim + 1
  
  result_missp[[n_sim]] = simsem::sim(seed = 12345, 
                                      model = syntax_sim,
                                      rawData = sym_data,
                                      multicore = T,
                                      numProc = 12)
  gc()
  cat("n_sim:", n_sim, "\n")
}
toc()
beepr::beep()


## extracting params
result_true
result_missp
params_true = lapply(result_true, summaryParam)
params_true = lapply(params_true, function(lst){
  as.data.frame(lst['d_HESP <- (HE2~e_SP1)', c('Estimate Average',
                                             'Estimate SD')]) %>%
    dplyr::select(true = `Estimate Average`)
})
params_true = do.call(rbind.data.frame, params_true)

params_missp = lapply(result_missp, summaryParam)
params_missp = lapply(params_missp, function(lst){
  as.data.frame(lst['d_HESP <- (HE2~e_SP1)', c('Estimate Average',
                                             'Estimate SD')]) %>%
    dplyr::select(missp = `Estimate Average`)
})
params_missp = do.call(rbind.data.frame, params_missp)

params_all = cbind.data.frame(params_true, params_missp, bw_grid)
params_all %<>% mutate(bias = (missp - true)/true)

ggplot(params_all, aes(Var1, Var2, fill = bias)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white',
                       limits = c(-50,1)) +
  theme_ipsum()

ggplot(params_all, aes(Var1, Var2, fill = bias)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white'#,
                       #limits = c(-1, 1)
  )+
  theme_ipsum()



##
fit_temp1 = sem(syntax_sim,
                data = temp1,
                estimator = "mlr",
                cluster = 'Group',
                orthogonal = T)
summary(fit_temp1)
fit_temp2 = sem(syntax_sim,
                data = temp2,
                estimator = "mlr",
                cluster = 'Group',
                orthogonal = T)
summary(fit_temp2)
fit_temp3 = sem(syntax_sim,
                data = temp3,
                estimator = "mlr",
                orthogonal = T)
summary(fit_temp3)

