
library(simsem)
library(future.apply)
library(lme4)
library(lavaan)
library(tictoc)
library(dplyr)

library(RColorBrewer)
library(ggplot2)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()

source("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/paper3_fun.r")

gen_data = function(confounding_within=0,
                    confounding_between=0,
                    n = 100,
                    n_groups = 100,
                    true_between = 0.5,
                    true_within = 0,
                    rand_effect_C_U = 0, 
                    sd_rand_effect_C_U = 0){

  cor_int = data.frame(matrix(NA, nrow = n_groups, ncol = 4))
  #cor(cor_int)
  
  cor_int[,c(3,4)] = as.data.frame(
    MASS::mvrnorm(n = n_groups,
            mu = c(0,confounding_within),
            Sigma = matrix(c(1.0, rand_effect_C_U,
                             rand_effect_C_U, 1.0), 
                           nrow = 2, ncol = 2, byrow = T)*c(1, sd_rand_effect_C_U)%*%t(c(1, sd_rand_effect_C_U)),
            empirical = T)
  )
  
  cor_int[,2] = confounding_between*(cor_int[,3] - mean(cor_int[,3])) + rnorm(n_groups, mean = 0, sd = 1)
  cor_int[,1] = true_between*(cor_int[,2] - mean(cor_int[,2])) +
    confounding_between*(cor_int[,2] - mean(cor_int[,3])) + rnorm(n_groups, mean = 0, sd = 1)
  
  #summary(lm(cor_int[,1] ~ cor_int[,2] + cor_int[,3]))
  #summary(lm(cor_int[,3] ~ cor_int[,4]))
  #mean(cor_int[,4])
  
  # generate data
  datasets = list()
  for (i in 1:n_groups) {

    C = cor_int[i,3] + rnorm(n, mean = 0, sd = 1)
    U = cor_int[i,4]*(C-mean(C)) + rnorm(n, mean = 0, sd = 1)
    U = U - mean(U)
    X = cor_int[i,2] + U
    Y = cor_int[i,1] + true_within*(X-mean(X)) + cor_int[i,4]*(C-mean(C)) + rnorm(n, mean = 0, sd = 1)
    
    
    X_rand = cor_int[i,2] + rnorm(n, mean = 0, sd = 0.05)
    X_tilted_line = cor_int[i,2] + 10*(Y-mean(Y))/200
    X_tilted_dots = X_tilted_line + rnorm(n, mean = 0, sd = 0.05)
    
    dat = data.frame(X, Y, U, C,
                     X_tilted_line,
                     X_tilted_dots,
                     X_rand)
    datasets[[i]] = dat
  }
  
  # list to data frame
  dat <- do.call(rbind, datasets)
  dat$ID <- rep(1:n_groups, each = n)
  
  # aggregate data
  dat = dat %>% 
    dplyr::group_by(ID) %>% 
    dplyr::mutate(X_bar = mean(X),
                  Y_bar = mean(Y),
                  X_centered = X - X_bar,
                  C_bar = mean(C)
    )
  

  
  dat
}

#lm(Y ~ 1, data = dat %>% filter(ID %in% 1))

# plotting dat: X and Y with lm line for X_bar and a dot plot for X
plot_dat = function(dat){
  p1 =  ggplot(dat) +
    geom_point(aes(x = X_bar, y = Y), size = 2) +
    geom_smooth(aes(x = X_bar, y = Y_bar), method = "lm", 
                se = FALSE, color = "red", size = 1.5)  +
    geom_point(aes(x = X_rand, y = Y), color = "violet", size = 1.5) +
    theme_minimal(base_size = 14) +
    geom_point(aes(x = X_bar, y = Y_bar), color = "red", size = 4)+
    labs(x = "X", y = "Y", title = "Random Deviation of X from Group Averages \n (𝛽^𝑊 = 0)") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right",
          legend.title = element_blank()) +
    scale_color_manual(values = c("red", "violet", "red", "green")) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  
  p2 =  ggplot(dat) +
    geom_point(aes(x = X_bar, y = Y), size = 2) +
    geom_smooth(aes(x = X_bar, y = Y_bar), method = "lm", se = FALSE, color = "red",
                size = 1.5) +
    geom_smooth(aes(x = X_tilted_line, y = Y, group = ID), method = "lm", se = FALSE,
                color = "green", size = 1.5)  +
    geom_point(aes(x = X_tilted_dots, y = Y, group = ID), color = "violet",
               size = 1.5) +
    theme_minimal(base_size = 14) +
    geom_point(aes(x = X_bar, y = Y_bar), color = "red", size = 4)+
    labs(x = "X", y = "Y", title = "Confounded Deviation of X from Group Averages \n (𝛽^𝑊 ≠ 0)") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right",
          legend.title = element_blank()) +
    scale_color_manual(values = c("red", "violet", "red", "green")) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}
set.seed(101801)
plot_dat(dat<-gen_data(n = 50,
                  n_groups = 5,
                  true_between = 4,
                  confounding_within = 0,
                  sd_rand_effect_C_U = 0))


set.seed(123)
grid_w = c(0, 0.5, 1, 1.5)
grid_sd = c(0, 0.5, 1, 1.5)
param_combinations <- expand.grid(conf_w=grid_w, rand_sd=grid_sd)
plan(multisession, workers = 2)

# Generate data in parallel
data_list <- future_lapply(1:nrow(param_combinations), function(idx) {
  conf_w <- param_combinations$conf_w[idx]
  rand_sd <- param_combinations$rand_sd[idx]
  
  dat_list <- vector("list", 500) # Pre-allocate a list for this combination
  for (i in 1:500) {
    dat_list[[i]] <- gen_data(confounding_within = conf_w,
                              sd_rand_effect_C_U = rand_sd)
  }
  return(dat_list)
})

names(data_list) = paste0('rand', rep(grid_sd, each = length(grid_w)),
                          '_w', grid_w)

# sim 2
set.seed(123)
grid_Nw = c(50, 100, 200)
grid_Nb = c(50, 100, 200)
param_combinations <- expand.grid(n=grid_Nw, n_groups=grid_Nb)
plan(multisession, workers = 2)

# Generate data in parallel
data_list_lmer <- future_lapply(1:nrow(param_combinations),
                                future.seed=TRUE, function(idx) {
                                  n <- param_combinations$n[idx]
                                  n_groups <- param_combinations$n_groups[idx]
                                  
                                  dat_list <- vector("list", 400) # Pre-allocate a list for this combination
                                  for (i in 1:400) {
                                    dat_list[[i]] <- gen_data(n = n,
                                                              n_groups = n_groups)
                                  }
                                  return(dat_list)
                                })


data_list_lm <- future_lapply(1:nrow(param_combinations),
                                future.seed=TRUE, function(idx) {
                                  n <- param_combinations$n[idx]
                                  n_groups <- param_combinations$n_groups[idx]
                                  
                                  dat_list <- vector("list", 400) # Pre-allocate a list for this combination
                                  for (i in 1:400) {
                                    dat_list[[i]] <- gen_data(n = n,
                                                              n_groups = n_groups)
                                  }
                                  return(dat_list)
                                })



# simulating
analyzeFUN = function(dat) {
  out = lme4::lmer(Y ~ X_centered + (1|ID), data = dat)
  coef = fixef(out)['X_centered']
  se = sqrt(diag(vcov(out)))['X_centered']
  fit = c(loglik = as.numeric(logLik(out)))
  converged = TRUE
  
  return(list(coef = coef,
              se = se,
              fit = fit,
              converged = converged))
}

analyzeFUN_lmer = function(dat) {
  out = lme4::lmer(Y ~ X_bar + (1|ID), data = dat)
  coef = fixef(out)['X_bar']
  se = sqrt(diag(vcov(out)))['X_bar']
  fit = c(loglik = as.numeric(logLik(out)))
  converged = TRUE
  
  return(list(coef = coef,
              se = se,
              fit = fit,
              converged = converged))
}

analyzeFUN_lm = function(dat) {
  out = lm(Y ~ X_bar, data = dat)
  coef = coef(out)['X_bar']
  se = sqrt(diag(vcov(out)))['X_bar']
  fit = c(loglik = as.numeric(logLik(out)))
  converged = TRUE
  
  return(list(coef = coef,
              se = se,
              fit = fit,
              converged = converged))
}

## sim 1
#tic()
## Set up parallelization
#plan(multisession, workers = 2)
#
#out_sim <- future_lapply(seq_along(data_list),
#                         future.seed=TRUE,
#                         function(i) {
#  simres <- sim(seed = 12345,
#                model = analyzeFUN,
#                rawData = data_list[[i]])
#  simres@paramValue <- tibble(X_centered = 0)
#  
#  compute_stat(simres,
#               detail = TRUE,
#               digits = 3)
#})
#plan(sequential)
#gc()
#beepr::beep()
#toc()


# sim 2
plan(multisession, workers = 2)

out_sim_lmer <- future_lapply(seq_along(data_list_lmer),
                         future.seed=TRUE,
                         function(i) {
                           simres <- sim(seed = 12345,
                                         model = analyzeFUN_lmer,
                                         rawData = data_list_lmer[[i]])
                           simres@paramValue <- tibble(X_bar = 0.5)
                           
                           compute_stat(simres,
                                        detail = TRUE,
                                        digits = 3)
                         })

out_sim_lm <- future_lapply(seq_along(data_list_lm),
                              future.seed=TRUE,
                              function(i) {
                                simres <- sim(seed = 12345,
                                              model = analyzeFUN_lm,
                                              rawData = data_list_lm[[i]])
                                simres@paramValue <- tibble(X_bar = 0.5)
                                
                                compute_stat(simres,
                                             detail = TRUE,
                                             digits = 3)
                              })
plan(sequential)
gc()
beepr::beep()
toc()

out_sim_lmer_df = do.call(rbind, out_sim_lmer)
out_sim_lmer_df$Nb = rep(grid_Nb, each = length(grid_Nw))
out_sim_lmer_df$Nw = rep(grid_Nw, length(grid_Nb))
out_sim_lmer_df$model = 'lmer'

out_sim_lm_df = do.call(rbind, out_sim_lm)
out_sim_lm_df$Nb = rep(grid_Nb, each = length(grid_Nw))
out_sim_lm_df$Nw = rep(grid_Nw, length(grid_Nb))
out_sim_lm_df$model = 'lm'

# combine dfs
out_sim_df_SEs = out_sim_lmer_df %>% 
  left_join(out_sim_lm_df, by = c('Nw', 'Nb'), suffix = c('_lmer', '_lm')) %>%
  #dplyr::select(Nw, Nb, model,
  #              `Estimate Average`,
  #              `Estimate SD`,
  #              `Average SE`,
  #              `Rel SE Bias`) %>%
  dplyr::mutate(se_dif = `Average SE_lmer`/`Average SE_lm`)

ggplot(out_sim_df_SEs, aes(x = as.factor(Nw), y = as.factor(Nb))) + 
  geom_tile(aes(fill = `se_dif`), color = "black",
            size = 0.3) +
  geom_text(aes(label = sprintf("%.4f", `se_dif`)),
            color = "black") +
  scale_fill_gradient2(low = "white",high = "blue") +
 # facet_wrap(~ model) +
  labs(x = "Within-Level Size",
       y = "Betweel-Level Size") +
  theme_ipsum_tw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 24))

out_sim_df = do.call(rbind, out_sim)
out_sim_df$rand_sd = rep(grid_sd, each = length(grid_w))
out_sim_df$w = rep(grid_w, length(grid_sd))
#out_sim_df$var = 'X_centered'

# out fin
fin = out_sim_df %>%
  dplyr::mutate(`Absolute Bias` = abs(`Average Bias`))

tab = fin %>% 
  dplyr::select(`Within-level Confounding Effect` = w,
                `SD of the Within-level Confounding Effect` = rand_sd,#var,
                `Absolute Bias`,
                `Estimate SD`,
                `Average SE`,
                `Rel SE Bias`#,
                #std_bias = `Std Bias`,
                #`Estimate Average`,
                #`Average Bias`,
                #rel_bias = `Rel Bias`
                ) 


ggplot(fin, aes(x = as.factor(w), y = as.factor(rand_sd))) + 
  geom_tile(aes(fill = `Rel SE Bias`), color = "black",
            size = 0.3) +
  geom_text(aes(label = sprintf("%.2f", `Rel SE Bias`)),
            color = "black") +
  scale_fill_gradient2(low = "white",high = "blue") +
  #facet_wrap(~ var) +
  labs(x = "Within-Level Confounding Effect, SDs",
       y = "Confounder Random Effect, SDs") +
  theme_ipsum_tw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 24),
        legend.position = "none")

ggplot(fin, aes(x = as.factor(w), y = as.factor(rand_sd))) + 
  geom_tile(aes(fill = `Absolute Bias`), color = "black",
            size = 0.3) +
  geom_text(aes(label = sprintf("%.2f", `Absolute Bias`)),
            color = "black") +
  scale_fill_gradient2(low = "white",high = "blue") +
  #facet_wrap(~ var) +
  labs(x = "Within-Level Confounding Effect",
       y = "SD of the Confounder Random Effect") +
  theme_ipsum_tw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 24),
        legend.position = "none")









