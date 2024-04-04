
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

gen_data = function(confounding_within=1.5,
                    confounding_between=0,
                    n = 100,
                    n_groups = 100,
                    true_between = 0.5,
                    true_within = 0,
                    rand_effect_C_U = 0.5, 
                    sd_rand_effect_C_U = 1){
set.seed(123)
  cor_int = data.frame(matrix(NA, nrow = n_groups, ncol = 5))
  #cor(cor_int)
  sig = matrix(c(1, rand_effect_C_U, 
                 rand_effect_C_U, 1), 
               nrow = 2) * sd_rand_effect_C_U^2
  #sig = matrix(c(4, 0, 
  #               0, 1), 
  #             nrow = 2)
  
  cor_int[,c(5,4)] = as.data.frame(
    MASS::mvrnorm(n = n_groups,
            mu = c(confounding_within,confounding_within),
            Sigma = sig,
            empirical = T)
  )
  
  cor_int[,3] = rnorm(n, mean = 0, sd = 1)
  cor_int[,2] = confounding_between*(cor_int[,3] - mean(cor_int[,3])) + rnorm(n_groups, mean = 0, sd = 1)
  cor_int[,1] = true_between*(cor_int[,2] - mean(cor_int[,2])) +
    confounding_between*(cor_int[,3] - mean(cor_int[,3])) + rnorm(n_groups, mean = 0, sd = 1)
  
  #summary(lm(cor_int[,1] ~ cor_int[,2] + cor_int[,3]))
  #summary(lm(cor_int[,3] ~ cor_int[,4]))
  #mean(cor_int[,4])
  
  # generate data
  datasets = list()
  for (i in 1:n_groups) {

    C = cor_int[i,3] + rnorm(n, mean = 0, sd = 1)
    U = cor_int[i,5]*(C-mean(C)) + rnorm(n, mean = 0, sd = 1)
    #U = confounding_within*(C-mean(C)) + rnorm(n, mean = 0, sd = 1)
    U = U - mean(U)
    X = cor_int[i,2] + U
    Y = cor_int[i,1] + true_within*(X-mean(X)) + cor_int[i,4]*(C-mean(C)) + rnorm(n, mean = 0, sd = 1)
    #Y = cor_int[i,1] + true_within*(X-mean(X)) + confounding_within*(C-mean(C)) + rnorm(n, mean = 0, sd = 1)
    
    
    X_rand = cor_int[i,2] + rnorm(n, mean = 0, sd = 0.05)
    X_tilted_line = cor_int[i,2] + (X-mean(X))/5
    X_tilted_dots = X_tilted_line + rnorm(n, mean = 0, sd = 0.005)
    
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
    ) %>%
    ungroup() %>%
    dplyr::mutate(X_centered = scale(X_centered),
                  Y = scale(Y))
  

  
  dat
}

# plotting dat: X and Y with lm line for X_bar and a dot plot for X
plot_dat = function(dat, null = F){
  
  if (null){
    p1 =  ggplot(dat) +
      geom_point(aes(x = X_bar, y = scale(Y)), size = 2) +
      geom_smooth(aes(x = X_bar, y = scale(Y_bar)), method = "lm", 
                  se = FALSE, color = "red", size = 1.5)  +
      #geom_point(aes(x = X_rand, y = Y), color = "violet", size = 1.5) +
      #geom_smooth(aes(x = X_bar, y = Y, group = ID), method = "lm", se = FALSE,
      #            color = "green", size = 1.5)  +
      theme_minimal(base_size = 14) +
      geom_point(aes(x = X_bar, y = scale(Y_bar)), color = "darkgreen", size = 4)+
      labs(x = "X", y = "Y", title = expression(beta^W == 0~"and"~X[ij] == bar(X)[.j])) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank()) +
      scale_color_manual(values = c("red", "violet", "red", "green")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
    
    p2 = ggplot(dat) +
      #geom_point(aes(x = X_bar, y = Y), size = 2) +
      geom_smooth(aes(x = X_bar, y = scale(Y_bar)), method = "lm", se = FALSE, color = "red",
                  size = 1.5) +
      geom_segment(aes(x = X_bar - 0.1, xend = X_bar + 0.1, y = scale(Y_bar), yend = scale(Y_bar)), 
                   color = "green", size = 1.5)  +
      geom_point(aes(x = X_rand, y = scale(Y), group = ID), color = "violet",
                 size = 1.5) +
      theme_minimal(base_size = 14) +
      geom_point(aes(x = X_bar, y = scale(Y_bar)), color = "darkgreen", size = 4)+
      labs(x = "X", y = "Y", title = expression(beta^W == 0~"and"~X[ij] != bar(X)[.j])) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank()) +
      scale_color_manual(values = c("red", "violet", "red", "green")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
  } 
  else {

      p2 = ggplot(gen_data(n = 50,
                         n_groups = 5,
                         true_between = 25,
                         true_within = 1,
                         confounding_within = 0,
                         sd_rand_effect_C_U = 0)) +
      #geom_point(aes(x = X_bar, y = Y), size = 2) +
      geom_smooth(aes(x = X_bar, y = scale(Y_bar)), method = "lm", se = FALSE, color = "red",
                  size = 1.5) +
      geom_smooth(aes(x = X_tilted_line, y = scale(Y), group = ID), method = "lm", se = FALSE,
                  color = "green", size = 1.5)  +
      geom_point(aes(x = X_tilted_dots, y = scale(Y), group = ID), color = "violet",
                 size = 1.5) +
      theme_minimal(base_size = 14) +
      geom_point(aes(x = X_bar, y = scale(Y_bar)), color = "darkgreen", size = 4)+
      labs(x = "X", y = "Y", title = expression(beta^W != 0~"and"~beta^W != beta^B)) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank()) +
      scale_color_manual(values = c("red", "violet", "red", "green")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
    p1 = ggplot(gen_data(n = 50,
                          n_groups = 5,
                          true_between = 25,
                          true_within = 4,
                          confounding_within = 0,
                          sd_rand_effect_C_U = 0)) +
      #geom_point(aes(x = X_bar, y = Y), size = 2) +
      geom_smooth(aes(x = X_bar, y = scale(Y_bar)), method = "lm", se = FALSE, color = "red",
                  size = 1.5) +
      geom_smooth(aes(x = X_tilted_line, y = scale(Y), group = ID), method = "lm", se = FALSE,
                  color = "green", size = 1.5)  +
      geom_point(aes(x = X_tilted_dots, y = scale(Y), group = ID), color = "violet",
                 size = 1.5) +
      theme_minimal(base_size = 14) +
      geom_point(aes(x = X_bar, y = scale(Y_bar)), color = "darkgreen", size = 4)+
      labs(x = "X", y = "Y", title = beta^W != 0~"and"~beta^W %~~% beta^B) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank()) +
      scale_color_manual(values = c("red", "violet", "red", "green")) +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
    }
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}
set.seed(101801)
plot_dat(dat <- gen_data(n = 50,
                  n_groups = 5,
                  true_between = 1,
                  confounding_within = 0,
                  sd_rand_effect_C_U = 0),
         null = T)
set.seed(101801)
plot_dat(dat <- gen_data(n = 50,
                         n_groups = 5,
                         true_between = 5,
                         true_within = 2,
                         confounding_within = 0,
                         sd_rand_effect_C_U = 0),
         null = F)

### Simulation

set.seed(123)
grid_w = c(0, 0.5, 1, 1.5)
grid_rand = c(0, 0.3, 0.6, 0.9)
param_combinations <- expand.grid(conf_w=grid_w, rand=grid_rand)
plan(multisession, workers = 2)

# Generate data in parallel
data_list <- future_lapply(1:nrow(param_combinations),
                           future.seed=TRUE, function(idx) {
  conf_w <- param_combinations$conf_w[idx]
  rand <- param_combinations$rand[idx]
  
  dat_list <- vector("list", 500) # Pre-allocate a list for this combination
  for (i in 1:500) {
    dat_list[[i]] <- gen_data(confounding_within = conf_w,
                              rand_effect_C_U = rand)
  }
  return(dat_list)
})

names(data_list) = paste0('rand', rep(grid_rand, each = length(grid_w)),
                          '_w', grid_w)


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

# sim 1
tic()
# Set up parallelization
plan(multisession, workers = 2)

out_sim <- future_lapply(seq_along(data_list),
                         future.seed=TRUE,
                         function(i) {
  simres <- sim(seed = 12345,
                model = analyzeFUN,
                rawData = data_list[[i]])
  simres@paramValue <- tibble(X_centered = 0)
  
  compute_stat(simres,
               detail = TRUE,
               digits = 3)
})
plan(sequential)
gc()
beepr::beep()
toc()

out_sim_df = do.call(rbind, out_sim)
out_sim_df$rand = rep(grid_rand, each = length(grid_w))
out_sim_df$w = rep(grid_w, length(grid_rand))

fin = out_sim_df %>%
  dplyr::mutate(`Absolute Bias` = abs(`Average Bias`))

tab = fin %>% 
  dplyr::select(`Within-level Confounding Effect` = w,
                `Correlation Between Confounding Random Effects` = rand,
                `Absolute Bias`,
                `Estimate SD`,
                `Average SE`,
                `Rel SE Bias`#,
                #std_bias = `Std Bias`,
                #`Estimate Average`,
                #`Average Bias`,
                #rel_bias = `Rel Bias`
                ) 

ggplot(fin, aes(x = as.factor(w), y = as.factor(rand))) + 
  geom_tile(aes(fill = `Absolute Bias`), color = "black",
            size = 0.3) +
  geom_text(aes(label = sprintf("%.2f", `Absolute Bias`)),
            color = "black") +
  scale_fill_gradient2(low = "white",high = "darkred") +
  #facet_wrap(~ var)  +
  theme_ipsum(base_size = 15, axis_title_size = 10) +
  theme(#axis.title = element_text(size = 35),
        #axis.text = element_text(size = 34),
        legend.position = "none")+
  labs(x = "Fixed Part of the Confounder's Effects on U and Y, SDs",
       y = "Correlation Between Random Parts\n
       of the Confounder's Effects on U and Y")









