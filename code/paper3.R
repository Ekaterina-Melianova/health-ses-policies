library(glue)
library(tidyverse)
library(lavaan)
library(data.table)
library(tidyr)
library(broom)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
options(max.print=3900)



gen_data = function(n = 50,
                    n_groups = 50,
                    true_between = -0.5,
                    true_within = 0.2,
                    seed = seed){
  set.seed(seed)
  cor_int = data.frame(matrix(NA, nrow = n_groups, ncol = 2))
  cor_int[,2] = 0.5 + rnorm(n_groups, mean = 0, sd = 1)
  cor_int[,1] = 0.5 + true_between*(cor_int[,2] - mean(cor_int[,2])) + rnorm(n_groups, mean = 0, sd = 1)

  # generate data
  datasets = list()
  for (i in 1:n_groups) {
    
    vec_X = paste0('X', 0:5)
    vec_Y = paste0('Y', 0:5)
    
    X_rand = cor_int[i,2] + rnorm(n, mean = 0, sd = 0.05)
    X_tilted_line = cor_int[i,2] + (X-mean(X))/5
    X_tilted_dots = X_tilted_line + rnorm(n, mean = 0, sd = 0.005)
    
    X0 = cor_int[i,2] + rnorm(n, mean = 0, sd = 1)
    Y0 = cor_int[i,1] + true_within*(X1-mean(X1)) + rnorm(n, mean = 0, sd = 1)
    #cor(X1, Y1)
    
    dat = data.frame(X0,
                     Y0,
                     X_rand,
                     X_tilted_line,
                     X_tilted_dots)
    
    trend = 1:4
    
    for (t in 1:(length(vec_X)-1)){
      
      #dat[vec_X[t+1]] = cor_int[i,2] + 0.7*(dat[vec_X[t]] - mean(dat[,vec_X[t]])) + 
      #  0.3*(dat[vec_Y[t]] - mean(dat[,vec_Y[t]])) +
      #  rnorm(n, mean = 0, sd = 1) + 0.2*trend[t]
      #
      #dat[vec_Y[t+1]] = cor_int[i,1] + 0.8*(dat[vec_Y[t]] - mean(dat[,vec_Y[t]])) + 
      #  0.2*(dat[vec_X[t]] - mean(dat[,vec_X[t]])) +
      #  rnorm(n, mean = 0, sd = 1) - 0.2*trend[t]
      
      set.seed(seed)
      dat[,vec_X[t+1]] = faux::rnorm_pre(dat[,c(vec_Y[t], vec_X[t])],
                                  mu = cor_int[i,2]# + 0.2*trend[t]
                                  ,
                                  sd = 1,
                                  r = c(0.2, 0.2), empirical = T)
      set.seed(seed)
      dat[,vec_Y[t+1]] = faux::rnorm_pre(dat[,c(vec_X[t+1], vec_X[t], vec_Y[t])],
                                     mu = cor_int[i,1]# + 0.2*trend[t]
                                     ,
                                     sd = 1,
                                     r = c(true_within, 0.3, 0.2), empirical = T)
      
    }
    summary(lm(Y3 ~ X3, data = dat))
    summary(lm(Y3 ~ X2 + Y2 + X3, data = dat))
    summary(lm(Y4 ~ X3 + Y3 + I(X4-mean(X4)), data = dat))
    summary(lm(Y5 ~ X4 + Y4, data = dat))
    
    datasets[[i]] = dat
  }
  
  # list to data frame
  dat <- as.data.frame(do.call(rbind, datasets))
  dat$ID <- rep(1:n_groups, each = n)
  
  # aggregate data
  dat = dat %>% 
    dplyr::group_by(ID) %>% 
    dplyr::mutate(X_bar = mean(X1),
                  X_bar2 = mean(X2),
                  X_bar3 = mean(X3),
                  Y_bar = mean(Y1),
                  #X = scale(X),
                  #Y = scale(Y),
                  X_centered = X - X_bar
    ) %>%
    ungroup()%>%
    # add id
    dplyr::mutate(id = row_number())
  
  # scale variables in vec_X
  dat = dat %>% 
    dplyr::mutate(across(all_of(vec_X), scale),
                  across(all_of(vec_Y), scale)) 
  
  dat
}

dat = gen_data(seed = 123,
               true_between = 0.6,
               true_within = 0.8)
rcclpm_fit = sem(rcclpm_syntax,
                 data = dat ,
                 estimator = "mlr",
                 orthogonal = T,
                 cluster = 'ID')
summary(rcclpm_fit, std=T, ci = F)
summary(dat)


# Prepare the data for plotting
long_data_melted <- dat %>% 
  dplyr::select(ID, id, 
                X1, X2, X3, X4, X5,
                Y1, Y2, Y3, Y4, Y5) %>%
  pivot_longer(
    cols = -c(ID, id), 
    names_to = c(".value", "time"), 
    names_pattern = "([XY])([0-9]+)"
  ) %>%
  group_by(ID,time) %>%
  dplyr::mutate(
                X_bar = mean(X),
                Y_bar = mean(Y),
                time = as.numeric(time))
#long_data_melted$X = scale(long_data_melted$X)
#summary(dat)

#summary(lme4::lmer(Y ~ time + X + (1 | ID) + (1 | id),
 #                  data = long_data_melted))
#summary(lme4::lmer(Y ~  I(X-X_bar) + X_bar + (1 | ID) + (1 | id),
#                   data = long_data_melted))
summary(lme4::lmer(Y ~ time + I(X-X_bar) + X_bar + (1 | ID) + (1 | id),
                   data = long_data_melted))
summary(lme4::lmer(Y ~ time +  X_bar  + (1 | ID),
                   data = long_data_melted))
summary(lme4::lmer(Y ~ time +  X_bar  + (1 | id),
                   data = long_data_melted))

summary(lme4::lmer(Y_bar ~ time +  X_bar + (1 | id),
                   data = long_data_melted))

summary(lme4::lmer(Y_bar ~ time +  X_bar + (1 | ID),
                   data = long_data_melted))
summary(lm(Y_bar ~ time +  X_bar,
                   data = long_data_melted))

summary(dat)
summary(lme4::lmer(Y1 ~ X1 + X_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y1 ~ I(X1 - X_bar) + X_bar + (1 | ID),
                   data = dat))
summary(lme4::lmer(Y2 ~ I(X2 - X_bar2) + X_bar2 + (1 | ID),
                   data = dat))
summary(lme4::lmer(Y3 ~ I(X3 - X_bar3) + X_bar3 + (1 | ID),
                   data = dat))



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
                         true_between = 0.5,
                         true_within = 0.5)) +
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
                         true_within = 4)) +
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
                         n_groups = 5),
         null = T)

plot_dat(dat <- gen_data(n = 50,
                         n_groups = 5),
         null = F)

# ---------------------------------------------temp data
#dat$X = as.numeric(dat$X)
#dat$Y = as.numeric(dat$Y)
#dat$X_bar = as.numeric(dat$X_bar)


    # Plotting
ggplot(long_data_melted %>% filter(ID %in% c(1,20))
       ,
       aes(x = as.numeric(time),
                             y = Y,
                             group = id,
                             color = as.factor(ID))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

ggplot(long_data_melted %>% filter(ID %in% c(1)),
       aes(x = as.numeric(time),
                             y = X,
                             group = id,
                             color = as.factor(ID)))  +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()


# 2. RC-CLPM
no_slopes = c('sX', 'sY')
dat = dat %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(X1 = mean(X1),
                X2 = mean(X2),
                X3 = mean(X3),
                X4 = mean(X4),
                X5 = mean(X5)
  ) %>%
  ungroup()
#summary(dat)
rcclpm_syntax = RC_GCLM_syntax(model = 'reclpm',
                               no_slopes = no_slopes,
                               endogeneous = c('X', 'Y'),
                               reverse = c('X', 'Y'),
                               control = NULL,
                               max_time = 5)
rcclpm_fit = sem(rcclpm_syntax,
                data = dat ,
                estimator = "mlr",
                orthogonal = T,
                cluster = 'ID')
summary(rcclpm_fit, std=T, ci = F)
fm_rcclpm_fit = fitmeasures(rcclpm_fit, measures)
