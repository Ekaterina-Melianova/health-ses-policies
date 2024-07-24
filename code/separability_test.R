
library(snow)
library(simsem)

df_lv = readRDS('df_lv.RDS')
df_lv %<>% dplyr::rename(!!!setNames(c('LAD21CD', paste0("as", 1:7), paste0("HE", 1:7)),
                                     c('ID', paste0("X", 1:7), paste0("Y", 1:7)))) %>%
  dplyr::select('ID', starts_with('X'), starts_with('Y'))


rcgclm_syntax_sim = RC_GCLM_syntax(endogeneous = c('X'#, 'Y'
                                                   ),
                                   control = NULL,
                                   model = 'regclm',
                                   max_time = 7)
rcgclm_fit_sim = sem(rcgclm_syntax_sim,
                     data = df_lv,
                     estimator = "mlr",
                     orthogonal = T,
                     cluster = 'ID'
                     )

time_vec = c(5, 10, 20)
coef_vec = c(0.001, 0.01, 0.1)
sample_size = 1000

sim_out = list()
t = 5
j = 0.01

for (t in time_vec){
  for (j in coef_vec){
    rcgclm_syntax_sim = RC_GCLM_syntax(endogeneous = c('X'),
                                       control = NULL,
                                       model = 'regclm',
                                       max_time = t)

    tab = broom::tidy(rcgclm_fit_sim)
    key_values = tab %>% filter(!label == '') %>%
      dplyr::select(label, estimate) %>%
      group_by(label) %>% top_n(n = 1)

    key_values = as.data.frame(key_values)
    key_values[key_values$label == 'd_XX','estimate'] = j
    key_values[key_values$label == 'b_XX','estimate'] = j
    key_values[key_values$label == 'evarX','estimate'] = 0.003
    key_values[key_values$label == 'var_sX','estimate'] = 0.005
    key_values[key_values$label == 'var_iX','estimate'] = 0.9
    key_values[key_values$label == 'mean_s_X','estimate'] = 0.1
    key_values[key_values$label == 'mean_i_X','estimate'] = -0.3
    key_values[key_values$label == 'cov_iX.sX','estimate'] = -0.05

    pop_syntax = rcgclm_syntax_sim
    for (i in 1:nrow(key_values)) {
      pop_syntax <- gsub(key_values$label[i], key_values$estimate[i], pop_syntax)
    }

    # baseline
    tic()
    sim_fit = sim(seed = 12345,
                  multicore = TRUE,
                  numProc = 12,
                  nRep = 1000,
                  n = sample_size,
                  generate = pop_syntax,
                  #cluster = 'ID',
                  orthogonal = T,
                  estimator = 'mlr',
                  model = rcgclm_syntax_sim,
                  silent = T)
    toc()
    #beepr::beep()
    gc()

    print(t)

    sim_out[[paste0('t', t, '_coef', j)]] = sim_fit
  }
}
# save sim out
saveRDS(sim_out, 'sim_out.RDS')

coefbase = sim_fit@coef # %>% na.omit()
cor(coefbase$`b_XX <- (X2~X1)`, coefbase$`d_XX <- (X2~e_X1)`)
plot(coefbase$`b_XX <- (X2~X1)`, coefbase$`d_XX <- (X2~e_X1)`)
beepr::beep()

extract_sim_par = function(lst){
  param = as.data.frame(t(colMeans(lst@coef, na.rm=T)))
  lst@paramValue = param
  sum = summaryParam(lst, std = F, detail = T) %>%
    rownames_to_column() %>%
    filter(rowname %in% c('b_XX <- (X2~X1)', 'd_XX <- (X2~e_X1)'))
  sum
}

res = lapply(sim_out, extract_sim_par)
res = do.call(rbind, res) %>% dplyr::select("Average Param",
                                            "Estimate Average",
                                            "Estimate SD",
                                            "Average SE",
                                            "Coverage")
rownames(res) = NULL
res$time = rep(time_vec, each = length(coef_vec)*2)
res$coef_type = rep(c('Long_Run', 'Short-Run'), length(time_vec))
res$coef = rep(rep(coef_vec, each = 2), length(time_vec))

# wide to long using pivot_longer
res %<>% pivot_longer(cols = c("Average Param",
                              "Estimate Average",
                              "Estimate SD",
                              "Average SE",
                              "Coverage"),
                     names_to = "Variable",
                     values_to = "Value") %>%
  pivot_wider(names_from = c("time", "coef"), values_from = "Value")

# cor
extract_cor = function(lst){
  coefbase = lst@coef # %>% na.omit()
  cor(coefbase$`b_XX <- (X2~X1)`, coefbase$`d_XX <- (X2~e_X1)`)
}
sim_cor = lapply(sim_out, extract_cor)
sim_cor = t(as.data.frame(do.call(rbind, sim_cor)))
sim_cor = as.data.frame(sim_cor)
colnames(sim_cor) = colnames(res)[3:length(res)]


fin2 = rbind.fill(res, sim_cor)
# save as csv
write.csv(fin2, 'sim_out.csv', row.names = F)
getwd()


#summary(baseline)

plot(coefbase$`b_XX <- (X2~X1)`, coefbase$`d_XX <- (X2~e_X1)`)

ggplot2::ggplot(coefbase, aes(x=`b_XX <- (X2~X1)`, y=`d_XX <- (X2~e_X1)`)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal() +
  # limit x axis
  coord_cartesian(ylim = c(-0.3, 0.1), xlim = c(0.3, 0.1))


