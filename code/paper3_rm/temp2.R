
list_wb_comb_2 = list()
for(i in 1:nrow(bw_grid)){
  
  # between group
  xx_bg = generate_autocorr_mat(k = 0.05)
  
  yy_bg = generate_autocorr_mat(k = 0.1)
  
  xy_bg = matrix(data = bw_grid[i,2], nrow = t, ncol = t)
  #xy_bg = generate_matrix(bw_grid[i,2])
  yx_bg = t(xy_bg)
  
  rbg = rbind(cbind(xx_bg, xy_bg), cbind(yx_bg, yy_bg))
  
  # within group
  xx_wg = generate_autocorr_mat(k = 0.05)
  
  yy_wg = generate_autocorr_mat(k = 0.1)
  
  xy_wg = matrix(data = bw_grid[i,1], nrow = t, ncol = t)
  #xy_wg = generate_matrix(bw_grid[i,1])
  yx_wg = t(xy_wg)
  
  rwg = rbind(cbind(xx_wg, xy_wg), cbind(yx_wg, yy_wg))
  
  list_wb_comb_2[[i]] = list(rbg, rwg, rep(bw_grid[i,'eta'], 2*t))
}

# 0.3 0.6 0.7 0.8 0.9
eta = rep(.75, 2*t)
test1 = as.data.frame(psych::sim.multilevel(nvar = 2*t,
                                           ncases = 50000,
                                           ngroups = 100,
                                           rbg = list_wb_comb[[38]][[1]],
                                           rwg = list_wb_comb[[38]][[2]],
                                           eta = list_wb_comb[[38]][[3]])$xy)


colnames(test1) = c('Group', paste0('HE', 1:7), paste0('SP', 1:7))

stat_test1 = psych::statsBy(test1 , 'Group')
stat_test1$rbg[8:14,1:7]
stat_test1$rwg[8:14,1:7]
stat_test1$etawg
stat_test1$etabg
stat_test1$ICC1
stat_test1$ICC2

library(multilevel)
waba(test1$HE1, test1$SP1, test1$Group)

####


test2 = test1 %>%
  group_by(Group) %>%
  mutate(across(starts_with('SP'), 
                function(x) mean(x)))

test3 = test1 %>%
  group_by(Group) %>%
  mutate(across(starts_with('SP')|starts_with('HE'), 
                function(x) mean(x)))

cor(test1$SP1, test1$HE1)

within_cor = test1 %>%
  group_by(Group) %>%
  summarise(cor = cor(SP1, HE1))
mean(within_cor$cor)

cor(test3$SP1, test3$HE1)
cor(test2$SP1, test2$HE1)

#mean(c(cor(test3$SP1, test3$HE1), mean(within_cor$cor)))
wb = waba(test1$HE1, test1$SP1, test1$Group)
sqrt(wb$Cov.Theorem[,2])*sqrt(wb$Cov.Theorem[,3])*cor(test3$SP1, test3$HE1)

# ICC is low -> mean(bw_cor, within_cor)
# ICC is high -> bw_cor

test1 = ToWide(test1)
test2 = ToWide(test2)
#test3 = ToWide(test3)

fit_sim1 = lme4::lmer(HE ~ SP + time + (1|Group) + (1|id),
                      data = test1)
coefs1 = as.numeric(summary(fit_sim1)[["coefficients"]][,'Estimate'][2])

fit_sim2 = lme4::lmer(HE ~ SP + time + (1|Group) + (1|id),
                      data = test2)
coefs2 = as.numeric(summary(fit_sim2)[["coefficients"]][,'Estimate'][2])
(coefs2 - coefs1)/coefs1

###
fit_sim1 = sem(syntax_sim,
              data = test1,
              estimator = "mlr",
              cluster = 'Group',
              orthogonal = T)

#summary(fit_sim1)
coefs1 = parameterestimates(fit_sim1) %>% filter(label %in% c('b_HESP', 'd_HESP')) 

#plot(as.numeric(test1[89,2:8]))


fit_sim2 = sem(syntax_sim,
              data = test2,
              estimator = "mlr",
              cluster = 'Group',
              orthogonal = T)

#summary(fit_sim2)
coefs2 = parameterestimates(fit_sim2) %>% filter(label %in% c('b_HESP', 'd_HESP')) 

coefs1[1:2,]
coefs2[1:2,]

(coefs2[1:2,'est'] - coefs1[1:2,'est'])/coefs1[1:2,'est']




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
  lst = lst %>%
    rownames_to_column('reg_pars') #%>%
    #left_join(effect_names_df, by = 'reg_pars')
    
})
params_true = do.call(rbind.data.frame, params_true)
table(params_true$reg_pars)
rownames(params_true)
colnames(params_true)[-1] = paste0(colnames(params_true)[-1], '.true')

params_missp_ = lapply(result_missp, function(lst) lapply(lst, summaryParam))
params_missp = flattenList(params_missp_)
params_missp = lapply(params_missp, function(lst){
  lst = lst %>%
    rownames_to_column('reg_pars')# %>%
    #left_join(effect_names_df, by = 'reg_pars')
  
})
params_missp = do.call(rbind.data.frame, params_missp)
colnames(params_missp)[-1] = paste0(colnames(params_missp)[-1], '.missp')
table(rownames(params_missp)==rownames(params_true))

params_all = cbind.data.frame(params_true, params_missp[,-1],
                              bw_grid %>%
                                slice(rep(1:n(),
                                          each=3*length(eta_range)))) %>%
  rownames_to_column('eta_npar') %>%
  separate(eta_npar, into = c("eta", "npar"), sep = "\\.(?=[^.]+$)")
params_all$eta = as.numeric(params_all$eta)
params_all$npar = as.numeric(params_all$npar)
table(params_all$eta)
table(params_all$npar)

params_all %<>% 
  mutate(bias = (`Estimate Average.missp` - `Estimate Average.true`)/`Estimate Average.true`)

## plotting

ggplot(params_all %>%
         filter(reg_pars == 'SP' ),
       aes(as.factor(Between), as.factor(Within), fill = bias)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white'#,
                      # limits = c(-20,20)
  )  +
  facet_wrap(~ eta)+
  theme_ipsum() 
