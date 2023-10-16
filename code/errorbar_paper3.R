
errorbar_simsem = function(true = params_true_rcgclm,
                           missp = params_missp_rcgclm,
                           grid = bw_grid_ordered,
                           plot = T,
                           #limits = c(-20, 20),
                           effect_to_plot,
                           bias = bias_rel){
  
  params_all = cbind.data.frame(true, missp[-1],
                                grid %>% 
                                  slice(rep(1:n(),
                                            each = length(table(true[,1])))))
  
  params_all %<>% 
    dplyr::mutate(bias_rel = (`Estimate Average.true` - `Estimate Average.missp`)/`Estimate Average.true`,
                  bias_abs = `Estimate Average.missp` - `Estimate Average.true`,
                  bias_rel2 = `Estimate Average.true`/`Estimate Average.missp`,
                  bias_upper = upper.missp - upper.true,
                  bias_lower = lower.missp - lower.true)
  
  ## plotting
  p = ggplot(params_all %>%
           dplyr::filter(effect_names.true %in% effect_to_plot &
                           !eta == '0'),
         aes(y = bias_rel, x = factor(Within),
             color = factor(Between))) +
    geom_hline(aes(yintercept = 0)) +
    geom_point(aes(#size = factor(eta)
      ), size = 3#,
               #position = position_jitterdodge(dodge.width = 0,
               #                                jitter.height = 1,
                #                               jitter.width = 1)
               ) +
    scale_y_continuous(name = 'Bias'#,
                       #limits = limits
                       ) +
   # scale_size_manual(values = c("0.3" = 1, "0.6" = 2, "0.9" = 3)) +
    theme_bw()  +
    facet_wrap(~ eta)
    
    
    if (plot){
      return(p)
      }else{
        return(params_all)
      }
    }

errorbar_simsem(true = params_true_rcgclm,
                missp = params_missp_rcgclm,
                effect_to_plot = c(#'short_cross_SP',
                                   'long_cross_SP')#,
                #limits = c(-15, 15)
                )
errorbar_simsem(true = params_true_rcgclm,
                missp = params_missp_rcgclm,
                effect_to_plot = c('short_cross_SP'#,
                  #'long_cross_SP'
                  )#,
                #limits = c(-15, 15)
)
errorbar_simsem(true = params_true_hlm,
                missp = params_missp_hlm,
                effect_to_plot = 'SP'#,
                #limits = c(-15, 15)
                )
ggarrange(p1, p2,
          labels = c('     rcgclm',
                     '     hlm'),
          ncol = 2, nrow = 1,
          font.label = list(size = 15), align ='hv', common.legend  = T,
          legend  = 'bottom')


