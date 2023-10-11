
library(tidyr)
library(ggplot2)
library(dplyr)


df_errorbar.full <- rio::import("C:/Users/ru21406/YandexDisk/PhD Research/Presentations/tab_to_erbar.xlsx")


for (var in unique(df_errorbar.full$Var)){
  
  df_errorbar <- df_errorbar.full %>% filter(Var==var)
  
  df_errorbar <- df_errorbar %>%
    pivot_longer(
      cols = c('AdSocCare','ChSocCare','Healthcare','Environment','Law&Order','Infrastructure'),
      names_to = "policy",
      values_to = "value"
    )
  
  df_errorbar <- df_errorbar %>%
    pivot_wider(
      names_from = Param,
      values_from = value
      
    )
  
  df_errorbar$significant <- sign(df_errorbar$Lower) == sign(df_errorbar$Upper)
  errorbar_linetype <- ifelse(df_errorbar$significant, "solid", "dashed")
  
  blue_color = '#377EB8'
  red_color = '#E41A1C'
  green_color = '#4DAF4A'
  
  df_errorbar$policy <- factor(df_errorbar$policy,
                               levels = c("Infrastructure", "Environment", "Law&Order",
                                          "ChSocCare", "AdSocCare", "Healthcare"))
  df_errorbar$policy <- recode("ChSocCare"="Children Social Care",
                               "AdSocCare"="Adult Social Care",
                               "Law&Order"="Law & Order",
                               df_errorbar$policy)
  
  df_errorbar$Dynamic <- factor(df_errorbar$Dynamic,
                                levels = c("Long-run", "Short-run"))
  
  lim.x <- 0.15
  # lim.x <- max(abs(c(df_errorbar$Upper, df_errorbar$Lower)))
  
  print(ggplot(data = df_errorbar, aes(y = policy, x = Est, color = Dynamic)) +
          
          geom_errorbarh(aes(xmin = Lower, xmax = Upper, height = 0.3),
                         position = position_dodge(width = 0.5),
                         linetype=errorbar_linetype, size=0.75
                         # linetype='solid', size=0.75
          ) +
          geom_point(aes(colour=Dynamic), size=2.5,
                     position = position_dodge(width = 0.5)) +
          xlim(-lim.x, lim.x) +
          scale_color_manual(values = c(
            "Short-run" = green_color,
            "Long-run" = blue_color),
            name = "") +
          theme_minimal() +
          theme(axis.text.y = element_text(color = "black", size = 18),
                axis.text.x = element_text(color = "black", size = 14),
                axis.title.x = element_text(size = 18, face = "bold"),
                axis.title.y = element_text(size = 18, face = "bold"))  +
          geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
          labs(y = "", x = '', title = NULL)  +
          theme(#plot.title = element_text(size = 20,
                #                          face = "bold", hjust=0.5),
                legend.position= c(0.8, 0.2),
                # legend.position= 'bottom',
                legend.direction ="vertical",
                legend.text=element_text(size=18)) +
          #ggtitle(df_errorbar$Var[1]) +
          # guides(colour = guide_legend(nrow = 1, reverse = TRUE))
          guides(colour = guide_legend(reverse = TRUE))
        
  )
  
  ggsave(paste0('er_', var, '.png'), width = 3000, height = 2000, units = 'px')
  
}
