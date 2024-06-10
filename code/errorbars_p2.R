

# load xlsx file
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)

## setting directory
USERNAME = Sys.getenv("USERNAME")
DIR = '/YandexDisk/PhD Research/health-ses-policies2'
path = paste0('C:/Users/', USERNAME, DIR)
setwd(paste0(path, '/data'))
source(paste0(path, '/code/functions.R'))
p2 = read_excel(paste0(path, '/data/error_bar_p2.xlsx'))

#wide format to long
p2_val = gather(p2, cat, est, top, bottom, -var, -name,
                -time, -top_lower, -top_upper, -bottom_lower, -bottom_upper)%>%
  dplyr::select(name, time, cat, est, var)
p2_upper = gather(p2, cat, upper, -top, -bottom, -name, -var,
                  -time, -top_lower, top_upper, -bottom_lower, bottom_upper) %>%
  dplyr::select(name, time, cat, upper, var)
p2_upper$cat = p2_val$cat
p2_lower = gather(p2, cat, lower, -top, -bottom, -name, -var,
                  -time, top_lower, -top_upper, bottom_lower, -bottom_upper)%>%
  dplyr::select(name, time, cat, lower,var)
p2_lower$cat = p2_val$cat
p2 = left_join(p2_val, p2_upper, by = c("name", "time", "cat", 'var')) %>%
  left_join( p2_lower, by = c("name", "time", "cat", 'var'))

# add significant column: if the confidence interval does not contain zero
p2$significant = ifelse((p2$lower > 0 & p2$upper > 0)| (p2$lower < 0 & p2$upper < 0), TRUE, FALSE)
p2$time = ifelse(p2$time == 'long', 'Long-Run', 'Short-Run')
##
p2$errorbar_linetype <- ifelse(p2$significant, "solid", "dashed")
vec_colors <- c( "aquamarine3", "lightcoral", "deepskyblue3")
p2$points_fill <- ifelse(p2$significant==F & p2$cat == 'bottom', 'white',
                         ifelse(p2$significant & p2$cat == 'bottom', vec_colors[2],
                                ifelse(p2$significant==F & p2$cat == 'top', 'white',vec_colors[1])))
p2$name = factor(p2$name, levels = rev(c('Adult Social Care',
                                     'Children Social Care',
                                     'Healthcare',
                                     'Environment',
                                     'Law and Order',
                                     'Infrastructure')))
#reverse a vector


vars = c('samhi',
         'dep',
         'hosp'
)
setwd(paste0(path, "/output/paper2"))  

list_plots_long = list()
list_plots_short = list()
for (i in 1:3){
  list_plots_long[[i]] = 
    ggplot(data = p2 %>% filter(time == 'Long-Run' & var == vars[i]), aes(y = name, x = est, color = cat)) +
    
    geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0.3),
                   position = position_dodge(width = 0.5),
                   linetype = as.data.frame(p2 %>% 
                                              filter(time == 'Long-Run'& var == vars[i])%>% 
                                              dplyr::select(errorbar_linetype))[[1]], size=1.2) +
    
    geom_point(aes(colour=cat), fill = as.data.frame(p2 %>% 
                                                       filter(time == 'Long-Run'& var == vars[i])%>% 
                                                       dplyr::select(points_fill))[[1]],
               pch=21, size=5, stroke = 2,
               position = position_dodge(width = 0.5)) + 
    scale_color_manual(values = c("top" = vec_colors[1], "bottom" = vec_colors[2]),
                       labels = rev(c("Top 50%", "Bottom 50%")),
                       name = "") +
    theme_minimal() +
    theme(axis.text.y = element_text(color = "black", size = 34),
          axis.text.x = element_text(size = 34, face = "bold"))  +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
    labs(y = "", x = '')  +
    theme(legend.text=element_text(size=40),
          legend.box.margin = margin(t = 60, r = 60, b = 60, l = 60))+ 
    scale_x_continuous(name = ' ', limits = c(-0.16, 0.16))+ 
    scale_y_discrete(labels = c('', '', '', '', '', ''))
  
  
  list_plots_short[[i]] = 
    ggplot(data = p2 %>% filter(time == 'Short-Run' & var == vars[i]), aes(y = name, x = est, color = cat)) +
    
    geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0.3),
                   position = position_dodge(width = 0.5),
                   linetype = as.data.frame(p2 %>% 
                                              filter(time == 'Short-Run'& var == vars[i])%>% 
                                              dplyr::select(errorbar_linetype))[[1]], size=1.2) +
    
    geom_point(aes(colour=cat), fill = as.data.frame(p2 %>% 
                                                       filter(time == 'Short-Run'& var == vars[i])%>% 
                                                       dplyr::select(points_fill))[[1]],
               pch=21, size=5, stroke = 2,
               position = position_dodge(width = 0.5)) + 
    scale_color_manual(values = c("top" = vec_colors[1], "bottom" = vec_colors[2]),
                       labels = rev(c("Top 50%", "Bottom 50%")),
                       name = "") +
    theme_minimal() +
    theme(axis.text.y = element_text(color = "black", size = 34),
          axis.text.x = element_text(size = 34, face = "bold"))  +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
    labs(y = "", x = '')  +
    theme(legend.text=element_text(size=40),
          legend.box.margin = margin(t = 60, r = 60, b = 60, l = 60))+ 
    scale_x_continuous(name = ' ', limits = c(-0.16, 0.16))+ 
    scale_y_discrete(labels = c('', '', '', '', '', ''))
}



samhi_combined_p2 = ggarrange(list_plots_long[[1]],
                              list_plots_short[[1]],
                              ncol = 2,
                              labels = c('Long-Run', 'Short-Run'),
                              nrow = 1,
                              font.label = list(size = 30, position = 'center'),# align ='hv',
                              hjust = -1.5,
                              common.legend = T, legend = "bottom")  

samhi_combined_p2 = annotate_figure(samhi_combined_p2, 
                                    bottom = text_grob("Standard Deviation Change in SAMHI from a 10% Increase in Spending", 
                                                       color = "black",
                                                       size = 25,
                                                       vjust = -0.5,
                                                       face = "bold"))

ggsave("samhi_errorbars_p2.jpeg",
       width = 40, height = 35, units = 'cm') 


dep_combined_p2 = ggarrange(list_plots_long[[2]],
                              list_plots_short[[2]],
                              ncol = 2,
                              labels = c('Long-Run', 'Short-Run'),
                              nrow = 1,
                              font.label = list(size = 30, position = 'center'),# align ='hv',
                              hjust = -1.5,
                              common.legend = T, legend = "bottom")  

dep_combined_p2 = annotate_figure(dep_combined_p2, 
                                    bottom = text_grob("Standard Deviation Change in Depression Prevalence from a 10% Increase in Spending", 
                                                       color = "black",
                                                       size = 25,
                                                       vjust = -0.5,
                                                       face = "bold"))

ggsave("dep_errorbars_p2.jpeg",
       width = 40, height = 35, units = 'cm') 


hosp_combined_p2 = ggarrange(list_plots_long[[3]],
                              list_plots_short[[3]],
                              ncol = 2,
                              labels = c('Long-Run', 'Short-Run'),
                              nrow = 1,
                              font.label = list(size = 30, position = 'center'),# align ='hv',
                              hjust = -1.5,
                              common.legend = T, legend = "bottom")  

hosp_combined_p2 = annotate_figure(hosp_combined_p2, 
                                    bottom = text_grob("Standard Deviation Change in MH Hospitalisations from a 10% Increase in Spending", 
                                                       color = "black",
                                                       size = 25,
                                                       vjust = -0.5,
                                                       face = "bold"))

ggsave("hosp_errorbars_p2.jpeg",
       width = 40, height = 35, units = 'cm') 

