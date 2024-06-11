

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
p3 = read_excel(paste0(path, '/data/error_bar_p3.xlsx'))

#wide format to long
p3_val = gather(p3, cat, est, top, bottom, -name,
                -time, -top_lower, -top_upper, -bottom_lower, -bottom_upper)%>%
  dplyr::select(name, time, cat, est)
p3_upper = gather(p3, cat, upper, -top, -bottom, -name,
                -time, -top_lower, top_upper, -bottom_lower, bottom_upper) %>%
  dplyr::select(name, time, cat, upper)
p3_upper$cat = p3_val$cat
p3_lower = gather(p3, cat, lower, -top, -bottom, -name,
                  -time, top_lower, -top_upper, bottom_lower, -bottom_upper)%>%
  dplyr::select(name, time, cat, lower)
p3_lower$cat = p3_val$cat
p3 = left_join(p3_val, p3_upper, by = c("name", "time", "cat")) %>%
  left_join( p3_lower, by = c("name", "time", "cat"))

# add significant column: if the confidence interval does not contain zero
p3$significant = ifelse((p3$lower > 0 & p3$upper > 0)| (p3$lower < 0 & p3$upper < 0), TRUE, FALSE)
p3$time = ifelse(p3$time == 'long', 'Long-Run', 'Short-Run')
##
p3$errorbar_linetype <- ifelse(p3$significant, "solid", "dashed")
vec_colors <- c( "#4A90E2", "lightcoral")
p3$points_fill <- ifelse(p3$significant==F & p3$cat == 'bottom', 'white',
                      ifelse(p3$significant & p3$cat == 'bottom', vec_colors[2],
                             ifelse(p3$significant==F & p3$cat == 'top', 'white',vec_colors[1])))
p3$name = factor(p3$name, levels = c("Safeguarding",
                                     "Preventative",
                                     "Children Looked After"))
p3_plot_long = 
  ggplot(data = p3 %>% filter(time == 'Long-Run'), aes(y = name, x = est, color = cat)) +
  
  geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0.3),
                 position = position_dodge(width = 0.5),
                 linetype = as.data.frame(p3 %>% 
                                            filter(time == 'Long-Run')%>% 
                                            dplyr::select(errorbar_linetype))[[1]], size=1.2) +
  
  geom_point(aes(colour=cat), fill = as.data.frame(p3 %>% 
                                                     filter(time == 'Long-Run')%>% 
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
  scale_y_discrete(labels = c('', '', ''))

  
p3_plot_short = 
  ggplot(data = p3%>% filter(time == 'Short-Run'), 
         aes(y = name, x = est, color = cat)) +
  
  geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0.3),
                 position = position_dodge(width = 0.5),
                 linetype = as.data.frame(p3 %>% 
                                            filter(time == 'Short-Run')%>% 
                                            dplyr::select(errorbar_linetype))[[1]], size=1.2) +
  
  geom_point(aes(colour=cat), fill=as.data.frame(p3 %>% 
                                                   filter(time == 'Short-Run')%>% 
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
  labs(y = "", x = '') +
  theme(legend.text=element_text(size=40),
        legend.box.margin = margin(t = 60, r = 60, b = 60, l = 60))+ 
  scale_x_continuous(name = ' ', limits = c(-0.16, 0.16))+ 
  scale_y_discrete(labels = c('', '', ''))
  

combined_p3 = ggarrange(p3_plot_long,
                           p3_plot_short,
                           ncol = 2,
                           labels = c('Long-Run', 'Short-Run'),
                           nrow = 1,
                           font.label = list(size = 30, position = 'center'),# align ='hv',
                           hjust = -2.5,
                           common.legend = T, legend = "bottom")  

combined_p3 = annotate_figure(combined_p3, 
                                  bottom = text_grob("Standard Deviation Change in MH Hospitalisation Rate from a 10% Increase in Spending", 
                                                     color = "black",
                                                     size = 30,
                                                     vjust = -0.5,
                                                     face = "bold"))

setwd(paste0(path, "/output/paper3"))  
ggsave("errorbars_p3.jpeg",
       width = 50, height = 30, units = 'cm') 

