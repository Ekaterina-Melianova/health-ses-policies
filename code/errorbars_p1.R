

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
p1 = read_excel(paste0(path, '/data/error_bar_p1.xlsx'))

# add significant column: if the confidence interval does not contain zero
p1$significant = ifelse((p1$lower > 0 & p1$upper > 0)| (p1$lower < 0 & p1$upper < 0), TRUE, FALSE)
p1$time = ifelse(p1$time == 'long', 'Long-Run', 'Short-Run')
##
p1$errorbar_linetype <- ifelse(p1$significant, "solid", "dashed")
vec_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
var_names_raw = unique(p1$var)
p1$points_fill = case_when(
  p1$var == var_names_raw[1] & p1$significant==T ~ vec_colors[1],
  p1$var == var_names_raw[2] & p1$significant==T ~ vec_colors[2],
  p1$var == var_names_raw[3] & p1$significant==T ~ vec_colors[3],
  p1$var == var_names_raw[4] & p1$significant==T ~ vec_colors[4],
  p1$var == var_names_raw[5] & p1$significant==T ~ vec_colors[5],
  TRUE ~ 'white'
)
p1$name = factor(p1$name, levels = rev(c('Adult Social Care',
                                         'Children Social Care',
                                         'Healthcare',
                                         'Environment',
                                         'Law & Order',
                                         'Infrastructure')))

setwd(paste0(path, "/output/paper1"))  

p1_plot_long = 
  ggplot(data = p1 %>% filter(time == 'Long-Run'), aes(y = name, x = est, color = var)) +
  
  geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0.3),
                 position = position_dodge(width = 0.5),
                 linetype = as.data.frame(p1 %>% 
                                            filter(time == 'Long-Run')%>% 
                                            dplyr::select(errorbar_linetype))[[1]], size=1.2) +
  
  geom_point(aes(colour=var), fill = as.factor(as.data.frame(p1 %>% 
                                                     filter(time == 'Long-Run')%>% 
                                                     dplyr::select(points_fill))[[1]]),
             pch=21, size=5, stroke = 2,
             position = position_dodge(width = 0.5)) + 
  scale_color_manual(values = c("samhi" = vec_colors[1],
                                "ibesa" = vec_colors[2],
                                "dep" = vec_colors[3],
                                "anti" = vec_colors[4],
                                "hosp" = vec_colors[5]),
                     labels = (nm_out[1:5]),
                    name = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 34),
        axis.text.x = element_text(size = 34, face = "bold"))  +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  labs(y = "", x = '')  +
  theme(legend.text=element_text(size=40),
        legend.box.margin = margin(t = 60, r = 60, b = 60, l = 60),
        legend.direction = "vertical")+ 
  scale_x_continuous(name = ' ', limits = c(-0.16, 0.16))+ 
  scale_y_discrete(labels = rep('',6))


p1_plot_short = 
  ggplot(data = p1%>% filter(time == 'Short-Run'), 
         aes(y = name, x = est, color = var)) +
  
  geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0.3),
                 position = position_dodge(width = 0.5),
                 linetype = as.data.frame(p1 %>% 
                                            filter(time == 'Short-Run')%>% 
                                            dplyr::select(errorbar_linetype))[[1]], size=1.2) +
  
  geom_point(aes(colour=var), fill=as.data.frame(p1 %>% 
                                                   filter(time == 'Short-Run')%>% 
                                                   dplyr::select(points_fill))[[1]],
             pch=21, size=5, stroke = 2,
             
             
             position = position_dodge(width = 0.5)) + 
  scale_color_manual(values = c("samhi" = vec_colors[1],
                                "ibesa" = vec_colors[2],
                                "dep" = vec_colors[3],
                                "anti" = vec_colors[4],
                                "hosp" = vec_colors[5]),
                     labels = (nm_out[1:5]),
                     name = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 34),
        axis.text.x = element_text(size = 34, face = "bold"))  +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  labs(y = "", x = '') +
  theme(legend.text=element_text(size=40),
        legend.box.margin = margin(t = 60, r = 60, b = 60, l = 60),
        legend.direction = "vertical")+ 
  scale_x_continuous(name = ' ', limits = c(-0.16, 0.16))+ 
  scale_y_discrete(labels = rep('',6))


combined_p1 = ggarrange(p1_plot_long,
                        p1_plot_short,
                        ncol = 2,
                        labels = c('Long-Run', 'Short-Run'),
                        nrow = 1,
                        font.label = list(size = 30, position = 'center'),# align ='hv',
                        hjust = -1.5,
                        common.legend = T, legend = "bottom")  

combined_p1 = annotate_figure(combined_p1, 
                              bottom = text_grob("Standard Deviation Change in MH from a 10% Increase in Spending", 
                                                 color = "black",
                                                 size = 30,
                                                 vjust = -0.5,
                                                 face = "bold"))
combined_p1
setwd(paste0(path, "/output/paper1"))  
ggsave("errorbars_p1.jpeg",
       width = 40, height = 50, units = 'cm') 
