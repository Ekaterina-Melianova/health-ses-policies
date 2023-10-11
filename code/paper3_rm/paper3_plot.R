
library(parallel)
library(doParallel)
library(tictoc)
library(simsem)
library(gridExtra)
library(performance)
library(viridis)
library(ggplot2)
library(ggrepel)
library(ggpubr)

load("C:/Users/ru21406/OneDrive - University of Bristol/Desktop/test2/df_-0.8_0.9_100.Rdata")
df = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies/data/df.rds')

spendsim_plot = function(spending,
                         cors = c(-0.8, 0.8), 
                         autocorrs = c(0.9, 0.9),
                         wd = 'C:/Users/ru21406/OneDrive - University of Bristol/Desktop/test2'){
  setwd(wd)
  
  n_cors = length(cors)
  n_autocorrs = length(autocorrs)
  
  # df unscaled
  df_wide_unscaled = df %>% dplyr::select(lsoa11,
                                          !!as.name(spending) := social_care_adult,
                                          year)
  
  list_nm = paste0('df_', cors, '_', autocorrs, '_100.Rdata')
  spendsim_list = vector("list", length = n_cors)
  
  for (i in 1:n_cors){
    load(list_nm[i])
    out = pivot_longer(out,
                       cols = contains('_'),
                       names_to = c(".value", "year"),
                       names_pattern = "(\\w)_(\\d+)")
    
    out$year = as.numeric(out$year)
    out %<>% left_join(df.full[,c('lsoa11', 'year', 'pop_census11')],
                       by = c('lsoa11', 'year'))
    out[, 'S'] = out[, 'S']/out[, 'pop_census11']
    out = df_wide_unscaled %>%
      left_join(out,
                by = c('lsoa11', 'year')) %>%
      filter(LAD21CD.x %in% 'E09000002')
    
    out$year = as.character(pick$year)
    
    spendsim_list[i] = list(out)
    
  }
  
  # df_hlm
  
  df_hlm = df_lv %>% dplyr::select(lsoa11, LAD21CD, starts_with(c(spending, 'HE'))) %>%
    pivot_longer(cols = starts_with(c('HE', spending)),
                 names_to = c(".value", "time"),
                 names_pattern = "(\\w+)(\\d+)") 
  df_hlm$time = as.numeric(df_hlm$time)
  return(spendsim_list)
  
}

spendsim_plot_dat = spendsim_plot(spending = 'as')

## plotting

p1 = ggplot(spendsim_plot_dat[[1]], aes(x = as, y = H)) +
  geom_point(aes(colour = year), size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  geom_smooth(aes(x = as, y = H,
                  colour = year,
                  group = lsoa11),
              method = "lm", se = FALSE,
              color = 'lightblue',
              linewidth = 0.6)+
  theme_minimal() + xlim(200,770)+
theme(legend.position = "none") + 
  xlab('Spending, £ per capita') +
  ylab('SAMHI, Z-scores') + 
  ggtitle('Spending Original')+ 
  theme(axis.text = element_text(size = 24, color = 'darkgrey')) + 
  theme(axis.title = element_text(size = 24, color = 'darkgrey'))  +
  theme(
    legend.key.size = unit(1, "cm"),  
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22)  
  ) +
  theme(plot.title = element_text(color = "black", size = 26, hjust = 0.5))

p2 = ggplot(spendsim_plot_dat[[1]], aes(x = S, y = H)) +
  geom_point(aes(colour = year), size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = 'red') +
  geom_smooth(aes(x = S, y = H,
                  group = lsoa11),
              method = "lm", se = FALSE, 
              color = 'lightblue',
              linewidth = 0.6)  +  
  theme_minimal() + xlim(200,770)+
  theme(legend.position = "none") + 
  xlab('Spending, £ per capita') +
  ylab('SAMHI, Z-scores') + 
  ggtitle('Spending Modified (within-LA correlation = -0.8, autocorr = 0.9)')+ 
  theme(axis.text = element_text(size = 24, color = 'darkgrey')) + 
  theme(axis.title = element_text(size = 24, color = 'darkgrey'))+
  theme(plot.title = element_text(color = "black", size = 26, hjust = 0.5))

p3 = ggplot(spendsim_plot_dat[[2]], aes(x = S, y = H)) +
  geom_point(aes(colour = year), size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = 'red') +
  geom_smooth(aes(x = S, y = H,
                  group = lsoa11),
              method = "lm", se = FALSE, 
              color = 'lightblue',
              linewidth = 0.6)  +  
  theme_minimal() + xlim(200,770)+
  theme(legend.position = "none") + 
  xlab('Spending, £ per capita') +
  ylab('SAMHI, Z-scores') + 
  ggtitle('Spending Modified (within-LA correlation = 0.8, autocorr = 0.9)') +
  theme(axis.text = element_text(size = 24, color = 'darkgrey')) + 
  theme(axis.title = element_text(size = 24, color = 'darkgrey'))+
  theme(plot.title = element_text(color = "black", size = 26, hjust = 0.5))

ggarrange(p1, NULL, p2, NULL, p3,
          ncol = 1, nrow = 5,
          common.legend = T,
          legend = "right",
          font.label = list(size = 30),
          heights = c(1, 0.2, 1, 0.2, 1))
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/simulated_spending.jpeg",
       width = 30, height = 40, units = 'cm') 
