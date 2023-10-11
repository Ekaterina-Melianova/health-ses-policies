councils = read.csv('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/opencouncildata_councils.csv')
councils %<>% select(name, ons.code, majority, coalition)
#party = read.csv("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/history1973-2023.csv")
#party %<>% filter(Year == 2015)
#party_ons = party %<>% left_join(councils, by = c('Council' = 'name'))
table(councils$coalition)

df_plot = df_before_scaling %>% select(year,
                                       lsoa11,
                                       LAD21CD,
                                       lsoa_dep_1,
                                       lsoa_dep_2,
                                       lsoa_dep_3,
                                       all_of(all_vars)) %>%
  left_join(councils, by = c('LAD21CD'='ons.code')) %>%
  filter(!is.na(majority))
table(df_plot$majority)
table(df_plot[is.na(df_plot$majority), 'LAD21CD' ])
df_plot$maj = ifelse(df_plot$majority == 'LAB', 1, 0)




# Z-scores for health domains
for (i in c("antidep_rate", "est_qof_dep", "prop_ibesa")){
  df_plot[, i] = scale(df_plot[, i])
}


panel_df = df_plot %>% 
  group_by(year, majority) %>%
  dplyr::summarise(across(all_of(all_vars), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = samhi_index:infrastructure) %>%
  ungroup()

panel_df$name = factor(panel_df$name,
                       levels = vars_original_names,
                       labels = nm_out[1:11])

panel_df %>%
  filter(name %in% nm_out[1:5]) %>%
  ggplot(aes(year, value, colour = factor(majority))) +
  scale_x_continuous(name = NULL, 
                     breaks = 2013:2019)+ 
  scale_y_continuous(name = 'Z-Standardised Scores', limits = c(-1.1, 1.1)) + 
  theme(axis.text = element_text(size = 24))  +
  geom_smooth(method = loess,
              se = T,
              formula = y ~ x,
              level = 0.9,
              fill = 'lightgrey') +
  facet_wrap(~ name, scales = 'free') +
  theme_pubclean()+ 
  theme(axis.title.x = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = 'bottom') 
