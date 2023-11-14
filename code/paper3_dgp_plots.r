# libraries
library(readxl)
library(plyr); library(dplyr)
library(magrittr)
library(glue)
library(tidyverse)
library(lavaan)
library(data.table)
library(tidyr)
library(broom)

library(parallel)
library(doParallel)
library(tictoc)
library(simsem)
library(foreach)
library(doSNOW)

library(gridExtra)
library(cowplot)
library(hrbrthemes)
library(RColorBrewer)

library(ggpubr)
library(tidySEM)


par_tab_rcgclm_long = data.frame(
  label = c('b_YY', 'b_YX', 'b_XY', 'b_XX',
            'cov_iY.iX', 'cov_iY.sY', 'cov_iY.sX', 'cov_iX.sY', 'cov_iX.sX', 'cov_sY.sX',
            'd_YY', 'd_YX', 'd_XY', 'd_XX',
            'ecov_YX', 'evarY', 'evarX',
            'mean_i_Y', 'mean_i_X', 'mean_s_Y', 'mean_s_X',
            'var_iY', 'var_iX', 'var_sY', 'var_sX'),
  par = c(0.03, 'k', '(-0.06)', '(-0.2)', 
          '(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.25)', '(-0.004)', '(-0.06)', '(-0.2)',
          0.002, 'evarY', 'evarX',
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iY', 'var_iX', 'var_sY', 'var_sX')
)

par_tab_rcgclm_short = data.frame(
  label = c('b_YY', 'b_YX', 'b_XY', 'b_XX',
            'cov_iY.iX', 'cov_iY.sY', 'cov_iY.sX', 'cov_iX.sY', 'cov_iX.sX', 'cov_sY.sX',
            'd_YY', 'd_YX', 'd_XY', 'd_XX',
            'ecov_YX', 'evarY', 'evarX',
            'mean_i_Y', 'mean_i_X', 'mean_s_Y', 'mean_s_X',
            'var_iY', 'var_iX', 'var_sY', 'var_sX'),
  par = c(0.03, 0.006, '(-0.06)', '(-0.2)', 
          '(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.25)', 'k', '(-0.06)', '(-0.2)',
          0.002, 'evarY', 'evarX',
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iY', 'var_iX', 'var_sY', 'var_sX')
)

par_tab_rcclpm = data.frame(
  label = c('cov_iY.iX', 'cov_iY.sY', 'cov_iY.sX', 'cov_iX.sY', 'cov_iX.sX', 'cov_sY.sX', 
            'd_YY', 'd_YX', 'd_XY', 'd_XX',
            'ecov_YX', 'evarY', 'evarX',
            'mean_i_Y', 'mean_i_X', 'mean_s_Y', 'mean_s_X', 
            'var_iY', 'var_iX', 'var_sY', 'var_sX'),
  par = c('(-0.005)', 0.003, '-(0.015)', '(-0.005)', 0.002, '(-0.002)',
          '(0.5)', 'k', '(0.06)', '(0.2)',
          0.003, 'evarY', 'evarX', 
          '(0.4)', '(-0.1)', '(-0.2)', '(0.02)',
          'var_iY', 'var_iX', 'var_sY', 'var_sX')
)

par_tab_lm = data.frame(
  label = c('b_YX', 'varY', 'meanY', 'meanX', 'varX'),
  par = c('k', 'varY', 0.4, '(-0.07)', 'varX')
)

par_tab_growth = data.frame(
  label = c('b_YX', 'b_YX_2',
            'cov_iY.sY',
            'var_iY', 'var_sY', 'varY',
            'mean_i_Y', 'mean_s_Y',
            'varX'),
  par = c('k', 0.01, 
          0.03,
          'var_iY', 'var_sY', 'varY',
          0.4, -0.2,
          'varX')
)



source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_lv.RDS')
df_lv %<>% dplyr::rename(!!!setNames(c('LAD21CD', paste0("as", 1:7), paste0("HE", 1:7)),
                              c('ID', paste0("X", 1:7), paste0("Y", 1:7)))) %>%
  dplyr::select('ID', starts_with('X'), starts_with('Y'))

layout_cross = get_layout(
  "iX", "", "",  "sX", "", "",      "", "", "", "", "","","",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  
  "", "", "", "", "", "", "X1", "", "",     "X2", "", "",      "X3",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "e_X1", "", "",   "e_X2", "", "",    "e_X3",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  
  
  "", "", "", "", "", "", "e_Y1", "", "",   "e_Y2", "", "",    "e_Y3",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",

  "", "", "", "", "", "", "Y1", "", "",     "Y2", "", "",      "Y3",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "", 
  
  "iY", "", "", "sY", "", "", "", "",       "", "", "", "", "", 
  
  rows = 25)

layout_growth_lm = get_layout( 
    
  "X1", "", "Y1",     "Y2",      "Y3",
 
  "", "", "", "", "",
  "", "", "", "", "",
  "", "", "", "", "",
  "", "", "", "", "",
  
  "iY", "sY", "","", "",
  
  rows = 6)


# fitted models

m_est_lm = '
Y1 ~ meanY*1 + b_YX*X1
#X1 ~ 0*1

Y1 ~~ varY*Y1
X1 ~~ varX*X1

'
fit_lm = growth(m_est_lm,
                estimator = 'mlr',
                orthogonal = T,
                data = df_lv,
                cluster = 'ID')

m_est_growth = '
iY =~ 1*Y1 + 1*Y2 + 1*Y3 
sY =~ 0*Y1 + 1*Y2 + 2*Y3

iY ~ b_YX*X1
sY ~ b_YX_2*X1

iY ~~ cov_iY.sY*sY
iY ~~ var_iY*iY
sY ~~ var_sY*sY

iY ~ mean_i_Y*1
sY ~ mean_s_Y*1

Y1 ~~ varY*Y1
Y2 ~~ varY*Y2
Y3 ~~ varY*Y3

X1 ~~ varX*X1

Y1 + Y2 + Y3 ~ 0*1

'
fit_growth = growth(m_est_growth,
                    estimator = 'mlr',
                    orthogonal = T,
                    data = df_lv,
                    cluster = 'ID')
summary(fit_growth)
m_est_rcclpm = RC_GCLM_syntax(endogeneous = c('Y', 'X'),
                              control = NULL,
                              model = 'reclpm',
                              max_time = 3)
fit_rcclpm = sem(m_est_rcclpm,
          estimator = 'mlr',
          orthogonal = T,
          data = df_lv,
          cluster = 'ID', meanstructure = T)

m_est_rcgclm = RC_GCLM_syntax(endogeneous = c('Y', 'X'),
                              control = NULL,
                              model = 'regclm',
                              max_time = 3)
fit_rcgclm = sem(m_est_rcgclm,
          estimator = 'mlr',
          orthogonal = T,
          data = df_lv,
          cluster = 'ID')




dgp_plot = function(fit,
                    par_tab,
                    layout_spec,
                    evarX,
                    var_iX,
                    var_sX,
                    varX,
                    evarY,
                    var_iY,
                    var_sY,
                    varY,
                    method){

    edges <- get_edges(fit) %>% 
    filter(!(lavaan_label == '' & !op %in% '=~')) %>%
    dplyr::rename(label_old = label) %>% 
    left_join(par_tab, by = c("lavaan_label" = "label")) %>%
    dplyr::mutate(par = case_when(
      par %in% "var_iX" ~ var_iX,
      par %in% "var_iY" ~ var_iY,
      par %in% "var_sX" ~ var_sX,
      par %in% "var_sY" ~ var_sY,
      par %in% "evarY" ~ evarY,
      par %in% "evarX" ~ evarX,
      par %in% "varY" ~ varY,
      par %in% "varX" ~ varX,
      is.na(par) ~ label_old,
      TRUE ~ par
    )) %>%
    mutate(label = case_when(
      par == lavaan_label ~ par,
      par == 'NA' ~ label_old,
      op == '=~' ~ substr(par, 1, 3),
      TRUE ~ gsub('\\(|\\)', '', par)
    ))  %>%
    mutate(connect_from =  "top", connect_to = "bottom")
  
  edges$label_location = ifelse(sub('[0-9]+', '', edges$from) == sub('[0-9]+', '', edges$to) & edges$op == '~~', 0.7,
   ifelse(edges$from %in% c('e_X1', 'e_X2', 'e_Y1', 'e_Y2', 'X1', 'X2', 'Y1', 'Y2') & 
            !sub('[0-9]+', '', edges$from) == sub('[0-9]+', '', edges$to) & edges$op == '~', 0.64, 
          ifelse(edges$from %in% c('iX', 'iY') & edges$op == '=~', 0.7, 
                 ifelse(edges$from %in% c('sX', 'sY') & edges$op == '=~', 0.7, 0.5))))
  edges$show = ifelse(edges$from %in% c('e_X2', 'e_X3', 'e_Y2', 'e_Y3') & edges$op =='~~',FALSE, TRUE)
  edges$curvature = ifelse(!is.na(edges$curvature), 60, edges$curvature)
  edges$curvature = ifelse(edges$from %in% c('iY') & edges$to %in% c('sY'), 110, edges$curvature)
  edges$size = 0.4

  nodes = get_nodes(fit) %>%
    left_join(par_tab, by = c("lavaan_label" = "label")) %>%
    mutate(label = ifelse(is.na(par), name, paste0(name, '\n', '[', gsub('\\(|\\)', '', par), ']' )))
  edges$color = ifelse(edges$from %in% c('iX', 'iY', 'sX', 'sY') & edges$op == '=~', 'darkgrey', 'black')

  #'\u03b4'
  edges$label = ifelse(edges$label == 'k' & method %in% c('lm', 'growth', 'rcclpm'), "\u03B2",
   ifelse(edges$label == 'k' & method %in% 'rcgclm', paste0("\u03B2", 1), 
   ifelse(edges$lavaan_label == 'd_YX' &  method %in% 'rcgclm',  paste0("\u03B2", 2), edges$label)))

   
  edges$color = ifelse(edges$label %in% c("\u03B2",  paste0("\u03B2", 1),  paste0("\u03B2", 2)), 'darkred', edges$color)
  edges$label_colour = ifelse(edges$label %in% c("\u03B2",  paste0("\u03B2", 1),  paste0("\u03B2", 2)), 'darkred', 'black')
  edges$label_fontface  = ifelse(edges$label %in% c("\u03B2",  paste0("\u03B2", 1),  paste0("\u03B2", 2)), 'bold', 'plain')

  nodes$label = ifelse(grepl('i', nodes$label), 
                     gsub('i', 'i', nodes$label),
                     ifelse(grepl('s', nodes$label), 
                            gsub('s', 's', nodes$label),
                            ifelse(grepl('e_', nodes$label), 
                                   gsub('e_', '\u03B5_', nodes$label), 
                                   nodes$label)))
  nodes$label_fill = NA

  if (method %in% c('lm', 'growth')){
    edges$label_location = ifelse(edges$from %in% c('iX', 'iY', 'sX', 'sY') & edges$op == '=~', 0.7, 0.5)
    graph_sem(edges = edges, 
            nodes = nodes,
            variance_diameter = 2, 
            spacing_y = 5,
            text_size = 3,
            spacing_x = 8,
            rect_height = 3,
            ellipses_height = 3,
            rect_width = 3,
            ellipses_width = 3,
            angle = 180,
            fix_coord = TRUE,
            layout = layout_spec)
  } else{
        graph_sem(edges = edges, 
            nodes = nodes,
            variance_diameter = 5, 
            spacing_y = 5,
            text_size = 3,
            spacing_x = 8,
            rect_height = 7,
            ellipses_height = 8.5,
            rect_width = 7,
            ellipses_width = 11,
            angle = 180,
            fix_coord = TRUE,
            layout = layout_spec)
  }
  
}


dgp_lm = dgp_plot(fit_lm,
         par_tab = par_tab_lm,
         layout_spec = layout_growth_lm,
                         evarX = NA,
                         var_iX = NA,
                         var_sX = NA,
                         varX = '0.8',
                         evarY = NA,
                         var_iY = NA,
                         var_sY = NA,
                         varY = '0.6',
                             method = 'lm')

dgp_growth = dgp_plot(fit_growth,
         par_tab = par_tab_growth,
         layout_spec = layout_growth_lm,
                             evarX = NA,
                             var_iX = NA,
                             var_sX = NA,
                             varX = '0.8',
                             evarY = NA,
                             var_iY = '0.6',
                             var_sY = '0.07',
                             varY = '0.02',
                             method = 'growth')
dgp_rcclpm = dgp_plot(fit=fit_rcclpm,
         par_tab = par_tab_rcclpm,
         layout_spec = layout_cross,
                             evarX = '0.05',
                             var_iX = '0.4',
                             var_sX = '0.05',
                             varX = NA,
                             evarY = '0.05',
                             var_iY = '0.6',
                             var_sY = '0.05',
                             varY = NA,
                             method = 'rcclpm')                         

dgp_rcgclm = dgp_plot(fit_rcgclm,
         par_tab = par_tab_rcgclm_long,
         layout_spec = layout_cross,
                                  evarX = '0.05',
                                  var_iX = '0.4',
                                  var_sX = '0.05',
                                  varX = NA,
                                  evarY = '0.05',
                                  var_iY = '0.6',
                                  var_sY = '0.05',
                                  varY = NA,
                             method = 'rcgclm') 


ggarrange(dgp_lm, dgp_growth,
          labels = c('Linear Regression',
                     'Growth Curve'),
          ncol = 2, nrow = 1)
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper3/dgp_plot_lm_growth.svg",
       width = 40, height = 18, units = 'cm')

ggarrange(dgp_rcclpm, dgp_rcgclm,
          labels = c('RC-CLPM',
                     'RC-GCLM'),
          ncol = 2, nrow = 1) 
ggsave("C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper3/dgp_plot_cross.svg",
       width = 40, height = 18, units = 'cm')

gc()
