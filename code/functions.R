
# normalise

normalize = function(x, na.rm = T)
  return((x - min(x)) /(max(x)- min(x)))

policy_names_6 = c('health',
                   'healthcare',
                   'education',
                   'env',
                   'law_order',
                   'infrastructure')

control_names = c('lsoa_ses_score',
                  'pop_census11',
                  'nonwhite',
                  'females',
                  'older',
                  'n',
                  
                  'rural')
health_vars = c('samhi_index',
                'z_mh_rate',
                'antidep_rate',
                'est_qof_dep',
                'prop_ibesa')

lad_inc_vars = paste0(policy_names_6[-2], '_inc')

endogeneous = c('HE', 'he', 'hc', 'ed', 'en', 'lo', 'ir')

measures = c('npar',
             'chisq.scaled',
             'df.scaled',
             'cfi.scaled',
             'tli.scaled',
             'srmr',
             'rmsea.scaled',
             'rmsea.ci.lower.scaled',
             'rmsea.ci.upper.scaled',
             'agfi',
             'aic',
             'bic',
             'logl')

nm_out = c('', 'Mental Health Index',
           'Incapacity Benefits Rate',
           'Depression Rate',
           'Antidepressants Rate',
           'Hospital Attendances Score',
           'Public Health & Social Care',
           'Healthcare',
           'Education',
           'Environment',
           'Law and Order',
           'Infrastructure',
           'IMD (inc., educ., empl. domains)',
           'LSOA population size',
           'Non-white, LSOA prop.',
           'Females, LSOA prop.',
           'Older, LSOA prop.',
           'Rural, prop. of rural LSOAs',
           'Number of LSOAs in a LAD')

# Random Effects GCLM lavaan syntax

RC_GCLM_syntax = function(endogeneous = c('HE', 'he', 'hc', 'ed', 'en', 'lo', 'ir'),
                          reverse = c('he', 'hc'),
                          full = F,
                          control = control_names,
                          max_time = 7,
                          impulses = T,
                          past_states = T,
                          cross = T,
                          multiple = F,
                          resid_stationary = T,
                          group_equality = NULL,
                          restricted_pars = NULL,
                          model = 'gclm'){
  require(dplyr)
  n_var = length(endogeneous)
  
  Intercept = map(endogeneous, ~ glue("1*{.x}{1:max_time}") %>%
                    glue_collapse(" + ") %>%
                    glue("i{.x} =~ ", .)) %>%
    glue_collapse("\n")
  Slope = map(endogeneous, ~ glue("{(1:max_time-1)}*{.x}{1:max_time}") %>%
                glue_collapse(" + ") %>%
                glue("s{.x} =~ ", .)) %>%
    glue_collapse("\n")
  
  Intercept_Var = map(endogeneous, ~  glue("i{.x} ~~ i{.x}")) %>%
    glue_collapse("\n")
  Slope_Var = map(endogeneous, ~  glue("s{.x} ~~ s{.x}")) %>%
    glue_collapse("\n")
  Intercept_Mean = map(endogeneous, ~  glue("i{.x} ~ 1")) %>%
    glue_collapse("\n")
  Slope_Mean = map(endogeneous, ~  glue("s{.x} ~ 1")) %>%
    glue_collapse("\n")
  
  #Intercept_Var = map(endogeneous, ~  glue("i{.x} ~~ vari{.x}*i{.x}")) %>%
  #  glue_collapse("\n")
  #Slope_Var = map(endogeneous, ~  glue("s{.x} ~~ vars{.x}*s{.x}")) %>%
  #  glue_collapse("\n")
  #Intercept_Mean = map(endogeneous, ~  glue("i{.x} ~ meani{.x}*1")) %>%
  #  glue_collapse("\n")
  #Slope_Mean = map(endogeneous, ~  glue("s{.x} ~ means{.x}*1")) %>%
  #  glue_collapse("\n")
  
  A = map(endogeneous, ~ glue("{.x}{1:max_time}") %>%
            glue_collapse(" + ") %>%
            glue(., " ~ 0*1")) %>%
    glue_collapse("\n")
  
  B = map(endogeneous, ~ glue("e_{.x}{1:max_time}") %>%
            glue_collapse(" + ") %>%
            glue(., " ~ 0*1")) %>%
    glue_collapse("\n")
  
  
  Intercept_Slope_Cov = map(endogeneous, ~  glue("i{.x} ~~ s{.x}")) %>%
    glue_collapse("\n")
  Slope_Intercept_Cov = map(endogeneous, ~  glue("s{.x} ~~ i{.x}")) %>%
    glue_collapse("\n")
  
# covar

  if(n_var > 2){
    iscov = expand_grid(x = endogeneous,
                         y = endogeneous) %>%
      filter(!x == y) %>%
      mutate(n = rep(1:(n_var-1), n_var)) %>%
      mutate(seq = rep(0:(n_var-1), each = n_var-1)) %>%
      filter(!seq >= n) %>%
      dplyr::select(x, y) %>%
      dplyr::mutate(cov_ii = glue('i{x} ~~ i{y}'),
                  cov_ss = glue('s{x} ~~ s{y}'),
                  cov_is = glue('i{x} ~~ s{y}'),
                  cov_si = glue('s{x} ~~ i{y}'))
  Cov_ii = iscov %>%
    pull(cov_ii) %>%
    glue_collapse("\n")
  Cov_ss = iscov %>%
    pull(cov_ss) %>%
    glue_collapse("\n")
  Cov_is = iscov %>%
    pull(cov_is) %>%
    glue_collapse("\n")
  Cov_si = iscov %>%
    pull(cov_si) %>%
    glue_collapse("\n")
  } else {
    Cov_ss = ''
    Cov_ii = ''
    Cov_is = ''
    Cov_si = ''
  }
  
  Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} =~ 1*{.x}{1:max_time}")%>%
                glue_collapse("\n")) %>%
    glue_collapse("\n")
  
  Cov_Observed = map(endogeneous, ~  glue("{.x}{1:max_time} ~~ 0*{.x}{1:max_time}")%>%
                       glue_collapse("\n")) %>%
    glue_collapse("\n")
  
  if (resid_stationary == T){
      if (multiple == T){
    Var_Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} ~~ c(evar{.x}{1}, evar{.x}{2})*e_{.x}{1:max_time}")%>%
                      glue_collapse("\n")) %>%
      glue_collapse("\n")
  } else{
    Var_Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} ~~ evar{.x}*e_{.x}{1:max_time}")%>%
                      glue_collapse("\n")) %>%
      glue_collapse("\n")
    }
  } else{
        Var_Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} ~~ e_{.x}{1:max_time}")%>%
                      glue_collapse("\n")) %>%
      glue_collapse("\n")
  }
  
# Regressions
  t = expand_grid(x = endogeneous,
                  y = endogeneous,
                  z = endogeneous) %>%
    filter(x == y) 
  
  require(tidyverse)
  
  t = rbind.data.frame(t, t)
  t = t %>% arrange(x)
  split_list = split(t, t$x)
  cf = c(rep('b_', n_var), rep('d_', n_var)) 
  fin = c()
  for (list in split_list){
    
    tib = list
    tib = cbind(cf, list) 
    
    strings = c()
    for (j in 1:(max_time-1)){
      
      if (multiple == T){
        string_out_vec =  tib %>%
        dplyr::mutate(o = ifelse(cf == 'b_',
                                 glue('c({cf}{y}{z}1,{cf}{y}{z}2)*{z}{j}'),
                                 glue('c({cf}{y}{z}1,{cf}{y}{z}2)*e_{z}{j}'))) %>%
        pull(o)
        current_var = substr(string_out_vec,5,6)[1]
      } else{
        string_out_vec =  tib %>%
          dplyr::mutate(o = ifelse(cf == 'b_',
                                   glue('{cf}{y}{z}*{z}{j}'),
                                   glue('{cf}{y}{z}*e_{z}{j}'))) %>%
          pull(o)
        current_var = substr(string_out_vec,3,4)[1]
      }
      
      string_out = string_out_vec %>%
        glue_collapse(" + ")
      
      
      if(cross == T){
        
        if(current_var == 'HE'|full ==T){
         strings = c(strings,
                  glue(current_var,
                       j+1, ' ~ ', string_out)) %>%
           glue_collapse("\n")
         } else if(current_var %in% reverse){
           pattern_pos = c(grep(paste0(current_var,current_var), string_out_vec),
                       grep('HE', string_out_vec))
           
           strings = c(strings,
                    glue(current_var,
                         j+1, ' ~ ', 
                         unique(string_out_vec[pattern_pos]) %>%
                           glue_collapse(" + "))) %>%
             glue_collapse("\n")
           
         } else{
            pattern_pos = (grep(paste0(current_var,current_var), string_out_vec))
            strings = c(strings,
                         glue(current_var,
                         j+1, ' ~ ', 
                         string_out_vec[pattern_pos] %>%
                           glue_collapse(" + "))) %>%
               glue_collapse("\n")
           }
        
        } else{ # if cross == F
        strings = c(strings,
                    glue(current_var,
                         j+1, ' ~ ', 
                         string_out_vec[grep(paste0(current_var,
                                                    current_var),
                                             string_out_vec)] %>%
                           glue_collapse(" + "))) %>%
          glue_collapse("\n")
        }
      
     }
    fin = c(fin, strings) %>%
      glue_collapse("\n")
  }
  
  Reg = glue('{fin}')
  
  if(length(group_equality) > 0){
    Reg = gsub(group_equality, gsub("[0-9]", "", group_equality), Reg) %>%
      glue_collapse("\n")
  }
  
  
  # Reg clpm
  if(model == 'clpm'){
    innovations = c()
    for (i in 1:length(endogeneous)){
      for (j in 1:length(endogeneous)){
        s = paste0(' \\+ d\\_', endogeneous[i], endogeneous[j],
                   '\\*e\\_', endogeneous[j],1:9)
        innovations = c(innovations, s)
      }
    }
    
    innovations = c(innovations, paste0('b_'))
    for (i in 1:length(innovations)){
      Reg = gsub(as.character(innovations[i]), '', Reg)
      
    }
    
  }

  
  ##
  
  if (length(endogeneous) > 1){
    
    ecov = expand_grid(x = endogeneous,
                       y = endogeneous) %>%
      filter(!x == y) %>%
      mutate(n = rep(1:(n_var-1), n_var)) %>%
      mutate(seq = rep(0:(n_var-1), each = n_var-1)) %>%
      filter(!seq>=n) %>%
      dplyr::select(x,y) 
    
    out_ecov = c()
    for (i in 1:max_time){
      
      col_name = paste0("ecov", i)
      
      s = expand_grid(x = endogeneous,
                      y = endogeneous) %>%
        filter(!x == y) %>%
        mutate(n = rep(1:(n_var-1), n_var)) %>%
        mutate(seq = rep(0:(n_var-1), each = n_var-1)) %>%
        filter(!seq>=n) %>%
        dplyr::select(x,y)
      
      if (resid_stationary == T){
        
        if (multiple == T){
        s %<>%
          dplyr::mutate(!!sym(col_name) := glue("e_{x}{i} ~~ c(ecov_{x}{y}1, ecov_{x}{y}2)*e_{y}{i}")
          ) %>%
          pull(!!sym(col_name))
                }else{
        s %<>%
          dplyr::mutate(!!sym(col_name) := glue("e_{x}{i} ~~ ecov_{x}{y}*e_{y}{i}")
          ) %>%
          pull(!!sym(col_name))
                }
      } else{
        s %<>%
          dplyr::mutate(!!sym(col_name) := glue("e_{x}{i} ~~ e_{y}{i}") 
                        ) %>%
        pull(!!sym(col_name))
        }
      
      out_ecov = c(out_ecov, s)
    }
    
    Cov_Resid = out_ecov %>%
      glue_collapse("\n")
  } else {
    Cov_Resid = ''
  }
  
  
  if(model == 'clpm'){
    cov_repl = out_ecov[1:nrow(ecov)]
    for (i in 1:length(cov_repl)){
      Cov_Resid = gsub(cov_repl[i], gsub('e_', '',cov_repl[i]), Cov_Resid)

    }
  }

# Controls
  
  if(model == 'clpm'){
    Control = c()
    for (i in 1:max_time){
      
      Control_df = data.frame(c('cntr' = control,
                                data.frame('end' = endogeneous)))
      nam = names(Control_df %>% 
                    select(starts_with('cntr')))
      library(eply)
      Control_df %<>%
        dplyr::mutate(cntr_sum = ifelse(length(control) >1, 
                                        apply(Control_df[ , nam ] , 1 , paste , collapse = " + " ), cntr)) %>%
        dplyr::mutate(out = glue("{end}{i} ~ {cntr_sum}")) %>%
        dplyr::select(out)
      
      Control = c(Control, Control_df$out) %>%
        glue_collapse("\n")
    }

  } else{
    Control_df = data.frame(c('cntr' = control,
                                data.frame('end' = endogeneous)))
      
    nam = names(Control_df %>% 
                  select(starts_with('cntr')))
      Control_df %<>%
        dplyr::mutate(cntr_sum = ifelse(length(control) >1, 
                                        apply(Control_df[ , nam ] , 1 , paste , collapse = " + " ), cntr)) %>%
        dplyr::mutate(int_c = glue("i{end} ~ {cntr_sum}"),
                      slope_c = glue("s{end} ~ {cntr_sum}")) %>%
        dplyr::select(int_c, slope_c)
      
      Control = c(Control_df$int_c, Control_df$slope_c) %>%
        glue_collapse("\n")
  }
  
  
  # Impulses and Past States
  
  #Reg = Reg_
  
  if(past_states == F){
    betas = c()
    for (i in 1:length(endogeneous)){
      for (j in 1:length(endogeneous)){
        s = paste0('b\\_', endogeneous[i], endogeneous[j],
                   '\\*', endogeneous[j], 1:max_time, ' \\+ ')
        betas = c(betas, s)
      }
    }
    
    for (i in 1:length(betas)){
      Reg = gsub(as.character(betas[i]), '', Reg)
      
    }
  }
  
  if(impulses == F){
    deltas = c()
    for (i in 1:length(endogeneous)){
      for (j in 1:length(endogeneous)){
        s = c(paste0(' \\+ d\\_', endogeneous[i], endogeneous[j],
                   '\\*e\\_', endogeneous[j], 1:max_time),
              paste0(' d\\_', endogeneous[i], endogeneous[j],
                     '\\*e\\_', endogeneous[j], 1:max_time))
        deltas = c(deltas, s)
      }
    }
    
    for (i in 1:length(deltas)){
      Reg = gsub(as.character(deltas[i]), '', Reg)
      
    }
  }
  
  if (impulses == F & past_states == F){
    left_hand_side = paste0(endogeneous, rep(1:max_time,
                                             each = length(endogeneous)), ' ~')
    for (i in 1:length(left_hand_side)){
      Reg = gsub(left_hand_side[i], '', Reg)
      Reg = gsub('\n', '', Reg)
    }
  }

  # printing
  
  if (model == 'clpm'){
    syntax_rigclm = glue('
  
                       # Intercepts
                      
                       {B}
                       
                       # Resid                     
                       
                       {Resid}
                       
                       {Cov_Observed}
                       
                       # Regressions
                       
                       {Reg}
                       
                       # Covariances between residuals
                       
                       {Cov_Resid}
                       
                       # Control variables
                       
                       {Control}
                       ')
  } else{
    syntax_rigclm = glue('
  
                       # Growth factors
                       
                       {Intercept}
                       {Slope}
                       
                       # Variancies                 
                       
                       {Intercept_Var}
                       {Slope_Var}
                       
                       # Means
                       
                       {Intercept_Mean}
                       {Slope_Mean}
                       
                       # Intercepts
                       
                       {A}
                       
                       {B}
                       
                       # Covariance between growth factors
                       
                       {Intercept_Slope_Cov}
                        
                       {Cov_ii}
                       {Cov_ss}
                       {Cov_is}
                       {Cov_si}
                       
                       # Impulses: deviations from growth trajectories                       
                       
                       {Resid}
                       
                       {Cov_Observed}
                       
                       {Var_Resid}
                       
                       # Long- and short-run effects
                       
                       {Reg}
                       
                       # Covariances between residuals
                       
                       {Cov_Resid}
                       
                       # Control variables
                       
                       {Control}
                       ')
  }
  
  # restrictions
  
  if (length(restricted_pars) > 0){
    names(restricted_pars) = c('pars', 'value')
    
    for (i in 1:nrow(restricted_pars)){
      
      lavaan_symbol = regmatches(restricted_pars[i,1], regexpr('~~|~|=~', restricted_pars[i,1]))
      syntax_rigclm = gsub(restricted_pars[i,1],
                           gsub(lavaan_symbol, paste0(lavaan_symbol, ' ', restricted_pars[i,2], '*'),
                                restricted_pars[i,1]), syntax_rigclm) %>%
      glue_collapse("\n")
    }

  }
  
  syntax_rigclm
  
}


ex = RC_GCLM_syntax(model = "gclm", reverse = c('ho', 'sc'))
ex

# wide data for lavaan

lavaan_df = function(dv,
                     ivs = c('health', 'healthcare','education',
                             'env', 'law_order', 'infrastructure'),
                     dv_map = 'HE',
                     ivs_map = c('he', 'hc','ed',
                                 'en', 'lo', 'ir'),
                     ids = c('lsoa11',
                             'MSOA11CD',
                             'LAD21CD'),
                     invariant = c(control_names, lad_inc_vars),
                     deprivation_cat = NULL,
                     time = 'time',
                     max_time = 6,
                     df){

  lookup = setNames(c(dv, ivs), c(dv_map, ivs_map))
  selected = c(dv_map, ivs_map, ids, invariant, deprivation_cat, time)

  out = df %>%
    dplyr::rename(all_of(lookup)) %>%
    dplyr::select(all_of(selected)) %>%
    tidyr::pivot_wider(id_cols = all_of(c(ids, invariant, deprivation_cat)),
                       names_from = time, 
                       values_from = all_of(c(dv_map, ivs_map)),
                       names_sep = '')
  return(out)
}



cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}


###
library(semptools)
library(semPlot)

plot_effects = function(models = growth_impulses_pastst_fit,
                        mod_names = 'growth_impulses_pastst_fit',
                        rhs_selected = c("HE1",
                                         "sc1",
                                         'ho1',
                                         'nh1',
                                         'ed1'),
                        lhs_selected = c("HE2",
                                         "sc2",
                                         'ho2',
                                         'nh2',
                                         'ed2'),
                        main_dv_name = 'Mental'){
  
  # models as list and naming
  models = list(models)
  names(models) = mod_names
  
  plot_list = list()
  
  # Storing as a list
  
  model_number = 0
  for (m in models){
    
    model_number =  model_number + 1 
    
    # selecting nodes
    sorting_vec = c(rbind(rhs_selected, lhs_selected))
    filtered = semptools::keep_nodes(
        semPlotModel(m),
        sorting_vec
        )
 
    # pars
    par = parameterEstimates(m, standardized=TRUE)  %>%
      filter(op == '~' & lhs %in% lhs_selected & rhs %in% rhs_selected) %>%
      filter(!(grepl('ho|nh|ed', lhs) & !(grepl('ho|nh|ed', rhs)&grepl('ho|nh|ed', lhs))))
    par = paste0(round(par$std.all, 3),
                 ifelse(par$pvalue < 0.001, '***',
                        ifelse(par$pvalue <0.01, '**',
                               ifelse(par$pvalue < 0.05, '*',
                                      ifelse(par$pvalue < 0.1, 'ǂ', '')))))
    
    # remove links from nh, ho to HE - looks cluttered
    to_cover = filtered@Pars %>% 
      filter((grepl('ho|nh|ed', rhs) & !(grepl('ho|nh|ed', rhs)&grepl('ho|nh|ed', lhs)))) %>%
      pull(par)
    
    # final list with pars
    filtered@Pars = filtered@Pars %>% filter(!(par %in% to_cover)) %>%
      filter(edge == '~>')

    filtered@Vars$manifest = TRUE
    
    filtered@Vars$name = filtered@Vars$name[order(match(filtered@Vars$name,
                                   sorting_vec))]
    
    
    # plotting
    if (grepl('mgc', names(models)[model_number])){
      
      plot_list[[model_number]] = vector('list', length = 2)
      
      for (i in 1:2){
        
        plot_list[[model_number]][[i]] = semPaths(filtered,
                                                  whatLabels = "std",
                                                  edgeLabels = par,
                                                  sizeMan = 15,
                                                  sizeMan2 = 13,
                                                  edge.label.cex = 1.1,
                                                  label.scale = F,
                                                  intercepts = F,
                                                  residuals = F,
                                                  rotation = 2,
                                                  curvePivot = TRUE,
                                                  layout = 'tree',
                                                  pastel = T,
                                                  #panelGroups = T,
                                                  borders = F,
                                                  edge.color = c('grey', 'pink', 'pink', 
                                                                 'pink', 'grey', 'grey',
                                                                 'grey', 'grey'),
                                                  edge.label.position = 0.4,
                                                  edge.label.color = c('darkgrey', 'black', 'black', 
                                                                       'black', 'darkgrey', 'darkgrey',
                                                                       'darkgrey', 'darkgrey'),
                                                  edge.label.bg = c('white', 'pink', 'pink', 
                                                                    'pink', 'white', 'white',
                                                                    'white', 'white'),
                                                  nodeLabels = c(main_dv_name, 
                                                                 main_dv_name,
                                                                 'Social Care/ \n Public Health',
                                                                 'Social Care/ \n Public Health',
                                                                 'Housing/ \n Transport',
                                                                 'Housing/ \n Transport',
                                                                 'Environment/ \n Culture/ \n Planning',
                                                                 'Environment/ \n Culture/ \n Planning'),
                                                  ask = 'N',
                                                  edge.width = 2,
                                                  label.color = c('darkred', 'darkred', 'black', 
                                                                  'black', 'black', 'black',
                                                                  'black', 'black'),
                                                  title = F,
                                                  include = i
                                                  
        ) 
      }
      plot_list[[model_number]][[2]] = plot_list[[model_number]][[2]][[2]]
    }
    
    else{
      plot_list[[model_number]] = semPaths(filtered,
                                           whatLabels = "std",
                                           edgeLabels = par,
                                           sizeMan = 15,
                                           sizeMan2 = 17,
                                           edge.label.cex = 1.2,
                                           label.scale = F,
                                           intercepts = F,
                                           residuals = F,
                                           rotation = 2,
                                           curvePivot = TRUE,
                                           layout = 'tree',
                                           pastel = T,
                                           borders = F,
                                           edge.label.position = 0.4,
                                           edge.color = c('grey', 'grey', 'black', 
                                                          'black', 'black', 'black',
                                                          'grey', 'grey', 'grey', 'black'),
                                           nodeLabels = c('Mental Health', 
                                                          'Mental Health',
                                                          'Social Care/ \n Public Health',
                                                          'Social Care/ \n Public Health',
                                                          'Housing/ \n Transport',
                                                          'Housing/ \n Transport',
                                                          'Environment/ \n Culture/ \n Planning',
                                                          'Environment/ \n Culture/ \n Planning',
                                                          'Education',
                                                          'Education'),
                                           edge.label.bg = c('white', 'white', 'lightgrey', 
                                                             'lightgrey', 'lightgrey', 'lightgrey',
                                                             'white', 'white',
                                                             'white', 'lightgrey'),
                                           label.color = c('blue', 'blue', 'black', 
                                                           'black', 'black', 
                                                           'black', 'black', 'black',
                                                           'black', 'black'),
                                           ask = 'N',
                                           edge.width = 2,
                                           title = F
                                           
      ) 
    }
    
  }
  
  names(plot_list) = names(models)
  
  return(plot_list)
}


# Extract coefficients for SEM models

CoefsExtract = function(models = NULL,
                        health = 'HE2',
                        end = paste0(c('HE', 'he', 'hc', 'ed', 'en', 'lo', 'ir'), '1'),
                        impulses = paste0(c('e_HE', 'e_he', 'e_hc', 'e_ed', 'e_en', 'e_lo', 'e_ir'), '1'),
                        growth = NULL){
  require(data.table)
  
  m_out = list()
  colnm = c("lhs", "op", "rhs", "group", "est.std", 'pvalue', 'se')
  tech = c('lhs', 'op', 'rhs')
  
  m_out <- lapply(models, function(model) {
    stdsol <- broom::tidy(eval(parse(text = model))) %>% 
      separate(term, into = c("lhs", "rhs"), sep = " =~ | ~~ | ~1 | ~ ") %>%
      rename(est.std = std.all, se = std.error, pvalue = p.value)
    
    stdsol %>% 
      filter(
        op == "~" & rhs %in% end |
          op == "~~" & lhs %in% growth & rhs %in% growth |
          op == "~1" & lhs %in% growth |
          op == "~" & rhs %in% impulses
      ) %>% 
      dplyr::select(all_of(intersect(colnames(stdsol), colnm))) %>% 
      mutate(id = str_c(lhs, op, rhs)) %>%
      select(-one_of(tech))
  })
  
  # curating
  
  coefs_long <- m_out %>% 
    reduce(full_join, by = 'id') %>% 
    select(id, everything()) %>% 
    mutate_if(is.numeric, ~ round(., 3))
  
  fun <- function(x){
    case_when(
      x <= 0.001 ~ '***',
      x <= 0.01 & x > 0.001 ~ '**',
      x <= 0.05 & x > 0.01 ~ '*',
      x > 0.05 & x <= 0.1 ~ '^',
      is.na(x) ~ 'l',
      TRUE ~ ''
    )
  }
  
  coefs_long <- coefs_long %>%
    mutate_at(vars(contains('pvalue')), fun) %>%
    mutate_all(~ ifelse(is.na(.), "", .))
  
  coefs_long$id <- str_replace(coefs_long$id, "~1", "~#") %>%
    gsub('[[:digit:]]+', '', .)
  end_ <- gsub('[[:digit:]]+', '', end)
  impulses_ <- gsub('[[:digit:]]+', '', impulses)
  
  # determining types of effects for further arrangement
  coefs_long = as.data.table(coefs_long)
  
  if (!is.null(growth)) {
    growth_variants <- expand.grid(growth, growth) %>% 
      dplyr::mutate(collapse = glue('{Var1}~~{Var2}')) %>% 
      pull(collapse)
    
    coefs_long[, type := case_when(
      id %in% c(paste0(end_, '~', end_), paste0(end_, '~', impulses_)) ~ 'a_auto',
      substr(id, 1, 2) == substr(health, 1, 2) ~ 'c_policy',
      id %in% growth_variants ~ 'd_growth_cov',
      grepl('~#', id) ~ 'd_growth_means',
      TRUE ~ 'b_health'
    )]
  } else {
    coefs_long[, type := case_when(
      id %in% c(paste0(end_, '~', end_), paste0(end_, '~', impulses_)) ~ 'a_auto',
      substr(id, 1, 2) == substr(health, 1, 2) ~ 'c_policy',
      TRUE ~ 'b_health'
    )]
  }

  coefs_long <- coefs_long %>%
    arrange(type) %>%
    mutate(
      long_or_short = ifelse(grepl('e_', id), 'short', 'long'),
      num = case_when(
        grepl('hc', id) ~ 2,
        grepl('he', id, ignore.case = FALSE) ~ 3,
        grepl('ed', id) ~ 4,
        grepl('en', id) ~ 5,
        grepl('lo', id) ~ 6,
        grepl('ir', id) ~ 7,
        TRUE ~ 1
      ),
      id = sub('e_', '', id),
      type_long_or_short = paste0(type, long_or_short)
    ) %>%
    arrange(type_long_or_short, num)
  
  # to wide format
  
  values_from <- colnames(coefs_long)[grepl('est|pvalue|se',colnames(coefs_long))]
  ids <- colnames(coefs_long)[colnames(coefs_long) %in% c('id', 'group')]
  
  coefs_wide <- pivot_wider(coefs_long, id_cols = all_of(ids),
                            names_from = long_or_short,
                            values_from = all_of(values_from))
  
  columns <- colnames(coefs_wide)[grep('est.std.', colnames(coefs_wide))]
  
  for (i in columns) {
    colending <- sub('est.std', '', i)
    coefs_wide[[i]] <- paste0(sprintf("%.3f", as.numeric(coefs_wide[[i]])),
                              coefs_wide[[paste0('pvalue', colending)]],
                              ' [',
                              sprintf("%.3f", as.numeric(coefs_wide[[paste0('se', colending)]])),
                              ']')
  }
  
  coefs_wide <- coefs_wide %>%
    select(all_of(ids), starts_with('est')) %>%
    as.data.frame()
  
  # removing NANA
  coefs_wide[coefs_wide == 'NANA [NA]' | coefs_wide == 'NA [NA]' | coefs_wide == 'NAl [NA]'] <- ''
  
  # sorting growth means
  coefs_wide$gsort <- case_when(
    substr(coefs_wide$id, 1, 1) == 'i' & grepl('~#', coefs_wide$id) ~ 1,
    substr(coefs_wide$id, 1, 1) == 's' & grepl('~#', coefs_wide$id) ~ 2,
    TRUE ~ 0
  )
  
  coefs_wide %<>% arrange(gsort) %>% select(-gsort)
  
  # for multiple group comparison
  
  if('group' %in% colnames(coefs_long)){
    values_from = colnames(coefs_wide)[!colnames(coefs_wide) %in% c('id', 'group')]
    coefs_wide = coefs_wide %>% pivot_wider(id_cols = id,
                                            names_from = group,
                                            values_from = all_of(values_from),
                                            names_sort = F)
    
    # sort colnames
    colnames = colnames(coefs_wide)
    get_number <- function(x) {
      if (!x == 'id'){
        as.numeric(sub("^.*_(\\d+)$", "\\1", x))
      }
    }
    coefs_wide = coefs_wide[,c('id', names((sort(unlist(sapply(colnames, get_number))))))]
    
  }
  
  

  return(coefs_wide)
}

# Extracting growth curve cor tables
GrowthCorTable = function(dat, cor_name, pars, 
                          growth = c(paste0('i', endogeneous), 
                                     paste0('s', endogeneous))){
  sgn = dat
  sgn %<>% tidyr::separate(cor_name, c('X', 'Y'), '~~')
  
  # Select columns and separate the cor_name column into X and Y columns using '~~' as the separator
  dat = dat %>%
    select(all_of(cor_name), all_of(pars)) %>%
    separate(cor_name, c('X', 'Y'), '~~') %>%
    # Remove certain characters from the pars columns and convert to numeric type
    mutate_at(vars(pars), ~ as.numeric(str_replace_all(., '\\*\\**\\**|\\^|\\[.*\\]', '')))
  
  # Calculate the covariance matrix
  pivot_initial = dat %>%
    # Group by X and Y columns, and calculate mean for the pars columns
    group_by(Y, X) %>%
    summarize(across(all_of(pars), identity)) %>%
    # Convert to wide format with X columns as columns and Y columns as rows
    pivot_wider(names_from = X, values_from = all_of(pars)) %>%
    column_to_rownames(var = "Y")
  
  pivot_initial = t(pivot_initial[growth, growth])
  
  # Convert covariance matrix to correlation matrix and remove values in lower triangle
  pivot = as.data.frame(cov2cor(as.matrix(pivot_initial)))
  pivot[] = lapply(pivot, sprintf, fmt = "%.2f")
  pivot[lower.tri(pivot, diag = F)] <- ''
  
  # Assign the column names to the row names for the matrix
  dimnames(pivot) = list(colnames(pivot_initial), colnames(pivot_initial))
  
  # Add additional information to the lower triangle of the matrix
  for (i in seq_along(colnames(pivot))){
    for (j in seq_along(colnames(pivot))){
      if (i != j){
        # Combine the correlation value with the additional information from the dat table
        pivot[i, j] = paste0(pivot[i, j], str_replace_all(
          sgn %>% filter(X == colnames(pivot)[i], Y == colnames(pivot)[j]) %>% pull(pars),
          c("[:digit:]|-|\\." = '', '\\[.*\\]' = '')))
      }
    }
  }
  
  # col and row names
  all_nam = c('Intercept Mental Health',
              'Intercept Public Health & Social Care',
              'Intercept Healthcare',
              'Intercept Education',
              'Intercept Environment',
              'Intercept Law and Order',
              'Intercept Infrastructure',
              'Slope Mental Health',
              'Slope Public Health & Social Care',
              'Slope Healthcare',
              'Slope Education',
              'Slope Environment',
              'Slope Law and Order',
              'Slope Infrastructure'
  )
  dimnames(pivot) = list(all_nam, all_nam)
  
  # Return the correlation table
  return(pivot)
}



# Compute descriptive statistics

summarize_data <- function(dat = df_before_scaling,
                           .stationary = stationary,
                           .nonstationary = nonstationary,
                           year = 'year',
                           stat = list('Mean' = mean,
                                       'SD' = sd),
                           rownames = nm_out[-1]) {
  
  vars_used <- c(.nonstationary, .stationary)
  stat_names = names(stat)
  
  # summary for nonstationary
  sumstat <- dat %>%
    select(all_of(.nonstationary), year) %>%
    group_by(year) %>%
    summarise(across(everything(), stat, .names = "{.col}__{.fn}")) %>%
    pivot_longer(-year, names_to = c("variable", "stat"), names_sep = "__") %>%
    pivot_wider(id_cols = c(year, variable), names_from = stat, values_from = value) %>%
    pivot_wider(id_cols = variable, names_from = year, values_from = all_of(stat_names))
  
  # sort colnames
  colnames = colnames(sumstat)
  get_number <- function(x) {
    if (!x == 'variable'){
      as.numeric(sub("^.*_(\\d+)$", "\\1", x))
    }
  }
  sumstat = sumstat[,c('variable', names((sort(unlist(sapply(colnames, get_number))))))]
  
  # summary for stationary
  overall <- dat %>%
    select(all_of(vars_used)) %>%
    summarise(across(everything(), stat, .names = "{.col}__{.fn}")) %>%
    pivot_longer(names_to = 'key', values_to = 'value', cols = everything()) %>%
    separate(key, into = c("variable", "stat"), sep = "__") %>%
    pivot_wider(id_cols = variable, names_from = stat, values_from = value)
  
  # combining
  sumstat_fin <- sumstat %>%
    full_join(overall, by = "variable") %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    mutate_all(~ ifelse(is.na(.), "", .)) %>%
    select(-variable) %>%
    add_column(`Names` = rownames, .before = 1)
  
  # adding names
  dat = as.data.frame(dat)
  colnames(sumstat_fin)[1] <- ""
  year_vec = as.vector(unique(dat[,year]))
  blank_rep = rep('', (length(stat_names)-1))
  colnames(sumstat_fin) <- c('', c(sapply(year_vec, function(.) c(., blank_rep))),
                             'Total', blank_rep)
  
  
  
  stat_head <- data.frame(matrix(nrow = 1, ncol = ncol(sumstat_fin)))
  colnames(stat_head) <- colnames(sumstat_fin)
  stat_head[1,] <- c('', rep(stat_names, (length(year_vec) + 1)))
  
  sumstat_fin <- rbind(stat_head, sumstat_fin)
  
  return(sumstat_fin)
  
}




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# earlier functions

# cbind data.frames with unequal number of rows

cbind.fill = function(...) {                                                                                                                                                       
  transpoted = lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe = lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
}


# setting a vector with website links
sites = c(
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2007-to-2008-individual-local-authority-data',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2008-to-2009-individual-local-authority-data',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2009-to-2010-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2010-to-2011-individual-local-authority-data--5',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2011-to-2012-individual-local-authority-data--2',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2012-to-2013-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2013-to-2014-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistical-data-sets/local-authority-revenue-expenditure-and-financing-england-2014-to-2015-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2015-to-2016-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2016-to-2017-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2017-to-2018-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2018-to-2019-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2019-to-2020-individual-local-authority-data-outturn',
  'https://www.gov.uk/government/statistics/local-authority-revenue-expenditure-and-financing-england-2020-to-2021-individual-local-authority-data-outturn'
)

# downloading excel files with individual local authority outturn data from gov.uk
loadLists = function(site = sites, year_seq = 2007:2020,
                     download = F, list_number, type){
  
  # creating an empty list
  list_df = vector("list", length(year_seq))
  list_df_names = unlist(lapply(year_seq, function(x) grep(x, site)[2]))
  list_df_names[is.na(list_df_names)] = 1
  names(list_df) = list_df_names
  
  for (i in list_df_names){
    
    # extracting links with files
    html = paste(readLines(site[i]), collapse="\n")
    
    if (grepl('2014', site[i]) & !grepl('2013', site[i])){ # there was a different html code in 2014
      #matched = str_match_all(html, '><a href=\"(.*?)\"')
      matched = str_match_all(html, '<a class=\"govuk-link\" tabindex=\"-1\" href=\"(.*?)\"')
    }
    else{
      matched = str_match_all(html, '<a aria-hidden="true" class="thumbnail" tabindex="-1" href=\"(.*?)\"')
      
    }
    links = matched[[1]][,2]
    
    file_name_vec = c()
    
    # storing files in folders by years   
    for (j in seq_along(links)){   
      
      folder = paste0("C:/Users/", Sys.getenv("USERNAME"),
                      '/YandexDisk/PhD Research/Data/Financial/', year_seq[which(list_df_names == i)])
      file_name = sub(".*/", "", links[j])
      
      if (download == T){
        download.file(links[j], file.path(folder, file_name), mode = 'wb')
      }
      
      # retrieving from folders the files of interest
      if (grepl(type, file_name)){
        
        # full path
        path = file.path(folder, file_name)
        
        # uploading all data into a large list
        if (year_seq[which(list_df_names == i)] %in% c(2019, 2020)){ # conditioning on a file extension
          
          # selecting a sheet (removing a hidden sheet 'Col refs' and picking the 3rd one out of the remaning)
          sheet_to_import = list_ods_sheets(path)
          sheet_to_import = sheet_to_import[which(!sheet_to_import == 'Col refs')][list_number]
          list_df[[which(list_df_names == i)]][[j]] = read_ods(path,  sheet = sheet_to_import)
          
        } 
        else {
          sheet_to_import = excel_sheets(path)
          sheet_to_import = sheet_to_import[which(!sheet_to_import == 'Col refs')][list_number]
          list_df[[which(list_df_names == i)]][[j]] = do.call(cbind, lapply(sheet_to_import, function(x) readxl::read_excel(path, sheet = x)))
          
        }
        
        # removing empty lists
        list_df[[which(list_df_names == i)]] = list_df[[which(list_df_names == i)]] %>% purrr::discard(function(x) length(x) == 0L)
        #list_df[[i]] = rlist::list.clean(list_df[[i]], function(x) length(x) == 0L, TRUE)
        
        # assigning names to lists
        file_name_vec = c(file_name_vec, file_name)
        names(list_df[[which(list_df_names == i)]]) = file_name_vec
        
      }     
    }
    
    # unifying the class of tables 
    for (i in seq_along(list_df)){
      list_df[[which(list_df_names == i)]] = lapply(list_df[[which(list_df_names == i)]], as.data.frame)  
    }   
    
  }
  
  return(list_df)
}


# Detrending
DetrendPanel = function(vars = NULL, id = id, time = year, data = df){
  detrended = list()
  for (i in vars){
    detrended[[which(vars == i)]] = left_join(moderndive::get_regression_points(lm(as.formula(paste(i, '~ id*time')), data = df)) %>% 
                                                select(id, time, residual),
                                              unique(moderndive::get_regression_points(lm(as.formula(paste(i, '~ id')), data = df)) %>% 
                                                       select(id, grep('hat', colnames(.))))) %>% 
      rowwise() %>% 
      dplyr::mutate(!!paste0(i, '') :=  sum(c(!!as.name(paste0(i, '_hat')), residual))) %>%
      select(c(id, time, !!paste0(i, '')))
  }
  
  out = purrr::reduce(detrended, full_join)
  print(i)
  return(out)
}

DetrendPanel_ = function(vars = NULL, id = id, time = time, data = df){
  detrended = list()
  for (i in vars){
    
    if (dim(table(df[, i])) == 2){
      
      detrended[[which(vars == i)]] = left_join(broom::augment(glm(as.formula(paste(i, '~ time*id + (I(time)^2)*id')), data = df))%>% 
                                                  select(id, time, .resid),
                                                unique(broom::augment(glm(as.formula(paste(i, '~ id')), data = df)) %>% 
                                                         select(id, .fitted))) %>%
        rowwise() %>% 
        dplyr::mutate(tot = sum(.fitted, .resid)) %>% 
        dplyr::mutate(!!paste0(i, '_det') := if_else(tot >= 0.5, 1, 0)) %>%
        select(c(id, time, !!paste0(i, '_det')))
      
    } else {
      detrended[[which(vars == i)]] =  left_join(broom::augment(lm(as.formula(paste(i, '~ time*id + (I(time)^2)*id')), data = df))%>% 
                                                   select(id, time, .resid),
                                                 unique(broom::augment(lm(as.formula(paste(i, '~ id')), data = df)) %>% 
                                                          select(id, .fitted))) %>% 
        rowwise() %>% 
        dplyr::mutate(!!paste0(i, '_det') :=  sum(.fitted, .resid)) %>%
        select(c(id, time, !!paste0(i, '_det')))
    }
    
  } 
  out = purrr::reduce(detrended, full_join)
  
  return(out)
}

