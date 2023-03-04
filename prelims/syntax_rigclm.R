
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

endogeneous = c('HE', 'he', 'hc', 'ed', 'en', 'lo', 'ir')

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
                          group_equality = NULL,
                          restricted_pars = NULL,
                          model = NULL){
  require(dplyr)

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
  n_var = length(endogeneous)
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
  
  #Var_Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} ~~ var{.x}{1:max_time}*e_{.x}{1:max_time}")%>%
  #                  glue_collapse("\n")) %>%
  #  glue_collapse("\n")
  Var_Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} ~~ evar{.x}*e_{.x}{1:max_time}")%>%
                    glue_collapse("\n")) %>%
    glue_collapse("\n")
  
  
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
        dplyr::select(x,y) %>%
        dplyr::mutate(!!sym(col_name) := glue("e_{x}{i} ~~ ecov_{x}{y}*e_{y}{i}") 
                      #glue("e_{x}{i} ~~ e_{y}{i}")
                      ) %>%
        pull(!!sym(col_name))
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

lavaan_df = function(HE,
                     he = 'health',
                     hc = 'healthcare',
                     ed = 'education',
                     en = 'env',
                     lo = 'law_order',
                     ir = 'infrastructure',
                     max_time = 7,
                     time = 'time',
                     df){
  df %>%
    dplyr::select(lsoa11, MSOA11CD, time, LAD21CD,
                  lsoa_ses_score,
                  all_of(control_names),
                  HE = all_of(HE),
                  he = all_of(he),
                  hc = all_of(hc), 
                  ed = all_of(ed), 
                  en = all_of(en),
                  lo = all_of(lo), 
                  ir = all_of(ir)) %>%
    #filter(time %in% 1:max_time) %>%
    tidyr::pivot_wider(id_cols = c(lsoa11, MSOA11CD, LAD21CD,
                                   lsoa_ses_score,
                                   all_of(control_names)),
                       names_from = time, 
                       values_from = c(HE, 
                                       he,
                                       hc,
                                       ed,
                                       en, 
                                       lo,
                                       ir),
                       names_sep = '')
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



CoefsExtract = function(models = NULL,
                        health = 'HE2',
                        end = paste0(c('HE', 'he', 'hc', 'ed', 'en', 'lo', 'ir'), '1'),
                        impulses = paste0(c('e_HE', 'e_he', 'e_hc', 'e_ed', 'e_en', 'e_lo', 'e_ir'), '1'),
                        growth = NULL){
  
  m_out = list()
  
  for (i in 1:length(models)){
    stdsol = standardizedSolution(eval(parse(text = models[i])))
    tech = c('lhs', 'op', 'rhs')
      
    if (any(names(stdsol) == 'label')){
      stdsol %<>% dplyr::select(!label)
      }
    if (is.null(growth)){
      m_out[[i]] = stdsol[stdsol[,'op'] %in% c('~') & stdsol[,'rhs'] %in% end|
                          stdsol[,'op'] %in% c('~') & stdsol[,'rhs'] %in% impulses,
                        c(1:3,4,7)]
      }
    else {
        m_out[[i]] = stdsol[stdsol[,'op'] %in% c('~') & stdsol[,'rhs'] %in% end|
                              stdsol[,'op'] %in% c('~~') & stdsol[,'lhs'] %in% growth & stdsol[,'rhs'] %in% growth|
                              stdsol[,'op'] %in% c('~1') & stdsol[,'lhs'] %in% growth|
                              stdsol[,'op'] %in% c('~') & stdsol[,'rhs'] %in% impulses,
                            c(1:3,4,7)]
        }
    rownames(m_out[[i]]) = NULL
    m_out[[i]][,'id'] = apply(m_out[[i]][,tech], 1, function(x) str_c(x, collapse = ""))
    m_out[[i]][,tech] = NULL
    }
  
  # curating
  coefs_long = purrr::reduce(m_out, dplyr::full_join, by = 'id')

  coefs_long %<>% dplyr::select(id, everything())
  
  coefs_long[,2:ncol(coefs_long)] = sapply(coefs_long[,2:ncol(coefs_long)], as.numeric)
  coefs_long[,2:ncol(coefs_long)] = round(coefs_long[,2:ncol(coefs_long)], 3)
  coefs_long = as.data.frame(coefs_long)
  
  fun = function(x){
    x = ifelse(x<=.001, '***',
           ifelse(x<=.01 & x>.001, '**',
                  ifelse(x<=.05 & x>.01, '*',
                         ifelse(x>.05 & x<=.1, '^', 
                                ifelse(is.na(x), 'l', '')))))
  }
  coefs_long[,grep('pvalue', names(coefs_long))] = 
    apply(as_tibble(coefs_long[,grep('pvalue', names(coefs_long))]), 2, FUN = fun)
  
  coefs_long[is.na(coefs_long)] = ''
  
  coefs_long$id <- str_replace(coefs_long$id, "~1", "~#")
  coefs_long$id = gsub('[[:digit:]]+', '', coefs_long$id)
  end_ = gsub('[[:digit:]]+', '', end)
  impulses_ = gsub('[[:digit:]]+', '', impulses)
  
  # determining types of effects for further arrangement
  if (!is.null(growth)){
    growth_variants = as.vector(expand.grid(growth,growth) %>% 
                                dplyr::mutate(collapse = glue('{Var1}~~{Var2}')) %>% 
                                pull(collapse))
    
    coefs_long$type = ifelse(coefs_long$id %in% c(paste0(end_, '~', end_),
                                                  paste0(end_, '~', impulses_)), 'a_auto',
                             ifelse(substr(coefs_long$id, 1, 2) == substr(health, 1, 2), 'c_policy',
                                    ifelse(coefs_long$id %in% growth_variants, 'd_growth_cov',
                                           ifelse(grepl('~#', coefs_long$id), 'd_growth_means', 'b_health'))))
  } else{
      coefs_long$type = ifelse(coefs_long$id %in% c(paste0(end_, '~', end_),
                                                paste0(end_, '~', impulses_)), 'a_auto',
                           ifelse(substr(coefs_long$id, 1, 2) == substr(health, 1, 2), 'c_policy', 'b_health'))
  }

  
  coefs_long %<>% arrange(type)
  coefs_long$long_or_short = ifelse(grepl('e_', coefs_long$id), 'short', 'long')
  coefs_long$num = ifelse(grepl('hc', coefs_long$id), 2, 
                          ifelse(grepl('he', coefs_long$id, ignore.case = F), 3,
                                 ifelse(grepl('ed', coefs_long$id), 4,
                                        ifelse(grepl('en', coefs_long$id), 5,
                                               ifelse(grepl('lo', coefs_long$id), 6,
                                                      ifelse(grepl('ir', coefs_long$id), 7, 1
                                                             ))))))
  coefs_long %<>% group_by(type) %>% arrange(long_or_short) %>% ungroup()
  coefs_long$type_long_or_short = paste0(coefs_long$type, coefs_long$long_or_short)
  coefs_long = coefs_long %>% dplyr::group_by(type_long_or_short) %>%
    arrange(num, .by_group=TRUE)
  coefs_long$id = sub('e_', '', coefs_long$id)
  
  # to wide format
  values_from = colnames(coefs_long)[grepl('est|pvalue',colnames(coefs_long))]
  coefs_wide = coefs_long %>% pivot_wider(id_cols = id, names_from = long_or_short,
                                               values_from = all_of(values_from))
  coefs_wide = as.data.frame(coefs_wide)
  
  columns = colnames(coefs_wide)[grep('est.std.', colnames(coefs_wide))]
  
  for (i in columns){
    colending = sub('est.std', '', i)
    coefs_wide[,i] = 
    paste0(coefs_wide[,i],
           coefs_wide[,c(paste0('pvalue',colending))])
  
  }
  coefs_wide %<>% dplyr::select(id, starts_with('est'))
  
  # removing NANA
  coefs_wide[coefs_wide=='NANA'] = ''
  
  # sorting growth means
  coefs_wide$gsort = ifelse(substr(coefs_wide$id, 1,1) == 'i' & grepl('~#', coefs_wide$id), 1, 
                            ifelse(substr(coefs_wide$id, 1,1) == 's'  & grepl('~#', coefs_wide$id), 2, 0))
  
  coefs_wide %<>% arrange(gsort) %>% select(-gsort)
  
  return(coefs_wide)
}

