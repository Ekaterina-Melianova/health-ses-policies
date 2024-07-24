
# Functions

# normalize

normalize = function(x, na.rm = T){
  return((x - min(x)) /(max(x)- min(x)))
}


# vectors with names
policy_names_6 = c('social_care_adult',
                   'social_care_children',
                   'healthcare',
                   'env',
                   'law_order',
                   'infrastructure'
                   )

policy_names_6_ch = c('ca',
                   'cb',
                   'cc',
                   'ot',
                   #'social_care_adult',
                   'healthcare'
)

control_names = c('public_health_mean',
                  'inc_mean',
                  'lsoa_ses_score',
                  'pop_census11',
                  'nonwhite',
                  'females',
                  'older',
                  'n',

                  'rural',
                  'London',
                  'SD',
                  'MD')
health_vars = c('samhi_index',
                'z_mh_rate',
                'antidep_rate',
                'est_qof_dep',
                'prop_ibesa')

parameters = c('Number of Parameters',
               'Chi-Squared',
               'Degrees of Freedom',
               'CFI',
               'TLI',
               'SMRM',
               'RMSEA',
               'RMSEA Lower Bound',
               'RMSEA Upper Bound',
               'AGFI',
               'AIC',
               'BIC',
               'Log-Likelihood')

endogeneous = c('HE', # 'Mental Health'
                'as', # 'Adult Social Care'
                'cs', # 'Children Social Care'
                'hc', # 'Healthcare'
                'en', # 'Environment'
                'lo', # 'Law and Order'
                'fr') # 'Infrastructure'

end_new = c('Mental Health',
            'Adult Social Care',
            'Children Social Care',
            'Healthcare',
            'Environment',
            'Law & Order',
            'Infrastructure')

end_new_ch = c('MH Hospitalisations',
            'Children Social Care 1',
            'Children Social Care 2',
            'Children Social Care 3',
            'Other Spending',
            #'Adult Social Care',
            'Healthcare')

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

descriptives_names = c('SAMHI Z-Score',
                       'Incapacity Benefits LSOA %',
                       'Depression LSOA %',
                       'Antidepressants per head',
                       'MH Hospitalisation Rate Z-Score',

                       'Adult Social Care',
                       'Children Social Care',
                       'Healthcare',
                       'Environment',
                       'Law and Order',
                       'Infrastructure',

                       'Public Health, 7-year mean',
                       'LTLA Income, 7-year mean',
                       'IMD (inc. + empl. domains)',
                       'LSOA Population Size',
                       'Non-white, LSOA %',
                       'Females, LSOA %',
                       'Older, LSOA %',
                       'N of LSOAs in LTLA',
                       'Rural, LSOA %',
                       'London Boroughs',
                       'Shire Districts',
                       'Metropolitan Districts')

descriptives_names_ch = c(
                       'Hospital Admission Z-Score',

                       'Children Social Care 1',
                       'Children Social Care 2',
                       'Children Social Care 3',
                       'Other Spending',
                       #'Adult Social Care',
                       'Healthcare',

                       'Public Health, 7-year mean',
                       'LTLA Income, 7-year mean',
                       'IMD (inc. + empl. domains)',
                       'LSOA Population Size',
                       'Non-white, LSOA %',
                       'Females, LSOA %',
                       'Older, LSOA %',
                       'N of LSOAs in LTLA',
                       'Rural, LSOA %',
                       'London Boroughs',
                       'Shire Districts',
                       'Metropolitan Districts')

nm_out = c('SAMHI',
           'Incapacity Benefits',
           'Depression',
           'Antidepressants',
           'MH Hospitalisation Rate',

           'Adult Social Care',
           'Children Social Care',
           'Healthcare',
           'Environment',
           'Law and Order',
           'Infrastructure',

           'Public Health',
           'LTLA Income',
           'IMD (inc. + empl.)',
           'LSOA Population Size',
           'Non-white',
           'Females',
           'Older',
           'N of LSOAs in LTLA',
           'Rural',
           'London Boroughs',
           'Shire Districts',
           'Metropolitan Districts')

nm_out_ch = c('Hospital Admission Z-Score',

              'Children Social Care 1',
              'Children Social Care 2',
              'Children Social Care 3',
              'Other Spending',
              #'Adult Social Care',
              'Healthcare',

              'Public Health',
              'LTLA Income',
              'LSOA Population Size',
              'Non-white',
              'Females',
              'Older',
              'N of LSOAs in LTLA',
              'Rural',
              'London Boroughs',
              'Shire Districts')


# a vector with the names of random slopes to exclude
no_slopes = c('sHE ', 'sas ', 'scs ', 'shc ',
              'sen ', 'slo ', 'sfr ',
              '\\*sHE', '\\*sas', '\\*scs', '\\*shc',
              '\\*sen', '\\*slo', '\\*sfr')

# Random Effects GCLM lavaan syntax - main function

RC_GCLM_syntax = function(endogeneous = c('HE', 'as', 'cs', 'hc',
                                          'en', 'lo', 'fr'), # all endogenous variables
                          reverse = c('as', 'cs', 'hc', # all endogenous variables except for the main outcome (mental health)
                                      'en', 'lo', 'fr'),
                          full = T, # include reverse effects from health to spending
                          no_slopes = NULL, # which random slopes to exclude
                          control = control_names, # control variables
                          max_time = 7, # maximum time points
                          impulses = T, # include impulse effects in GCLM (starts from 'd_')
                          past_states = T, # include the effect of past states in GCLM (starts from 'b_')
                          cross = T, # include cross-lagged effects or keep only the autoregressive ones
                          multiple = F, # include multiple group effects
                          resid_stationary = T, # whether residual variances and covariances should be stationary
                          group_equality = NULL, # group equality constraints
                          restricted_pars = NULL, # restricted parameters
                          cor = F, # transform covariances to correlations
                          model = 'regclm' # regclm - random effects GCLM
                                           # reclpm - random effects CLPM

                          ){
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

  Intercept_Var = map(endogeneous, ~  glue("i{.x} ~~ var_i{.x}*i{.x}")) %>%
    glue_collapse("\n")
  Slope_Var = map(endogeneous, ~  glue("s{.x} ~~ var_s{.x}*s{.x}")) %>%
    glue_collapse("\n")
  Intercept_Mean = map(endogeneous, ~  glue("i{.x} ~ mean_i_{.x}*1")) %>%
    glue_collapse("\n")
  Slope_Mean = map(endogeneous, ~  glue("s{.x} ~ mean_s_{.x}*1")) %>%
    glue_collapse("\n")

  A = map(endogeneous, ~ glue("{.x}{1:max_time}") %>%
            glue_collapse(" + ") %>%
            glue(., " ~ 0*1")) %>%
    glue_collapse("\n")

  B = map(endogeneous, ~ glue("e_{.x}{1:max_time}") %>%
            glue_collapse(" + ") %>%
            glue(., " ~ 0*1")) %>%
    glue_collapse("\n")

  # covar

  if(n_var >= 1){
    rand_all = c(paste0('i', endogeneous), paste0('s', endogeneous))
    n_rand = 2*n_var
    rand = expand_grid(x = rand_all,
                       y = rand_all) %>%
      filter(!x == y) %>%
      dplyr::mutate(n = rep(1:(n_rand-1), n_rand)) %>%
      dplyr::mutate(seq = rep(0:(n_rand-1), each = n_rand-1)) %>%
      filter(!seq>=n) %>%
      dplyr::select(x,y)

    Cov_Rand = rand %>%
      dplyr::mutate(cov_is = glue('{x} ~~ cov_{x}.{y}*{y}')) %>%
      pull(cov_is) %>%
      glue_collapse("\n")
  } else {
    Cov_Rand = ''
  }



  Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} =~ 1*{.x}{1:max_time}")%>%
                glue_collapse("\n")) %>%
    glue_collapse("\n")

  Cov_Observed = map(endogeneous, ~  glue("{.x}{1:max_time} ~~ 0*{.x}{1:max_time}")%>%
                       glue_collapse("\n")) %>%
    glue_collapse("\n")

  if (resid_stationary == T){
      if (multiple == T){

    Var_Resid = map(endogeneous, ~  glue("e_{.x}{1:max_time} ~~ evar{.x}*e_{.x}{1:max_time}")%>%
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
    #list = split_list[[1]]
    tib = list
    tib = cbind(cf, list)

    strings = c()
    for (j in 1:(max_time-1)){
      #j=1
      if (multiple == T){
        string_out_vec =  tib %>%
        dplyr::mutate(o = ifelse(cf == 'b_',
                                 glue('c({cf}{y}{z}1,{cf}{y}{z}2)*{z}{j}'),
                                 glue('c({cf}{y}{z}1,{cf}{y}{z}2)*e_{z}{j}'))) %>%
        pull(o)

        #current_var = sub(".*\\*(.*?)\\d[^0-9]*$", "\\1", string_out_vec[2])
        #current_var = substr(string_out_vec,5,6)[1]
        current_var = tib$x[1]


      } else{
        string_out_vec =  tib %>%
          dplyr::mutate(o = ifelse(cf == 'b_',
                                   glue('{cf}{y}{z}*{z}{j}'),
                                   glue('{cf}{y}{z}*e_{z}{j}'))) %>%
          pull(o)

        current_var = tib$x[1]
        #current_var = sub(".*\\*(.*?)\\d[^0-9]*$", "\\1", string_out_vec[2])
        #current_var = substr(string_out_vec,3,4)[1]
      }

      string_out = string_out_vec %>%
        glue_collapse(" + ")

      if(cross == T){

        if(current_var == endogeneous[1]|full ==T){
         strings = c(strings,
                  glue(current_var,
                       j+1, ' ~ ', string_out)) %>%
           glue_collapse("\n")
         } else if(current_var %in% reverse){
           pattern_pos = c(grep(paste0(current_var,current_var), string_out_vec),
                       grep(endogeneous[1], string_out_vec))

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


  ## Cov_Resid

  if (length(endogeneous) > 1){

    ecov = expand_grid(x = endogeneous,
                       y = endogeneous) %>%
      filter(!x == y) %>%
      dplyr::mutate(n = rep(1:(n_var-1), n_var)) %>%
      dplyr::mutate(seq = rep(0:(n_var-1), each = n_var-1)) %>%
      filter(!seq>=n) %>%
      dplyr::select(x,y)

    out_ecov = c()
    for (i in 1:max_time){

      col_name = paste0("ecov", i)

      s = expand_grid(x = endogeneous,
                      y = endogeneous) %>%
        filter(!x == y) %>%
        dplyr::mutate(n = rep(1:(n_var-1), n_var)) %>%
        dplyr::mutate(seq = rep(0:(n_var-1), each = n_var-1)) %>%
        filter(!seq>=n) %>%
        dplyr::select(x,y)

      if (resid_stationary == T){

        if (multiple == T){
        #s %<>%
        #  dplyr::mutate(!!sym(col_name) := glue("e_{x}{i} ~~ c(ecov_{x}{y}1, ecov_{x}{y}2)*e_{y}{i}")
        #  ) %>%
        #  pull(!!sym(col_name))

          s %<>%
            dplyr::mutate(!!sym(col_name) := glue("e_{x}{i} ~~ ecov_{x}{y}*e_{y}{i}")
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

  # all covariances to correlations

  if (cor == T){
    Cor_Resid = ecov %>%
      dplyr::mutate(g = glue("ecor_{x}.{y} := ecov_{x}{y} / (sqrt(evar{x}) * sqrt(evar{y}))"))%>%
      pull(g)%>%
      glue_collapse("\n")

    Cor_Rand = rand %>%
      dplyr::mutate(g = glue("cor_{x}.{y} := cov_{x}.{y} / (sqrt(var_{x}) * sqrt(var_{y}))"))%>%
      pull(g)%>%
      glue_collapse("\n")
  } else{
    Cor_Resid = ''
    Cor_Rand = ''
  }

# Controls
  if (!is.null(control)){
    if(model == 'clpm'){
    Control = c()
    for (i in 1:max_time){

      Control_df = data.frame(c('cntr' = control,
                                data.frame('end' = endogeneous)))
      nam = names(Control_df %>%
                    dplyr::select(starts_with('cntr')))
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
                  dplyr::select(starts_with('cntr')))
      Control_df %<>%
        dplyr::mutate(cntr_sum = ifelse(length(control) >1,
                                        apply(Control_df[ , nam ] , 1 , paste , collapse = " + " ), cntr)) %>%
        dplyr::mutate(int_c = glue("i{end} ~ {cntr_sum}"),
                      slope_c = glue("s{end} ~ {cntr_sum}")) %>%
        dplyr::select(int_c, slope_c)

      Control = c(Control_df$int_c, Control_df$slope_c) %>%
        glue_collapse("\n")
      }
    } else{
      Control = ''
      }

  # Impulses and Past States

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

  # stationary

  #if(stationary == F){
  #  Var_Resid = gsub("\\s\\S*\\*", " ", Var_Resid)
  #}

  # reclpm
  if(model == 'reclpm' ){

    # Split the glue object by line
    Reg_lines <- strsplit(Reg, "\n")[[1]]
    # Remove all terms starting from "b_" and add "e_" in front of each line
    modified_lines = gsub("b_[^+]+", "REMOVED_TERM", Reg_lines, perl = TRUE)
    #modified_lines = gsub("d_[^+]+", "", modified_lines, perl = TRUE)
    modified_lines = gsub("REMOVED_TERM\\+ ", " ", modified_lines)
    modified_lines = gsub("\\s+", " ", modified_lines)
    modified_lines = paste0("e_", modified_lines)
    Reg = glue(paste(modified_lines, collapse = "\n"))
    # remove all letters and symbols between "~" and "c(d"

    if (multiple == T){
      Reg = gsub("~[^d]+d", "~c(d", Reg)
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



                       {Cov_Rand}

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

                       # Correlations between residuals

                       {Cor_Resid}

                       # Correlations between random effects

                       {Cor_Rand}

                       #


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


  # curve

  if(length(no_slopes) > 0){
    # split the string into separate equations
    syntax_rigclm_spl = strsplit(syntax_rigclm, "\n")[[1]]

    # loop through the equations and add a '#' at the beginning if it contains the search character
    for (i in seq_along(syntax_rigclm_spl)) {
      for (no_slope in no_slopes) {
        if (grepl(no_slope, syntax_rigclm_spl[i])) {
          syntax_rigclm_spl[i] = paste0("#", syntax_rigclm_spl[i])
        }
      }
    }
    syntax_rigclm = glue(paste(syntax_rigclm_spl, collapse = "\n"))
  }


  return(syntax_rigclm)

}


# wide data for lavaan

lavaan_df = function(dv,
                     ivs = c('social_care_adult',
                             'social_care_children',
                             'healthcare',
                             'env', 'law_order', 'infrastructure',
                             'public_health'),
                     dv_map = 'HE',
                     ivs_map = c('as', 'cs','hc',
                                 'en', 'lo', 'fr',
                                 'ph'),
                     ids = c('lsoa11',
                             'MSOA11CD',
                             'LAD21CD',
                             'class'),
                     invariant = control_names,
                     deprivation_cat = NULL,
                     other = NULL,
                     time = 'time',
                     max_time = 7,
                     df){

  lookup = setNames(c(dv, ivs), c(dv_map, ivs_map))
  selected = c(dv_map, ivs_map, ids, invariant, deprivation_cat, time, other)

  out = df %>%
    dplyr::rename(all_of(lookup)) %>%
    dplyr::select(all_of(selected)) %>%
    tidyr::pivot_wider(id_cols = all_of(c(ids, invariant, deprivation_cat, other)),
                       names_from = time,
                       values_from = all_of(c(dv_map, ivs_map)),
                       names_sep = '')
  return(out)
}


# cbind.fill
cbind.fill <- function(...){
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

# Extract coefficients for SEM models

# sign fun
sig_fun = function(x){
  case_when(
    x <= 0.001 ~ '***',
    x <= 0.01 & x > 0.001 ~ '**',
    x <= 0.05 & x > 0.01 ~ '*',
    x > 0.05 & x <= 0.1 ~ '^',
    is.na(x) ~ 'l',
    TRUE ~ ''
  )
}

# Extract coefficients
CoefsExtract = function(models = NULL,
                        health = 'HE2',
                        standardized = F,
                        end = paste0(c('HE', 'as', 'cs', 'hc', 'en', 'lo', 'fr'), '1'),
                        impulses = paste0(c('e_HE', 'e_as', 'e_cs', 'e_hc', 'e_en', 'e_lo', 'e_fr'), '1'),
                        growth = NULL,
                        controls = control_names,
                        df_transform = NULL){
  require(data.table)

  m_out = list()
  colnm = c("lhs", "op", "rhs", "group", 'est.std', 'pvalue', 'se')
  tech = c('lhs', 'op', 'rhs')

  m_out = lapply(models, function(model) {

    if (standardized == T){
      stdsol = standardizedSolution(eval(parse(text = model)))

     # main effects - standardized
      stdsol_main = stdsol %>%
        filter(
          op == "~" & rhs %in% end |
          op == "~1" & lhs %in% growth |
          op == "~" & rhs %in% impulses |
          op == "~" & rhs %in% controls #|
          #op == "~~" & rhs %in% impulses & lhs %in% impulses & !rhs==lhs
         ) %>%
        dplyr::select(all_of(intersect(colnames(stdsol), colnm))) %>%
        mutate(id = str_c(lhs, op, rhs)) %>%
        select(-one_of(tech))


    } else{
      # main effects - unstandardized
      stdsol = broom::tidy(eval(parse(text = model)))
      stdsol$term = ifelse(grepl('ecor_|cor_',  stdsol$label),
             str_replace_all(stdsol$label, c('ecor_'='',
                                             'cor_'='')),  stdsol$term)

      stdsol = stdsol %>%
        separate(term, into = c("lhs", "rhs"), sep = " =~ | ~~ | ~1 | ~ |\\.") %>%
        dplyr::rename(est.std = std.all, se = std.error, pvalue = p.value)

      stdsol_main = stdsol %>%
        select(-est.std) %>%
        dplyr::rename(est.std = estimate) %>%
        filter(
          op == "~" & rhs %in% end |
          op == "~1" & lhs %in% growth |
          op == "~" & rhs %in% impulses |
          op == "~" & rhs %in% controls
          ) %>%
        dplyr::select(all_of(intersect(colnames(stdsol), colnm))) %>%
        mutate(id = str_c(lhs, op, rhs)) %>%
        select(-one_of(tech))
    }

    # covariance - unstandardized always (to make correlations)
    stdsol_cov = broom::tidy(eval(parse(text = model)))
    stdsol_cov$term = ifelse(grepl('ecor_|cor_',  stdsol_cov$label),
                         str_replace_all(stdsol_cov$label, c('ecor_'='',
                                                         'cor_'='')),  stdsol_cov$term)

    stdsol_cov = stdsol_cov %>%
      separate(term, into = c("lhs", "rhs"), sep = " =~ | ~~ | ~1 | ~ |\\.") %>%
      dplyr::rename(est.std = std.all, se = std.error, pvalue = p.value)

    stdsol_cov = stdsol_cov %>%
      select(-est.std) %>%
      dplyr::rename(est.std = estimate) %>%
      filter(
          op == "~~" & lhs %in% growth & rhs %in% growth|
          op == "~~" & rhs %in% impulses & lhs %in% impulses|
          op == ":="
      ) %>%
      dplyr::select(all_of(intersect(colnames(stdsol_cov), colnm))) %>%
      mutate(id = str_c(lhs, op, rhs)) %>%
      select(-one_of(tech))

    stdsol_fin = rbind.data.frame(stdsol_main, stdsol_cov)

    # if reclpm_fit, remove 'e_' for the main effects
    stdsol_fin$id <- sapply(stdsol_fin$id, function(s) {
      if (length(gregexpr("e_", s)[[1]]) >= 2 & !grepl('~~', s)) {
        s <- gsub("e_", "", s)
      }
      return(s)
    }
    )

    return(stdsol_fin)

  })

  # curating
  if (any(grepl('group', colnames(m_out[[1]])))){
    BY = c('id', 'group')
  } else{
    BY = 'id'
  }
  coefs_long = m_out %>%
    reduce(full_join, by = BY) %>%
    select(id, everything())

  coefs_long = coefs_long %>%
    mutate_at(vars(contains('pvalue')), sig_fun) %>%
    mutate_all(~ ifelse(is.na(.), "", .))

  coefs_long$id = str_replace(coefs_long$id, "~1", "~#") %>%
    gsub('[[:digit:]]+', '', .)


  end_ = gsub('[[:digit:]]+', '', end)
  impulses_ = gsub('[[:digit:]]+', '', impulses)

  # determining types of effects for further arrangement
  coefs_long = as.data.table(coefs_long)

  impulse_variants = data.frame(t(combn(c(impulses_, impulses_), 2)))%>%
    dplyr::mutate(collapse = glue('{X1}~~{X2}')) %>%
    pull(collapse)

  if (!is.null(growth)) {
    growth_variants <- expand.grid(growth, growth) %>%
      dplyr::mutate(collapse = glue('{Var1}~~{Var2}')) %>%
      pull(collapse)

    coefs_long[, type := case_when(
      id %in% c(paste0(end_, '~', end_), paste0(end_, '~', impulses_)) ~ 'a_auto',
      grepl(':=', id) ~ 'i_cor',
      substr(id, 1, 2) == substr(health, 1, 2) ~ 'c_policy',
      id %in% growth_variants ~ 'e_growth_cov',
      id %in% impulse_variants ~ 'd_impulse_cov',
      grepl('~#', id) ~ 'f_growth_means',
      substr(id, 6, 7) == substr(health, 1, 2) ~ 'b_health',
      substr(id, 4, 5) == substr(health, 1, 2) ~ 'b_health',
      substr(id, 4, nchar(id)) %in% gsub('[[:digit:]]+', '', paste0('~', controls)) ~ 'h_controls',
      TRUE ~ 'g_other_policies'
    )]
  } else {
    coefs_long[, type := case_when(
      id %in% c(paste0(end_, '~', end_), paste0(end_, '~', impulses_)) ~ 'a_auto',
      grepl(':=', id) ~ 'i_cor',
      substr(id, 1, 2) == substr(health, 1, 2) ~ 'c_policy',
      id %in% impulse_variants ~ 'd_impulse_cov',
      substr(id, 6, 7) == substr(health, 1, 2) ~ 'b_health',
      substr(id, 4, 5) == substr(health, 1, 2) ~ 'b_health',
      substr(id, 4, nchar(id)) %in% gsub('[[:digit:]]+', '', paste0('~', controls)) ~ 'h_controls',
      TRUE ~ 'g_other_policies'
    )]
  }

  coefs_long = coefs_long %>%
    arrange(type) %>%
    mutate(
      long_or_short = ifelse(grepl('e_', id) & !grepl('~~', id), 'short', 'long'),
      num = case_when(
        grepl(':=', id) ~ 10,
        grepl('as', substr(id, 1, 2)) ~ 2,
        grepl('cs', substr(id, 1, 2)) ~ 3,
        grepl('hc', substr(id, 1, 2)) ~ 4,
        grepl('en', substr(id, 1, 2)) ~ 5,
        grepl('lo', substr(id, 1, 2)) ~ 6,
        grepl('fr', substr(id, 1, 2)) ~ 7,
        TRUE ~ 1
      ),
      id = sub('e_', '', id),
      type_long_or_short = paste0(type, long_or_short)
    ) %>%
    arrange(type_long_or_short, num)

  # to wide format

  values_from = colnames(coefs_long)[grepl('est|pvalue|se',colnames(coefs_long))]
  ids = colnames(coefs_long)[colnames(coefs_long) %in% c('id', 'group', 'type')]

  coefs_wide = pivot_wider(coefs_long, id_cols = all_of(ids),
                            names_from = long_or_short,
                            values_from = all_of(values_from))

  columns = colnames(coefs_wide)[grep('est.std.', colnames(coefs_wide))]

  # apply transformation

  if (!is.null(df_transform)){
    coefs_wide = coefs_wide %>% left_join(df_transform, by = 'id')
    coefs_wide$ratio = ifelse(is.na(coefs_wide$ratio), 1, coefs_wide$ratio)
  } else{
    coefs_wide$ratio = 1
  }

  # forming the cells

  for (i in columns) {
    colending = sub('est.std', '', i)
    coef =  as.numeric(coefs_wide[[i]])
    signif = coefs_wide[[paste0('pvalue', colending)]]
    se = as.numeric(coefs_wide[[paste0('se', colending)]])

    for (row in seq_along(coef)){
      if(!coefs_wide[[i]][row]==''&!is.na(coefs_wide[[i]][row])){

        if (!is.null(df_transform) & coefs_wide[['type']][row] == 'b_health'){
          coefs_wide[[i]][row] = paste0(sprintf("%.3f", 100*(exp(coefs_wide[row, 'ratio']*coef[row])-1)),
                                        signif[row],
                                        ' [',
                                        sprintf("%.3f", 100*(exp(coefs_wide[row, 'ratio']*(coef[row] - qt(0.95, df = Inf)*se[row]))-1)
                                                ),
                                        '; ',
                                        sprintf("%.3f", 100*(exp(coefs_wide[row, 'ratio']*(coef[row] + qt(0.95, df = Inf)*se[row]))-1)
                                                ),
                                        ']')
        }else{
          coefs_wide[[i]][row] = paste0(sprintf("%.3f", coefs_wide[row, 'ratio']*coef[row]),
                              signif[row],
                              ' [',
                              sprintf("%.3f", coefs_wide[row, 'ratio']*(coef[row] - qt(0.95, df = Inf)*se[row])),
                              '; ',
                              sprintf("%.3f", coefs_wide[row, 'ratio']*(coef[row] + qt(0.95, df = Inf)*se[row])),
                              ']')
        }

        # exp for the effects of health variables on polices (if there is a transformation)

        } else{
        coefs_wide[[i]][row] = ''
      }
    }

  }

  coefs_wide = coefs_wide %>%
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
    get_number = function(x) {
      if (!x == 'id'){
        as.numeric(sub("^.*_(\\d+)$", "\\1", x))
      }
    }
    coefs_wide = coefs_wide[,c('id', names((sort(unlist(sapply(colnames, get_number)),
                                                 decreasing = T))))]

  }



  return(coefs_wide)
}

# creating tables with effects
TableEffects = function(dat = effects_all,
                        .end_new = end_new,
                        .parameters = parameters,
                        param_range = 34:46,
                        section_name_rows = c(1, 8, 14, 20, 27, 34),
                        subsections = T,
                        .section_names = section_names,
                        fit_measures = NULL,
                        patterns = c("~HE|HE~#",
                                     "~as|as~#",
                                     "~cs|cs~#",
                                     "~hc|hc~#",
                                     "~en|en~#",
                                     "~lo|lo~#",
                                     "~fr|fr~#")) {
  library(dplyr)


  dat = dat %>%
    filter(!type %in% c('d_impulse_cov',
                        'e_growth_cov',
                        'g_other_policies',
                        'h_controls',
                        'i_cor')) %>%
    dplyr::mutate(id = reduce(patterns, function(x, y) if_else(grepl(y, x),
                                                        .end_new[match(y, patterns)], x),
                       .init = id)) %>%
    rbind.fill(., fit_measures %>% rownames_to_column("id") %>%
                 dplyr::mutate(id = str_remove(id, ".scaled"))) %>%
    dplyr::mutate(across(1, ~replace(.x, param_range, .parameters)))

  if (subsections == T){
      # subsections in a table
    for (i in rev(section_name_rows)) {
      dat = tibble::add_row(dat, .before = i)
      }
    dat = dat %>%
      dplyr::mutate(across(1, ~replace(.x, is.na(.x), .section_names)))%>%
      dplyr::mutate_all(~ ifelse(is.na(.), "", .)) %>%
      select(-type)
  } else{
    dat = dat %>%
      select(-type)
    }

  return(dat)
}

# Extracting growth curve cor tables
MatrixEffects = function(dat,
                         cor_name,
                         pars,
                         colnames = c(paste0('i', endogeneous),
                                     paste0('s', endogeneous)),
                         rownames = c(paste0('i', endogeneous),
                                      paste0('s', endogeneous)),
                         cor = T,
                         sep = '~~'){

  sgn = dat
  sgn %<>% tidyr::separate(cor_name, c('X', 'Y'), sep)
  cov_mat = function(dat){
      pivot_initial = dat %>%
        group_by(Y, X) %>%
        summarize(across(all_of(pars), identity), .groups = 'drop') %>%
        pivot_wider(names_from = X, values_from = all_of(pars)) %>%
        column_to_rownames(var = "Y")
      pivot = pivot_initial[rownames, colnames]

      return(pivot)
  }

  # Select columns and separate the cor_name column into X and Y columns using '~~' as the separator
  dat = dat %>%
    select(all_of(cor_name), all_of(pars)) %>%
    separate(cor_name, c('X', 'Y'), sep)

  # Calculate the covariance matrix
    pivot = cov_mat(dat)

  if (cor == T){
    dat1 = dat %>%
      mutate_at(vars(all_of(pars)), ~ as.numeric(str_replace_all(., '\\*\\**\\**|\\^|\\[.*\\]', '')))
    dat2 = dat %>%
      mutate_at(vars(all_of(pars)), ~ as.numeric(str_replace_all(., ".*?\\[([-0-9.]+).*", "\\1")))
    dat3 = dat %>%
      mutate_at(vars(all_of(pars)), ~ as.numeric(str_replace_all(., ".*?\\[[-0-9.]+; ([-0-9.]+)\\].*", "\\1")))

    # Calculate the covariance matrix
    cov_mat_cor = function(dat){
      pivot_initial = dat %>%
        group_by(Y, X) %>%
        summarize(across(all_of(pars), identity), .groups = 'drop') %>%
        pivot_wider(names_from = X, values_from = all_of(pars)) %>%
        column_to_rownames(var = "Y")

      pivot = pivot_initial[rownames, colnames]
      pivot = t(pivot)

      # Convert covariance matrix to correlation matrix and remove values in lower triangle
      pivot = as.data.frame(cov2cor(as.matrix(pivot)))
      dimnames(pivot) = list(colnames(pivot), colnames(pivot))
      pivot[] = lapply(pivot, sprintf, fmt = "%.3f")
      pivot[lower.tri(pivot, diag = F)] <- ''

      return(pivot)
    }

    pivot = cov_mat_cor(dat1)
    pivot2 = cov_mat_cor(dat2)
    pivot3 = cov_mat_cor(dat3)

    # Add additional information to the lower triangle of the matrix

    for (i in seq_along(colnames(pivot))){
      for (j in seq_along(colnames(pivot))){
        if (i != j){
          # Combine the correlation value with the additional information from the dat table
          pivot[i, j] = paste0(pivot[i, j], str_replace_all(
            sgn %>% filter(X == colnames(pivot)[i], Y == colnames(pivot)[j]) %>% pull(all_of(pars)),
            c("[^*^]+" = ''
              )), ' [', pivot2[i, j],'; ', pivot3[i, j], ']'
            )
        }
      }
    }

    pivot[pivot == ' [; ]'] <- ''

  }
  return(pivot)
}

# a function to create a head with the long-run and short-run indication

SubHead = function(tab, which_null = NULL, n, colnames,
                   sub_head = c('Long-run', 'Short-run'),
                   sub_head_add = NULL){



  if (!is.null(sub_head_add)){
    head = c('', paste(rep(sub_head, n), sub_head_add))
  } else{
    head = c('', rep(sub_head, n))
  }

  if (!is.null(which_null)){
    head = head[-which_null]
  }

  tab = rbind.data.frame(head, tab)
  colnames(tab) = colnames

  return(tab)
}

# Clean Tables

CiSplit = function(dat, rownm = F){

  # Create a function to extract values outside and inside the brackets
  split_brackets = function(x) {
    if (grepl("\\[", x)) {
      outside <- gsub(" *\\[.*", "", x)
      inside <- gsub(".*\\[(.*?)\\].*", "[\\1]", x)
      return(list(outside = outside, inside = inside))
    } else {
      return(list(outside = x, inside = NA))
    }
  }

  if (rownm == T){
    dat = rownames_to_column(dat, var = "id")
  }

  dat_ = dat
  names(dat_) = c('id', paste0('col', 1:(ncol(dat_)-1)))
  nested_df = dat_ %>%
    mutate(across(-1, ~lapply(., split_brackets), .names = "{col}_"))

  # Unnest the nested dataframe
  df_split = unnest_wider(nested_df, col = starts_with("col"), names_sep = "_")

  # Convert the wide format to a long format
  df_split = df_split %>%
    pivot_longer(
      cols = -starts_with('id'),
      names_to = c(".value", "type"),
      names_pattern = "(.*)_([^_]+)$",
      values_drop_na = F
    )

  # Display the resulting data frame
  df_split %<>% filter(!type == 1) %>% select(id, ends_with("_")) %>%
    mutate(id = ifelse(id == lag(id) & !row_number() == 1, "", id))

  colnames(df_split) = colnames(dat)
  colnames(df_split)[1] = '' # id col

  df_split[is.na(df_split)] = ''
  df_split = df_split[rowSums(df_split == "") != ncol(df_split), ]

  return(df_split)
}

# Compute descriptive statistics

summarize_data = function(dat = df_before_scaling,
                          .stationary = stationary,
                          .nonstationary = nonstationary,
                          group = TRUE,
                          year = 'year',
                          id = 'LAD21CD',
                          stat = list('Mean' = mean,
                                       'SD' = sd),
                          rownames = nm_out,
                          quant = T) {
  require(dplyr)

  vars_used = c(.nonstationary, .stationary)
  stat_names = names(stat)

  dat = dat %>%
    ungroup()

  if(is.null(group)){

  # summary for non stationary
  sumstat = dat %>%
    dplyr::select(all_of(.nonstationary), year) %>%
    group_by(year) %>%
    dplyr::summarise(across(everything(), stat, .names = "{.col}__{.fn}")) %>%
    pivot_longer(-year, names_to = c("variable", "stat"), names_sep = "__") %>%
    pivot_wider(id_cols = c(year, variable), names_from = stat, values_from = value) %>%
    pivot_wider(id_cols = variable, names_from = year, values_from = all_of(stat_names))

  # sort colnames
  colnames = colnames(sumstat)
  get_number = function(x) {
    if (!x == 'variable'){
      as.numeric(sub("^.*_(\\d+)$", "\\1", x))
    }
  }
  sumstat = sumstat[,c('variable', names((sort(unlist(sapply(colnames, get_number))))))]
  sumstat[,-1] = lapply(sumstat[,-1], sprintf, fmt = "%.2f")

  # summary for stationary

  # adjusting for the LAD-based vars - n
  dat_st = dat
  dat_st = dat_st %>%
    group_by(!!sym(id)) %>%
    mutate(n = ifelse(row_number() == 1, n, NA)) %>% ungroup()

  overall = dat_st %>%
    dplyr::select(all_of(vars_used)) %>%
    dplyr::summarise(across(everything(), stat, .names = "{.col}__{.fn}", na.rm=T)) %>%
    pivot_longer(names_to = 'key', values_to = 'value', cols = everything()) %>%
    separate(key, into = c("variable", "stat"), sep = "__") %>%
    pivot_wider(id_cols = variable, names_from = stat, values_from = value)

  overall[,-1] = lapply(overall[,-1], sprintf, fmt = "%.2f")

  # combining
  sumstat_fin <- sumstat %>%
    full_join(overall, by = "variable") %>%
    mutate_all(~ ifelse(is.na(.), "", .)) %>%
    dplyr::select(-variable) %>%
    add_column(`Names` = rownames, .before = 1)

   } else{

  sumstat_fin = dat %>%
    dplyr::select(all_of(vars_used), as.name(group)) %>%
    group_by(!!as.name(group)) %>%
    dplyr::summarise(across(everything(), stat, .names = "{.col}__{.fn}")) %>%
    pivot_longer(-as.name(group), names_to = c("variable", "stat"), names_sep = "__") %>%
    pivot_wider(id_cols = c(as.name(group), variable), names_from = stat, values_from = value) %>%
    pivot_wider(id_cols = variable, names_from = as.name(group), values_from = all_of(stat_names))%>%
    dplyr::select(-variable) %>%
    add_column(`Names` = rownames, .before = 1)

  ending_numbers = as.numeric(gsub(".*_", "", names(sumstat_fin)[-1]))
  ordered_columns = names(sumstat_fin)[-1][order(ending_numbers)]
  sumstat_fin = sumstat_fin[, c('Names', ordered_columns)]

  sumstat_fin[,-1] = lapply(sumstat_fin[,-1], sprintf, fmt = "%.2f")

  year = group
   }

  if (quant == T){

  # if IQR == T
  colnames(sumstat_fin) = sub('_', '', colnames(sumstat_fin))
  suffixes = unique(sub("^Q\\d{2}", "", grep("^Q25", colnames(sumstat_fin), value = T)))

  # Collapse each pair of columns
  for (i in seq_along(suffixes)) {
    q25_col <- paste0("Q25", suffixes[i])
    q75_col <- paste0("Q75", suffixes[i])
    sumstat_fin = sumstat_fin %>%
      mutate(!!q25_col := paste0('[', !!sym(q25_col), ';', !!sym(q75_col), ']')) %>%
      dplyr::select(-!!q75_col) %>% dplyr::rename(!!!setNames(q25_col, paste0('IQR',suffixes[i])))
  }

  stat_names = c(stat_names[!stat_names %in% c("Q25", "Q75")], 'IQR')
  sumstat_fin %<>%
    mutate_all(~ ifelse(. == '[;]', "", .))
  }

  # adding names
  colnames(sumstat_fin)[1] <- ""
  dat = as.data.frame(dat)
  year_vec = as.vector(unique(dat[,year]))
  blank_rep = rep('', (length(stat_names)-1))
  colnames(sumstat_fin) <- c('', c(sapply(year_vec, function(.) c(., blank_rep))),
                             'Total', blank_rep)



  stat_head = data.frame(matrix(nrow = 1, ncol = ncol(sumstat_fin)))
  colnames(stat_head) = colnames(sumstat_fin)

  if (!is.null(group)){
  stat_head[1,] <- c('', rep(stat_names, (length(year_vec))))
  } else{
    stat_head[1,] <- c('', rep(stat_names, (length(year_vec) + 1)))
    }

  sumstat_fin = rbind(stat_head, sumstat_fin)

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
    detrended[[which(vars == i)]] = left_join(moderndive::get_regression_points(lm(as.formula(paste(i, '~ id*time')), data)) %>%
                                                dplyr::select(id, time, residual),
                                              base::unique(moderndive::get_regression_points(lm(as.formula(paste(i, '~ id')), data)) %>%
                                                       dplyr::select(id, grep('hat', colnames(.)))), by = 'id') %>%
      dplyr::rowwise() %>%
      dplyr::mutate(!!paste0(i, '') :=  sum(c(!!as.name(paste0(i, '_hat')), residual))) %>%
      dplyr::select(c(id, time, !!paste0(i, '')))
  print(i)
  }

  out = purrr::reduce(detrended, left_join)
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

