
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

library(gridExtra)
library(cowplot)
library(hrbrthemes)
library(RColorBrewer)

library(furrr)
library(dplyr)
library(progressr)
library(ff)

#options(max.print=3900)
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_lv.RDS')
df_lv %<>% rename(!!!setNames(c('LAD21CD', paste0("as", 1:7)),
                              c('Group', paste0("SP", 1:7))))

## extracting parameters 
flattenList = function(x, depth = 0, name = NULL, lab = F, k = 1) {
  res <- list()
  for (i in seq_along(x)) {
    el <- x[[i]]
    if (lab == T){
      el$label = rownames(el)
      rownames(el) = NULL
    }
    newName <- if (is.null(name)) names(x)[i] else paste(name, names(x)[i], sep = "_")
    if (is.list(el) && depth < k) {
      res <- c(res, flattenList(el, depth + 1, newName, lab = F))
    } else {
      res[[newName]] <- el
    }
  }
  return(res)
}

t = 7

generate_matrix = function(diag_val, dim = t) {
  # Initialize an empty matrix
  mat <- matrix(0, nrow=dim, ncol=dim)
  
  set.seed(1234)
  
  # Fill in the matrix
  for(i in 1:dim) {
    for(j in 1:dim) {
      mat[i, j] <- diag_val + (i - j) * rnorm(100, mean = 0.01, sd = 0.01)[i]
    }
  }
  
  # Return the matrix
  return(mat)
}

generate_autocorr_mat = function(n = t, k) {
  # Initialize an n x n matrix filled with zeros
  mat <- matrix(0, ncol=n, nrow=n)
  
  # Loop through each row and column
  for(i in 1:n) {
    for(j in 1:n) {
      if(i == j) {
        mat[i, j] <- 1
      } else {
        # Calculate the autocorrelation based on distance
        mat[i, j] <- 1 - k* abs(i - j)
      }
    }
  }
  
  return(mat)
}

fit_sim_hlm = function(dat){
  
  model = lme4::lmer(HE ~ SP + time + (1|Group) + (1|id),
                     data = dat, REML = FALSE)
  out_list = list(coef = summary(model)[["coefficients"]][,'Estimate'],
                  se = summary(model)[["coefficients"]][,'Std. Error'],
                  converged = TRUE)
  
  return(out_list)
}

syntax_sim_rcgclm = RC_GCLM_syntax(endogeneous = c('HE', 'SP'), control = NULL)
fit_sim_rcgclm = sem(syntax_sim_rcgclm,
                     data = df_lv,
                     estimator = "mlr",
                     cluster = 'Group',
                     orthogonal = T)
syntax_sim_rcclpm = RC_GCLM_syntax(endogeneous = c('HE', 'SP'), control = NULL, 
                                   model = 'reclpm')
fit_sim_rcclpm = sem(syntax_sim_rcclpm,
                     data = df_lv,
                     estimator = "mlr",
                     cluster = 'Group',
                     orthogonal = T)

syntax_sim_rcgclm_norev = RC_GCLM_syntax(endogeneous = c('HE', 'SP'), control = NULL, full = F)
fit_sim_rcgclm_norev = sem(syntax_sim_rcgclm_norev,
                           data = df_lv,
                           estimator = "mlr",
                           cluster = 'Group',
                           orthogonal = T)

# for the between only
fit_sim_hlm_bw = function(dat){
  
  model = lme4::lmer(HE ~ SP + time + (1|id),
                     data = dat, REML = FALSE)
  out_list = list(coef = summary(model)[["coefficients"]][,'Estimate'],
                  se = summary(model)[["coefficients"]][,'Std. Error'],
                  converged = TRUE)
  
  return(out_list)
}


ToLong = function(dat,
                  all_ids = c("Group", "id", "sample_index", ".id")){
  
  setDT(dat)
  
  # Generate id column
  dat[, id := .I]
  
  # Identify columns to melt
  melt_cols = setdiff(names(dat), all_ids)
  
  dat = melt(dat, 
             id.vars = all_ids,
             measure.vars = melt_cols,
             variable.name = "time",
             value.name = "value")
  
  # Split the time column into value and time parts using regex
  dat[, c("var_name", "time") := tstrsplit(time, "(?<=\\D)(?=\\d)", perl=TRUE)]
  
  # Extract numeric part from 'time' and convert to numeric
  dat[, time := as.numeric(sub("\\D", "", time))]
  
  dat = dcast(dat, ... ~ var_name, value.var = "value")
  
  
  return(dat)
}

##
#dat[, c('w', 'b', 'et') := tstrsplit(.id, '_')]
#dat[, c("wb", "w", "b") := list(paste0(w, '_', b), NULL, NULL)]

# define correlation matrices

bg_value = c(-0.8, -0.4, 0, 0.4, 0.8)
wg_value = c(-0.8, -0.4, 0, 0.4, 0.8)
eta_range = c(0.4, 0.6, 0.75, 0.9)

bw_grid = expand.grid(wg_value, bg_value)
colnames(bw_grid) = c('Within', 'Between')
bw_grid %<>% mutate(bwratio = as.character(Within/Between)) %>%
  group_by(bwratio) %>% #slice(1) %>%
  mutate(bwratio = round(as.numeric(bwratio),1)) %>%
  as.data.frame %>% group_by(bwratio) %>%
  mutate(id_ratio = 1:n()) %>% ungroup() %>%
  as.data.frame %>%
  slice(rep(1:n(), each=length(eta_range)))
bw_grid$eta = rep(eta_range, length(bg_value)*length(wg_value))


list_wb_comb = list()
for(i in 1:nrow(bw_grid)){
  
  # between group
  xx_bg = generate_autocorr_mat(k = 0.03)
  
  yy_bg = generate_autocorr_mat(k = 0.03)
  
  xy_bg = matrix(data = bw_grid[i,2], nrow = t, ncol = t)
  #xy_bg = generate_matrix(bw_grid[i,2])
  yx_bg = t(xy_bg)
  
  rbg = rbind(cbind(xx_bg, xy_bg), cbind(yx_bg, yy_bg))
  
  # within group
  xx_wg = generate_autocorr_mat(k = 0.03)
  
  yy_wg = generate_autocorr_mat(k = 0.03)
  
  xy_wg = matrix(data = bw_grid[i,1], nrow = t, ncol = t)
  #xy_wg = generate_matrix(bw_grid[i,1])
  yx_wg = t(xy_wg)
  
  rwg = rbind(cbind(xx_wg, xy_wg), cbind(yx_wg, yy_wg))
  
  list_wb_comb[[i]] = list(rbg, rwg, c(rep(0.75, t),
                                       rep(bw_grid[i,'eta'], t)))
}

# generating data

no_cores = 14

wd = 'D:/paper3_output_2'
simulate_multilevel_data = function(t,
                                    n_groups,
                                    group_size,
                                    n_samples,
                                    list_wb_comb,
                                    long,
                                    wd = wd) {
  
  plan(multicore, workers = no_cores)
  n_comb = length(list_wb_comb)
  
  # Using progress
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    future_map(1:n_comb,
               .progress = T,
               .options = furrr_options(seed = TRUE),
               function(i) {
                 
                 set.seed(12345)
                 
                 sample_data = as.data.frame(psych::sim.multilevel(
                   nvar = 2*t,
                   ncases = n_groups*group_size*n_samples,
                   ngroups = n_groups,
                   rbg = list_wb_comb[[i]][[1]],
                   rwg = list_wb_comb[[i]][[2]],
                   eta = list_wb_comb[[i]][[3]])$xy)
                 colnames(sample_data) = c('Group',
                                           paste0('HE', 1:t),
                                           paste0('SP', 1:t))
                 
                 set.seed(12345)
                 
                 sample_data %<>% group_by(Group) %>%
                   mutate(sample_index = sample(rep(1:1000, each = n()/1000))) %>%
                   ungroup()
                 
                 if (long){
                   sample_data = ToLong(sample_data,
                                        all_ids = c("Group", "id", "sample_index"))
                 }
                 
                 p(sprintf("Combination %g", i)) 
                 
                 return(saveRDS(sample_data, file = paste0(wd, '/sim_data_', i, '.rds')))                           
                 })
  }) 
  
}

transform_multilevel_data = function(list_data,
                                      aggregate = FALSE,
                                      hlm = FALSE) {
  
  plan(multicore, workers = no_cores)
  n_comb = length(list_data)
  
  # Using progress
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    data_list <- future_map(1:n_comb, .progress = T, .options = furrr_options(seed = TRUE), function(i) {
      
      dat = list_data[[i]]
      n_samples = length(dat[[1]])
      
      for (et in seq_along(dat)) {
        dat[[et]] = lapply(1:n_samples, function(j) {
          
          sample_data = dat[[et]][[j]]
          
          if (aggregate) {
            sample_data = sample_data %>% 
              group_by(Group) %>% 
              mutate(across(starts_with('S'), mean))
          }
          
          if (hlm) {
            sample_data = ToWide(sample_data)
          }
          
          sample_data
        })
      }
      
      names(dat) = eta_range
      p(sprintf("Combination %g", i)) 
      
      return(dat)
    })
  }) 
  
  data_list
}

## APPLYING
#tic()
#simulate_multilevel_data(t = t,
#                         n_groups = 50,
#                         group_size = 50,
#                         n_samples = 1000,
#                         list_wb_comb = list_wb_comb)
#toc()

wd_long = 'D:/paper3_output_2_long'
tic()
simulate_multilevel_data(t = t,
                         n_groups = 50,
                         group_size = 50,
                         n_samples = 1000,
                         list_wb_comb = list_wb_comb,
                         long = T,
                         wd = wd_long)
toc()

## load the data

load_and_process_files = function(wd) {

  # Set up the future plan
  plan(multisession, workers = 14)
  
  # Get the list of .rds files in the specified directory
  rds_files = list.files(path = wd, pattern = "\\.rds$", full.names = TRUE)
  
  # Process each .rds file and collect results
  with_progress({
    p = progressor(along = 1:length(rds_files))
    
    data_sim = future_map(rds_files, 
                          .options = furrr_options(seed = TRUE),
                          function(file) {
                            loaded_objs = readRDS(file)
                            gc()
                            p(sprintf("Combination %s", file))
                            return(as.ffdf(loaded_objs))
                          })
  })
  
  return(data_sim)
}


# apply
#data_sim = load_and_process_files(wd = wd)

gc()
data_sim_long = load_and_process_files(wd = wd_long)

# saving
saveRDS(data_sim, file = paste0('D:', '/data_sim.rds'))
saveRDS(data_sim_long, file = paste0('D:', '/data_sim_long.rds'))
rm(data_sim)
rm(data_sim_long)

# loading
data_sim = readRDS(paste0('D:', '/data_sim.rds'))
data_sim_long = readRDS(paste0('D:', '/data_sim_long.rds'))

# arrange bw_grid
rds_files = list.files(path = wd, pattern = "\\.rds$", full.names = TRUE)
numbers = as.numeric(sub(".*/sim_data_([0-9]+)\\.rds", "\\1", rds_files))

bw_grid_ordered = bw_grid %>%
  rownames_to_column('row_id') 
bw_grid_ordered = bw_grid_ordered[match(numbers, bw_grid_ordered$row_id), ]

nms = paste0(bw_grid_ordered[,'Within'], '_',
             bw_grid_ordered[,'Between'], '_',
             bw_grid_ordered[,'eta'])
names(data_sim) = nms
names(data_sim_long) = nms

bw_grid_ordered_ = bw_grid_ordered %>% group_by(Between, eta) %>%
  slice(1)

#pb <- progress::progress_bar$new(total = length(data_sim[1]), format = "[:bar] :percent")
#dt1 <- rbindlist(
#  lapply(data_sim[1], function(x) {
#    pb$tick()
#    return(as.data.table(x))
#  }),  idcol = TRUE
#)

### RUNNING


simsem_pipeline = function(dat = data_sim,
                           aggregate = 'none',
                           model_name,
                           hlm = T){
  
  
  
  if (aggregate == 'XY'){
    iter = as.numeric(bw_grid_ordered_$row_id)
  } else{
    iter = seq_along(dat)
  }
  
  results = vector(mode = 'list', length = length(iter))
  
  for (i in iter){
    
    dt1 = rbindlist(lapply(dat[i], as.data.table), idcol = TRUE)
    
   # if (hlm) dt1 = ToLong(dt1)
    
    if(!aggregate == 'none'){
      
      if (aggregate == 'XY') cols = grep("^S|^H", names(dt1))
      
      if (aggregate == 'X') cols = grep("^S", names(dt1))
      
      dt1[, (cols) := lapply(.SD, mean), by = c("Group", ".id", "sample_index", if(hlm) "time"), .SDcols = cols]
      
      #if (aggregate == 'XY') dt1 = dt1[, .SD[1], by = .(Group, .id, sample_index, if(hlm) time)]
    }
    
    dt1 = split(dt1, by = "sample_index")
    
    res = simsem::sim(seed = 12345, 
                      model = model_name,
                      rawData = dt1,
                      multicore = T,
                      numProc = 14)

    gc()
    results[[i]] = res
    print(i)
  }
  
  return(results)
  
}
#summaryParam(res)

## HLM

tic()
results_true_hlm = simsem_pipeline(aggregate = 'none',
                                   model_name = fit_sim_hlm)
toc()

gc()

tic()
results_missp_hlm = simsem_pipeline(aggregate = 'X',
                                    model_name = fit_sim_hlm)
toc()


## RCGCLM
tic()
results_true_rcgclm = simsem_pipeline(aggregate = 'none',
                                      model_name = fit_sim_rcgclm,
                                      hlm = F)
toc()

gc()

tic()
results_missp_rcgclm = simsem_pipeline(aggregate = 'X',
                                       model_name = fit_sim_rcgclm,
                                       hlm = F)
toc()

## RCCLPM
tic()
results_true_rcclpm = simsem_pipeline(aggregate = 'none',
                                      model_name = fit_sim_rcclpm,
                                      hlm = F)
toc()

gc()

tic()
results_missp_rcclpm = simsem_pipeline(aggregate = 'X',
                                       model_name = fit_sim_rcclpm,
                                       hlm = F)
toc()

## Totally between-level models

tic()
results_bw_hlm = simsem_pipeline(aggregate = 'XY',
                                 model_name = fit_sim_hlm_bw,
                                 hlm = T)
toc()

gc()

tic()
results_bw_rcgclm = simsem_pipeline(aggregate = 'XY',
                                    model_name = fit_sim_rcgclm,
                                    hlm = F)
toc()

gc()

tic()
results_bw_rcclpm = simsem_pipeline(aggregate = 'XY',
                                    model_name = fit_sim_rcclpm,
                                    hlm = F)
toc()

# test no reverse effect from health to spending

tic()
results_true_rcgclm_norev = simsem_pipeline(aggregate = 'none',
                                            model_name = fit_sim_rcgclm_norev,
                                            hlm = F)
toc()

gc()

tic()
results_missp_rcgclm_norev = simsem_pipeline(aggregate = 'X',
                                             model_name = fit_sim_rcgclm_norev,
                                             hlm = F)
toc()


outwd = 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper3'
objects <- c("results_true_hlm",
             "results_missp_hlm",
  
             "results_true_rcgclm", 
             "results_missp_rcgclm",
  
             "results_true_rcclpm", 
             "results_missp_rcclpm",
  
             "results_bw_hlm",
             "results_bw_rcgclm",
             "results_bw_rcclpm"
  
             #'results_true_rcgclm_norev',
             #'results_missp_rcgclm_norev'
  
             )

# Loop over the objects and save/load them
for (obj in objects) {
  saveRDS(get(obj), file = paste0(outwd, '/', obj, '.rds'))
}

for (obj in objects) {
  assign(obj, readRDS(file = paste0(outwd, '/', obj, '.rds')))
}

## plotting
reg_pars_rcgclm = c('b_HESP <- (HE2~SP1)',
                    'b_SPHE <- (SP2~HE1)',
                    'b_HEHE <- (HE2~HE1)',
                    'b_SPSP <- (SP2~SP1)',
                    
                    'd_HESP <- (HE2~e_SP1)',
                    'd_SPHE <- (SP2~e_HE1)',
                    'd_HEHE <- (HE2~e_HE1)',
                    'd_SPSP <- (SP2~e_SP1)')
effect_names_rcgclm = c('long_cross_SP',
                        'long_cross_HE',
                        'long_auto_HE',
                        'long_auto_SP',
                        
                        'short_cross_SP',
                        'short_cross_HE',
                        'short_auto_HE',
                        'short_auto_SP')
enm_rcgclm = cbind.data.frame(reg_pars_rcgclm, effect_names_rcgclm)

reg_pars_rcclpm = c('d_HESP <- (e_HE2~e_SP1)',
                    'd_SPHE <- (e_SP2~e_HE1)',
                    'd_HEHE <- (e_HE2~e_HE1)',
                    'd_SPSP <- (e_SP2~e_SP1)')
effect_names_rcclpm = c('cross_SP',
                        'cross_HE',
                        'auto_HE',
                        'auto_SP')
enm_rcclpm = cbind.data.frame(reg_pars_rcclpm, effect_names_rcclpm)

reg_pars_hlm = c('(Intercept)', 'SP', 'time')
effect_names_hlm = c('Int', 'SP', 'time')

enm_rcgclm = cbind.data.frame(reg_pars = reg_pars_rcgclm, effect_names = effect_names_rcgclm)
enm_rcclpm = cbind.data.frame(reg_pars = reg_pars_rcclpm, effect_names = effect_names_rcclpm)
enm_hlm = cbind.data.frame(reg_pars = reg_pars_hlm, effect_names = effect_names_hlm)


extract_simsem_params = function(results_list,
                                 enm_df,
                                 suffix){
  
  out = lapply(results_list,  summaryParam)
  out = lapply(out, function(lst){
    lst = na.omit(lst[enm_df$reg_pars, ] )%>%
      rownames_to_column('reg_pars') %>%
      left_join(enm_df, by = 'reg_pars')
    
  })
  out = do.call(rbind.data.frame, out)
  
  out %<>% mutate(upper = `Estimate Average` + 1.96*`Average SE`,
                  lower = `Estimate Average` - 1.96*`Average SE`)
  colnames(out) = paste0(colnames(out), suffix)
  return(out)
}

# extracting params
params_true_hlm = extract_simsem_params(results_true_hlm, enm_hlm, '.true')
params_missp_hlm = extract_simsem_params(results_missp_hlm, enm_hlm, '.missp')

params_true_rcgclm = extract_simsem_params(results_true_rcgclm, enm_rcgclm, '.true')
params_missp_rcgclm = extract_simsem_params(results_missp_rcgclm, enm_rcgclm, '.missp')

params_true_rcclpm = extract_simsem_params(results_true_rcclpm, enm_rcclpm, '.true')
params_missp_rcclpm = extract_simsem_params(results_missp_rcclpm, enm_rcclpm, '.missp')

#params_true_rcgclm_norev = extract_simsem_params(results_true_rcgclm_norev, enm_rcgclm, '.true')
#params_missp_rcgclm_norev = extract_simsem_params(results_missp_rcgclm_norev, enm_rcgclm, '.missp')

# between
params_bw_hlm = extract_simsem_params(Filter(function(x) !is.null(x), results_bw_hlm), enm_hlm, '.bw')
params_bw_rcgclm = extract_simsem_params(Filter(function(x) !is.null(x), results_bw_rcgclm), enm_rcgclm, '.bw')
params_bw_rcclpm = extract_simsem_params(Filter(function(x) !is.null(x), results_bw_rcclpm), enm_rcclpm, '.bw')

facet_titles = c("Eta = 0.4, ICC = 0.96",
                  "Eta = 0.6, ICC = 0.75",
                  "Eta = 0.75, ICC = 0.40",
                  "Eta = 0.9, ICC = 0.05")

facet_titles = setNames(c("Eta = 0.4, ICC = 0.96",
                           "Eta = 0.6, ICC = 0.75",
                           "Eta = 0.75, ICC = 0.40",
                           "Eta = 0.9, ICC = 0.05"), 
                         c("0.4", "0.6", "0.75", "0.9"))


plot_simsem = function(true = params_true_rcgclm,
                       missp = params_missp_rcgclm,
                       bw = params_bw_rcgclm,
                       grid = bw_grid_ordered,
                       grid_bw = bw_grid_ordered_,
                       plot = T,
                       limits = c(-80, 80),
                       effect_to_plot,
                       bias = 'bias_rel',
                       title = 'RC-GCLM: Long-Run'){
  
  params_bw = cbind.data.frame(bw,
                               grid_bw[,c('Between', 'eta')] %>% slice(rep(1:n(),
                                                   each = length(table(bw[,1])))))
  
  params_all = cbind.data.frame(true, missp[-1],
                                grid %>% slice(rep(1:n(),
                                                   each = length(table(true[,1]))))) %>%
    left_join(params_bw, by = c('Between', 'eta', 'reg_pars.true' = 'reg_pars.bw'))
  
  params_all %<>% 
    dplyr::mutate(bias_rel = (`Estimate Average.true` - `Estimate Average.missp`)/`Estimate Average.true`,
                  bias_abs = `Estimate Average.missp` - `Estimate Average.true`,
                  bias_rel2 = `Estimate Average.missp`/`Estimate Average.true`,
                  bias_rel_bw = `Estimate Average.bw`/`Estimate Average.true`,
                  bias_upper = upper.missp - upper.true,
                  bias_lower = lower.missp - lower.true)
  
  ## plotting
  
  p = ggplot(params_all %>%
           dplyr::filter(effect_names.true == effect_to_plot),
         aes(x = as.factor(Between), y = as.factor(Within), fill = .data[[bias]])) + 
    geom_tile(color = "black", size = 0.3) +
    
    # Add geom_text to overlay the bias value on each tile
    geom_text(aes(label = sprintf("%.2f", .data[[bias]])), size = 4, color = "black") +
    
    scale_fill_gradient2(low = brewer.pal(11, "RdBu")[1], # Red
                         high = brewer.pal(11, "RdBu")[11], # Blue
                         mid = "white", # White
                         midpoint = 0,
                         name = "Relative Bias",
                         limits = limits) +
    facet_wrap(~ eta, labeller = as_labeller(facet_titles)) +
    labs(x = "Between Correlation", y = "Within Correlation") +
    theme_ipsum_tw() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 24))+
    labs(title = title)
  
  
  if(plot){
    return(p)
  }else{
    return(params_all)
  }
}


plot_simsem(effect_to_plot = 'long_cross_SP',
            bias = 'bias_rel',
            title = 'RC-GCLM: Long-Run')
ggsave(paste0(outwd, "/bias_rel_rcgclm_long.jpeg"), width = 25, height = 18, units = 'cm') 

plot_simsem(effect_to_plot = 'short_cross_SP',
            bias = 'bias_rel',
            title = 'RC-GCLM: Short-Run')
ggsave(paste0(outwd, "/bias_rel_rcgclm_short.jpeg"), width = 25, height = 18, units = 'cm') 

plot_simsem(true = params_true_rcclpm,
            missp = params_missp_rcclpm,
            effect_to_plot = 'cross_SP',
            bias = 'bias_rel',
            title = 'RC-CLPM')
ggsave(paste0(outwd, "/bias_rel_rcclpm.jpeg"), width = 25, height = 18, units = 'cm') 

plot_simsem(true = params_true_hlm,
            missp = params_missp_hlm,
            effect_to_plot = 'SP',
            bias = 'bias_rel',
            title = 'Multilevel Model')
ggsave(paste0(outwd, "/bias_rel_hlm.jpeg"), width = 25, height = 18, units = 'cm') 

## bias relative to a between model
#plot_simsem(effect_to_plot = 'long_cross_SP', bias = 'bias_rel_bw')
#plot_simsem(effect_to_plot = 'short_cross_SP', bias = 'bias_rel_bw')
#plot_simsem(true = params_true_rcclpm,
#            missp = params_missp_rcclpm,
#            bw = params_bw_rcclpm,
#            effect_to_plot = 'cross_SP',
#            bias = 'bias_rel_bw')
#plot_simsem(true = params_true_hlm,
#            missp = params_missp_hlm,
#            bw = params_bw_hlm,
#            effect_to_plot = 'SP',
#            bias = 'bias_rel_bw')
#
### norev
#plot_simsem(true = params_true_rcgclm_norev,
#            missp = params_missp_rcgclm_norev,
#            bw = params_bw_rcgclm,
#            effect_to_plot = 'long_cross_SP',
#            bias = 'bias_rel2')




##
sub = as.data.table(data_sim[["0_0.4_0.4"]]) 
sub %<>% filter(sample_index %in% 1)

stat1 = psych::statsBy(sub %>% filter(sample_index == 1) %>%
                         dplyr::select(-sample_index) , 'Group')
stat1$rbg[8:14,1:7]
stat1$rwg[8:14,1:7]
stat1$etawg
stat1$etabg

stat1$ICC1
stat1$ICC2

list_wb_comb[[1]]
etabg <- sqrt(1-0.9^2)
cor_sub = cor(sub[,-1])



sub2 = sub %>%
  group_by(Group) %>%
  mutate(across(starts_with('SP'), 
                function(x) mean(x)))
sub3 = sub %>%
  group_by(Group) %>%
  dplyr::summarise(across(starts_with('SP')|starts_with('HE'), 
                function(x) mean(x)))
cor(sub$SP1, sub$HE1)

sd(sub3$SP1)

within_cor = sub %>%
  group_by(Group) %>%
  dplyr::summarise(cor = cor(SP1, HE1),
                   var = var(SP1))
mean(within_cor$cor)

cor(sub3$SP1, sub3$HE1)
cor(sub2$SP1, sub2$HE1)

var(sub$SP1)
mean(within_cor$var)
var(sub2$SP1)
var(sub3$SP1)

eta = 0.85
etabg=sqrt(1-eta^2)
eta^2
etabg^2


df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_lv.RDS')
df_lv %<>% rename(!!!setNames(paste0("as", 1:7), paste0("SP", 1:7)))

df_lv_sub = df_lv %>% select(LAD21CD,starts_with('HE'), starts_with('SP')
                             )
stat_df_lv = psych::statsBy(df_lv_sub , 'LAD21CD')
stat_df_lv$rbg#[8:14,1:7]
stat_df_lv$rwg#[8:14,1:7]
stat_df_lv$etawg
stat_df_lv$etabg
stat_df_lv$ICC1
stat_df_lv$ICC2

df_lv_sub =  df_lv_sub %>%
    ungroup() %>% 
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -c(LAD21CD, id),
                 names_to = c(".value", "time"),
                 names_pattern = "(\\w+)(\\d+)")
df_lv_sub$time = as.numeric(df_lv_sub$time)
m1 = lme4::lmer(HE ~ SP + time + (1|LAD21CD) + (1|id),
                data = df_lv_sub, REML = F)
performance::icc(m1, by_group = T)
## ICC by Group
#
#Group   |   ICC
#---------------
#  id    | 0.541
#LAD21CD | 0.399


## 

params_all %>% 
  dplyr::filter(eta == '0.9' &# Within == 0 & 
                  effect_names.true == 'SP') %>%
  select(bias_rel, `Estimate Average.true`, `Estimate Average.missp`, Between, Within)










library(viridis)

sub = as.data.table(data_sim[["-0.8_0.8_0.4"]]) 
sub %<>% filter(sample_index %in% 3)
sub_long = ToLong(sub,
                  all_ids = c("Group", "id", "sample_index"))
sub_long %<>% group_by(id) %>%
  mutate(lag = lag(SP))

sub_long2 = sub_long %>%
  group_by(Group, time) %>%
  dplyr::mutate(across(starts_with('SP'), 
                          function(x) mean(x)))

ggplot(sub_long %>% filter(Group %in% c(5:9) #& 
  #id %in% 1:50
  ),
  aes(x = SP, y = HE)) +
  geom_point(aes(colour = as.factor(Group)), size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  geom_smooth(aes(x = SP, y = HE,
                  colour = time,
                  group = id),
              method = "lm", se = FALSE,
              color = 'grey',
              linewidth = 0.6)+
  theme_minimal() + ylim(-2.5,2.5)

ggplot(sub_long %>% filter(#Group %in% c(5:6) & 
  id %in% 1:50),
  aes(x = lag, y = HE)) +
  geom_point(aes(colour = as.factor(time)), size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  geom_smooth(aes(x = lag, y = HE,
                  colour = time,
                  group = id),
              method = "lm", se = FALSE,
              color = 'grey',
              linewidth = 0.6)+
  theme_minimal() + ylim(-2.5,2.5)


ggplot(sub_long2 %>% filter(Group %in% c(5) ),
       aes(x = time, y = SP)) +
  geom_point(aes(colour = as.factor(Group)), size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  geom_smooth(aes(x = time, y = SP,
                  colour = time,
                  group = id),
              method = "lm", se = FALSE,
              color = 'grey',
              linewidth = 0.6)+
  theme_minimal() + ylim(-2.5,2.5)

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