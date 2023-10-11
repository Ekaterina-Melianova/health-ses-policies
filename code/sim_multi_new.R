
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


library(furrr)
library(dplyr)
library(progressr)
library(ff)

#options(max.print=3900)
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

source('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/code/functions.R')
df_lv = readRDS('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/data/df_lv.RDS')
df_lv %<>% rename(!!!setNames(paste0("as", 1:7), paste0("SP", 1:7)))

## extracting parameters 
flattenList <- function(x, depth = 0, name = NULL, lab = F, k = 1) {
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

generate_matrix <- function(diag_val, dim = t) {
  # Initialize an empty matrix
  mat <- matrix(0, nrow=dim, ncol=dim)
  
  # Fill in the matrix
  for(i in 1:dim) {
    for(j in 1:dim) {
      mat[i, j] <- diag_val + (i - j) * rnorm(100, mean = 0.01, sd = 0.01)[i]
    }
  }
  
  # Return the matrix
  return(mat)
}

generate_autocorr_mat <- function(n = t, k) {
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

syntax_sim = RC_GCLM_syntax(endogeneous = c('HE', 'SP'), control = NULL)
fit_sim_rcgclm = sem(syntax_sim,
                     data = df_lv,
                     estimator = "mlr",
                     cluster = 'LAD21CD',
                     orthogonal = T)

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
eta_range = seq(0, 0.9, 0.3)
eta_list = list()
for (i in seq_along(eta_range)){
  eta_list[[i]] = rep(eta_range[i], 2*t)
}

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
  xx_bg = generate_autocorr_mat(k = 0.05)
  
  yy_bg = generate_autocorr_mat(k = 0.05)
  
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
  
  list_wb_comb[[i]] = list(rbg, rwg, rep(bw_grid[i,'eta'], 2*t))
}

# generating data

#n_samp = 10

no_cores = 12

wd = 'D:/paper3_output'
simulate_multilevel_data = function(t,
                                    n_groups,
                                    group_size,
                                    n_samples,
                                    list_wb_comb) {
  
  plan(multicore, workers = no_cores)
  n_comb = length(list_wb_comb)
  
  # Using progress
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    future_map(1:n_comb,
               .progress = T,
               .options = furrr_options(seed = TRUE),
               function(i) {
                 
                 set.seed(1234)
                 
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
                 
                 sample_data %<>% group_by(Group) %>%
                   mutate(sample_index = sample(rep(1:1000, each = n()/1000))) %>%
                   ungroup()
                 
                 p(sprintf("Combination %g", i)) 
                 
                 return(save(sample_data, file = paste0(wd, '/sim_data_', i, '.Rdata')))                           })
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
simulate_multilevel_data(t = t,
                         n_groups = 50,
                         group_size = 50,
                         n_samples = 1000,
                         list_wb_comb = list_wb_comb)

## load the data
plan(multisession, workers = 12)
rdata_files = list.files(path = wd, pattern = "\\.Rdata$", full.names = TRUE)
with_progress({
  
  p = progressor(along = 1:length(rdata_files))
  
  data_sim = future_map(rdata_files, 
                        .options = furrr_options(seed = TRUE),
                        function(file) {
    
    loaded_objs = new.env()
    load(file, envir = loaded_objs)
    
    gc()
    
    p(sprintf("Combination %s", file)) 
    
    return(as.ffdf(as.list(loaded_objs)[[1]]))
    })
}) 
beepr::beep()
#save(data_sim, file = paste0('D:', '/data_sim.Rdata'))

load(paste0('D:', '/data_sim.Rdata'))
numbers <- as.numeric(sub(".*/sim_data_([0-9]+)\\.Rdata", "\\1", rdata_files))


names(data_sim) = paste0(bw_grid[,'Within'], '_',
                         bw_grid[,'Between'], '_',
                         bw_grid[,'eta'])


#pb <- progress::progress_bar$new(total = length(data_sim[1]), format = "[:bar] :percent")
#dt1 <- rbindlist(
#  lapply(data_sim[1], function(x) {
#    pb$tick()
#    return(as.data.table(x))
#  }),  idcol = TRUE
#)

### RUNNING


simsem_pipeline = function(dat = data_sim, missp, model_name){
  
  results = vector(mode = 'list', length = length(dat))
  
  for (i in seq_along(data_sim)){
    
    dt1 <- rbindlist(
      lapply(dat[i], as.data.table), 
      idcol = TRUE
    )
    
    # to Long
    dt1 = ToLong(dt1)
    
    if (missp){
      dt1[, (grep("^S", names(dt1))) := lapply(.SD, mean), 
          by = .(Group, .id, sample_index, time), .SDcols = grep("^S", names(dt1))]
      
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

## HLM

tic()
results_true_hlm = simsem_pipeline(missp = FALSE, model_name = fit_sim_hlm)
toc()

gc()

tic()
results_missp_hlm = simsem_pipeline(missp = TRUE, model_name = fit_sim_hlm)
toc()

## RCGCLM
tic()
results_true_rcgclm = simsem_pipeline(missp = FALSE, model_name = fit_sim_rcgclm)
toc()

gc()

tic()
results_missp_rcgclm = simsem_pipeline(missp = TRUE, model_name = fit_sim_rcgclm)
toc()

outwd = 'C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper3'
#save(results_true_hlm, file = paste0(outwd, '/results_true_hlm.Rdata'))
#save(results_missp_hlm, file = paste0(outwd, '/results_missp_hlm.Rdata'))
#save(results_true_rcgclm, file = paste0(outwd, '/results_true_rcgclm.Rdata'))
#save(results_missp_rcgclm, file = paste0(outwd, '/results_missp_rcgclm.Rdata'))

## plotting

params_true = lapply(results_true, summaryParam)
params_true = do.call(rbind.data.frame, params_true)
colnames(params_true) = paste0(colnames(params_true), '.true')

params_missp = lapply(results_missp,  summaryParam)
params_missp = do.call(rbind.data.frame, params_missp)
colnames(params_missp) = paste0(colnames(params_missp), '.missp')

bw_grid_ordered <- bw_grid %>%
  rownames_to_column('row_id') 
bw_grid_ordered = bw_grid_ordered[match(numbers, bw_grid_ordered$row_id), ]

params_all = cbind.data.frame(params_true, params_missp,
                              bw_grid_ordered %>% slice(rep(1:n(), each=3))) %>%
  mutate(effect_name = rep(c('int', 'SP', 'time'), nrow(bw_grid_ordered)))

params_all %<>% 
  mutate(bias = (`Estimate Average.missp` - `Estimate Average.true`)#/`Estimate Average.true`
  )
summary(params_all$bias[params_all$effect_name == 'SP'])

## plotting

ggplot(params_all %>%
         filter(effect_name == 'SP' ),
       aes(as.factor(Between), as.factor(Within), fill = bias)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", high="darkgreen", mid = 'white'#,
                      #limits = c(-12,12)
  )  +
  facet_wrap(~ eta)+
  theme_ipsum() 

p1 = ggplot(params_all %>%
         filter(effect_name == 'SP' ),
       aes(as.factor(Between), as.factor(Within), fill = `Power (Not equal 0).missp`)) + 
  geom_tile() +
  scale_fill_gradient2(name = 'power miss', 
                       low="red", high="darkgreen", mid = 'white'#,
                       #limits = c(-12,12)
  )  +
  facet_wrap(~ eta)+
  theme_ipsum() 


p2 = ggplot(params_all %>%
         filter(effect_name == 'SP' ),
       aes(as.factor(Between), as.factor(Within), fill = `Power (Not equal 0).true`)) + 
  geom_tile() +
  scale_fill_gradient2(name = 'power true', 
                       low="red", high="darkgreen", mid = 'white'#,
                       #limits = c(-12,12)
  )  +
  facet_wrap(~ eta)+
  theme_ipsum() 


ggarrange(p, p1, p2,
          labels = c('           effect difference',
                     'power miss',
                     'power true'
          ),
          ncol = 1, nrow = 3,
          font.label = list(size = 50), align ='hv')
ggsave(paste0(outwd, "/test_hlm.jpeg"),
       width = 40, height = 60, units = 'cm') 

##

sub = as.data.table(data_sim[["-0.8_-0.8_0.9"]]) 
sub %<>% filter(sample_index %in% 1:10)

stat1 = psych::statsBy(sub %>% filter(sample_index == 10) %>%
                         dplyr::select(-sample_index) , 'Group')
stat1$rbg[8:14,1:7]
stat1$rwg[8:14,1:7]
stat1$etawg
stat1$etabg
stat1$etabg / (stat1$etabg + stat1$etawg)

list_wb_comb[[1]]

