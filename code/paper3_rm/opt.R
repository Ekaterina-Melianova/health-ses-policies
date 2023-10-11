
simulate_multilevel_data <- function(n_comb, 
                                     n_samples, 
                                     t,
                                     n_groups,
                                     eta_list,
                                     list_wb_comb,
                                     seed_vec,
                                     aggregate = FALSE,
                                     hlm = FALSE) { 
  
  plan(multisession, workers = no_cores)
  
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    
    data_list <- future_map(1:n_comb, .progress = T,
                            .options = furrr_options(seed = TRUE),
                            function(i) {
                              dat <- lapply(seq_along(eta_list), function(et) {
                                set.seed(seed_vec)
                                dat <- as.data.frame(
                                  psych::sim.multilevel(
                                    nvar = 2*t,
                                    ncases = 1000,
                                    ngroups = 50,
                                    rbg = list_wb_comb[[i]][[1]],
                                    rwg = list_wb_comb[[i]][[2]],
                                    eta = eta_list[[et]])$xy)
                                colnames(dat) = c('Group', paste0('HE', 1:t),
                                                  paste0('SP', 1:t))
                                if (aggregate) {
                                  dat <- dat %>% 
                                    group_by(Group) %>% 
                                    mutate(across(starts_with('S'), mean))
                                }
                                if (hlm) {
                                  dat <- ToWide(dat)
                                }
                                return(dat)
                              })
                              
                              p(sprintf("Combination %g", i)) 
                              
                              names(dat) = eta_range
                              return(dat)
                              
                            })
  }) 
  
  data_list
}

test = simulate_multilevel_data(n_comb = length(list_wb_comb), 
                                n_samples = 100, 
                                t = 7,
                                n_groups = 50,
                                eta_list = eta_list,
                                list_wb_comb = list_wb_comb,
                                seed_vec = 1:100,
                                aggregate = TRUE,
                                hlm = FALSE)
length(test[[1]])

