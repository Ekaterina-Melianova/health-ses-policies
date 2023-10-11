simulate_multilevel_data <- function(n_comb, 
                                     n_samples, 
                                     t,
                                     n_groups,
                                     eta_list,
                                     list_wb_comb,
                                     seed_vec,
                                     aggregate = FALSE,
                                     hlm = FALSE) {
  
  
  plan(multicore, workers = no_cores)
  data_list = vector(mode = 'list', 
                     length = length(eta_range))
  for (lst in seq_along(data_list)){
    data_list[[lst]] = vector(mode = 'list',
                              length = n_samples)
    }
  
  with_progress({    
    
  p = progressor(steps = n_comb, label = paste0("Combination ", i))
  
  future_map(1:n_comb, 
             .options = furrr_options(seed = TRUE),
             function(i) {
               map2(eta_list, seed_vec, ~ {
                              set.seed(.y)
                              data = vector(mode = 'list', length = n_samples)
                              
                              for (j in 1:n_samples) {
                                
                                data[[j]] = as.data.frame(
                                  psych::sim.multilevel(
                                    nvar = 2*t,
                                    ncases = 200,
                                    ngroups = n_groups,
                                    rbg = list_wb_comb[[i]][[1]],
                                    rwg = list_wb_comb[[i]][[2]],
                                    eta = .x)$xy)
                                colnames(data[[j]]) = c('Group',
                                                        paste0('HE', 1:t),
                                                        paste0('SP', 1:t))
                                
                                if (aggregate) {
                                  data[[j]] <- data[[j]] %>% 
                                    group_by(Group) %>% 
                                    mutate(across(starts_with('S'), mean))
                                }
                                
                                if (hlm) {
                                  data[[j]] <- ToWide(data[[j]])
                                }
                                
                            
                                
                              }  
                            })
                              p(sprintf("j=%g", i))
                          
                            
                              data_list[[i]] = data
                          }) 
  
                        })
  
}


test = simulate_multilevel_data(n_comb = length(list_wb_comb[1:10]), 
                                n_samples = 10, 
                                t = 7,
                                n_groups = 10,
                                eta_list = eta_list,
                                list_wb_comb = list_wb_comb[1:10],
                                seed_vec = 1:10,
                                aggregate = TRUE,
                                hlm = FALSE)
length(test[[1]])
