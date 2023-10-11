library(furrr)
library(dplyr)
library(progress)

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
  
  # Using progress
  with_progress({
    
    p = progressor(along = 1:n_comb)                       
    
    data_list <- future_map(1:n_comb,
                            .progress = T,
                            .options = furrr_options(seed = TRUE),
                            function(i) {
      
      dat = vector(mode = 'list', length = length(eta_list))
      
      for (et in seq_along(eta_list)) {
        dat[[et]] = lapply(1:n_samples, function(j) {
          
          set.seed(seed_vec[j])
          
          sample_data = as.data.frame(psych::sim.multilevel(
            nvar = 2*t,
            ncases = 1000,
            ngroups = n_groups,
            rbg = list_wb_comb[[i]][[1]],
            rwg = list_wb_comb[[i]][[2]],
            eta = eta_list[[et]])$xy)
          colnames(sample_data) = c('Group', paste0('HE', 1:t), paste0('SP', 1:t))
          
         #if (aggregate) {
         #  sample_data = sample_data %>% 
         #    group_by(Group) %>% 
         #    mutate(across(starts_with('S'), mean))
         #}
         #
         #if (hlm) {
         #  sample_data = ToWide(sample_data)
         #}
          
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

test = simulate_multilevel_data(n_comb = length(list_wb_comb), 
                                n_samples = 5, 
                                t = 7,
                                n_groups = 50,
                                eta_list = eta_list,
                                list_wb_comb = list_wb_comb,
                                seed_vec = 1:5,
                                aggregate = TRUE,
                                hlm = FALSE)
length(test[[1]])



