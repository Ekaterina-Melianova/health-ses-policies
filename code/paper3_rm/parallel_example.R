

library(doParallel)
library(dplyr)
library(psych)

tic()
no_cores = detectCores() - 6
cl = makeCluster(no_cores)
registerDoParallel(cl)

# Define a helper function
process_data <- function(i, et, j) {
  data_to_process <- data_sim[[i]][[et]][[j]]
  data_to_process %>%
    group_by(Group) %>%
    dplyr::mutate(across(starts_with('S'), mean))
}

# Generate combinations of i, et, and j
combinations <- expand.grid(i = 1:n_comb, et = seq_along(eta_list), j = 1:n_samples)

# Parallel processing
result_list <- foreach(comb = iter(combinations, by = 'row'),
                       export = c("data_sim", "eta_list", "n_samples"),
                       .packages = c('dplyr')) %dopar% {
                         process_data(comb$i, comb$et, comb$j)
                       }
toc()


stopCluster(cl)
