
# Number of cores to use (adjust this based on your system)
num_cores = 5

# Register a parallel backend using doParallel
cl = makeCluster(num_cores)
registerDoParallel(cl)

tic()
# Parallel processing using foreach
simfit = foreach(i = 1:n_params, .combine = list,
                  .packages = c('dplyr', 'tidyr',
                                'lme4', 'magrittr')) %dopar% {
  
  df_list = vector("list", length = 100)
  
  csv_files_n = unique(csv_files_unique)[i]
  csv_files_n = csv_files[grepl(csv_files_n, csv_files)]
  
  # Loop through each .csv file
  for (file in csv_files_n) {
    file_name = basename(file)
    last_number = as.numeric(sub(".*_(\\d+)\\.Rdata", "\\1", file_name))
    load(file)
    df_list[last_number] = list(out)
  }
  
  df_list =  Filter(Negate(is.null), df_list)
  wdf_list = lapply(df_list, LongToWide2)
  
  results = lapply(wdf_list, function(dataset) {
    model = lmer(HE ~ as + year + (1|LAD21CD), data = dataset)
    summary(model)
  })
  
  coefficients_list = lapply(results, function(model_summary) {
    coef_table = model_summary$coefficients
    fixed_effects = coef_table[, "Estimate"]
    return(fixed_effects)
  })
  
  # Convert the list of coefficients to a matrix for easier aggregation
  coefficients_matrix = as.data.frame(do.call(rbind, coefficients_list))
  
  # Calculate summary statistics (e.g., mean, standard deviation) across models
  mean_coefficients = colMeans(coefficients_matrix)
  sd_coefficients = apply(coefficients_matrix, 2, sd)
  
  print(i)
  gc()
  
  return(list(mean_coefficients, sd_coefficients))
}
toc()

# Stop the parallel backend
stopCluster(cl)



# Combine results from parallel processing
simfit_combined = do.call(rbind, simfit)
