true_data = vector(mode = 'list', length = length(sim_result))
for (i in seq_along(true_data)){
  true_data[[i]] = vector(mode = 'list', length = length(sim_result))
}

for (i in 1:length(sim_result)) {
  for (j in 1:length(sim_result[[1]][[2]])) {
    true_data[[i]][[j]] <- sim_result[[i]][[2]][[j]][[1]]
  }
}

true_models = true_data
for (i in 1:length(true_data)) {
  for (j in 1:length(true_data[[1]])) {
    true_data[[i]][[j]] = cbind.data.frame(true_data[[i]][[j]], 
                                           LAD21CD = rep(1:100, each = 30))
    true_models[[i]][[j]] = sem(syntax_sim,
                                data = true_data[[i]][[j]],
                                estimator = "mlr",
                                cluster = 'LAD21CD',
                                orthogonal = T)
    
    true_models[[i]][[j]] = tidy( true_models[[i]][[j]]) %>% 
      filter(label %in% c('b_HEas', 'd_HEas')) %>%
      slice(1:2) %>% dplyr::select(label, estimate, std.error) %>%
      mutate(label = ifelse(label == 'b_HEas', 'long', 'short'))
    
    print(j)
  }
  print(i)
}

names(true_models) = cov_vec_
for (i in seq_along(true_models)){
  names(true_models[[i]]) = cov_vec_
}
true_models = flattenList(true_models, lab = F)
true_models_df = do.call(rbind.data.frame, true_models)
true_models_df = cbind(true_models_df, vec_names = rownames(true_models_df))
true_models_df$vec_names = substr(true_models_df$vec_names, 1,
                                  nchar(true_models_df$vec_names)-2)
true_models_df %<>%
  separate(vec_names, into = c("Between", "Within"), sep = "_")
true_models_df$Between = as.numeric(true_models_df$Between)
true_models_df$Within = as.numeric(true_models_df$Within)
rownames(true_models_df) = NULL

cors = c()
for (i in seq_along(sim_result)){
  cors[i] = cov2cor(sim_result[[i]][[4]])[1,2]
}


# checking within-level cors with aggregated X
test = lapply(sim_result6, function(x) x[[3]])
for (i in seq_along(test)){
  names(test[[i]]) = cov_vec
}
names(test) = cov_vec
test = flattenList(test)
cor_list = test
for (i in seq_along(cor_list)){
  cor_list[[i]] = lapply(cor_list[[i]], function(x) cor(x$as1, x$HE1))
  cor_list[[i]] = mean(unlist(cor_list[[i]]))
}
cor_df = cbind.data.frame(wb = names(cor_list), cor = unlist(cor_list))
