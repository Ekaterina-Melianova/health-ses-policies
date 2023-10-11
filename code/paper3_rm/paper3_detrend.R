detrend_wide = function(list_df){
  # Convert to long format
  long_data <- list_df %>% 
    mutate(ID = 1:n()) %>%
    pivot_longer(-c(LAD21CD, ID), names_to = c("variable", "time"),
                 names_pattern = "(..)(.)", values_to = "value") %>%
    ungroup() 
  
  # Detrending function for long data
  detrend_long <- function(data) {
    model <- lm(value ~ time , data = data)
    detrended <- residuals(model)
    return(detrended)
  }
  
  # Apply detrending to long data
  long_data$detrended_value <- detrend_long(long_data)
  
  # Convert detrended data back to wide format
  wide_detrended <- long_data %>%
    dplyr::select(-value) %>% 
    pivot_wider(names_from = c(variable, time),
                values_from = detrended_value,
                id_cols = c(LAD21CD, ID),
                names_sep = '') %>%
    dplyr::select(-ID)
  return(wide_detrended)
}
####


DetrendPanel = function(vars = NULL, id = id, time = year, data){
  
  data <- data %>% ungroup()%>% 
    mutate(ID = 1:n()) %>%
    pivot_longer(-c(LAD21CD, ID), names_to = c("variable", "time"),
                 names_pattern = "(..)(.)", values_to = "value") %>%
    ungroup() %>% pivot_wider(id_cols = c(LAD21CD, ID, time),
                              names_from = variable) %>%
    dplyr::select(id = ID, time, HE = HE, as = as) 
  
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
  
  out <- out %>%
    pivot_wider(names_from = time,
                values_from = c(HE, as),
                id_cols = id,
                names_sep = '')
                  
  return(out)
}

det = DetrendPanel(vars = c('HE', 'as'), time = time,
                   data = temp[[1]][[1]])

temp = test

temp <- lapply(seq_along(temp), function(i) {
  lapply(temp[[i]], function(l) DetrendPanel(vars = c('HE', 'as'),time = time,data = l))
})

temp_list = temp
for (i in seq_along(temp_list)){
  temp_list[[i]] = lapply(temp_list[[i]], function(x) cor(x$as1, x$HE1))
  temp_list[[i]] = var(unlist(temp_list[[i]]))
}

cor_df = cbind.data.frame(wb = names(temp_list), cor = unlist(temp_list))




