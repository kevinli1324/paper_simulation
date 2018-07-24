source("pca_functions.R")
source("t_sim_functions.R")

for(i in 1:100){
  foo <- gen_t_data(100, 162, .75)
  
  t_matrix <- foo$t_data
  logfit_matrix <- foo$log_data
  label_matrix <- foo$labels
  mix_vec <- foo$mix_vec
  fit_vec <- foo$fit
  mean0 <- foo$mean0
  pca_object <- prcomp(logfit_matrix)
  princomps <-  which(pca_object$sdev^2/sum(pca_object$sdev^2) < .05)[1]
  
  stan_object <- pca_wrapper(data = logfit_matrix, princomps = 2, clusters = princomps,  method = "kmeans", file = "simple_multivar.stan", optimize = F, chains = 1, iter = 2000)
  #stan_count <- pca_wrapper(data = count_matrix, princomps = princomps, mexthod = "kmeans", file = "pois_multivar.stan", mean0 = mean0)
  mclust_list <- apply(logfit_matrix, MARGIN = 1, Mclust, G = 2, modelNames = "E") 
  store_list_1 <- list(stan_object = stan_object, mclust_list  = mclust_list, simulation_object = foo)
  save(store_list_1, file = paste0(i, "_store_list_75.RData"))
}