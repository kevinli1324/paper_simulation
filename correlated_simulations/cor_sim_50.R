source("correlated_sim.R")
source("pca_functions.R")
source("t_sim_functions.R")
real_counts <- read.delim("gene_counts.tab.txt", quote = "", as.is = T)

for(i in 1:100){
  simulation_object <- cor_wrapper(num_clusters = 30, n_experiments = 162, max_group = 8, prop_affec = .5, mean0_vec = real_counts$set1IT002.Time0)
  #t_matrix
  t_matrix <- simulation_object$t_matrix
  #logfit matirxx
  logfit_matrix <- simulation_object$log_fit
  #count_matrix
  count_matrix <- simulation_object$count_matrix
  #labels
  label_matrix <- simulation_object$label_matrix
  #fitness ratios
  fit_vec <- as.vector(simulation_object$fit)
  #mixing probabilities 
  mix_vec <- as.vector(simulation_object$mixing)
  #controls 
  mean0 <- as.vector(simulation_object$init)
  #pca mess
  pca_object <- prcomp(logfit_matrix)
  princomps <-  which(pca_object$sdev^2/sum(pca_object$sdev^2) < .01)[1]
  
  stan_object <- pca_wrapper(data = logfit_matrix, princomps = 5, clusters = princomps,  method = "kmeans", file = "simple_multivar.stan", optimize = F)
  #stan_count <- pca_wrapper(data = count_matrix, princomps = princomps, mexthod = "kmeans", file = "pois_multivar.stan", mean0 = mean0)
  mclust_list <- apply(logfit_matrix, MARGIN = 1, Mclust, G = 2, modelNames = "E") 
  
  store_list_1 <- list(stan_object = stan_object, mclust_list  = mclust_list, simulation_object = simulation_object)
  save(store_list_1, file = paste0(i, "cor_store_list_50.RData"))
  
}