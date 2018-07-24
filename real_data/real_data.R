source("pca_functions.R")
logfit <- read.delim("fit_logratios_good.tab.txt", quote = "" , as.is = T)
logfit_final <- data.matrix(logfit[, 5:ncol(logfit)])

stanFeed <- list(N = nrow(logfit_final), J = ncol(logfit_final), y = logfit_final)


fit = pca_wrapper(data = logfit_final, princomps = 1, method = "kmeans", file = "simple_multivar.stan")
save(fit, file = "real_data.RData")
