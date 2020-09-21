context("RolePredictPreProcessing")





# RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping)


test_that("No errors are shown", {
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "calculate"
  n_it <- 2
  species_remapping <- NULL
  weighting = NULL
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)


  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "conservatism_output"
  n_it <- 2
  species_remapping <- NULL
  weighting = Conservatism(network_list = training_networks, n_it = n_it, species = "all")
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)








})




