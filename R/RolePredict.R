RolePredict <- function(training_networks, networks_to_predict, conservatism, weighting = NULL, n_it, species_remapping = NULL, ...){
  # Pre-process inputs before the optimisation -----------------
  pre_processed <- RolePredictPreProcessing(training_networks = training_networks, networks_to_predict = networks_to_predict, conservatism = conservatism, weighting = weighting, n_it = n_it, species_remapping = species_remapping, ...)



}












