#' placeholder
#'
#' @param training_networks A number.
#' @param networks_to_predict A number.
#' @param conservatism sss
#' @param weighting ddd
#' @param n_it ssss
#' @param species_remapping ddd
#' @param ... ddd
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' #
#' @export
RolePredict <- function(training_networks, networks_to_predict, conservatism, weighting = NULL, n_it, species_remapping = NULL, ...){
  # Pre-process inputs before the optimisation -----------------
  pre_processed <- RolePredictPreProcessing(training_networks = training_networks, networks_to_predict = networks_to_predict, conservatism = conservatism, weighting = weighting, n_it = n_it, species_remapping = species_remapping, ...)



}












