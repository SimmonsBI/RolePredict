#' placeholder
#'
#' @param network_list A number.
#' @param n_it A number.
#' @param species A number.
#' @param ... ddd
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' #
#' @export
Conservatism <- function(network_list, n_it, species, ...){
  cat("Calculating species roles...\n")
  roles <- CalculateRolesMultipleNetworks(network_list = network_list, ...)
  cat("\nCalculating conservatism...\n")
  conservatism <- ConservatismPermTest(roles = roles, n_it = n_it, species = species)
  cat("\n")
  conservatism
}
