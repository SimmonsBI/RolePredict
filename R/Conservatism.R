#' placeholder
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' @export
Conservatism <- function(network_list, n_it, species, ...){
  cat("Calculating species roles...\n")
  roles <- CalculateRolesMultipleNetworks(network_list = network_list, ...)
  cat("\nCalculating conservatism...\n")
  conservatism <- ConservatismPermTest(roles = roles, n_it = n_it, species = species)
  cat("\n")
  conservatism
}
