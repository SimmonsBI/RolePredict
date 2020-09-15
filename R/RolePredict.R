RolePredict <- function(training_networks, networks_to_predict, conservatism, weights, n_it, species_remapping, ...){
  # Argument checks -----------------
  # training networks
  if(any(sapply(training_networks, function(x){any(is.null(rownames(x)), is.null(colnames(x)))}))){stop("All elements of training_networks must have row and column names")}
  all_species <- unlist(lapply(training_networks, function(x) unlist(dimnames(x)))) # make a single vector of all species names across all networks
  training_roles <- CalculateRolesMultipleNetworks(network_list = training_networks, ...) # calculate roles of all species in training_networks

  # networks_to_predict
  if(!all(sapply(networks_to_predict, function(x) inherits(x, "data.frame") | inherits(x, "matrix")))){stop("All elements of networks_to_predict must be a data.frame or a matrix")}
  if(!all(sapply(networks_to_predict, function(x) ncol(x) == 2))){stop("All elements of networks_to_predict must be a data.frame or a matrix with 2 columns")}
  if(!all(sapply(networks_to_predict, function(x) identical(colnames(x), c("level","species"))))){stop("All elements of networks_to_predict must be a data.frame or a matrix with 2 columns. The first column must be called 'level' and the second column must be called 'species")}
  if(!all(sapply(networks_to_predict, function(x) all(x$level %in% c("row","column"))))){stop("All elements of networks_to_predict must have a column called 'level' whose entries are either 'row' or 'column'")}
  if(!all(sapply(networks_to_predict, function(x) all(table(x$level)>=4)))){stop("All elements of networks_to_predict must have 4 or more row species and 4 or more column species")}
  if(!all(sapply(networks_to_predict, function(x){
    all(x[x$level == "row","species"] %in% all_species) && all(x[x$level == "column","species"] %in% all_species)
  }))){stop("All species in networks_to_predict must be present in training_networks")}
  if(!all(sapply(networks_to_predict, function(x){
    all(x[x$level == "row","species"] %in% training_roles[[1]]$species) && all(x[x$level == "column","species"] %in% training_roles[[2]]$species)
  }))){stop("All species in networks_to_predict must be present in training_roles. If you see this message, something has gone very wrong. Please contact the package author with a reproducible example of how you got this error.")}

  # conservatism
  if(!inherits(conservatism, "character")){stop("'conservatism' must be a character string equal to: 'none','conservatism_output', 'calculate', 'custom'")}
  if(!length(conservatism) != 1){stop("'conservatism' must be a character string of length 1 equal to: 'none','conservatism_output', 'calculate', 'custom'")}
  if(!conservatism %in% c("none","conservatism_output", "calculate", "custom")){stop("'conservatism' must be a character string equal to: 'none','conservatism_output', 'calculate', 'custom'")}


  conservatism <- ConservatismPermTest(roles = roles, n_it = n_it, species = wanted_predictions)




}



networks_to_predict <- list(data.frame(level = c("row","row","row","row","column","column","column","column"), species = c("Erythroxylum","Paragenipa","Dillenia","Mimusops","C1","C11","C15","C19")))
