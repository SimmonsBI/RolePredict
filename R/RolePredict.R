RolePredict <- function(training_networks, networks_to_predict, conservatism, weights = NULL, n_it, species_remapping, ...){
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

  # weights
  if(conservatism == "none"){
    if(!is.null(weights)){stop("If 'conservatism' == 'none', you cannot set the 'weights' argument, it must be left blank")}
  } else if(conservatism == "conservatism_output"){
    if(!inherits(weights, "data.frame")){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): the output of those functions should be a data.frame")}
    if(ncol(weights) != 7){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): the output of those functions should have 7 columns")}
    if(!identical(colnames(weights), c("level","species","observed_dissimilarity","mean_null_dissimilarity","delta","z","P"))){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): the column names do not match the output of that function")}
    if(any(!weights$level %in% c("row","column"))){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): the level column must only contain strings equal to 'row' or 'column'")}
    if(!inherits(weights$observed_dissimilarity, "numeric")){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): column observed_dissimilarity must be numeric")}
    if(!inherits(weights$mean_null_dissimilarity, "numeric")){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): column mean_null_dissimilarity must be numeric")}
    if(!inherits(weights$delta, "numeric")){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): column delta must be numeric")}
    if(!inherits(weights$z, "numeric")){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): column z must be numeric")}
    if(!inherits(weights$P, "numeric")){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): column P must be numeric")}
    if(any(weights$P > 1)){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): values of column P are > 1")}
    if(any(weights$P < 0)){stop("'weights' must be the output of Conservatism() or ConservatismPermTest(): values of column P are < 0")}
    if(!all(weights$species %in% all_species)){stop("'weights' must be the output of Conservatism() or ConservatismPermTest() run on the same data as in training_networks. Not all species in 'weights' are present in training_networks")}
  } else if(conservatism == "calculate"){
    if(!is.null(weights)){stop("If 'conservatism' == 'calculate', you cannot set the 'weights' argument, it must be left blank")}
  } else if(conservatism == "custom"){
    if(!inherits(weights, "data.frame") || !inherits(weights, "matrix")){stop("'weights' must be a data.frame or a matrix")}
    if(ncol(weights) != 3){stop("'weights' must have 3 columns: level, species, and weight")}
    if(!identical(colnames(weights), c("level","species","weight"))){stop("'weights' must have 3 columns: level, species, and weight")}
    if(any(!weights$level %in% c("row","column"))){stop("The level column of 'weights' must only contain strings equal to 'row' or 'column'")}
    if(!inherits(weights$weight, "numeric")){stop("The column 'weight' must be numeric")}
    if(any(weights$weight < 0)){stop("All weights in the weight column must be positive numbers")}
    if(!all(weights$species %in% all_species)){stop("All species in 'weights' must be present in training_networks")}
  }

  conservatism <- ConservatismPermTest(roles = roles, n_it = n_it, species = wanted_predictions)




}



networks_to_predict <- list(data.frame(level = c("row","row","row","row","column","column","column","column"), species = c("Erythroxylum","Paragenipa","Dillenia","Mimusops","C1","C11","C15","C19")))
