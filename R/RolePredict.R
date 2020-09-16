RolePredict <- function(training_networks, networks_to_predict, conservatism, weights = NULL, n_it, species_remapping, ...){
  # Argument checks -----------------
  # training networks
  if(any(sapply(training_networks, function(x){any(is.null(rownames(x)), is.null(colnames(x)))}))){stop("All elements of training_networks must have row and column names")}
  all_species <- unlist(lapply(training_networks, function(x) unlist(dimnames(x)))) # make a single vector of all species names across all networks
  training_roles <- CalculateRolesMultipleNetworks(network_list = training_networks, ...) # calculate roles of all species in training_networks

  # species_remapping
  if(!(inherits(species_remapping, "data.frame"))){stop("'species_remapping' must be a data.frame")}
  if(ncol(species_remapping) != 2){stop("'species_remapping' must be a data.frame with 2 columns: 'species' and 'proxy'")}
  if(!identical(colnames(species_remapping), c("species","proxy"))){stop("'species_remapping' must be a data.frame with 2 columns: 'species' and 'proxy'")}
  if(!inherits(species_remapping$species, "character")){stop("The species column in 'species_remapping' must be a character variable")}
  if(!inherits(species_remapping$proxy, "character")){stop("The proxy column in 'species_remapping' must be a character variable")}
  ntp_species <- unlist(lapply(networks_to_predict, function(x) x$species))
  if(!all(species_remapping$species %in% networks_to_predict)){stop("All species in 'species' column of species_remapping must be present in networks_to_predict")}
  if(!all(species_remapping$proxy %in% all_species)){stop("All species in 'proxy' column of species_remapping must be present in training_networks")}

  # networks_to_predict
  if(!all(sapply(networks_to_predict, function(x) inherits(x, "data.frame")))){stop("All elements of networks_to_predict must be a data.frame")}
  if(!all(sapply(networks_to_predict, function(x) ncol(x) == 2))){stop("All elements of networks_to_predict must be a data.frame with 2 columns")}
  if(!all(sapply(networks_to_predict, function(x) identical(colnames(x), c("level","species"))))){stop("All elements of networks_to_predict must be a data.frame with 2 columns. The first column must be called 'level' and the second column must be called 'species")}
  if(!all(sapply(networks_to_predict, function(x) all(x$level %in% c("row","column"))))){stop("All elements of networks_to_predict must have a column called 'level' whose entries are either 'row' or 'column'")}
  if(!all(sapply(networks_to_predict, function(x) inherits(x$species, "character")))){stop("All elements of networks_to_predict must have a column called 'species' whose entries are character strings giving species names. Currently, elements of one or more 'species' columns are not of class 'chaarcter'.")}
  if(!all(sapply(networks_to_predict, function(x) all(table(x$level)>=4)))){stop("All elements of networks_to_predict must have 4 or more row species and 4 or more column species")}
  networks_to_predict_remapping <- networks_to_predict # do remapping for proxy species in networks_to_predict
  for(i in seq(networks_to_predict_remapping)){
    x <- networks_to_predict_remapping[[i]]
    x$species <- sapply(x$species, function(y){
      if(y %in% species_remapping$species){
        species_remapping[species_remapping$species == y,"proxy"]
      } else {
        y
      }
    })
    networks_to_predict_remapping[[i]] <- x
  }
  if(!all(sapply(networks_to_predict_remapping, function(x){all(x$species %in% all_species)}))){stop("Not all species in networks_to_predict are in training_networks, even after performing any remapping specified using species_remapping. All species in networks_to_predict must be present in training_networks or be replaced by a proxy that is in training_networks using the species_remapping argument")}
  if(!all(sapply(networks_to_predict_remapping, function(x){
    all(x[x$level == "row","species"] %in% training_roles[[1]]$species) && all(x[x$level == "column","species"] %in% training_roles[[2]]$species)
  }))){stop("All species in networks_to_predict must be present in training_roles or be replaced by a proxy that is in training_networks using the species_remapping argument. If you see this message, something has gone very wrong. Please contact the package author with a reproducible example of how you got this error.")}

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
    if(!inherits(weights, "data.frame")){stop("'weights' must be a data.frame")}
    if(ncol(weights) != 3){stop("'weights' must have 3 columns: level, species, and weight")}
    if(!identical(colnames(weights), c("level","species","weight"))){stop("'weights' must have 3 columns: level, species, and weight")}
    if(any(!weights$level %in% c("row","column"))){stop("The level column of 'weights' must only contain strings equal to 'row' or 'column'")}
    if(!inherits(weights$weight, "numeric")){stop("The column 'weight' must be numeric")}
    if(any(weights$weight < 0)){stop("All weights in the weight column must be positive numbers")}
    if(!all(weights$species %in% all_species)){stop("All species in 'weights' must be present in training_networks")}
  }

  # checking networks_to_predict row and column assignments match training_networks
  training_species_levels <- as.data.frame(rbind(cbind(unlist(lapply(training_networks, rownames)),"row"),cbind(unlist(lapply(training_networks, colnames)),"column")))
  colnames(training_species_levels) <- c("species","level")
  networks_to_predict_remapping_rbind <- do.call("rbind", networks_to_predict_remapping)
  networks_to_predict_remapping_rbind$training_species_levels <- training_species_levels[match(networks_to_predict_remapping_rbind$species, training_species_levels$species),"level"]
  if(!all(networks_to_predict_remapping_rbind$level == networks_to_predict_remapping_rbind$training_species_levels)){stop("The level of species  (row or column) in networks_to_predict, after any remapping from species_remapping, must match whether these species are row or column names in training_networks. e.g. a species specified as 'row' in networks_to_predict, directly or via remapping, must also be a row name, not a column name, in training_networks")}

  # checking species and proxy row and column assignments match in species_remapping
  networks_to_predict_rbind <- do.call("rbind", networks_to_predict)
  species_species_remapping_levels <- networks_to_predict_rbind[match(species_remapping$species, networks_to_predict_rbind$species),"level"] # level of species in species_remapping
  proxy_species_remapping_levels <- training_species_levels[match(species_remapping$proxy, training_species_levels$species),"level"] # level or proxy in species_remapping
  if(!all(species_species_remapping_levels == proxy_species_remapping_levels)){stop("In species_remapping, species in the 'species' column must be in the same level (row or column) as determined by networks_to_predict as the corresponding species in the 'proxy' column")}

  conservatism <- ConservatismPermTest(roles = roles, n_it = n_it, species = wanted_predictions)




}



networks_to_predict <- list(data.frame(level = c("row","row","row","row","column","column","column","column"), species = c("Erythroxylum","Paragenipa","Dillenia","Mimusops","C1","C11","C15","C19")))
