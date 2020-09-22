RolePredictPreProcessing <- function(training_networks, networks_to_predict, conservatism, weighting, n_it, species_remapping, ...){
  # Argument checks -----------------
  # training networks
  if(any(sapply(training_networks, function(x){any(is.null(rownames(x)), is.null(colnames(x)))}))){stop("All elements of training_networks must have row and column names")}
  if(length(intersect(unlist(lapply(training_networks, rownames)),unlist(lapply(training_networks, colnames)))) != 0){stop("Row names must only occur as row names, and column names must only occur as column names. This error occurs if a row name is also used as a column name, or vice versa.")}
  if(any(names(training_networks) == "remapped_proxy")){stop("Networks cannot be named 'remapped_proxy' because this is a name reserved for the package to distinguish species which are introduced via the species_remapping argument and thus which have no associated network.")}
  all_species <- unlist(lapply(training_networks, function(x) unlist(dimnames(x)))) # make a single vector of all species names across all networks
  training_roles <- CalculateRolesMultipleNetworks(network_list = training_networks, ...) # calculate roles of all species in training_networks

  # species_remapping
  ntp_species <- unlist(lapply(networks_to_predict, function(x) x$species))
  if(!is.null(species_remapping)){
    if(!(inherits(species_remapping, "data.frame"))){stop("'species_remapping' must be a data.frame")}
    if(ncol(species_remapping) != 2){stop("'species_remapping' must be a data.frame with 2 columns: 'species' and 'proxy'")}
    if(!identical(colnames(species_remapping), c("species","proxy"))){stop("'species_remapping' must be a data.frame with 2 columns: 'species' and 'proxy'")}
    if(!inherits(species_remapping$species, "character")){stop("The species column in 'species_remapping' must be a character variable")}
    if(!inherits(species_remapping$proxy, "character")){stop("The proxy column in 'species_remapping' must be a character variable")}
    if(!all(species_remapping$species %in% ntp_species)){stop("All species in 'species' column of species_remapping must be present in networks_to_predict")}
    if(!all(species_remapping$proxy %in% all_species)){stop("All species in 'proxy' column of species_remapping must be present in training_networks")}
    if(!all(table(species_remapping$species)==1)){stop("Species must only have one proxy. This error appears because one or more species occurs more than once in the 'species' column of species_remapping")}
    if(any(species_remapping$species == species_remapping$proxy)){stop("A species cannot be a proxy for itself. At least one species has itself as a proxy in species_remapping.")}
    species_remapping_species_frequency <- sapply(species_remapping$species, function(x) sum(all_species == x)) # how many times each species_remapping species occurs in training_networks?
  }

  # networks_to_predict
  if(!all(sapply(networks_to_predict, function(x) inherits(x, "data.frame")))){stop("All elements of networks_to_predict must be a data.frame")}
  if(!all(sapply(networks_to_predict, function(x) ncol(x) == 2))){stop("All elements of networks_to_predict must be a data.frame with 2 columns")}
  if(!all(sapply(networks_to_predict, function(x) identical(colnames(x), c("level","species"))))){stop("All elements of networks_to_predict must be a data.frame with 2 columns. The first column must be called 'level' and the second column must be called 'species")}
  if(!all(sapply(networks_to_predict, function(x) all(x$level %in% c("row","column"))))){stop("All elements of networks_to_predict must have a column called 'level' whose entries are either 'row' or 'column'")}
  if(!all(sapply(networks_to_predict, function(x) inherits(x$species, "character")))){stop("All elements of networks_to_predict must have a column called 'species' whose entries are character strings giving species names. Currently, elements of one or more 'species' columns are not of class 'chaarcter'.")}
  if(!all(sapply(networks_to_predict, function(x) all(table(x$level)>=4)))){stop("All elements of networks_to_predict must have 4 or more row species and 4 or more column species")}
  networks_to_predict_rbind <- do.call("rbind", networks_to_predict)
  if(!all(sapply(split(networks_to_predict_rbind, networks_to_predict_rbind$species), function(x){
    length(unique(x$level)) == 1
  }))){stop("All species in network_to_predict should be either row or column. This error is displayed because one of more species have been assigned more than one level i.e. a species is assigned as a row in one entry and a column in another entry")}
  networks_to_predict_remapping <- networks_to_predict # create networks_to_predict_remapping even if there is no remapping, so there is a unified variable name for the subsequent checks
  if(!is.null(species_remapping)){ # do remapping for proxy species in networks_to_predict if any is specified
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
  }
  if(!all(sapply(networks_to_predict_remapping, function(x){all(x$species %in% all_species)}))){stop("Not all species in networks_to_predict are in training_networks, even after performing any remapping specified using species_remapping. All species in networks_to_predict must be present in training_networks or be replaced by a proxy that is in training_networks using the species_remapping argument")}
  if(!all(sapply(networks_to_predict_remapping, function(x){
    all(x[x$level == "row","species"] %in% training_roles[[1]]$species) && all(x[x$level == "column","species"] %in% training_roles[[2]]$species)
  }))){stop("All species in networks_to_predict must be present in training_roles or be replaced by a proxy that is in training_networks using the species_remapping argument. If you see this message, something has gone very wrong. Please contact the package author with a reproducible example of how you got this error.")}

  # conservatism
  if(!inherits(conservatism, "character")){stop("'conservatism' must be a character string equal to: 'none','conservatism_output', 'calculate', 'custom'")}
  if(length(conservatism) != 1){stop("'conservatism' must be a character string of length 1 equal to: 'none','conservatism_output', 'calculate', 'custom'")}
  if(!conservatism %in% c("none","conservatism_output", "calculate", "custom")){stop("'conservatism' must be a character string equal to: 'none','conservatism_output', 'calculate', 'custom'")}

  # weighting
  if(conservatism == "none"){
    using_weights <- FALSE
  } else {
    using_weights <- TRUE
  }
  if(conservatism == "none"){
    if(!is.null(weighting)){stop("If 'conservatism' == 'none', you cannot set the 'weighting' argument, it must be left blank")}
  } else if(conservatism == "conservatism_output"){
    if(!inherits(weighting, "data.frame")){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): the output of those functions should be a data.frame")}
    if(ncol(weighting) != 7){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): the output of those functions should have 7 columns")}
    if(!identical(colnames(weighting), c("level","species","observed_dissimilarity","mean_null_dissimilarity","delta","z","P"))){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): the column names do not match the output of that function")}
    if(any(!weighting$level %in% c("row","column"))){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): the level column must only contain strings equal to 'row' or 'column'")}
    if(!inherits(weighting$observed_dissimilarity, "numeric")){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): column observed_dissimilarity must be numeric")}
    if(!inherits(weighting$mean_null_dissimilarity, "numeric")){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): column mean_null_dissimilarity must be numeric")}
    if(!inherits(weighting$delta, "numeric")){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): column delta must be numeric")}
    if(!inherits(weighting$z, "numeric")){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): column z must be numeric")}
    if(!inherits(weighting$P, "numeric")){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): column P must be numeric")}
    if(any(weighting$P > 1)){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): values of column P must be between 0 and 1")}
    if(any(weighting$P < 0)){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): values of column P must be between 0 and 1")}
    if(!all(weighting$species %in% all_species)){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest() run on the same data as in training_networks. Not all species in 'weighting' are present in training_networks")}
    if(!all(table(weighting$species) == 1)){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): All species in 'weighting' must only occur once i.e. each species must only have once weight assigned to it")}
    if(is.null(species_remapping)){
      ntp_species_frequency <- sapply(ntp_species, function(x) sum(all_species == x)) # how many times each ntp species occurs in training_networks
      if(!all(names(which(ntp_species_frequency>1)) %in% weighting$species)){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): All species in networks_to_predict that occur more than once in training_networks should be present in 'weighting'")}
    } else { # weightings must have: 1. weights for all networks_to_predict spp w/ no proxies; 2. all ntp spp w/ proxies which occur more than once in training_networks; 3. and all proxy species
      species_to_calculate_conservatism <- intersect(names(which(table(all_species)>1)), c(ntp_species, species_remapping$proxy)) # calculate conservatism for: all ntp species and all proxy species, that occur more than once in training_networks i.e. 1. all networks_to_predict spp w/ no proxies which occur more than once in training_networks; 2. all ntp spp w/ proxies which occur more than once in training_networks; 3. and all proxy species which occur more than once in training_networks
      if(!all(species_to_calculate_conservatism %in% weighting$species)){stop("'weighting' must be the output of Conservatism() or ConservatismPermTest(): at a minimum it must contain weightings for all species in networks_to_predict and all species in species_remapping$proxy that occur more than once in training_networks")}
    }
  } else if(conservatism == "calculate"){
    if(!is.null(weighting)){stop("If 'conservatism' == 'calculate', you cannot set the 'weighting' argument, it must be left blank")}
  } else if(conservatism == "custom"){
    if(is.null(weighting)){stop("weighting is NULL. Data must be provided for the 'weighting' argument when conservatism = 'custom'.")}
    if(!inherits(weighting, "data.frame")){stop("'weighting' must be a data.frame")}
    if(ncol(weighting) != 3){stop("'weighting' must have 3 columns: level, species, and weight")}
    if(!identical(colnames(weighting), c("level","species","weight"))){stop("'weighting' must have 3 columns: level, species, and weight")}
    if(any(!weighting$level %in% c("row","column"))){stop("The level column of 'weighting' must only contain strings equal to 'row' or 'column'")}
    if(!inherits(weighting$weight, "numeric")){stop("The column 'weight' must be numeric")}
    if(any(weighting$weight < 0)){stop("All weighting in the weight column must be positive numbers")}
    if(!all(weighting$species %in% all_species)){stop("All species in 'weighting' must be present in training_networks")}
    if(!all(table(weighting$species) == 1)){stop("All species in 'weighting' must only occur once i.e. each species must only have once weight assigned to it")}
    if(is.null(species_remapping)){
      if(!all(ntp_species %in% weighting$species)){stop("All species in 'networks_to_predict' must have weights specified in 'weighting' when using conservatism = 'custom'")}
    } else { # weightings must have: 1. weights for all networks_to_predict spp w/ no proxies; 2. all ntp spp w/ proxies which occur more than once in training_networks; 3. and all proxy species
      if(!all(setdiff(ntp_species, species_remapping$species) %in% weighting$species)){stop("All species in networks_to_predict that do not have proxies specified via species_remapping must have weighted assigned to them in 'weighting'")}
      if(!all(species_remapping$proxy %in% weighting$species)){stop("All proxy species in species_remapping must have weights assigned to them in 'weighting'")}
      # species_remapping_species_frequency <- sapply(species_remapping$species, function(x) sum(all_species == x)) # how many times each species_remapping species occurs in training_networks
      if(any(species_remapping_species_frequency>1)){
        if(!all(names(which(species_remapping_species_frequency > 1)) %in% weighting$species)){stop("All species in the 'species' column of species_remapping must have weights assigned to them in 'weighting' if they occur more than once in training_networks. This is because for species occurring more than once in training_networks, RolePredict can calculate their conservatism and average these values with the conservatism values of the proxy species.")}
      }
    }
  }

  # checking networks_to_predict row and column assignments match training_networks
  training_species_levels <- as.data.frame(rbind(cbind(unlist(lapply(training_networks, rownames)),"row"),cbind(unlist(lapply(training_networks, colnames)),"column")))
  colnames(training_species_levels) <- c("species","level")
  networks_to_predict_remapping_rbind <- do.call("rbind", networks_to_predict_remapping)
  networks_to_predict_remapping_rbind$training_species_levels <- training_species_levels[match(networks_to_predict_remapping_rbind$species, training_species_levels$species),"level"]
  if(!all(networks_to_predict_remapping_rbind$level == networks_to_predict_remapping_rbind$training_species_levels)){stop("The level of species  (row or column) in networks_to_predict, after any remapping from species_remapping, must match whether these species are row or column names in training_networks. e.g. a species specified as 'row' in networks_to_predict, directly or via remapping, must also be a row name, not a column name, in training_networks")}

  # checking species and proxy row and column assignments match in species_remapping
  if(!is.null(species_remapping)){
    # networks_to_predict_rbind <- do.call("rbind", networks_to_predict) # this is now done earlier
    species_species_remapping_levels <- networks_to_predict_rbind[match(species_remapping$species, networks_to_predict_rbind$species),"level"] # level of species in species_remapping
    proxy_species_remapping_levels <- training_species_levels[match(species_remapping$proxy, training_species_levels$species),"level"] # level or proxy in species_remapping
    if(!all(species_species_remapping_levels == proxy_species_remapping_levels)){stop("In species_remapping, species in the 'species' column must be in the same level (row or column) as determined by networks_to_predict as the corresponding species in the 'proxy' column")}
  }

  # checking row/column assignments in weighting match training_networks
  if(conservatism %in% c("conservatism_output","custom")){
    species_weightings_levels <- training_species_levels[match(weighting$species, training_species_levels$species),"level"] # level of species in weightings in training_networks
    if(!all(species_weightings_levels == weighting$level)){stop("The level (row/column) of species in the 'species' column of 'weightings' must match must match whether these species are row or column names in training_networks. e.g. a species specified as 'row' in weightings must also be a row name, not a column name, in training_networks")}
  }

  # Calculate conservatism -----------------
  if(conservatism == "calculate"){
    if(is.null(species_remapping)){ # identify species to calculate conservatism for
      species_to_calculate_conservatism <- intersect(names(which(table(all_species)>1)), c(ntp_species)) # calculate conservatism for: all ntp species that occur more than once in training_networks
    } else {
      species_to_calculate_conservatism <- intersect(names(which(table(all_species)>1)), c(ntp_species, species_remapping$proxy)) # calculate conservatism for: all ntp species and all proxy species, that occur more than once in training_networks i.e. 1. all networks_to_predict spp w/ no proxies which occur more than once in training_networks; 2. all ntp spp w/ proxies which occur more than once in training_networks; 3. and all proxy species which occur more than once in training_networks
    }
    weighting <- ConservatismPermTest(roles = training_roles, n_it = n_it, species = species_to_calculate_conservatism)
  }

  # Impute missing species weights if necessary ----------------- (impute weights for any species which we need to predict, but which don't have weights, UNLESS the species has a proxy and only occurs in training_networks 0 or 1 times. Also impute for any proxy species which don't have weights)
  if(conservatism %in% c("calculate","conservatism_output")){
    weighting <- weighting[,c("level","species","P")] # match format of custom weightings
    colnames(weighting) <- c("level","species","weight") # match format of custom weightings
    if(is.null(species_remapping)){
      species_to_impute <- setdiff(ntp_species, weighting$species) # which species do we need to predict but do not have weights for
    } else {
      species_to_impute <- setdiff(c(ntp_species, species_remapping$proxy), weighting$species) # which species do we not have weights for but we need weights for (anything in ntp or proxy which doesnt have weights)
    }





    ############### what if we have the weights for all species in ntp, but not the weights for all the proxies and thus need to impute the proxy but it isn't triggered because length(weights missing) is 0.


    if(length(species_to_impute) > 1){ # if there are any species we need which don't have weights, move into imputation
      if(is.null(species_remapping)){ # if no remapping, just impute species which we don't have in weighting as 0.5
        imputation <- networks_to_predict_rbind[networks_to_predict_rbind$species %in% species_to_impute,] # find level of species that are missing
        imputation$weight <- 0.5 # add the imputed weight
        weighting <- rbind(weighting, imputation) # join onto weightings
      } else { # if there is remapping, impute for networks_to_predict species we dont have in weighting unless they have proxies and occur in training_networks < 2 times, as well as all species in proxies for which we dont have weighting
        if(any(species_remapping_species_frequency < 2)){ # which species have proxies and only occur in training_networks < 2 times
          species_with_proxies_that_occur_01 <- names(which(species_remapping_species_frequency<2))
          species_to_impute <- setdiff(species_to_impute, species_with_proxies_that_occur_01) # species we need to predict but don't have weights for, which are not species with proxies that only occur 0 or 1 times in training_networks
        }
        # species_to_impute <- c(species_to_impute, setdiff(species_remapping$proxy, weighting$species)) # Make sure we also impute any proxy species we don't have weights for - this is now commented out because we check for proxy species earlier now
        species_to_impute <- unique(species_to_impute)
        if(length(species_to_impute) > 0){ # impute any species we need to
          species_to_impute_levels <- unname(sapply(species_to_impute, function(x){ # find the level of these species
            if(x %in% networks_to_predict_rbind$species){
              unique(networks_to_predict_rbind[networks_to_predict_rbind$species == x, "level"])
            } else if(x %in% species_remapping$proxy){
              unique(training_species_levels[training_species_levels$species == x, "level"])
            } else {
              stop("Error IM1: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")
            }
          }))
          weighting <- rbind(weighting, data.frame(level = species_to_impute_levels, species = species_to_impute, weight = 0.5)) # add them to weighting with an uninformative weight of 0.5 i.e. coin flip
        }
      }
    }
  }

  if(using_weights){
    # Check we have the weightings we should have at this point -----------------
    if(!all(table(weighting$species) == 1)){stop("Error: IM6: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")}
    if(is.null(species_remapping)){
      if(!all(networks_to_predict_rbind$species %in% weighting$species)){stop("Error IM3: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")}
    } else {
      if(!all(c(setdiff(networks_to_predict_rbind$species, names(which(species_remapping_species_frequency<2))), species_remapping$proxy) %in% weighting$species)){stop("Error IM4: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")} # all species we want to predict, except those with proxies that are in training_networks 0 or 1 times + all proxy species, should be in weighting at this point
    }

    # Relabel species weights with proxies if necessary -----------------
    if(!is.null(species_remapping)){
      for(i in 1:nrow(species_remapping)){
        focal_proxy <- species_remapping[i,"proxy"]
        focal_species <- species_remapping[i,"species"]
        proxy_to_add <- weighting[weighting$species == focal_proxy,]
        proxy_to_add$species <- focal_species
        weighting <- rbind(weighting, proxy_to_add)
      }
      if(any(table(weighting$species)>1)){
        weighting <- aggregate(weight ~ level + species, FUN = "mean", data = weighting) # take mean of weights if a proxy species needs to be combined with a focal_species that occurred more than once in training_networks and thus already had a weight
      }
    }
    if(!all(networks_to_predict_rbind$species %in% weighting$species)){stop("Error IM5: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")}
  }

  # Relabel species roles with proxies if necessary -----------------
  if(!is.null(species_remapping)){
    for(i in 1:nrow(species_remapping)){
      focal_proxy <- species_remapping[i,"proxy"]
      focal_species <- species_remapping[i,"species"]
      if(training_species_levels[match(focal_proxy, training_species_levels$species),"level"] == "row"){
        role_index <- 1
      } else if(training_species_levels[match(focal_proxy, training_species_levels$species),"level"] == "column"){
        role_index <- 2
      }
      proxy_to_add <- training_roles[[role_index]][training_roles[[role_index]]$species == focal_proxy,]
      proxy_to_add$network <- "remapped_proxy"
      proxy_to_add$species <- focal_species
      training_roles[[role_index]] <- rbind(training_roles[[role_index]], proxy_to_add)
    }
  }

  # Take average of each species' role -----------------
  mean_training_roles <- lapply(training_roles, function(x) aggregate(.~species, x[,-which(colnames(x) == "network")], median))

  # Remove roles and weights we don't need -----------------
  if(using_weights){
    weighting <- weighting[weighting$species %in% ntp_species,]
  }
  lapply(mean_training_roles, function(x){
    x[x$species %in% ntp_species,]
  })

  # Final checks to make sure we have everything we need -----------------
  if(!all(networks_to_predict_rbind$species %in% unlist(sapply(mean_training_roles, function(x) x$species)))){stop("Error IM9: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")}
  if(using_weights){
    if(!all(networks_to_predict_rbind$species %in% weighting$species)){stop("Error IM10: This error should never occur. Please contact the package maintainer with this error message and a reproducible example.")}
  }

  # Output -----------------
  if(conservatism == "none"){
    list(mean_roles = mean_training_roles)
  } else {
    list(mean_roles = mean_training_roles, processed_weighting = weighting)
  }

}
