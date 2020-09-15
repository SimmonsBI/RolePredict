ConservatismPermTest <- function(roles, n_it, species){
  # Argument checks -----------------
  # roles
  if(!inherits(roles,"list")){stop("'roles' must be a list of length 2")}
  if(length(roles) != 2){stop("'roles' must be a list of length 2")}
  if(!all(sapply(split(roles[[1]], roles[[1]]$network), function(x) all(table(x$species)==1)))){stop("Species cannot occur more than once in a network")}
  if(!all(sapply(roles, function(x){all(rowSums(x[,grepl(pattern = "np[0-9]", x = colnames(x))])>0)}))){stop("All species must occur in at least one position i.e. a species role cannot be 0 in all positions")}

  # n_it
  if(!(inherits(n_it,"numeric") | inherits(n_it, "integer"))){stop("'n_it' must be of class numeric or integer")}
  if(length(n_it) != 1){stop("'n_it' must be of length 1")}
  if(n_it < 1){stop("'n_it' must be greater than 0")}
  if(isFALSE(all.equal(n_it%%1, 0))){stop("'n_it' must be a whole number")}

  # species
  if(!inherits(species, "character")){stop("'species' must be a vector of character strings. The vector must be of length >= 1.")}
  if(length(species) < 1){stop("'species' must be a vector of length >= 1")}
  if(!all(table(species)==1)){stop("'species' must not contain any duplicate names")}

  # Make species argument if set to all, rows or columns -----------------
  all_species <- c(roles[[1]]$species, roles[[2]]$species) # all occurrences of all species in 'roles'
  if(length(species) == 1){
    if(species == "all"){ # if species == "all", assign it all species that occur more than once in 'roles'
      species <- names(which(table(all_species)>1))
    } else if(species == "rows"){
      species <- names(which(table(roles[[1]]$species)>1))
    } else if(species == "columns"){
      species <- names(which(table(roles[[2]]$species)>1))
    }
  }

  # More checks of species argument -----------------
  if(!all(species %in% all_species)){ # make sure all elements of 'species' are in 'roles'
    species_not_in_data <- setdiff(x = unique(species), y = unique(all_species))
    message("Error: All elements of 'species' must be present in 'roles'. Species which are not present have been returned as a vector object if you want to access these programmatically")
    return(species_not_in_data)
  }
  if(!all(table(all_species)[species]>1)){ # make sure all elements of 'species' are in 'roles' more than once
    species_not_more_than_once <- names(which(!(table(all_species)[species]>1)))
    message("Error: To assess conservatism, all elements of 'species' must occur more than once in 'roles' (it is not possible to assess conservatism across a single occurrence). Species in 'species' that do not occur more than once have been returned as a vector object if you want to access these programmatically")
    return(species_not_more_than_once)
  }

  # Make results container -----------------
  conservatism <- data.frame(level = NA, species = NA, observed_dissimilarity = NA, mean_null_dissimilarity = NA, delta = NA, z = NA, P = rep(NA, length(species)))

  # Calculate conservatism -----------------
  row_roles <- roles[[1]] # extract roles of row species
  column_roles <- roles[[2]] # extract roles of column species
  row_species <- row_roles$species # extract names of row species
  column_species <- column_roles$species # extract names of column species
  wr <- 0 # initialise row write counter at 0
  pb <- txtProgressBar(min = 0, max = length(species), style = 3)

  for(focal_species in species){ # for each member of species...
    wr <- wr + 1 # increment the write counter
    if(focal_species %in% row_species){ # if the focal species is a row species
      conservatism$level[wr] <- "row" # record that this is a row species
      focal_species_roles <- row_roles[row_roles$species == focal_species,grepl(pattern = "np[0-9]", x = colnames(row_roles))] # extract roles of focal species
      focal_species_mpd <- mean(vegan::vegdist(x = focal_species_roles, method = "bray")) # calculate mean pairwise Bray-Curtis distance between the focal species' roles
      null_roles <- row_roles # null roles to permute are set to the row roles
    } else if(focal_species %in% column_species){ # if the focal species is a column species
      conservatism$level[wr] <- "column" # record that this is a column species
      focal_species_roles <- column_roles[column_roles$species == focal_species,grepl(pattern = "np[0-9]", x = colnames(column_roles))] # extract roles of focal species
      focal_species_mpd <- mean(vegan::vegdist(x = focal_species_roles, method = "bray")) # calculate mean pairwise Bray-Curtis distance between the focal species' roles
      null_roles <- column_roles
    }
    null_mpds <- NULL # initialise a container to record the distribution of null mean pairwise distances
    for(i in 1:n_it){ # for 1 to the number of iterations
      null_roles$species <- ave(null_roles$species, null_roles$network, FUN = sample) # permute the species labels randomly within each network
      sampled_null_roles <- null_roles[null_roles$species == focal_species,grepl(pattern = "np[0-9]", x = colnames(null_roles))] # extract roles of 'focal' species from the permuted data (equivalent to randomly selecting a stratified sample)
      null_mpds <- c(null_mpds, mean(vegan::vegdist(x = sampled_null_roles, method = "bray"))) # record mean pairwise Bray-Curtis distance between the permuted focal species' roles
    }

    # Record output -----------------
    conservatism$species[wr] <- focal_species # species ID
    conservatism$observed_dissimilarity[wr] <- focal_species_mpd # observed mean pairwise dissimilarity
    conservatism$mean_null_dissimilarity[wr] <- mean(null_mpds) # mean of the null mean pairwise dissimilarities
    conservatism$delta[wr] <- conservatism$mean_null_dissimilarity[wr] - focal_species_mpd # difference between mean null dissimilarity and observed dissimilarity; if positive, the observed dissimilarity is less than random, and thus there is a positive conservatism signal
    conservatism$z[wr] <- (conservatism$mean_null_dissimilarity[wr] - focal_species_mpd)/sd(null_mpds) # z score; if positive, there is a positive conservatism signal
    conservatism$P[wr] <- sum(null_mpds < focal_species_mpd)/n_it # P value - proportion of iterations where the null MPD was less than the observed MPD
    setTxtProgressBar(pb, wr)
    cat("\r")
  }
  conservatism # output
}
