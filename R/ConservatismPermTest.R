ConservatismPermTest <- function(roles, n_it, focal_taxa){
  # Argument checks -----------------
  # roles
  if(!inherits(roles,"list")){stop("'roles' must be a list of length 2")}
  if(length(roles) != 2){stop("'roles' must be a list of length 2")}

  # n_it
  if(!(inherits(n_it,"numeric") | inherits(n_it, "integer"))){stop("'n_it' must be of class numeric or integer")}
  if(length(n_it) != 1){stop("'n_it' must be of length 1")}
  if(n_it < 1){stop("'n_it' must be greater than 0")}
  if(isFALSE(all.equal(n_it%%1, 0))){stop("'n_it' must be a whole number")}

  # focal_taxa
  if(!inherits(focal_taxa, "character")){stop("'focal_taxa' must be a vector of character strings. The vector must be of length >= 1.")}
  if(length(focal_taxa) < 1){stop("'focal_taxa' must be a vector of length >= 1")}
  if(!all(table(focal_taxa)==1)){stop("focal_taxa must not contain any duplicate names")}

  all_species <- c(roles[[1]]$species, roles[[2]]$species) # all occurrences of all species in 'roles'

  if(focal_taxa == "all"){ # if focal_taxa == "all", assign it all species that occur more than once in 'roles'
    focal_taxa <- names(which(table(all_species)>1))
  }
  if(!all(focal_taxa %in% all_species)){ # make sure all focal_taxa are in 'roles'
    species_not_in_data <- setdiff(x = unique(focal_taxa), y = unique(all_species))
    message(paste0("Error: All elements of 'focal_taxa' must be present in 'roles'. The following species are not present (these have also been returned as a vector object if you want to access these programmatically): ", paste(species_not_in_data, collapse = "; ")))
    return(species_not_in_data)
  }
  if(!all(table(all_species)[focal_taxa]>1)){ # make sure all focal_taxa are in 'roles' more than once
    species_not_more_than_once <- names(which(!(table(all_species)[focal_taxa]>1)))
    message("Error: To assess conservatism, all elements of 'focal_taxa' must occur more than once in 'roles' (it is not possible to assess conservatism across a single occurrence). Species in 'focal_taxa' that do not occur more than once have been returned as a vector object if you want to access these programmatically")
    return(species_not_more_than_once)
  }

    # Make results container -----------------
    conservatism <- data.frame(level = NA, species = NA, observed_variance = NA, mean_null_variance = NA, delta = NA, z = NA, P = rep(NA, length(focal_taxa)))

    # Calculate conservatism -----------------
    for(taxon in focal_taxa){
      if(taxon %in% roles[[1]]$species){

      } else if(taxon %in% roles[[2]]$species){
      # roles_taxon <- roles[roles$speces]
    }


    }
}

library(nettools)
m1 <- default_names(rbm(10,10,0.3))
m2 <- default_names(rbm(10,10,0.3))
roles <- CalculateRolesMultipleNetworks(network_list = list(m1,m2), weights_method = "none")
focal_taxa <- c("r1","r2")
strat_level <- 1
n_it <- 10
