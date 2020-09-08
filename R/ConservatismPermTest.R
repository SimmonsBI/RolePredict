ConservatismPermTest <- function(network_list, roles, strat_level, n_it, which_taxa){
  # Argument checks -----------------
  # network_list
  if(!inherits(network_list, "list")){stop("'network_list' must be a list")}
  if(length(network_list)<2){stop("'network_list' must be a list of length 2 or more")}
  if(!all(sapply(network_list, inherits, "matrix"))){stop("Elements of 'network_list' must be of class 'matrix'")}
  if(!all(sapply(network_list, function(x){
    all(apply(x, 1:2, function(y) is.numeric(y) | is.integer(y)))
  }))){stop("Elements of 'network_list' must be matrices which only contain numeric or integer elements")}
  if(!all(sapply(network_list, function(x){
    all(apply(x, 1:2, function(x) length(x) == 1))
  }))){stop("Elements of 'network_list' must be matrices whose elements are of length 1")}
  if(!all(sapply(network_list, function(x){
    all(apply(x, 1:2, function(x) x >= 0))
  }))){stop("Elements of 'network_list' must be matrices whose elements are >= 0")}
  if(!all(sapply(network_list, function(x){
    sum(x) > 0
  }))){stop("Elements of 'network_list' must be matrices with at least one non-zero element")}
  for(i in seq(network_list)){
    dn <- dimnames(network_list[[i]])
    if(is.null(dn)){
      stop("All matrices in 'network_list' must have row and column names i.e. taxa names")
    } else if(any(sapply(dn, is.null))){
      stop("All matrices in 'network_list' must have row and column names i.e. taxa names")
    }
  }
  all_species <- unlist(lapply(network_list, function(x) unlist(dimnames(x))))
  if(!any(table(all_species)>1)){stop("At least one taxon must occur more than once in the data to assess conservatism")}

  # roles
  if(!inherits(roles,"list")){stop("'roles' must be a list of length 2")}
  if(length(roles) != 2){stop("'roles' must be a list of length 2")}

  # strat_level
  if(!(inherits(strat_level,"numeric") | inherits(strat_level, "integer"))){stop("'strat_level' must be of class numeric or integer")}
  if(length(strat_level) != 1){stop("'strt_level' must be of length 1")}
  if(!strat_level %in% c(1,2,3)){stop("'strt_level' must be 1, 2 or 3")}

  # n_it
  if(!(inherits(n_it,"numeric") | inherits(n_it, "integer"))){stop("'n_it' must be of class numeric or integer")}
  if(length(n_it) != 1){stop("'n_it' must be of length 1")}
  if(n_it < 1){stop("'n_it' must be greater than 0")}
  if(isFALSE(all.equal(n_it%%1, 0))){stop("'n_it' must be a whole number")}

  # which_taxa
  if(!inherits(which_taxa, "character")){stop("'which_taxa' must be a vector of character strings. The vector must be of length >= 1.")}
  if(length(which_taxa) < 1){stop("'which_taxa' must be a vector of length >= 1")}
  if(!all(which_taxa %in% all_species)){
    species_not_in_data <- setdiff(x = unique(which_taxa), y = unique(all_species))
    message(paste0("Error: All elements of 'which_taxa' must be present in network_list as either a row or column name. The following species are not present (these have also been returned as a vector object if you want to access these programmatically): ", paste(species_not_in_data, collapse = "; ")))
    return(species_not_in_data)
  }
  if(!all(table(all_species)[which_taxa]>1)){
    species_not_more_than_once <- names(which(!(table(all_species)[which_taxa]>1)))
    message(paste0("Error: To assess conservatism, all elements of 'which_taxa' must be present in more than one network in network_list as a row or column name. The following species in 'which_taxa' do not occur more than once (these have also been returned as a vector object if you want to access these programmatically): ", paste(species_not_more_than_once, collapse = "; ")))
    return(species_not_more_than_once)
  }
}





