CalculateRolesMultipleNetworks <- function(network_list, ...){
  # Argument checks -----------------
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

  # Extract network names -----------------
  if(!is.null(names(network_list))){
    network_names <- names(network_list)
  } else {
    network_names <- paste0("network", 1:length(network_list))
  }

  # Calculate species roles for each network -----------------
  roles_rows <- list() # initialise results container
  roles_columns <- list() # initialise results container

  for(i in seq(network_list)){
    roles_allspp <- bmotif::node_positions(M = network_list[[i]], ...) # calculate roles of species in network i
    roles_allspp$network <- network_names[i] # add network name
    roles_allspp$species <- rownames(roles_allspp) # move species name from rowname to its own column
    rownames(roles_allspp) <- NULL # remove row names
    roles_allspp <- roles_allspp[,c((ncol(roles_allspp)-1), ncol(roles_allspp), which(grepl(pattern = "np", x = colnames(roles_allspp))))] # reorder columns, so it goes network, species, np1...
    roles_rows[[i]] <- roles_allspp[roles_allspp$np1 == 0,] # store row species roles in their own list
    roles_columns[[i]] <- roles_allspp[roles_allspp$np1 != 0,] # store column species roles in their own list
  }

  # Prepare output -----------------
  roles_rows <- do.call("rbind", roles_rows) # join row roles into a single df
  roles_columns <- do.call("rbind", roles_columns) # join column roles into a single df
  list(roles_rows, roles_columns) # output as list
}
