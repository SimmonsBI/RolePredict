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
  if(!all(sapply(network_list, function(x){
    sum(x) > 0
  }))){stop("Elements of 'network_list' must be matrices with at least one non-zero element")}
  args <- list(...)
  if(length(args$level) != 0){
    if(args$level != "all"){stop("'level' must be set to 'all'. This does not affect computation time.")}
  }

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
    nr <- nrow(network_list[[i]]) # number of rows in focal network
    nc <- ncol(network_list[[i]]) # number of columns in focal network
    roles_rows[[i]] <- roles_allspp[1:nr,] # store row species roles in their own list
    roles_columns[[i]] <- roles_allspp[(nr+1):(nr+nc),] # store column species roles in their own list
  }

  # Prepare output -----------------
  roles_rows <- do.call("rbind", roles_rows) # join row roles into a single df
  roles_columns <- do.call("rbind", roles_columns) # join column roles into a single df

  position_names <- colnames(roles_rows)[grepl(pattern = "np[0-9]", x = colnames(roles_rows))] # extract column names containing position frequencies i.e. the np columns
  position_numbers <- sapply(strsplit(position_names, split = "\\D+"), function(x) as.numeric(x[2])) # extract the position number from the np column names
  row_position_numbers <- c(2,4,6,8,11,12,14,16,18,21,22,24,25,28,29,31,34,35,38,41,42,44,46,48,51,52,55,56,57,60,61,63,64,67,68,70,73,74,77,78,81,82,86,87,88,92,93,94,98,99,102,103,106,107,108,111,112,114,117,118,121,122,124,127,128,132,133,136,137,140,143,144,146,148) # all possible position numbers for row nodes

  roles_rows <- roles_rows[,c(TRUE, TRUE, position_numbers %in% row_position_numbers)] # subset to row positions
  roles_columns <- roles_columns[,c(TRUE, TRUE, !position_numbers %in% row_position_numbers)] # subset to column positions

  list(roles_rows, roles_columns) # output as list
}
