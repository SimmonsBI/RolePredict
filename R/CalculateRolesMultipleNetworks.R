#' Calculate roles of species in mutliple networks
#'
#' @param network_list A list of numeric biadjacency matrices representing interactions between two groups of nodes. Each row corresponds to a node in one level
#' and each column corresponds to a node in the other level. Elements of M are positive numbers if nodes do interact, and 0
#' otherwise. Formally, M is a biadjacency matrix. When nodes i and j interact, m_ij > 0; if they do not interact, m_ij = 0. Matrix can be binary or quantitative.
#' @param ... Arguments to be passed to `bmotif`, such as whether motifs up to six nodes should be considered, and whether species roles should be based on
#' weighted or binary interactions. See `?node_positions` for details of the arguments that can be passed to `bmotif`.
#' @return A list of length two, where the first element (`row_roles`) contains the roles of all row species from across all networks in `network_list` and the
#' second element (`column_roles`) contains the roles of all column species from across all networks in `network_list`.
#'
#' Each element is a data frame with one row for each occurrence of each species. The first column is the network in which that row's species occurred. The second column
#' is the species name itself. Columns 3 onwards are one column for each node position: 23 columns if \code{six_node} is FALSE, and 74 columns if \code{six_node} is TRUE (depending on which was specified via
#' the optional arguments passed through to `bmotif` using the '`...`' argument; default is \code{six_node} is FALSE).
#'
#' Columns names are given as "npx" where x is the ID of the position as described in Simmons et al. (2019) (and originally in Appendix 1 of Baker et al. (2015)). \strong{To view the 'motif dictionary' showing
#' which node position a given ID corresponds to, load `bmotif` via `library(bmotif)` then enter \code{vignette("bmotif-dictionary")}.}
#'
#' For a network with A rows and P columns, by default (where \code{level} = "all") the data frame has A + P rows, one for each node. If \code{level} = "rows", the data frame will have A rows, one for each row node;
#' if \code{level} = "columns", it will have P rows, one for each column node.
#'
#' By default, the elements of this data frame will be the raw binary or weighted position measures (depending on which was requested). If \code{normalisation} is set to something other than "none", the elements will be
#' normalised position counts as described above.
#'
#' If \code{weights_method} is set to 'all', \code{node_positions} instead returns a list of length five, each containing a data.frame corresponding to
#' one of the five weighting methods described above.
#'
#' @references
#' Baker, N., Kaartinen, R., Roslin, T., and Stouffer, D. B. (2015). Species’ roles in food webs show fidelity across a highly variable oak forest. Ecography, 38(2):130–139.
#'
#' Simmons, BI, Sweering, MJM, Schillinger, M, Dicks, LV, Sutherland, WJ, Di Clemente, R. (2019). bmotif: A package for motif analyses of bipartite networks. Methods Ecol Evol; 10: 695– 701. https://doi.org/10.1111/2041-210X.13149

#' @examples
#' m1 <- matrix(c(1,0,0,1),2,2) # a sample network
#' m2 <- matrix(c(1,1,0,1,0,1),3,2) # another sample network
#' CalculateRolesMultipleNetworks(network_list = list(m1,m2))
#'
#' # for six_nodes we can pass an optional argument to bmotif
#' CalculateRolesMultipleNetworks(network_list = list(m1,m2), six_node = TRUE)
#'
#' # if networks are weighted, we can specify
#' @export
CalculateRolesMultipleNetworks <- function(network_list, ...){
  # Argument checks -----------------
  args <- list(...)
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
  if(!all(sapply(network_list, function(x){all(colSums(x)>0) & all(rowSums(x)>0)}))){stop("One or more networks in 'network_list' has one or more empty rows or empty columns. Networks must not have any empty rows or empty columns")}

  # Extract network names -----------------
  if(!is.null(names(network_list))){
    network_names <- names(network_list)
  } else {
    network_names <- paste0("network", 1:length(network_list))
  }

  # Calculate species roles for each network -----------------
  roles_rows <- list() # initialise results container
  roles_columns <- list() # initialise results container
  pb <- utils::txtProgressBar(min = 0, max = length(network_list), style = 3) # progress bar

  for(i in seq(network_list)){
    if(is.null(args$weights_method)){ # if weights_method isn't set, set it to 'none'
      roles_allspp <- bmotif::node_positions(M = network_list[[i]], weights_method = "none", ...) # calculate roles of species in network i, assuming weights_method = 'none'
    } else {
      roles_allspp <- bmotif::node_positions(M = network_list[[i]], ...) # calculate roles of species in network i
    }
    roles_allspp$network <- network_names[i] # add network name
    roles_allspp$species <- rownames(roles_allspp) # move species name from rowname to its own column
    rownames(roles_allspp) <- NULL # remove row names
    roles_allspp <- roles_allspp[,c((ncol(roles_allspp)-1), ncol(roles_allspp), which(grepl(pattern = "np", x = colnames(roles_allspp))))] # reorder columns, so it goes network, species, np1...
    nr <- nrow(network_list[[i]]) # number of rows in focal network
    nc <- ncol(network_list[[i]]) # number of columns in focal network
    roles_rows[[i]] <- roles_allspp[1:nr,] # store row species roles in their own list
    roles_columns[[i]] <- roles_allspp[(nr+1):(nr+nc),] # store column species roles in their own list
    utils::setTxtProgressBar(pb, i)
  }

  # Prepare output -----------------
  roles_rows <- do.call("rbind", roles_rows) # join row roles into a single df
  roles_columns <- do.call("rbind", roles_columns) # join column roles into a single df

  position_names <- colnames(roles_rows)[grepl(pattern = "np[0-9]", x = colnames(roles_rows))] # extract column names containing position frequencies i.e. the np columns
  position_numbers <- sapply(strsplit(position_names, split = "\\D+"), function(x) as.numeric(x[2])) # extract the position number from the np column names
  row_position_numbers <- c(2,4,6,8,11,12,14,16,18,21,22,24,25,28,29,31,34,35,38,41,42,44,46,48,51,52,55,56,57,60,61,63,64,67,68,70,73,74,77,78,81,82,86,87,88,92,93,94,98,99,102,103,106,107,108,111,112,114,117,118,121,122,124,127,128,132,133,136,137,140,143,144,146,148) # all possible position numbers for row nodes

  roles_rows <- roles_rows[,c(TRUE, TRUE, position_numbers %in% row_position_numbers)] # subset to row positions
  roles_columns <- roles_columns[,c(TRUE, TRUE, !position_numbers %in% row_position_numbers)] # subset to column positions

  list(row_roles = roles_rows, column_roles = roles_columns) # output as list
}
