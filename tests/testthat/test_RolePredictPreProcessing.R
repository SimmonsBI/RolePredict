context("RolePredictPreProcessing")

test_that("No errors are shown for all weighting-remapping combinations", {
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "calculate"
  n_it <- 2
  species_remapping <- NULL
  weighting = NULL
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  conservatism <- "conservatism_output"
  weighting = Conservatism(network_list = training_networks, n_it = n_it, species = "all")
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # no weightings, no remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "none"
  species_remapping <- NULL
  weighting = NULL
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # no weightings, remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  networks_to_predict[[1]]$species[1] <- "test_species"
  conservatism <- "none"
  species_remapping <- data.frame(species = c("test_species","sp27", "sp1001"), proxy = c("sp34", "sp30", "sp1133"))
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # calculate, no remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "calculate"
  species_remapping <- NULL
  weighting = NULL
  n_it <- 2
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # calculate, remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))),
                              data.frame(level = c(rep("row", nrow(test_networks_binary[[1]])),rep("column", ncol(test_networks_binary[[1]]))),species = c(rownames(test_networks_binary[[1]]), colnames(test_networks_binary[[1]]))))
  networks_to_predict[[1]]$species[1] <- "test_species"
  conservatism <- "calculate"
  species_remapping <- data.frame(species = c("test_species","sp27", "sp1001"), proxy = c("sp34", "sp30", "sp1133"))
  weighting = NULL
  n_it <- 2
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # custom, no remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "custom"
  species_remapping <- NULL
  weighting = cbind(networks_to_predict[[1]], runif(nrow(networks_to_predict[[1]]), 1, 100))
  colnames(weighting)[3] <- "weight"
  n_it <- 2
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # custom, remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "custom"
  species_remapping <- data.frame(species = c("sp1107","sp11"), proxy = c("sp1118", "sp12"))
  weighting <- cbind(networks_to_predict[[1]], runif(nrow(networks_to_predict[[1]]), 1, 100))
  colnames(weighting)[3] <- "weight"
  proxy_weights <- as.data.frame(cbind(c("row"), c("sp12"), c(9)))
  names(proxy_weights) <- colnames(weighting)
  weighting <- rbind(weighting, proxy_weights)
  weighting$weight <- as.numeric(weighting$weight)
  n_it <- 2
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # conservatism_output, no remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))),
                              data.frame(level = c(rep("row", nrow(test_networks_binary[[1]])),rep("column", ncol(test_networks_binary[[1]]))),species = c(rownames(test_networks_binary[[1]]), colnames(test_networks_binary[[1]]))))
  conservatism <- "conservatism_output"
  species_remapping <- NULL
  n_it <- 2
  weighting <- Conservatism(network_list = training_networks, n_it = n_it, species = setdiff(unlist(lapply(networks_to_predict, function(x) x$species)), c("sp1001","sp1004")))
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)

  # conservatism_output, remapping
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))),
                              data.frame(level = c(rep("row", nrow(test_networks_binary[[1]])),rep("column", ncol(test_networks_binary[[1]]))),species = c(rownames(test_networks_binary[[1]]), colnames(test_networks_binary[[1]]))))
  conservatism <- "conservatism_output"
  species_remapping <- data.frame(species = c("sp1136","sp11"), proxy = c("sp1118", "sp33"))
  n_it <- 2
  weighting <- Conservatism(network_list = training_networks, n_it = n_it, species = "all")
  expect_error(RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping), NA)
})

test_that("Remapping produces the right weights",{
  # if we assigned test_species as a species which isn't in training networks to have sp11 as a proxy, the weight should = sp11 by itself
  # if we assigned sp11 to equal sp15 as a proxy, the weight should equal sp11 supplemented with sp15, i.e. the mean of sp11 and sp15 weights
  # sp15 is in networks_to_predict so its weight should be the weight of just sp15
  # therefore mean(test_species weight, sp15 weight) == sp11 weight
  set.seed(123)
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  networks_to_predict[[1]]$species[1] <- "test_species"
  conservatism <- "calculate"
  species_remapping <- data.frame(species = c("test_species","sp11"), proxy = c("sp11", "sp15"))
  weighting <- NULL
  n_it <- 1000
  out <- RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping)$processed_weighting
  expect_equal(mean(c(out[out$species == "test_species","weight"], out[out$species == "sp15","weight"])) , out[out$species == "sp11","weight"])
})

test_that("Remapping produces the right roles",{
  # the species_remapping$species should have the same roles as the species_remapping$proxy
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  networks_to_predict[[1]]$species[1] <- "test_species"
  networks_to_predict[[1]]$species[28] <- "test_species2"
  conservatism <- "none"
  species_remapping <- data.frame(species = c("test_species","test_species2"), proxy = c("sp4","sp1002"))
  weighting <- NULL
  out <- RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping)
  row_roles <- out$mean_roles[[1]]
  col_roles <- out$mean_roles[[2]]

  r1 <- row_roles[row_roles$species == "sp4",2:ncol(row_roles)]
  r2 <- row_roles[row_roles$species == "test_species",2:ncol(row_roles)]
  rownames(r1) <- NULL
  rownames(r2) <- NULL
  expect_identical(r1,r2)

  r1 <- col_roles[col_roles$species == "test_species2",2:ncol(col_roles)]
  r2 <- bmotif::node_positions(M = training_networks$network27, weights_method = "none")["sp1002",]
  r2 <- r2[,colnames(r1)]
  rownames(r1) <- NULL
  rownames(r2) <- NULL
  expect_identical(r1,r2)
})


test_that("Imputation works",{
  # a species which does not occur, with a proxy which only occurs once, should have a weight of 0.5
  training_networks <- test_networks_binary
  rownames(training_networks[[1]])[1] <- "test_proxy"
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  networks_to_predict[[1]]$species[1] <- "test_species"
  conservatism <- "calculate"
  n_it <- 100
  species_remapping <- data.frame(species = "test_species", proxy = "test_proxy")
  weighting = NULL
  out <- RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping)[[2]]
  expect_equal(out[out$species == "test_species","weight"], 0.5) # proxy only occurs once in data, and test_species doesnt occur at all, so should be imputed to 0.5
  expect_equal(out[out$species == "sp1004","weight"], 0.5) # only occurs once in data so should be imputed to 0.5
  expect_equal(out[out$species == "sp1001","weight"], 0.5) # only occurs once in data so should be imputed to 0.5

  # a species which occurs, with a proxy which only occurs once, should have a weight of 0.5
  training_networks <- test_networks_binary
  networks_to_predict <- list(data.frame(level = c(rep("row", nrow(test_networks_binary[[60]])),rep("column", ncol(test_networks_binary[[60]]))),species = c(rownames(test_networks_binary[[60]]), colnames(test_networks_binary[[60]]))))
  conservatism <- "calculate"
  n_it <- 100
  species_remapping <- data.frame(species = "sp1001", proxy = "sp1004")
  weighting = NULL
  out <- RolePredictPreProcessing(training_networks = training_networks,networks_to_predict = networks_to_predict,conservatism = conservatism,n_it = n_it,weighting = weighting,species_remapping = species_remapping)[[2]]
  expect_equal(out[out$species == "sp1001","weight"], 0.5)
  expect_equal(out[out$species == "sp1004","weight"], 0.5)
})


