context("CalculateRolesMultipleNetworks")

test_that("Argument checks work", {
  expect_error(CalculateRolesMultipleNetworks(network_list = "a"),"'network_list' must be a list")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(1,2,2))),"'network_list' must be a list of length 2 or more")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(data.frame(),data.frame())),"Elements of 'network_list' must be of class 'matrix'")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(1,2,2),matrix("a",2,2))),"Elements of 'network_list' must be matrices which only contain numeric or integer elements")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(FALSE,2,2),matrix("a",2,2))),"Elements of 'network_list' must be matrices which only contain numeric or integer elements")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(-1,2,2),matrix(1,2,2))),"Elements of 'network_list' must be matrices whose elements are >= 0")
  expect_error(CalculateRolesMultipleNetworks(list(matrix(0,2,2),matrix(0,2,2)),weights_method="none"), "Elements of 'network_list' must be matrices with at least one non-zero element")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(1,2,2),matrix(1,2,2)), weights_method = "none", level = "rows"),"'level' must be set to 'all'. This does not affect computation time.")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(1,2,2),matrix(1,2,2)), weights_method = "none", level = "columns"),"'level' must be set to 'all'. This does not affect computation time.")
})

test_that("Output is correct", {
  network_list <- list(matrix(1,5,5),matrix(2,10,3))
  out <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none")
  expect_is(out, "list")
  expect_equal(length(out), 2)
  expect_equal(nrow(out[[1]]), 15)
  expect_equal(nrow(out[[2]]), 8)
  expect_true(all(unlist(lapply(out, ncol))==25))
  expect_true(all(unlist(lapply(out, function(x) inherits(x, "data.frame")))))
  expect_true(inherits(out[[1]][,1], "character"))
  expect_true(inherits(out[[1]][,2], "character"))
  expect_true(inherits(out[[2]][,1], "character"))
  expect_true(inherits(out[[2]][,2], "character"))
  expect_true(all(apply(out[[1]][,3:25], 1:2, function(x) inherits(x, "numeric"))))
  expect_true(all(apply(out[[2]][,3:25], 1:2, function(x) inherits(x, "numeric"))))
  expect_identical(out, data_CalculateRolesMultipleNetworks)
})

test_that("bmotif arguments work",{
  set.seed(123)
  network_list <- list(matrix(sample(0:1,25,TRUE),5,5),matrix(sample(0:1,12,TRUE),3,4))
  out <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none", six_node = TRUE)
  expect_true(all(unlist(lapply(out, ncol))==76))
})

test_that("Row and column species are split correctly between output list elements",{
  m <- matrix(0,20,20)
  m[5,5] <- 1
  out <- CalculateRolesMultipleNetworks(list(m,m),weights_method="none")

  expect_false(any(grepl(pattern = "c", x = out[[1]]$species))) # make sure no c species in the row element
  expect_false(any(grepl(pattern = "r", x = out[[2]]$species))) # make sure no r species in the column element

  expect_true(all(sapply(out, function(x){ # for each level, each network should have 20 data points
    out_split <- split(x, x$network)
    all(sapply(out_split, nrow) == 20)
  })))

  out_split <- split(out[[1]], out[[1]]$network) # at the row level, each network should contain *only* species r1:r20
  expect_true(all(sapply(out_split, function(y){
    identical(y$species, paste0("r",1:20))
  })))
  out_split <- split(out[[2]], out[[2]]$network) # at the column level, each network should contain *only* species c1:c20
  expect_true(all(sapply(out_split, function(y){
    identical(y$species, paste0("c",1:20))
  })))

  expect_true(all(table(out[[1]]$species)[paste0("r",1:20)] == 2)) # at the row level, species r1:r20 should each occur twice
  expect_true(all(table(out[[2]]$species)[paste0("c",1:20)] == 2)) # at the column level, species c1:c20 should each occur twice
})
