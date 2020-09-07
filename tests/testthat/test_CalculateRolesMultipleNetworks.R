context("CalculateRolesMultipleNetworks")

test_that("Argument checks work", {
  expect_error(CalculateRolesMultipleNetworks(network_list = "a"),"'network_list' must be a list")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(1,2,2))),"'network_list' must be a list of length 2 or more")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(data.frame(),data.frame())),"Elements of 'network_list' must be of class 'matrix'")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(1,2,2),matrix("a",2,2))),"Elements of 'network_list' must be matrices which only contain numeric or integer elements")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(FALSE,2,2),matrix("a",2,2))),"Elements of 'network_list' must be matrices which only contain numeric or integer elements")
  expect_error(CalculateRolesMultipleNetworks(network_list = list(matrix(-1,2,2),matrix(1,2,2))),"Elements of 'network_list' must be matrices whose elements are >= 0")
})

test_that("Output is correct", {
  network_list <- list(matrix(1,5,5),matrix(2,10,3))
  out <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none")
  expect_is(out, "list")
  expect_equal(length(out), 2)
  expect_equal(nrow(out[[1]]), 15)
  expect_equal(nrow(out[[2]]), 8)
  expect_true(all(unlist(lapply(out, ncol))==48))
  expect_true(all(unlist(lapply(out, function(x) inherits(x, "data.frame")))))
  expect_true(inherits(out[[1]][,1], "character"))
  expect_true(inherits(out[[1]][,2], "character"))
  expect_true(inherits(out[[2]][,1], "character"))
  expect_true(inherits(out[[2]][,2], "character"))
  expect_true(all(apply(out[[1]][,3:48], 1:2, function(x) inherits(x, "numeric"))))
  expect_true(all(apply(out[[2]][,3:48], 1:2, function(x) inherits(x, "numeric"))))
  expect_identical(out, data_CalculateRolesMultipleNetworks)
})

test_that("bmotif arguments work",{
  set.seed(123)
  network_list <- list(matrix(sample(0:1,25,TRUE),5,5),matrix(sample(0:1,12,TRUE),3,4))
  out <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none", six_node = TRUE)
  expect_true(all(unlist(lapply(out, ncol))==150))

  out <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none", six_node = FALSE, level = "rows")
  expect_equal(nrow(out[[2]]), 0)
})
