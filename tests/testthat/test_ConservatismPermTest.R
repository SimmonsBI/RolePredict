context("ConservatismPermTest")

test_that("Output is correct", {
  set.seed(123)
  m <- matrix(0,5,5)
  diag(m) <- 1
  m[5,1] <- 1
  m2 <- m
  m2[1,5] <- 1
  roles <- CalculateRolesMultipleNetworks(network_list = list(m,m2), weights_method = "none")
  out_all <- ConservatismPermTest(roles = roles, n_it = 10, species = "all")
  out_rows <- ConservatismPermTest(roles = roles, n_it = 10, species = "rows")
  out_columns <- ConservatismPermTest(roles = roles, n_it = 10, species = "columns")
  out_specificspecies <- ConservatismPermTest(roles = roles, n_it = 10, species = c("r1","r2","c1"))

  expect_identical(out_all, data_ConservatismPermTest_all)
  expect_identical(out_rows, data_ConservatismPermTest_rows)
  expect_identical(out_columns, data_ConservatismPermTest_columns)
  expect_identical(out_specificspecies, data_ConservatismPermTest_specificspecies)

  expect_is(out_all, "data.frame")
  expect_equal(nrow(out_all), 10)
  expect_equal(ncol(out_all), 7)
  expect_identical(out_all$level, c("column","column","column","column","column","row","row","row","row","row"))
  expect_identical(out_all$species, c("c1","c2","c3","c4","c5","r1","r2","r3","r4","r5"))
})

test_that("Argument checks work", {
  set.seed(123)
  m <- matrix(0,5,5)
  diag(m) <- 2.4
  m[5,1] <- 7
  m2 <- m
  m2[1,5] <- 3
  roles <- CalculateRolesMultipleNetworks(network_list = list(m,m2), weights_method = "none", six_node = TRUE, normalisation = "sum")

  expect_error(ConservatismPermTest(roles = "a", n_it = 10, species = "all"),"'roles' must be a list of length 2")
  expect_message(ConservatismPermTest(roles = roles, n_it = 10, species = c("r1","r2","c1","c9")),"Error: All elements of 'species' must be present in 'roles'. Species which are not present have been returned as a vector object if you want to access these programmatically")
  expect_equal(ConservatismPermTest(roles = roles, n_it = 10, species = c("r1","r2","c1","c9")), "c9")
  rownames(m) <- LETTERS[1:5]
  roles <- CalculateRolesMultipleNetworks(network_list = list(m,m2), weights_method = "none")
  expect_message(ConservatismPermTest(roles = roles, n_it = 10, species = c("c1","A")))
  expect_equal(ConservatismPermTest(roles = roles, n_it = 10, species = c("c1","A")), "A")
})
