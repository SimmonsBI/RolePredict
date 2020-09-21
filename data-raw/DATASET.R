## code to prepare `DATASET` dataset goes here

# # roles
# network_list <- list(matrix(1,5,5),matrix(2,10,3))
# data_CalculateRolesMultipleNetworks <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none")
#
# # conservatism
# set.seed(123)
# m <- matrix(0,5,5)
# diag(m) <- 1
# m[5,1] <- 1
# m2 <- m
# m2[1,5] <- 1
# roles <- CalculateRolesMultipleNetworks(network_list = list(m,m2), weights_method = "none")
# data_ConservatismPermTest_all <- ConservatismPermTest(roles = roles, n_it = 10, species = "all")
# data_ConservatismPermTest_rows <- ConservatismPermTest(roles = roles, n_it = 10, species = "rows")
# data_ConservatismPermTest_columns <- ConservatismPermTest(roles = roles, n_it = 10, species = "columns")
# data_ConservatismPermTest_specificspecies <- ConservatismPermTest(roles = roles, n_it = 10, species = c("r1","r2","c1"))

usethis::use_data(data_CalculateRolesMultipleNetworks,
                  data_ConservatismPermTest_all,
                  data_ConservatismPermTest_rows,
                  data_ConservatismPermTest_columns,
                  data_ConservatismPermTest_specificspecies,
                  test_networks_binary,
                  test_networks_weighted,
                  internal = TRUE, overwrite = TRUE)
