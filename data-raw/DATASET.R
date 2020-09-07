## code to prepare `DATASET` dataset goes here

network_list <- list(matrix(1,5,5),matrix(2,10,3))
data_CalculateRolesMultipleNetworks <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none")

usethis::use_data(data_CalculateRolesMultipleNetworks, internal = TRUE, overwrite = TRUE)
