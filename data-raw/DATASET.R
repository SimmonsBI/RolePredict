## code to prepare `DATASET` dataset goes here

data_CalculateRolesMultipleNetworks <- CalculateRolesMultipleNetworks(network_list = network_list, weights_method = "none")

usethis::use_data(data_CalculateRolesMultipleNetworks, internal = TRUE, overwrite = TRUE)
