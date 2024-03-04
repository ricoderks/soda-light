# Check the normalisations applied to the class table
# * z-score normalisation
# * total area normalisation
# * z-score and class normalisation

dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))
  z_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_table_z_norm.RDS"))
  tot_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_table_tot_norm.RDS"))
  tot_z_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_table_tot_z_norm.RDS"))

  data_table <- r6_lipid$tables$class_table

  #### z-score normalisation ####
  # mean centering and UV scaling
  data_centered <- t(t(data_table) - colMeans(data_table, na.rm = TRUE))
  data_z_norm <- t(t(data_centered) / Rfast::colVars(data_table, std = TRUE, na.rm = TRUE))

  # compare the results
  waldo::compare(
    data_z_norm,
    z_norm_org
  ) # small deviation far behind the comma, all fine


  #### Total area normalisation ####
  data_tot_norm <- data_table / rowSums(data_table, na.rm = TRUE)

  # compare the results
  waldo::compare(
    data_tot_norm,
    tot_norm_org
  ) # small deviation far behind the comma, all fine


  #### z-score and total area normalisation ####
  # z-score normalisation applied to the total area normalisation
  # mean centering and UV scaling
  data_tot_norm <- data_table / rowSums(data_table, na.rm = TRUE)
  data_tot_norm_centered <- t(t(data_tot_norm) - colMeans(data_tot_norm, na.rm = TRUE))
  data_tot_z_norm <- t(t(data_tot_norm_centered) / Rfast::colVars(data_tot_norm, std = TRUE, na.rm = TRUE))

  # compare the results
  waldo::compare(
    data_tot_z_norm,
    data_tot_z_norm
  ) # all fine
}