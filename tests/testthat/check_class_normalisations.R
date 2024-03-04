# Check the normalisations applied to the class table
# * z-score normalisation

dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))
  z_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_table_z_norm.RDS"))

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





}