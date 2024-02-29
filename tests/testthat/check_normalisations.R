# Check the different normalisations:
# * Class normalisation, total area normalisation per lipid class
# * Total area normalisation
# * z-score normalisation
# * z-score and class normalisation
#
# Normalisation is done on the raw data table, i.e. the filtered data.

dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))
  class_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_norm.RDS"))
  tot_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "tot_norm.RDS"))
  z_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "z_norm.RDS"))
  class_z_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_z_norm.RDS"))
  tot_z_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "tot_z_norm.RDS"))

  data_table <- r6_lipid$tables$raw_data

  #### Class normalisation ####
  # use the lipid class total to normalise the lipids of each class
  classes <- r6_lipid$tables$feature_table[, "lipid_class"]
  names(classes) <- rownames(r6_lipid$tables$feature_table)
  unique_classes <- unique(classes)

  # initialise result matrix
  data_class_norm <- data_table

  for(c in unique_classes) {
    # c <- unique_classes[1]
    lipids <- names(classes)[classes == c]

    if(length(lipids) > 1) {
      data_table_class <- data_table[, lipids]

      class_total <- rowSums(data_table_class, na.rm = TRUE)
    } else {
      class_total <- data_table_class <- data_table[, lipids]
      # fix NA's
      class_total[is.na(class_total)] <- 0
    }
    data_class_norm[, lipids] <- data_table_class[, lipids] / class_total
  }

  # compare the results
  waldo::compare(
    data_class_norm,
    class_norm_org
  )

  #### Total area normalisation ####
  data_tot_norm <- data_table / rowSums(data_table, na.rm = TRUE)

  # compare the results
  waldo::compare(
    data_tot_norm,
    tot_norm_org
  )

  #### z-score normalisation ####
  # mean centering and UV scaling
  data_centered <- t(t(data_table) - colMeans(data_table, na.rm = TRUE))
  data_z_norm <- t(t(data_centered) / Rfast::colVars(data_table, std = TRUE, na.rm = TRUE))

  # compare the results
  waldo::compare(
    data_z_norm,
    z_norm_org
  ) # small deviation far behind the comma, all fine


  #### z-score and class normalisation ####
  # z-score normalisation applied to the class normalisation
  # mean centering and UV scaling
  data_class_norm <- r6_lipid$tables$class_norm_data
  data_class_norm_centered <- t(t(data_class_norm) - colMeans(data_class_norm, na.rm = TRUE))
  data_class_z_norm <- t(t(data_class_norm_centered) / Rfast::colVars(data_class_norm, std = TRUE, na.rm = TRUE))

  # compare the results
  waldo::compare(
    data_class_z_norm,
    class_z_norm_org
  ) # small deviation far behind the comma, all fine


  #### z-score and total area normalisation ####
  # z-score normalisation applied to the total area normalisation
  # mean centering and UV scaling
  data_tot_norm <- r6_lipid$tables$total_norm_data
  data_tot_norm_centered <- t(t(data_tot_norm) - colMeans(data_tot_norm, na.rm = TRUE))
  data_tot_z_norm <- t(t(data_tot_norm_centered) / Rfast::colVars(data_tot_norm, std = TRUE, na.rm = TRUE))

  # compare the results
  waldo::compare(
    data_tot_z_norm,
    data_tot_z_norm
  ) # small deviation far behind the comma, all fine

}