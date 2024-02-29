# Check the different normalisations:
# * Class normalisation, total area normalisation per lipid class
#
# Normalisation is done on the raw data table, i.e. the filtered data.

dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))
  class_norm_org <- readRDS(file.path("tests", "testthat", "test_data", "class_norm.RDS"))

  data_table <- r6_lipid$tables$raw_data

  ## Class normalisation
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
}