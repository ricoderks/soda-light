# Check creating the lipid class table
# * The lipid class table is created from the raw_data table, i.e. the already
#   filtered data.

dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))
  class_table_org <- readRDS(file.path("tests", "testthat", "test_data", "class_table.RDS"))

  classes <- r6_lipid$tables$feature_table$lipid_class
  names(classes) <- rownames(r6_lipid$tables$feature_table)
  unique_classes <- unique(classes)

  data_table <- r6_lipid$tables$raw_data

  class_table <- matrix(NA,
                        nrow = nrow(data_table),
                        ncol = length(unique_classes))
  rownames(class_table) <- rownames(data_table)
  colnames(class_table) <- unique_classes

  for(c in unique_classes) {
    lipids <- names(classes)[classes == c]
    class_table[, c] <- rowSums(data_table[, lipids], na.rm = TRUE)
  }

  # compare the results
  waldo::compare(
    class_table,
    # the order of the column names is different
    class_table_org[, colnames(class_table)]
  )
   # all fine
}