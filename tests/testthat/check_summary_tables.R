# Check the summary tables. A summary table gives the median per species/class
# per group.
# There are 2 summary tables:
# * species
# * classes
#
# Grouping information is stored in the meta table(s) in the column `genoType`.

dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))
  species_sum_org <- readRDS(file.path("tests", "testthat", "test_data", "summary_species.RDS"))
  class_sum_org <- readRDS(file.path("tests", "testthat", "test_data", "summary_classes.RDS"))

  data_table <- r6_lipid$tables$raw_data
  groups <- r6_lipid$tables$raw_meta[, "genoType"]
  names(groups) <- rownames(r6_lipid$tables$raw_meta)

  #### Species ####
  species_sum <- aggregate(data_table, by = list(groups), median, na.rm = TRUE)
  # add rownames
  rownames(species_sum) <- species_sum$Group.1
  # Remove the group column
  species_sum$Group.1 <- NULL

  # the order of the groups can be changed, fix here
  species_sum <- species_sum[unique(groups), ]

  # compare
  waldo::compare(
    species_sum,
    species_sum_org
  ) # all fine

  #### Classes ####
  class_table <- r6_lipid$tables$class_table
  groups <- r6_lipid$tables$raw_meta[, "genoType"]
  names(groups) <- rownames(r6_lipid$tables$raw_meta)

  class_sum <- aggregate(class_table, by = list(groups), median, na.rm = TRUE)
  # add rownames
  rownames(class_sum) <- class_sum$Group.1
  # Remove the group column
  class_sum$Group.1 <- NULL

  # the order of the groups can be changed, fix here
  class_sum <- class_sum[unique(groups), ]

  # compare
  waldo::compare(
    class_sum,
    class_sum_org
  ) # all fine

}