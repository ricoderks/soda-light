# Check the creation of the feature table
# * The feature table is created from the column names of raw_data table, i.e.
#   the filtered table!


dev <- FALSE

if(dev) {
  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))

  # get the raw data table
  data_table <- r6_lipid$tables$raw_data

  feature_table <- data.frame(row.names = sort(colnames(data_table)))

  # extract the lipid classes from the lipid name
  feature_table$lipid_class <- stringr::str_extract(string = rownames(feature_table),
                                                    pattern = "^[a-zA-Z]*")

  # get all fa tails
  feature_table$temp <- stringr::str_extract(string = rownames(feature_table),
                                             pattern = "[0-9]{1,2}:[0-9]{1,2}(/|_|-FA)?([0-9]{1,2}:[0-9]{1,2})?")

  # get the fa tails separately
  my_split <- stringr::str_split(string = feature_table$temp,
                                 pattern = "/|_|-FA")
  feature_table$fa1 <- sapply(my_split, function(x) x[1])
  feature_table$fa2 <- sapply(my_split, function(x) x[2])

  # extract the number of carbons and number of unsaturation from the fa tails
  feature_table$carbons_1 <- NA
  feature_table$unsat_1 <- NA
  feature_table[, c("carbons_1", "unsat_1")] <- as.numeric(stringr::str_split(string = feature_table$fa1,
                                                                              pattern = ":",
                                                                              simplify = TRUE))
  feature_table$carbons_2 <- NA
  feature_table$unsat_2 <- NA
  feature_table[, c("carbons_2", "unsat_2")] <- as.numeric(stringr::str_split(string = feature_table$fa2,
                                                                              pattern = ":",
                                                                              simplify = TRUE))

  # get rid of some NA's
  feature_table$carbons_2[is.na(feature_table$carbons_2)] <- 0
  feature_table$unsat_2[is.na(feature_table$unsat_2)] <- 0

  # calculate total carbons and total unsaturation
  feature_table$carbons_sum <- ifelse(feature_table$lipid_class == "TG",
                                      feature_table$carbons_1,
                                      feature_table$carbons_1 + feature_table$carbons_2)
  feature_table$unsat_sum <- ifelse(feature_table$lipid_class == "TG",
                                      feature_table$unsat_1,
                                      feature_table$unsat_1 + feature_table$unsat_2)

  # select only the correct columns
  feature_table <- feature_table[, c("lipid_class", "carbons_1", "carbons_2", "carbons_sum", "unsat_1",  "unsat_2", "unsat_sum")]

  # compare
  waldo::compare(
    r6_lipid$tables$feature_table,
    feature_table
  )
}
