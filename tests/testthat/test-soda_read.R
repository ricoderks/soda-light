# run the test by executing this script and not with devtools or button Run Tests
library(testthat)
source("./R/class_lips_exp.R")
source("./R/utils.R")

raw_meta <- readRDS(test_path("test_data", "meta_data_raw.RDS"))
r6 <- readRDS(test_path("test_data", "r6.RDS"))

test_that("Read data", {
  # meta data file
  expect_equal(
    soda_read_table(test_path("test_data", "SampleMasterfile.xlsx")),
    raw_meta
  )

  # creating r6 object
  expect_equal(
    example_lipidomics(name = "Lips_1",
                       id = "mod_exp_1",
                       slot = "exp_1",
                       experiment_id = "VDK_220223_01"),
    r6
  )

})
