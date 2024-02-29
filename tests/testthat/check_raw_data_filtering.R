# Check the filtering of lipids of the raw data.
# * Lipids are removed if in 20% of the samples they are below the sample / blank ratio of 2.
# * Lipids are introduced back if they are in more than 80% of the samples of one group above
#   the sample / blank ratio of 2.

dev <- FALSE

if(dev) {
  ### default settings use to create r6$tables$raw_data
  # apply_imputation = FALSE,
  # impute_before = FALSE,
  # apply_filtering = TRUE,
  # imputation_function = 'minimum',
  # val_threshold = 0.6,
  # blank_multiplier = 2,
  # sample_threshold = 0.8,
  # group_threshold = 0.8,
  # norm_col = ''

  # read the r6 object, this is from experiment id VDK_220223_01
  r6_lipid <- readRDS(file.path("tests", "testthat", "test_data", "r6.RDS"))

  # set some parameters
  blank_multiplier <- 2
  sample_threshold <- 0.8
  group_threshold = 0.8

  # get the imported data, this contains all samples, blanks and qc's and all lipids
  data_table <- r6_lipid$tables$imp_data

  # remove the ID column
  data_table$ID <- NULL

  # convert to matrix
  data_table <- as.matrix(data_table)

  # only keep the sample rows
  data_table <- data_table[rownames(r6_lipid$tables$raw_meta), ]

  # filter out columns which only contain NA's
  idx_col_na <- apply(data_table, 2, function(x) {
    all(is.na(x))
  })
  data_table <- data_table[, !idx_col_na]

  ### go over all batches
  # get batches
  all_batches <- unique(r6_lipid$tables$imp_meta$batchNumber)
  for(batch in all_batches) {
    # get the blanks and the samples
    batch_idx <- which(r6_lipid$tables$imp_meta[, "batchNumber"] == batch)
    batch_idx_blanks <- base::intersect(batch_idx, r6_lipid$indices$idx_blanks)
    batch_idx_samples <- base::intersect(batch_idx, r6_lipid$indices$idx_samples)

    # Get rownames of the blanks and samples
    batch_blanks <- rownames(r6_lipid$tables$imp_meta)[batch_idx_blanks]
    batch_samples <- rownames(r6_lipid$tables$imp_meta)[batch_idx_samples]
    batch_samples <- base::intersect(rownames(data_table), batch_samples)

    # here data_table has less columns than blank_table
    # empty columns where removed from data_table
    data_table <- data_table[batch_samples, ]
    blank_table <- r6_lipid$tables$blank_table[as.character(batch_blanks), ]

    ## which lipids should be filter based on the whole batch
    # get the means of the columns
    blank_means <- apply(blank_table, 2, mean, na.rm = TRUE)
    # make sure to use the same columns as in the samples (some columns where already removed because they only contained NA's)
    blank_means <- blank_means[colnames(blank_table) %in% colnames(data_table)]
    # get rid of NA's
    blank_means[is.na(blank_means)] <- 0
    # multiply by the ratio sample / blank
    blank_means <- blank_means * blank_multiplier

    # check which lipids in each sample is above the blank
    threshold <- t(t(data_table) >= blank_means)
    # for how many samples a lipid is above the threshold
    above_threshold <- colSums(threshold, na.rm = TRUE) / nrow(data_table) >= sample_threshold
    del_cols_overall <- names(above_threshold)[!above_threshold]
  } # end over all batches

  ### do the same, but per group
  # determine which ones I should keep
  # get all groups
  all_groups <- unique(r6_lipid$tables$raw_meta[, "genoType"])

  keep_group_cols <- c()
  temp <- vector(mode = "list", length = 4)
  names(temp) <- all_groups
  # find everything per group
  for(g in all_groups) {
    # g <- all_groups[1]
    group_idx <- which(r6_lipid$tables$raw_meta[, "genoType"] == g)

    # go over batches per group
    for(batch in unique(r6_lipid$tables$raw_meta[group_idx, "batchNumber"])) {
      # get the blanks and the samples
      batch_idx <- which(r6_lipid$tables$imp_meta[, "batchNumber"] == batch)
      batch_idx_blanks <- base::intersect(batch_idx, r6_lipid$indices$idx_blanks)
      batch_idx_samples <- base::intersect(batch_idx, group_idx)

      # Get rownames of the blanks and samples
      batch_blanks <- rownames(r6_lipid$tables$imp_meta)[batch_idx_blanks]
      batch_samples <- rownames(r6_lipid$tables$imp_meta)[batch_idx_samples]
      batch_samples <- base::intersect(rownames(data_table), batch_samples)

      ## which lipids should be filter based on the whole batch
      # get the means of the columns
      blank_means <- apply(blank_table, 2, mean, na.rm = TRUE)
      # make sure to use the same columns as in the samples (some columns where already removed because they only contained NA's)
      blank_means <- blank_means[colnames(blank_table) %in% colnames(data_table)]
      # get rid of NA's
      blank_means[is.na(blank_means)] <- 0
      # multiply by the ratio sample / blank
      blank_means <- blank_means * blank_multiplier

      # check which lipids in each sample is above the blank
      threshold <- t(t(data_table[batch_samples, ]) >= blank_means)
      # for how many samples a lipid is above the threshold
      above_threshold <- colSums(threshold, na.rm = TRUE) / length(group_idx) >= group_threshold
      keep_cols <- names(above_threshold)[above_threshold]
      # see which ones are below threshold
      temp[[g]] <- names(above_threshold)[!above_threshold]
      keep_group_cols <- c(keep_group_cols, keep_cols)
    } # batches per group
  } # end all groups

  # get the unique ones
  keep_group_cols <- sort(unique(keep_group_cols))

  # lipids should only be removed if they appear in del_cols_overall and not in keep_group_cols
  remove_cols <- base::setdiff(del_cols_overall, keep_group_cols)

  data_table <- data_table[, !(colnames(data_table) %in% remove_cols)]

  # check if they are the same
  waldo::compare(r6_lipid$tables$raw_data, data_table)

}