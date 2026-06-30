update_expected <- function(actual) {
  saveRDS(actual, file.path("..", "testdata", "aligned", "output_create-features.rds"))
}

update_input <- function(table_path, sample_names){
  tmp <- arrow::read_parquet(table_path)
  tmp$sample_id <- rep(sample_names, 2)  # beware of data in sample_id column, now only 2 arranged repetitions so this works
  arrow::write_parquet(tmp, table_path)
}

test_that("create_features_from_cluster() function works", {

  input_path <- file.path("..", "testdata", "input", "feature-align_create-features.parquet")

  sample <- read_parquet(input_path)
  sample_names <- c("data_06", "data_07", "data_08")
  min_occurrence <- 2
  mz_tol_relative <- 6.85676325338646e-06
  rt_tol_relative <- 2.17918873407775

  # update_input(input_path, sample_names)
  actual <- create_features_from_cluster(sample,
                      mz_tol_relative,
                      rt_tol_relative,
                      min_occurrence,
                      sample_names)
  
  # update_expected(actual)
  
  expected <- readRDS("../testdata/aligned/output_create-features.rds")
  expect_equal(actual, expected)
})