update_expected <- function(actual) {
  saveRDS(actual, file.path("..", "testdata", "aligned", "output_create-features.rds"))
}

test_that("create_features_from_cluster() function works", {
  sample <- read_parquet("../testdata/input/feature-align_create-features.parquet")
  sample_names <- c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened")
  min_occurrence <- 2
  mz_tol_relative <- 6.85676325338646e-06
  rt_tol_relative <- 2.17918873407775

  actual <- create_features_from_cluster(sample,
                      mz_tol_relative,
                      rt_tol_relative,
                      min_occurrence,
                      sample_names)
  
  expected <- readRDS("../testdata/aligned/output_create-features.rds")
  expect_equal(actual, expected)
})