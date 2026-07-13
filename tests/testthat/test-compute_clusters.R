update_expected <- function(input, files, actual, suffix = "_clusters") {
  testdata <- file.path("..", "testdata")
  for(i in seq_along(files)) {
    arrow::write_parquet(actual$feature_tables[[i]], file.path(testdata, "clusters", paste0(files[i], "_", input , suffix, ".parquet")))
  }
}

patrick::with_parameters_test_that(
  "test compute_clusters",
  {
    testdata <- file.path("..", "testdata")

    extracted <- read_parquet_files(files, input, ".parquet")

    actual <- compute_clusters(
      feature_tables = extracted,
      mz_tol_relative = NA,
      rt_tol_relative = NA,
      mz_max_diff = mz_max_diff,
      mz_tol_absolute = mz_tol_absolute,
      do.plot = FALSE,
      sample_names = get_sample_names(extracted)
    )
    
    # update_expected(input, files, actual)
    expected <- read_parquet_files(files, "clusters", paste0("_", input, "_clusters.parquet"))

    for(i in seq_along(files)) {
      expect_equal(actual$feature_tables[[i]], expected[[i]])
    }

    expect_equal(actual$mz_tol_relative, expected_mz_tol_relative, tolerance=0.1)
    expect_equal(actual$rt_tol_relative, expected_rt_tol_relative, tolerance=0.1)

  },
  patrick::cases(
    RCX_shortened_extracted = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      input = "extracted",
      mz_max_diff = 10 * 1e-05,
      mz_tol_absolute = 0.01,
      expected_mz_tol_relative = 6.849039e-06,
      expected_rt_tol_relative = 36.9
    ),
    RCX_shortened_adjusted = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      input = "adjusted",
      mz_max_diff = 10 * 1e-05,
      mz_tol_absolute = 0.01,
      expected_mz_tol_relative = 6.856763e-06,
      expected_rt_tol_relative = 0.82
    )
  )
)

test_that("compute clusters simple", {
  files <- c("8_qc_no_dil_milliq", "21_qc_no_dil_milliq", "29_qc_no_dil_milliq")

  extracted <- read_parquet_files(files, "extracted", ".mzml.parquet")

  actual <- compute_clusters_simple(
    feature_tables = extracted,
    sample_names = files,
    mz_tol_ppm = 10,
    rt_tol = 2
  )

  actual <- actual[order(sapply(actual, function(x) x$sample_id[1]))]

  expected <- read_parquet_files(files, "clusters", ".parquet")
  expected <- expected[order(sapply(expected, function(x) x$sample_id[1]))]

  expect_equal(as.list(actual), expected, tolerance = 0.02)
})

test_that("compute clusters_simple_sd", {
  files <- c("8_qc_no_dil_milliq", "21_qc_no_dil_milliq", "29_qc_no_dil_milliq")

  extracted <- read_parquet_files(files, "extracted", ".mzml.parquet")

  actual <- compute_clusters_simple_sd(
    feature_tables = extracted,
    sample_names = files,
    mz_tol_ppm = 10,
    rt_tol = 10,
    sd_ratio_tol = 3
  )

  actual <- actual[order(sapply(actual, function(x) x$sample_id[1]))]
  result <- lapply(actual, summary)

  expected <- readRDS(file.path("..", "testdata", "clusters", "clusters_simple_sd.Rds"))

  expect_equal(result, expected, tolerance = 0.02)
})

# parametrised test for compute_clusters_sd function
patrick::with_parameters_test_that(
  "test compute_clusters_sd",
  {
    testdata <- file.path("..", "testdata")

    extracted <- read_parquet_files(files, input, ".parquet")

    actual <- compute_clusters_sd(
      feature_tables = extracted,
      mz_tol_relative = NA,
      rt_tol_relative = NA,
      mz_max_diff = mz_max_diff,
      mz_tol_absolute = mz_tol_absolute,
      sd_ratio_tol_relative = NA,
      do.plot = FALSE,
      sample_names = get_sample_names(extracted)
    )

    result <- lapply(actual$feature_tables, summary)
    # saveRDS(result, file.path(testdata, "clusters", paste0("clusters_sd_", input, ".Rds")))
    expected <- readRDS(file.path(testdata, "clusters", paste0("clusters_sd_", input, ".Rds")))
    expect_equal(result, expected, tolerance = 0.02)
  },
  patrick::cases(
    RCX_shortened_extracted = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      input = "extracted",
      mz_max_diff = 10 * 1e-05,
      mz_tol_absolute = 0.01,
      expected_mz_tol_relative = 6.849039e-06,
      expected_rt_tol_relative = 36.9
    ),
    RCX_shortened_adjusted = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      input = "adjusted",
      mz_max_diff = 10 * 1e-05,
      mz_tol_absolute = 0.01,
      expected_mz_tol_relative = 6.856763e-06,
      expected_rt_tol_relative = 0.82
    )
  )
)


# unit test for new compute_clusters_sd function
test_that("compute_clusters_sd()", {
  sample_names <- c("sample_1", "sample_2")

  # input data for unit test - sample 1
  features_1 <- dplyr::tibble(
    mz = c(1,2,6,6,7,7),
    rt = c(1,1,2,3,4,4),
    sd1 = c(1,1,1,1,3,3),
    sd2 = c(1,1,1,1,3,1),
    area = c(10,10,10,10,10,10)
  )

  # expected output
  expected_1 <- dplyr::tibble(
    mz = c(1,2,6,6,7,7),
    rt = c(1,1,2,3,4,4),
    sd1 = c(1,1,1,1,3,3),
    sd2 = c(1,1,1,1,3,1),
    area = c(10,10,10,10,10,10),
    sample_id = rep("sample_1", 6),
    sd_ratio = c(1,1,1,1,1,3),
    cluster = c(2,3,4,5,6,7)
  )

  # input data for unit test - sample 2
  features_2 <- dplyr::tibble(
    mz = c(1,2,6,6,7,7),
    rt = c(1,1,2,3,4,4),
    sd1 = c(1,1,1,1,3,3),
    sd2 = c(1,1,1,1,3,1),
    area = c(10,10,10,10,10,10)
  )

  # expected output
  expected_2 <- dplyr::tibble(
    mz = c(1,2,6,6,7,7),
    rt = c(1,1,2,3,4,4),
    sd1 = c(1,1,1,1,3,3),
    sd2 = c(1,1,1,1,3,1),
    area = c(10,10,10,10,10,10),
    sample_id = rep("sample_2", 6),
    sd_ratio = c(1,1,1,1,1,3),
    cluster = c(2,3,4,5,6,7)
  )

  actual <- compute_clusters_sd(feature_tables = list(features_1, features_2),
                             mz_tol_relative = 0.9,
                             mz_tol_absolute = 0.9,
                             mz_max_diff = 1e-04,
                             rt_tol_relative = 0.9,
                             sd_ratio_tol_relative = 0.9,
                             do.plot = FALSE,
                             sample_names = sample_names
  )

  expect_equal(length(actual$feature_tables), 2)
  expect_equal(actual$feature_tables[[1]], expected_1)
  expect_equal(actual$feature_tables[[2]], expected_2)
})