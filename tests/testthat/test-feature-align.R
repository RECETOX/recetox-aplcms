update_expected <- function(actual) {
  arrow::write_parquet(actual$metadata, file.path("..", "testdata", "aligned", "metadata_table.parquet"))
  arrow::write_parquet(actual$intensity, file.path("..", "testdata", "aligned", "intensity_table.parquet"))
  arrow::write_parquet(actual$rt, file.path("..", "testdata", "aligned", "rt_table.parquet"))
}

test_that("create_features_from_cluster() function works", {
  sample_names <- c("sample_1", "sample_2", "sample_3")
  min_occurrence <- 2

  # input data for unit test
  clustered_features <- data.frame(
    mz = c(1, 1, 1),
    rt = c(1, 1, 1),
    sd1 = c(3, 4, 5),
    sd2 = c(1, 2, 3),
    area = c(1, 2, 3),
    sample_id = c("sample_1", "sample_2", "sample_3"),
    cluster = c(1, 1, 1)
  )

  # output data for unit test
  aligned_features_metadata <- data.frame(
    id = c(1),
    mz = c(1),
    mzmin = c(1),
    mzmax = c(1),
    rt = c(1),
    rtmin = c(1),
    rtmax = c(1),
    npeaks = c(3),
    sd1_mean = c(4),
    sd1_min = c(3),
    sd1_max = c(5),
    sd2_mean = c(2),
    sd2_min = c(1),
    sd2_max = c(3),
    sample_1 = c(1),
    sample_2 = c(1),
    sample_3 = c(1)
  )

  aligned_features_intensity <- data.frame(
    id = c(1),
    sample_1 = c(1),
    sample_2 = c(2),
    sample_3 = c(3)
  )

  aligned_features_rt <- data.frame(
    id = c(1),
    sample_1 = c(1),
    sample_2 = c(1),
    sample_3 = c(1)
  )

  actual <- create_features_from_cluster_simple(
    clustered_features,
    min_occurrence,
    sample_names
  )

  expect_equal(actual$metadata_row, aligned_features_metadata)
  expect_equal(as.data.frame(actual$rt_row), aligned_features_rt)
  expect_equal(as.data.frame(actual$intensity_row), aligned_features_intensity)
})

test_that("create_features_from_cluster() function works", {
  sample_names <- c("sample_1", "sample_2", "sample_3")
  min_occurrence <- 2

  # input data for unit test
  clustered_features <- data.frame(
    mz = c(1, 1, 1, 2),
    rt = c(1, 1, 1, 1),
    sd1 = c(3, 4, 5, 1),
    sd2 = c(1, 2, 3, 1),
    area = c(1, 2, 3, 4),
    sample_id = c("sample_1", "sample_2", "sample_3", "sample_3"),
    cluster = c(1, 1, 1, 1)
  )

  # output data for unit test
  aligned_features_metadata <- data.frame(
    id = c(1),
    mz = c(1.25),
    mzmin = c(1),
    mzmax = c(2),
    rt = c(1),
    rtmin = c(1),
    rtmax = c(1),
    npeaks = c(4),
    sd1_mean = c(3.25),
    sd1_min = c(1),
    sd1_max = c(5),
    sd2_mean = c(1.75),
    sd2_min = c(1),
    sd2_max = c(3),
    sample_1 = c(1),
    sample_2 = c(1),
    sample_3 = c(1)
  )

  aligned_features_intensity <- data.frame(
    id = c(1),
    sample_1 = c(1),
    sample_2 = c(2),
    sample_3 = c(7)
  )

  aligned_features_rt <- data.frame(
    id = c(1),
    sample_1 = c(1),
    sample_2 = c(1),
    sample_3 = c(1)
  )

  actual <- create_features_from_cluster_simple(
    clustered_features,
    min_occurrence,
    sample_names
  )


  expect_equal(actual$metadata_row, aligned_features_metadata)
  expect_equal(as.data.frame(actual$rt_row), aligned_features_rt)
  expect_equal(as.data.frame(actual$intensity_row), aligned_features_intensity)
})

test_that("create_features_from_cluster() function works", {
  sample_names <- c("sample_1", "sample_2", "sample_3")
  min_occurrence <- 2

  # input data for unit test
  clustered_features <- data.frame(
    mz = c(1, 1),
    rt = c(1, 1),
    sd1 = c(3, 4),
    sd2 = c(1, 2),
    area = c(1, 2),
    sample_id = c("sample_1", "sample_2"),
    cluster = c(1, 1)
  )

  # output data for unit test
  aligned_features_metadata <- data.frame(
    id = c(1),
    mz = c(1),
    mzmin = c(1),
    mzmax = c(1),
    rt = c(1),
    rtmin = c(1),
    rtmax = c(1),
    npeaks = c(2),
    sd1_mean = c(3.5),
    sd1_min = c(3),
    sd1_max = c(4),
    sd2_mean = c(1.5),
    sd2_min = c(1),
    sd2_max = c(2),
    sample_1 = c(1),
    sample_2 = c(1),
    sample_3 = c(0)
  )

  aligned_features_intensity <- data.frame(
    id = c(1),
    sample_1 = c(1),
    sample_2 = c(2)
  )

  aligned_features_rt <- data.frame(
    id = c(1),
    sample_1 = c(1),
    sample_2 = c(1)
  )

  actual <- create_features_from_cluster_simple(
    clustered_features,
    min_occurrence,
    sample_names
  )


  expect_equal(actual$metadata_row, aligned_features_metadata)
  expect_equal(as.data.frame(actual$rt_row), aligned_features_rt)
  expect_equal(as.data.frame(actual$intensity_row), aligned_features_intensity)
})

test_that("create_features_from_cluster() function works", {
  sample_names <- c("sample_1", "sample_2", "sample_3")
  min_occurrence <- 2

  # input data for unit test
  clustered_features <- data.frame(
    mz = c(1),
    rt = c(1),
    sd1 = c(3),
    sd2 = c(1),
    area = c(1),
    sample_id = c("sample_1"),
    cluster = c(1)
  )

  actual <- create_features_from_cluster_simple(
    clustered_features,
    min_occurrence,
    sample_names
  )

  expect_equal(actual, NULL)
})


test_that("create_features_from_cluster() function works", {
  sample_names <- c("sample_1", "sample_2", "sample_3")
  min_occurrence <- 2

  # input data for unit test
  clustered_features <- data.frame(
    mz = c(1, 2),
    rt = c(1, 1),
    sd1 = c(3, 2),
    sd2 = c(1, 4),
    area = c(1, 4),
    sample_id = c("sample_1", "sample_1"),
    cluster = c(1, 1)
  )

  actual <- create_features_from_cluster_simple(
    clustered_features,
    min_occurrence,
    sample_names
  )

  expect_equal(actual, NULL)
})

ones <- function(reps) {
  return(rep(1, reps))
}

###############################################################################

test_that("create_aligned_feature_table_simple() function works", {
  sample_names <- c("sample_1", "sample_2", "sample_3")
  min_occurrence <- 2

  # input data for unit test
  clustered_features <- data.frame(
    mz = c(1, 1, 1, 1, 1, 1, 1, 1,1,1,1,1,12),
    rt = c(1, 3, 2, 4, 1, 3.5, 2, 4,1,3.5,2,4,4),
    sd1 = ones(13),
    sd2 = ones(13),
    area = c(1, 2, 3, 4, 1, 2, 3, 4, 1,2,3,4,4),
    sample_id = c("sample_1", "sample_1", "sample_1", "sample_1","sample_2", "sample_2", "sample_2", "sample_2", "sample_3", "sample_3", "sample_3", "sample_3", "sample_3"),
    cluster = c(1, 2, 2, 3, 1, 4, 2, 3, 1, 4, 2, 3, 5)
  )

  # output data for unit test
  aligned_features_metadata <- data.frame(
    id = c(1, 2, 3, 4),
    mz = c(1, 1, 1, 1),
    mzmin = c(1, 1, 1, 1),
    mzmax = c(1, 1, 1, 1),
    rt = c(1, 2.25, 4, 3.5),
    rtmin = c(1,2,4,3.5),
    rtmax = c(1,3,4,3.5),
    npeaks = c(3,4,3,2),
    sd1_mean = ones(4),
    sd1_min = ones(4),
    sd1_max = ones(4),
    sd2_mean = ones(4),
    sd2_min = ones(4),
    sd2_max = ones(4),
    sample_1 = c(1,1,1,0),
    sample_2 = c(1,1,1,1),
    sample_3 = c(1,1,1,1)
  )

  aligned_features_intensity <- data.frame(
    id = c(1, 2, 3, 4),
    sample_1 = c(1,5,4,0),
    sample_2 = c(1,3,4,2),
    sample_3 = c(1,3,4,2)
  )

  aligned_features_rt <- data.frame(
    id = c(1, 2, 3, 4),
    sample_1 = c(1,2.5,4,0),
    sample_2 = c(1,2,4,3.5),
    sample_3 = c(1,2,4,3.5)
  )

  actual <- create_aligned_feature_table_simple(
    features_table = clustered_features,
    min_occurrence = min_occurrence,
    sample_names = sample_names
  )

  expect_equal(actual$metadata, tibble::as_tibble(aligned_features_metadata))
  expect_equal(as.data.frame(actual$rt), aligned_features_rt)
  expect_equal(as.data.frame(actual$intensity), aligned_features_intensity)
})
patrick::with_parameters_test_that(
  "feature.align test",
  {
    testdata <- file.path("..", "testdata")

    corrected_features <- read_parquet_files(files, "adjusted", ".parquet")
    
    res <- compute_clusters(
        corrected_features,
        mz_tol_relative,
        mz_tol_absolute,
        10 * mz_tol,
        rt_tol_relative,
        do.plot,
        files
    )
    
    aligned_actual <- create_aligned_feature_table(
        dplyr::bind_rows(res$feature_tables),
        min_occurrence,
        files,
        res$rt_tol_relative,
        res$mz_tol_relative,
        cluster = get_num_workers()
    )
 
    aligned_expected <- load_aligned_features(
      file.path(testdata, "aligned", "metadata_table.parquet"),
      file.path(testdata, "aligned", "intensity_table.parquet"),
      file.path(testdata, "aligned", "rt_table.parquet")
    )

    expect_equal(aligned_actual, aligned_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      min_occurrence = 2,
      mz_tol_relative = NA,
      rt_tol_relative = NA,
      mz_tol = 1e-05,
      mz_tol_absolute = 0.01,
      do.plot = FALSE
    )
  )
)


patrick::with_parameters_test_that(
  "compute_aligned_feature_table test",
  {
    testdata <- file.path("..", "testdata")

    corrected_features <- read_parquet_files(files, "clusters", "_adjusted_clusters.parquet")

    aligned_actual <- create_aligned_feature_table(
        dplyr::bind_rows(corrected_features),
        min_occurrence,
        files,
        rt_tol_relative,
        mz_tol_relative,
        cluster = get_num_workers()
    )

    aligned_expected <- load_aligned_features(
      file.path(testdata, "aligned", "metadata_table.parquet"),
      file.path(testdata, "aligned", "intensity_table.parquet"),
      file.path(testdata, "aligned", "rt_table.parquet")
    )

    expect_equal(aligned_actual, aligned_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      min_occurrence = 2,
      mz_tol_relative = 6.84903911826453e-06,
      rt_tol_relative = 1.93185408267324
    )
  )
)
