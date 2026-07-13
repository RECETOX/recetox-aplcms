test_that("basic two-step hybrid test", {
  # skip("Disabled")
  skip_on_ci()
  files <- c(
    "SPECIMEN_b1_006_110_120.mzML",
    "SPECIMEN_b1_038_110_120.mzML",
    "SPECIMEN_b1_053_110_120.mzML", 
    "SPECIMEN_b1_067_110_120.mzML", 
    "SPECIMEN_b1_095_110_120.mzML", 
    "SPECIMEN_b2_007_110_120.mzML", 
    "SPECIMEN_b2_053_110_120.mzML", 
    "SPECIMEN_b2_095_110_120.mzML"   
  )

  # Set up paths
  testdata <- file.path("..", "testdata")
  test_files <- sapply(files, function(x) {
    file.path(testdata, "input", x)
  })

  # Load metadata
  metadata <- read.table(
    file.path(testdata, "two-step-hybrid", "two_step_hybrid_info.csv"),
    sep = ",",
    header = TRUE,
    strip.white = TRUE
  )

  # Load known table 
  known_table <- arrow::read_parquet(
    file.path(testdata, "hybrid", "known_table.parquet")
  )
  
  result <- two.step.hybrid(
    filenames = test_files,
    metadata = metadata,
    work_dir = test_path,
    known_table = known_table,
    cluster = 4,
    do_plot = FALSE,     
    min_run = 2,
    mz_tol = 5e-06,
    mz_tol_relative = 3e-06,
    rt_tol_relative = 20,
    grouping_threshold = 3      
  )
  final_features <- as.data.frame(result$final_features)

  # saveRDS(result$final_features, file.path(testdata, "two-step-hybrid", "final_features.rds"))
  
  # Expected final features
  expected_final_features <- as.data.frame(readRDS(file.path(testdata, "two-step-hybrid", "final_features.rds")))

  keys <- c("metadata.id", "metadata.mz", "metadata.rt", "metadata.mzmin", "metadata.mzmax")
  final_features <- arrange_at(final_features, keys)
  expected_final_features <- arrange_at(expected_final_features, keys)

  expect_equal(final_features, expected_final_features, tolerance = 0.001)
  expect_equal(final_features$metadata.mz, expected_final_features$metadata.mz, tolerance = 0.001)
})
