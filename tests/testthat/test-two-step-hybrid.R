test_that("basic two-step hybrid test", {
  # skip("Disabled")
  skip_on_ci()
  files <- c(
    # "mbr_test0.mzml",
    # "mbr_test1.mzml",
    # "mbr_test2.mzml",
    # "mbr_test0_copy.mzml"
    "RCX_06_shortened.mzML", 
    "RCX_07_shortened.mzML", 
    "RCX_08_shortened.mzML",
    "RCX_06_shortened_copy.mzML" 
  )
  # test_files <- paste0("./tests/testdata/input/", files)
  # print(getwd())
  print(files)
  test_path <- file.path("..", "testdata")
  test_files <- sapply(files, function(x) {
    file.path(test_path, "input", x)
  })
  metadata <- read.table("../testdata/two_step_hybrid_info.csv", sep = ",", header = TRUE)
#  metadata <- read.table("./tests/testdata/two_step_hybrid_info.csv", sep = ",", header = TRUE)

  # tempdir <- tempdir()
  # dir.create(tempdir)
  # temp_path <- paste0(tempdir, "/", test_names)
  # file.copy(test_path, temp_path)

  expected_final_features <- readRDS("../testdata/final_ftrs.Rda")
  known_table <- arrow::read_parquet("../testdata/hybrid/known_table.parquet")
  # known_table <- file.path("./tests/testdata", "hybrid", "known_table.parquet")

  result <- two.step.hybrid(
    filenames = test_files,
    metadata = metadata,
    work_dir = tempdir,
    known.table = known_table,
    cluster = get_num_workers(),
    do.plot = TRUE
  )
  final_features <- result$final_features

  keys <- c("feature", "mz", "rt", "mz_min", "mz_max", "sample")
  final_features <- as_tibble(arrange_at(final_features, keys))
  expected_final_features <- as_tibble(arrange_at(expected_final_features, keys))
  
  comparison <- dataCompareR::rCompare(
    final_features,
    expected_final_features,
    keys = keys
  )

  dataCompareR::saveReport(
    comparison,
    reportName = "final_features_comparison",
    reportLocation = ".",
    showInViewer = FALSE,
    missmatchCount = 10000
  )

  # unlink(tempdir, recursive = TRUE)

  expect_equal(final_features, expected_final_features, tolerance = 0.001)
  expect_equal(final_features$mz, expected_final_features$mz, tolerance = 0.001)
})
