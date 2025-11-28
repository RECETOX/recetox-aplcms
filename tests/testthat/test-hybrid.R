patrick::with_parameters_test_that("basic hybrid test", {
  if(ci_skip == TRUE) skip_on_ci()

  if (full_testdata) {
    skip("skipping whole data test case")
  }

  store_reports <- FALSE
  testdata <- file.path("..", "testdata")

  test_files <- sapply(files, function(x) {
    file.path(testdata, "input", x)
  })

  known_table <- arrow::read_parquet(
    file.path(testdata, "hybrid", "known_table.parquet")
  )
  
  result <- hybrid(
    test_files,
    known_table,
    mz_tol_relative = NA,
    rt_tol_relative = NA,
    cluster = get_num_workers())
  actual <- as_tibble(result$recovered_feature_sample_table)
  keys <- c("mz", "rt", "sample", "sample_rt", "sample_intensity")

  expected_path <- file.path(testdata, "hybrid", paste0(.test_name, "_recovered_feature_sample_table.parquet"))

  # arrow::write_parquet(actual, expected_path)

  expected <- arrow::read_parquet(expected_path)

  if (store_reports) {
    report <- dataCompareR::rCompare(
      actual,
      expected,
      keys = keys,
      roundDigits = 3,
      mismatches = 100000
    )
    dataCompareR::saveReport(
      report,
      reportName = paste0(.test_name, "_hybrid_report"),
      showInViewer = FALSE,
      HTMLReport = FALSE,
      mismatchCount = 10000
    )
  }

  expect_equal(actual, expected, tolerance = 1e-06)
},
patrick::cases(
  mbr = list(
    files = c("mbr_test0.mzml", "mbr_test1.mzml", "mbr_test2.mzml"),
    ci_skip = TRUE,
    full_testdata = FALSE
  ),
  RCX_shortened = list(
    files = c("RCX_06_shortened.mzML", "RCX_07_shortened.mzML", "RCX_08_shortened.mzML"),
    ci_skip = FALSE,
    full_testdata = FALSE
  ),
  qc_no_dil_milliq = list(
    files = c("8_qc_no_dil_milliq.mzml", "21_qc_no_dil_milliq.mzml", "29_qc_no_dil_milliq.mzml"),
    ci_skip = TRUE,
    full_testdata = TRUE
  )
))
