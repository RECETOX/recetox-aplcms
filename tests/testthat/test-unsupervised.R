patrick::with_parameters_test_that(
  "basic unsupervised test",
  {
    store_reports <- FALSE

    if (full_testdata) {
      skip("skipping whole data test case")
    }

    test_files <- sapply(files, function(x) file.path("../testdata/input", x))

    result <- unsupervised(test_files, cluster = get_num_workers())
    keys <- c("mz", "rt", "sample", "sample_rt", "sample_intensity")
    actual <- as_tibble(result$recovered_feature_sample_table)

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
        reportName = paste0(.test_name, "_unsupervised_report"),
        showInViewer = FALSE,
        HTMLReport = FALSE,
        mismatchCount = 10000
      )
    }

    # arrow::write_parquet(actual, file.path("../testdata/unsupervised", paste0(.test_name, "_unsupervised.parquet")))
    expected <- arrow::read_parquet(file.path("../testdata/unsupervised", paste0(.test_name, "_unsupervised.parquet")))

    expect_equal(actual, expected)
  },
  patrick::cases(
    mbr_test = list(
      files = c("mbr_test0.mzml", "mbr_test1.mzml", "mbr_test2.mzml"),
      full_testdata = FALSE
    ),
    RCX_shortened = list(
      files = c("RCX_06_shortened.mzML", "RCX_07_shortened.mzML", "RCX_08_shortened.mzML"),
      full_testdata = FALSE
    ),
    qc_no_dil_milliq = list(
      files = c("8_qc_no_dil_milliq.mzml", "21_qc_no_dil_milliq.mzml", "29_qc_no_dil_milliq.mzml"),
      full_testdata = TRUE
    )
  )
)
