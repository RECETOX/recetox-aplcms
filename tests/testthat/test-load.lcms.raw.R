testthat::test_that("load.lcms.raw fails if rawrr is not installed", {
  # Act & Assert: Expect an error when trying to load a non-existent file
  testthat::skip_if(requireNamespace("rawrr", quietly = TRUE), "The 'rawrr' package is already installed.")	
  testthat::expect_error(load.lcms.raw("test.raw"), "The 'rawrr' package is required but not installed. Please install it with install.packages('rawrr').")
})


testthat::test_that("load.lcms.raw fails if rawrr is not installed correctly.", {
  # Act & Assert: Expect an error when trying to load a non-existent file
  testthat::skip_if(!requireNamespace("rawrr", quietly = TRUE), "The 'rawrr' package needs to be installed for this check.")
  testthat::skip_if(rawrr::rawrrAssemblyPath() != "", "The 'rawrr' package is set up correctly, skipping this test.")
  testthat::expect_error(load.lcms.raw("test.raw"), "The 'rawrr' package is not set up correctly. Please ensure that the rawrr package is installed and configured properly.")
})

testthat::test_that("load.lcms.raw reads a raw file correctly", {
  # Arrange: Set up test inputs
  testthat::skip_if(!requireNamespace("rawrr", quietly = TRUE), "The 'rawrr' package is required for reading raw files but is not installed.")
  sample_raw_file <- rawrr::sampleFilePath()

  # Act: Execute the function with the test inputs
  actual <- load.lcms.raw(sample_raw_file)

  # Assert: the peak count depends on the rawrr reader build, so it is looked up per
  # reader version - see expected_raw_peak_count() in helper-reader.R
  expected_n <- expected_raw_peak_count()
  if (is.null(expected_n)) {
    testthat::skip(sprintf("no peak count recorded for rawrr %s, observed %s - add it to helper-reader.R",
                           as.character(packageVersion("rawrr")), nrow(actual)))
  }

  # Assert: Verify the function output matches expected results
  testthat::expect_equal(nrow(actual), expected_n)
  testthat::expect_equal(ncol(actual), 4)
  testthat::expect_equal(length(actual$mz), expected_n)
  testthat::expect_equal(length(actual$rt), expected_n)
  testthat::expect_equal(length(actual$intensity), expected_n)
})

testthat::test_that("load.lcms.raw correctly removes 0 intensity values with future.apply parallelism", {
  skip_on_ci()

  filename <- file.path("..", "testdata", "input", "8_qc_no_dil_milliq.raw")

  plan(multicore, workers = 4)
  actual <- load.lcms.raw(filename, chunk_size = 100)
  plan(sequential)

  testthat::expect_true(all(actual$intensity > 0), "All intensity values should be greater than 0.")
})
