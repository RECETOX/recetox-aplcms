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
  sample_raw_file <- rawrr::sampleFilePath()

  # Act: Execute the function with the test inputs
  actual <- load.lcms.raw(sample_raw_file)

  # Assert: Verify the function output matches expected results
  testthat::expect_equal(nrow(actual), 30689)
  testthat::expect_equal(ncol(actual), 3)
  testthat::expect_equal(length(actual$mz), 30689)
  testthat::expect_equal(length(actual$rt), 30689)
  testthat::expect_equal(length(actual$intensities), 30689)
})

testthat::test_that("load.lcms.raw correctly removes 0 intensity values with future.apply parallelism", {
  skip_on_ci()

  filename <- file.path("..", "testdata", "input", "8_qc_no_dil_milliq.raw")

  plan(multicore, workers = 4)
  actual <- load.lcms.raw(filename, chunk_size = 100)
  plan(sequential)

  testthat::expect_true(all(actual$intensities > 0), "All intensity values should be greater than 0.")
})
