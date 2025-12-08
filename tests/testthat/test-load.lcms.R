create_test_case <- function(filename, mz_length, rt_length, intensities_length) {
  list(
    filename = filename,
    expected_type = "list",
    expected_lengths = c(mz = mz_length, rt = rt_length, intensities = intensities_length),
    expected_num_na = c(mz = 0, rt = 0, intensities = 0)
  )
}

patrick::with_parameters_test_that(
  "test load.lcms reads different file types",
  {
    if(packageVersion("mzR") >= "2.29.0" && tools::file_ext(filename) == "mzdata") {
      print("mzR >= 2.29.0 no longer supports mzdata.")
      skip()
    }

    # Skip if rawrr is not installed for raw files
    if (tools::file_ext(filename) == "raw" && !requireNamespace("rawrr", quietly = TRUE)) {
      testthat::skip("The 'rawrr' package is required for reading raw files but is not installed.")
    }

    # Check if path is absolute or relative, if relative, prepend testdata directory
    if (!file.exists(filename)) {
      filename <- file.path("..", "testdata", "input", filename)
    }

    # Act: Execute the function with the test inputs
    data <- load.lcms(filename)

    # Assert: Verify the function output matches expected results
    # Check that the function returns an object of the expected type
    actual_type <- data
    testthat::expect_type(actual_type, expected_type)
    
    # Check the lengths of the vectors in the list
    actual_lengths <- lengths(data)
    testthat::expect_equal(actual_lengths, expected_lengths)

    # Check the number of NA values in each vector
    actual_num_na <- sapply(data, function(x) sum(is.na(x)))
    testthat::expect_equal(actual_num_na, expected_num_na)
  },
  
  patrick::cases(
    test_case_1 = create_test_case("RCX_06_shortened.mzML", 879476, 879476, 879476),
    test_case_2 = create_test_case("test_file.mzXML", 9647575, 9647575, 9647575),
    test_case_3 = create_test_case("alg3.mzdata", 543894, 543894, 543894),
    test_case_3 = create_test_case(rawrr::sampleFilePath(), 30689, 30689, 30689)
  )
)

