patrick::with_parameters_test_that(
  "test run_filter",
  {
    if(ci_skip == TRUE) skip_on_ci()

    testdata <- file.path("..", "testdata")
    input_path <- file.path(testdata, "filtered", "run_filter", paste0(filename, ".parquet"))

    input_data <- arrow::read_parquet(input_path)
    actual <- run_filter(input_data, min_pres, min_run)

    expected_path <- file.path(testdata, "filtered", "run_filter", paste0(filename, "_run_filter.parquet"))
    expected <- arrow::read_parquet(expected_path)

    expect_equal(actual, expected)
  },
  patrick::cases(
    mbr_test0 = list(
      filename = "mbr_test0",
      min_pres = 0,
      min_run = 0,
      ci_skip = FALSE
    ),
    RCX_06_shortened = list(
      filename = "RCX_06_shortened",
      min_pres = 0.7,
      min_run = 4,
      ci_skip = FALSE
    ),
    RCX_07_shortened = list(
      filename = "RCX_07_shortened",
      min_pres = 0.7,
      min_run = 4,
      ci_skip = FALSE
    ),
    RCX_08_shortened = list(
      filename = "RCX_08_shortened",
      min_pres = 0.7,
      min_run = 4,
      ci_skip = FALSE
    )
  )
)


test_that("Calculate scan rate works", {
  rts <- c(1, 1.2, 1.4, 1.6, 2, 2.2, 2.6)

  actual <- calculate_scan_rate(rts)
  expected <- 5

  expect_equal(actual, expected)
})

test_that("Calculating min_num_datapoints works", {
  actual <- min_num_datapoints(4, 0.5, 5)
  expect_equal(actual, 10)
})