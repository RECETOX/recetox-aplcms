patrick::with_parameters_test_that(
  "test run_filter",
  {
    if(ci_skip == TRUE) skip_on_ci()

    testdata <- file.path("..", "testdata")
    input_path <- file.path(testdata, "filtered", "run_filter", paste0(filename, ".parquet"))

    input_data <- arrow::read_parquet(input_path)
    actual <- run_filter(input_data, min_pres, min_run, max_run)

    expected_path <- file.path(testdata, "filtered", "run_filter", paste0(filename, "_run_filter.parquet"))
    expected <- arrow::read_parquet(expected_path)

    expect_equal(actual, expected)
    
  },
  patrick::cases(
    mbr_test0 = list(
      filename = "mbr_test0",
      min_pres = 0.5,
      min_run = 12,
      max_run = Inf,
      ci_skip = FALSE
    ),
    RCX_06_shortened = list(
      filename = "RCX_06_shortened",
      min_pres = 0.7,
      min_run = 4,
      max_run = Inf,
      ci_skip = FALSE
    ),
    RCX_07_shortened = list(
      filename = "RCX_07_shortened",
      min_pres = 0.7,
      min_run = 4,
      max_run = Inf,
      ci_skip = FALSE
    ),
    RCX_08_shortened = list(
      filename = "RCX_08_shortened",
      min_pres = 0.7,
      min_run = 4,
      max_run = Inf,
      ci_skip = FALSE
    )
  )
)


create_test_data <- function() {
  # creates a test dataframe with simulated data to test the maximum elution time filter
  dataframe <- dplyr::tibble(
        mz = c(110.03339715227077, 110.03355203634901, 110.03355203642177, 110.03355203647997, 110.03370692076884, 
        110.03370692081249, 110.03386180561755, 110.0338618056612, 110.03386180570486, 110.03401669088059, 
        110.03417157622329, 110.03417157626694, 110.0341715763106, 110.03417157654343, 110.03417157654343, 
        110.03417157686357, 110.03417157687812),
        rt = c(190.42361, 195.4938, 189.33464, 191.50827, 192.23296, 191.87143, 189.69868, 190.06143, 188.97246,
        190.7852, 191.14694, 188.61037, 192.9564, 192.59476, 193.31831, 195.13101, 194.0442),
        intensity = c(1535, 1200, 1300, 1400, 1100, 1150, 1250, 1350, 1450, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200),
        grps = rep(1, 17)
    )

  return(dataframe)
}


test_that("1. Testing the maximum elution time filter", {
  dataframe <- create_test_data() 
  actual <- run_filter(dataframe, min_pres=0.8, min_run=2, max_run=4)  
  expected <- dplyr::tibble(
        mz = double(),
        rt = double(),
        intensity = double(),
        group_number = double()
    ) 

  expect_equal(actual, expected)
})


test_that("2. Testing the maximum elution time filter", {
  dataframe <- create_test_data() 
  actual <- run_filter(dataframe, min_pres=0.8, min_run=2, max_run=15)  
  expected <- dataframe |> dplyr::rename(group_number = grps)

  expect_equal(actual, expected)
})
