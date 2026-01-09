load_expected <- function(path) {
  return(arrow::read_parquet(path) |> dplyr::select(-group_number) |> dplyr::arrange_at(c("mz", "rt")))
}

patrick::with_parameters_test_that(
  "test remove_noise",
  {
    testdata <- file.path("..", "testdata")
    input_path <- file.path(testdata, "input", filename)

    sut <- remove_noise(
      input_path,
      min_pres = min_pres,
      min_run = min_run,
      max_run = max_run,
      mz_tol = mz_tol,
      baseline_correct = 0.0,
      baseline_correct_noise_percentile = 0.05,
      intensity_weighted = intensity_weighted,
      do.plot = FALSE,
      cache = FALSE,
      grouping_threshold = grouping_threshold
    )

    expected_path <- file.path(testdata, "filtered", paste0(.test_name, ".parquet"))

    # arrow::write_parquet(sut, expected_path)

    # exclude last column from comparison as there lies the stochastic nature
    actual <- sut |> dplyr::select(-group_number) |> dplyr::arrange_at(c("mz", "rt"))

    expected <- load_expected(expected_path)

    expect_equal(actual, expected)
  },
  patrick::cases(
    mbr_test0 = list(
      filename = c("mbr_test0.mzml"),
      mz_tol = 1e-05,
      min_pres = 0.5,
      min_run = 12,
      max_run = Inf,
      grouping_threshold = Inf,
      intensity_weighted = FALSE
    ),
    RCX_06_shortened = list(
      filename = c("RCX_06_shortened.mzML"),
      mz_tol = 1e-06,
      min_pres = 0.8,
      min_run = 1.2,
      max_run = Inf,
      grouping_threshold = 1,
      intensity_weighted = TRUE
    ),
    single_eic = list(
      filename = c("Tribrid_201106_009-QC1_1_NEG_FISABIO_single_eic.raw.mzML"),
      mz_tol = 5e-05,
      min_pres = 0.8,
      min_run = 0.2,
      max_run = Inf,
      grouping_threshold = 4,
      intensity_weighted = FALSE
    )
  )
)

patrick::with_parameters_test_that("remove noise on raw with parallel workers works", {
  if(ci_skip == TRUE) skip_on_ci()
  testdata <- file.path("..", "testdata")

  plan(multicore, workers = 4)
  sut <- remove_noise(
    input_path,
    min_pres = 0.8,
    min_run = 1,
    max_run = Inf,
    mz_tol = 5e-06,
    baseline_correct = 0.0,
    baseline_correct_noise_percentile = 0.05,
    intensity_weighted = FALSE,
    grouping_threshold = 2,
    do.plot = FALSE,
    cache = FALSE
  )
  plan(sequential)

  actual <- sut |> dplyr::select(-group_number) |> dplyr::arrange_at(c("mz", "rt"))

  expected <- readRDS(expected_path)
  expect_equal(summary(actual), expected)
}, patrick::cases(
  thermo_raw_profile = list(
    input_path = file.path("..", "testdata", "input", "8_qc_no_dil_milliq.raw"),
    expected_path = file.path("..", "testdata", "filtered", "thermo_raw_profile_threshold_summary.rds"),
    ci_skip = TRUE
  ),
  rawrr_sample_data = list(
    input_path = rawrr::sampleFilePath(),
    expected_path = file.path("..", "testdata", "filtered", "rawrr.rds"),
    ci_skip = FALSE
)))
