patrick::with_parameters_test_that(
  "test remove_noise",
  {
    if(ci_skip == TRUE) skip_on_ci()

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
      cache = cache,
      grouping_threshold = Inf
    )

    expected_path <- file.path(testdata, "filtered", paste0(.test_name, ".parquet"))

    # exclude last column from comparison as there lies the stochastic nature
    actual <- sut |> dplyr::select(-group_number) |> dplyr::arrange_at(c("mz", "rt"))
    expected <- arrow::read_parquet(expected_path) |> dplyr::select(-group_number) |> dplyr::arrange_at(c("mz", "rt"))

    expect_equal(actual, expected)
  },
  patrick::cases(
    mbr_test0 = list(
      filename = c("mbr_test0.mzml"),
      mz_tol = 1e-05,
      min_pres = 0.5,
      min_run = 12,
      max_run = Inf,
      intensity_weighted = FALSE,
      cache = FALSE,
      ci_skip = FALSE
    ),
    RCX_06_shortened = list(
      filename = c("RCX_06_shortened.mzML"),
      mz_tol = 1e-06,
      min_pres = 0.7,
      min_run = 4,
      max_run = Inf,
      intensity_weighted = TRUE,
      cache = FALSE,
      ci_skip = FALSE
    ),
    RCX_07_shortened = list(
      filename = c("RCX_07_shortened.mzML"),
      mz_tol = 1e-06,
      min_pres = 0.7,
      min_run = 4,
      max_run = Inf,
      intensity_weighted = TRUE,
      cache = FALSE,
      ci_skip = TRUE
    ),
    RCX_08_shortened = list(
      filename = c("RCX_08_shortened.mzML"),
      mz_tol = 1e-06,
      min_pres = 0.7,
      min_run = 4,
      max_run = Inf,
      intensity_weighted = TRUE,
      cache = FALSE,
      ci_skip = TRUE
    ),
    single_eic = list(
      filename = c("single_eic.mzml"),
      mz_tol = 5e-05,
      min_pres = 0.8,
      min_run = 0.2,
      max_run = Inf,
      intensity_weighted = FALSE,
      cache = FALSE,
      ci_skip = FALSE
    )
    # thermo_raw_profile = list(
    #   filename = c("8_qc_no_dil_milliq.raw"),
    #   mz_tol = 5e-06,
    #   min_pres = 0.8,
    #   min_run = 1,
    #   intensity_weighted = FALSE,
    #   cache = FALSE,
    #   ci_skip = TRUE
    # )
  )
)

test_that("remove noise works with grouping threshold", {
  testdata <- file.path("..", "testdata")
  input_path <- file.path(testdata,
                          "input",
                          "Tribrid_201106_009-QC1_1_NEG_FISABIO_single_eic.raw.mzML")

  expected <- tibble(group_number = c(1, 2, 3, 5, 6, 7, 8, 9),
                     n = c(67, 73, 3, 39, 2, 6, 3, 7))

  threshold <- 4

  sut <- remove_noise(
    input_path,
    min_pres = 0.8,
    min_run = 0.2,
    max_run = Inf,
    mz_tol = 5e-05,
    baseline_correct = 0.0,
    baseline_correct_noise_percentile = 0.05,
    intensity_weighted = FALSE,
    do.plot = FALSE,
    cache = FALSE,
    grouping_threshold = threshold
  )

  diffs <- sut |> group_by(group_number) |> arrange_at("rt") |> summarise(max_diff = max(abs(diff(rt))))
  expect_true(all(diffs$max_diff <= threshold))

  actual <- sut %>%
    mutate(group = factor(group_number)) %>%
    group_by(group_number) %>%
    summarize(n = n())

  expect_equal(actual, expected)
})

test_that("remove noise really really works", {
  testdata <- file.path("..", "testdata")
  input_path <- file.path(testdata,
                          "input",
                          "RCX_06_shortened.mzML")

  threshold <- 1

  sut <- remove_noise(
    input_path,
    min_pres = 0.8,
    min_run = 1.2,
    max_run = Inf,
    mz_tol = 5e-06,
    baseline_correct = 0.0,
    baseline_correct_noise_percentile = 0.05,
    intensity_weighted = FALSE,
    do.plot = FALSE,
    cache = FALSE,
    grouping_threshold = threshold
  )

  diffs <- sut |> group_by(group_number) |> arrange_at("rt") |> summarise(max_diff = max(abs(diff(rt))))
  expect_true(all(diffs$max_diff <= threshold))
})

test_that("remove noise on raw with parallel workers works", {
  testdata <- file.path("..", "testdata")
  input_path <- file.path(testdata, "input", "8_qc_no_dil_milliq.raw")

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

  expected <- readRDS(file.path(testdata, "filtered", "thermo_raw_profile_threshold_summary.rds"))
  actual <- sut |> dplyr::select(-group_number) |> dplyr::arrange_at(c("mz", "rt"))
  expect_equal(summary(actual), expected)
})