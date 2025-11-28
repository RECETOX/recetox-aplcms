patrick::with_parameters_test_that(
  "extract single feature works",
  {
    skip_on_ci()
    if (skip_tests) {
      skip("skipping whole data test case")
    }

    testdata <- file.path("..", "testdata")

    filenames <- lapply(files, function(x) {
      file.path(testdata, "input", x)
    })

    cluster <- get_num_workers()

    if (!is(cluster, "cluster")) {
      cluster <- parallel::makeCluster(cluster)
      on.exit(parallel::stopCluster(cluster))
    }
    
    register_functions_to_cluster(cluster)
    
    profiles <- snow::parLapply(cluster, filenames, function(filename) {
        remove_noise(
            filename = filename,
            min_pres = min_pres,
            min_run = min_run,
            max_run = max_run,
            mz_tol = mz_tol,
            baseline_correct = 0,
            baseline_correct_noise_percentile = 0.05,
            intensity_weighted = intensity_weighted,
            do.plot = FALSE,
            cache = FALSE,
            grouping_threshold = Inf
        )
    })
    
    actual <- snow::parLapply(cluster, profiles, function(profile) {
        prof.to.features(
            profile = profile,
            bandwidth = 0.5,
            min_bandwidth = NA,
            max_bandwidth = NA,
            sd_cut = sd_cut,
            sigma_ratio_lim = sigma_ratio_lim,
            shape_model = "bi-Gaussian",
            peak_estim_method = peak_estim_method,
            component_eliminate = 0.01,
            moment_power = 1,
            BIC_factor = 2.0,
            do.plot = FALSE
        )
    })

    expected <- read_parquet_files(expected_files, "extracted", ".parquet")
    expect_equal(actual, expected, tolerance = 0.02)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened.mzML", "RCX_07_shortened.mzML", "RCX_08_shortened.mzML"),
      expected_files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      mz_tol = 1e-05,
      min_pres = 0.5,
      min_run = 12,
      max_run = Inf,
      intensity_weighted = FALSE,
      sd_cut = c(0.01, 500),
      sigma_ratio_lim = c(0.01, 100),
      peak_estim_method = "moment",
      skip_tests = FALSE
    ),
    qc_no_dil_milliq = list(
      files = c("8_qc_no_dil_milliq.mzml", "21_qc_no_dil_milliq.mzml", "29_qc_no_dil_milliq.mzml"),
      expected_files = c("8_qc_no_dil_milliq.mzml", "21_qc_no_dil_milliq.mzml", "29_qc_no_dil_milliq.mzml"),
      mz_tol = 5e-05,
      min_pres = 0.7,
      min_run = 0.5,
      max_run = Inf,
      intensity_weighted = FALSE,
      sd_cut = c(0.05, 10),
      sigma_ratio_lim = c(0, Inf),
      peak_estim_method = "EM",
      skip_tests = TRUE
    )
  )
)
