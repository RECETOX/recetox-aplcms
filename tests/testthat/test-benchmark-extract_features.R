patrick::with_parameters_test_that(
  "test benchmark",
  {
    if (skip_benchmark) {
      skip("Disabled")
    }

    skip_on_ci()

    testdata <- file.path("..", "testdata")

    filenames <- lapply(filename, function(x) {
      file.path(testdata, "input", paste0(x, ".mzML"))
    })

    cluster <- get_num_workers()

    if (!is(cluster, "cluster")) {
      cluster <- parallel::makeCluster(cluster)
      on.exit(parallel::stopCluster(cluster))
    }
    
    register_functions_to_cluster(cluster)

    res <- microbenchmark::microbenchmark(
        extract_feature = {
            profiles <- snow::parLapply(cluster, filenames, function(filename) {
                remove_noise(
                    filename = filename,
                    min_pres = min_pres,
                    min_run = min_run,
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
                    peak_estim_method = "moment",
                    component_eliminate = 0.01,
                    moment_power = 1,
                    BIC_factor = 2.0,
                    do.plot = FALSE
                )
            })
        },
      times = 10L
    )

    expected <- lapply(filename, function(x) {
      xx <- file.path(testdata, "extracted", paste0(x, ".parquet"))
      tibble::as_tibble(arrow::read_parquet(xx))
    })

    expect_equal(actual, expected, tolerance = 0.02)

    cat("\n\n")
    print(res)
    cat("\n")
  },
  patrick::cases(
    RCX_shortened = list(
      filename = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      mz_tol = 1e-05,
      min_pres = 0.5,
      min_run = 12,
      intensity_weighted = FALSE,
      sd_cut = c(0.01, 500),
      sigma_ratio_lim = c(0.01, 100),
      skip_benchmark = TRUE
    )
  )
)
