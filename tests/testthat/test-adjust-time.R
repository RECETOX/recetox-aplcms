patrick::with_parameters_test_that(
  "compute template",
  {
    testdata <- file.path("..", "testdata")
    extracted <- read_parquet_files(files, "clusters", "_extracted_clusters.parquet")

    actual <- compute_template(extracted)

    expected_path <- file.path(testdata, "template", "RCX_shortened.parquet")
    expected <- arrow::read_parquet(expected_path)
    
    expect_equal(actual, expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened")
    )
  )
)

patrick::with_parameters_test_that(
  "correct time test",
  {
    testdata <- file.path("..", "testdata")

    template_features <- file.path(testdata, "template", "RCX_shortened.parquet")
    template_features <- arrow::read_parquet(template_features)

    extracted <- read_parquet_files(files, "clusters", "_extracted_clusters.parquet")

    corrected <- lapply(extracted, function(x){
      correct_time(x, template_features)
    })

    # for(i in 1:3) {
    #   arrow::write_parquet(corrected[[i]], file.path(testdata, "adjusted", paste0(files[i], ".parquet")))
    # }

    expected <- read_parquet_files(files, "adjusted", ".parquet")
    expect_equal(corrected, expected, tolerance = 0.01)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened")
    )
  )
)

test_that("correct_time_v2 is close to correct time", {

  skip("Currently failing - need to investigate.")

  template_features <- arrow::read_parquet(
    file.path("..", "testdata", "template", "RCX_shortened.parquet")
  )

  clustered_table <- read_parquet_files(
    c("RCX_06_shortened"),
    "clusters",
    "_extracted_clusters.parquet"
  )[[1]]

  expected <- correct_time(clustered_table, template_features)
  actual <- correct_time_v2(clustered_table, template_features)

  actual <- dplyr::arrange_at(actual, c("cluster", "mz", "area", "rt"))
  expected <- dplyr::arrange_at(expected, c("cluster", "mz", "area", "rt"))

  expect_equal(actual, expected, tolerance = 0.02)
})