test_that("mz tolerance is found", {
  file <- file.path("..", "testdata", "extracted", "RCX_06_shortened.parquet")
  mz <- arrow::read_parquet(file)$mz

  mz_tol_relative <- find_mz_tolerance(
    mz,
    mz_max_diff = 0.1,
    aver.bin.size = 4000,
    min.bins = 50,
    max.bins = 200,
    do.plot = FALSE
  )

  expect_equal(mz_tol_relative, 0.0234, tolerance=0.002)
})