data_frame <- data.frame(
  id = c(1, 2, 3),
  rt = c(100, 101, 102),
  mz = c(50, 51, 52)
)

local_parquet <- function(dir = tempdir(), env = parent.frame()) {
  outpath <- file.path(dir, "output.parquet")
  arrow::write_parquet(data_frame, outpath)
  withr::defer(fs::file_delete(outpath), envir = env)
  outpath
}

local_arrow <- function(metadata, dir = tempdir(), env = parent.frame()) {
  outpath <- file.path(dir, "output.parquet")
  write_arrow(data_frame, outpath, metadata)
  withr::defer(fs::file_delete(outpath), envir = env)
  outpath
}

test_that("write_arrow writes correct number of rows without metadata", {
  outpath <- file.path(tempdir(), "output.parquet")
  write_arrow(data_frame, outpath)

  actual <- arrow::read_parquet(outpath)
  expect_equal(as_tibble(data_frame), actual)
})

test_that("write_arrow writes simple metadata", {
  outpath <- file.path(tempdir(), "output.parquet")
  expected <- list("name" = "peter")

  write_arrow(data_frame, outpath, metadata = expected)
  actual <- arrow::read_parquet(outpath, as_data_frame = FALSE)

  expect_equal(expected, actual$metadata)
})

test_that('read_arrow can read file without metadata', {
  actual <- read_arrow(local_parquet())
  expected <- data_frame
  expect_equal(as.data.frame(actual), expected)
})

test_that('read_arrow can read file with metadata', {
  expected <- list('name' = 'peter')
  actual <- read_arrow(local_arrow(expected))$metadata
  expect_equal(actual, expected)
})