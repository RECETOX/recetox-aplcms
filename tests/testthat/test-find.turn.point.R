test_that("find.turn.point identifies single gaussian peak", {
  # Create a single gaussian peak
  x <- seq(-10, 10, length.out = 200)
  y <- dnorm(x, mean = 0, sd = 2)
  
  result <- find.turn.point(y)
  
  # Should have exactly one peak
  expect_equal(length(result$pks), 1)
  # Should have two valleys (at the ends)
  expect_equal(length(result$vlys), 2)
  # Peak should be near the center
  expect_true(result$pks[1] > 90 && result$pks[1] < 110)
  # Valleys should be at the boundaries
  expect_equal(result$vlys[1], 1)
  expect_equal(result$vlys[2], length(y))
})

test_that("find.turn.point identifies two-component gaussian mixture", {
  # Create a mixture of two gaussians
  x <- seq(-10, 10, length.out = 400)
  y <- 0.6 * dnorm(x, mean = -3, sd = 1) + 0.4 * dnorm(x, mean = 3, sd = 1.5)
  
  result <- find.turn.point(y)
  
  midpoint <- length(y) / 2
  
  # Should have exactly two peaks
  expect_equal(length(result$pks), 2)
  # Should have at least one valley between peaks plus boundaries
  expect_true(length(result$vlys) >= 3)
  # First peak should be in the left half
  expect_true(result$pks[1] < midpoint)
  # Second peak should be in the right half
  expect_true(result$pks[2] > midpoint)
  # Peaks should be ordered
  expect_true(result$pks[1] < result$pks[2])
})

test_that("find.turn.point identifies three-component gaussian mixture", {
  # Create a mixture of three gaussians
  x <- seq(-15, 15, length.out = 600)
  y <- 0.4 * dnorm(x, mean = -7, sd = 1.2) + 
       0.5 * dnorm(x, mean = 0, sd = 1.5) + 
       0.3 * dnorm(x, mean = 6, sd = 1)
  
  result <- find.turn.point(y)
  
  # Should have exactly three peaks
  expect_equal(length(result$pks), 3)
  # Should have valleys including boundaries
  expect_true(length(result$vlys) >= 4)
  # Peaks should be ordered
  expect_true(all(diff(result$pks) > 0))
  # Valleys should be ordered
  expect_true(all(diff(result$vlys) > 0))
})

test_that("find.turn.point handles constant values", {
  # All values are the same
  y <- rep(5, 100)
  
  result <- find.turn.point(y)
  
  # Should have one peak in the middle
  expect_equal(length(result$pks), 1)
  expect_equal(result$pks[1], 50)
  # Should have two valleys at the ends
  expect_equal(length(result$vlys), 2)
  expect_equal(result$vlys[1], 1)
  expect_equal(result$vlys[2], 100)
})

test_that("find.turn.point handles NA values", {
  # Create data with NA values
  x <- seq(-10, 10, length.out = 200)
  y <- dnorm(x, mean = 0, sd = 2)
  y[c(1, 50, 100, 150)] <- NA
  
  result <- find.turn.point(y)
  
  # Should still identify a peak
  expect_equal(length(result$pks), 1)
  # Should have valleys
  expect_true(length(result$vlys) >= 2)
})

test_that("find.turn.point handles narrow overlapping gaussians", {
  # Create two closely overlapping gaussians
  x <- seq(-5, 5, length.out = 500)
  y <- dnorm(x, mean = -0.5, sd = 0.8) + dnorm(x, mean = 0.5, sd = 0.8)
  
  result <- find.turn.point(y)
  
  # Depending on overlap, might detect 1 or 2 peaks
  # At minimum should detect peaks
  expect_true(length(result$pks) >= 1)
  # Should have valleys
  expect_true(length(result$vlys) >= 2)
})

test_that("find.turn.point with well-separated gaussians", {
  # Create widely separated gaussians
  x <- seq(-20, 20, length.out = 800)
  y <- dnorm(x, mean = -10, sd = 1) + dnorm(x, mean = 10, sd = 1)
  
  result <- find.turn.point(y)
  
  # Should clearly identify two peaks
  expect_equal(length(result$pks), 2)
  # Should have valley in the middle and at boundaries
  expect_true(length(result$vlys) >= 3)
  # Middle valley should be between the peaks
  middle_valleys <- result$vlys[result$vlys > result$pks[1] & result$vlys < result$pks[2]]
  expect_true(length(middle_valleys) >= 1)
})

test_that("find.turn.point handles unequal amplitude gaussians", {
  # Create gaussians with very different amplitudes
  x <- seq(-10, 15, length.out = 500)
  y <- 5 * dnorm(x, mean = -4, sd = 1.5) + 0.5 * dnorm(x, mean = 8, sd = 1)
  
  result <- find.turn.point(y)
  
  # Should detect both peaks despite different amplitudes
  expect_equal(length(result$pks), 2)
  # Should have valleys
  expect_true(length(result$vlys) >= 3)
})

test_that("msExtrema identifies maxima and minima correctly", {
  # Create a simple wave pattern
  x <- seq(0, 4 * pi, length.out = 200)
  y <- sin(x)
  
  result <- msExtrema(y)
  
  # Should have both maxima and minima
  expect_true(sum(result$index.max) > 0)
  expect_true(sum(result$index.min) > 0)
  # Maxima and minima should not overlap
  expect_equal(sum(result$index.max & result$index.min), 0)
})

test_that("find_local_maxima identifies peaks correctly", {
  # Simple curve with clear peaks
  y <- c(1, 2, 3, 2, 1, 2, 4, 3, 2)
  
  result <- find_local_maxima(y, ties.method = "first")
  
  # Should identify the two peaks (at positions 3 and 7)
  expect_true(result[3])
  expect_true(result[7])
  # Other positions should not be peaks
  expect_false(result[1])
  expect_false(result[5])
})
