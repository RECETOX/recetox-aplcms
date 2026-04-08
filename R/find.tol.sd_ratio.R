#' Compute sd ratio relative tolerance to use.
#' @description
#' Compute the sd ratio tolerance based on the kernel density estimation.
#' It plots the fitting function if set to TRUE.
#' @param max.num.segments the maximum number of segments.
#' @param aver.bin.size The average bin size to determine the number of equally spaced points in the kernel density estimation.
#' @param number_of_samples The number of spectra in this analysis.
#' @param rt retention time of all peaks in all profiles in the study.
#' @param min.bins the minimum number of bins to use in the kernel density estimation. It overrides aver.bin.size when too few observations are present.
#' @param max.bins the maximum number of bins to use in the kernel density estimation. It overrides aver.bin.size when too many observations are present.
#' @param do.plot Indicates whether plot should be drawn.
#' @return sd_ratio_tol_relative the sd ratio tolerance.
compute_sd_ratio_tol_relative <- function(breaks,
                                    max.num.segments,
                                    aver.bin.size,
                                    number_of_samples,
                                    sd_ratio, 
                                    min.bins,
                                    max.bins,
                                    do.plot = FALSE,
                                    title = "sd ratio tolerance estimation") { 
    distances <- 0
    
    #' This conditional makes sure that length(s) is <= max.num.segments
    #' If False, length(s) =  max.num.segments, and s[i] is the largest
    #' integer no greater than the corresponding element. Otherwise
    #' length(s) =  length(breaks) - 1
    if (length(breaks) > max.num.segments) {
        s <- floor(seq(2, length(breaks), length.out = max.num.segments))
    } else {
        s <- 2:length(breaks)
    }

    #' This loop creates a vector with distances between rt peaks. Distances
    #' are stored in a triangular matrix and converted to a vector subsequently.
    #' Vector length should be < 100, otherwise, vector is
    #' constructed extracting only 100 samples.
    for (i in s) {
        subset_idx <- (breaks[i - 1] + 1):breaks[i] # create subset of indices
        if (length(subset_idx) <= 3 * number_of_samples) {
            sd_ratio_distances <- as.vector(dist(sd_ratio[subset_idx]))
            if (length(sd_ratio_distances) > 100) {
                sd_ratio_distances <- sample(sd_ratio_distances, 100)
            }
            distances <- c(distances, sd_ratio_distances)
        }
    }

    # a long vector of distances between rt values (with no particular order)
    distances <- distances[!is.na(distances)]
    max_distance <- max(distances) # maximal distance

    # number of equally spaced points at which the density is to be estimated
    n <- min(max.bins, max(min.bins, round(length(distances) / aver.bin.size)))

    # estimate probability density function of distances
    des <- density(distances,
        kernel = "gaussian", n = n,
        bw = max_distance / n * 2, from = 0
    )
    # the n (-1?) coordinates of the points where the density is estimated
    points <- des$x[des$x > 0]

    # the estimated density values
    density_values <- des$y[des$x > 0]

    linear_regression_model <- lm(density_values[points > max_distance / 4] ~ points[points > max_distance / 4])

    # compute probability density values (y) using the linear function
    estimated_density_values <- linear_regression_model$coef[1] + linear_regression_model$coef[2] * points

    values_not_last <- density_values[1:(length(density_values) - 1)] # density values without the last one
    values_not_first <- density_values[2:(length(density_values))] # density values without the first one

    # pair-wise copy greater value to the left
    values_not_last[which(values_not_last < values_not_first)] <- values_not_first[which(values_not_last < values_not_first)]
    density_values[1:(length(density_values) - 1)] <- values_not_last

    # cumulative sum - where density value is greater than estimated density value
    # cutoff is selected where the density of the empirical distribution is >1.5 times the density of the distribution
    cumulative <- cumsum(density_values > 1.5 * estimated_density_values)
    cumulative_indices <- seq_along(cumulative)

    # find last index where density value is greater than estimated density value
    selected <- min(which(cumulative < cumulative_indices)) - 1

    # corresponding coordinate is used as rt tolerance
    sd_ratio_tol_relative <- points[selected]

    if (do.plot) {
        tolerance_plot(
            points,
            density_values,
            estimated_density_values,
            selected,
            main = "find sd ratio tolerance"
        )
    }
    return(sd_ratio_tol_relative)
}
