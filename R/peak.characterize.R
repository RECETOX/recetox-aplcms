#' Internal function: Merge statistics from existing data with new observations.
#'
#' @description
#' This function updates summary statistics (mean, standard deviation, min, max) by combining
#' existing statistics with new data points. It uses the weighted update formulas to compute
#' the new mean and pooled standard deviation while maintaining the minimum and maximum values.
#'
#' @param mean0 The existing mean value.
#' @param sd0 The existing standard deviation.
#' @param min0 The existing minimum value.
#' @param max0 The existing maximum value.
#' @param n The number of observations in the existing data.
#' @param x A vector of new observations to be merged with the existing statistics.
#'
#' @return A numeric vector of length 4 containing: updated mean, updated standard deviation,
#'   updated minimum, and updated maximum.
#' @export
merge.new <- function(mean0, sd0, min0, max0, n, x) {
    x <- x[!is.na(x)]
    if (n <= 1) {
        if (n == 1) {
            x <- c(x, mean0)
        }
        mean1 <- mean(x)
        sd1 <- sd(x)
        min1 <- min(x, Inf)
        max1 <- max(x, -Inf)
    } else {
        m <- length(x)
        mean1 <- sum(mean0 * n, x) / (n + m)
        sd1 <- sqrt((n * (mean0 - mean1)^2 + sum((x - mean1)^2) + (n - 1) * sd0^2) / (m + n - 1))
        min1 <- min(min0, x)
        max1 <- max(max0, x)
    }
    return(c(mean1, sd1, min1, max1))
}

#' Internal function: Update feature characteristics in the known feature table row.
#'
#' @description
#' This function updates a single row in the known feature table by merging existing statistics
#' with new data from the current experiment. It updates the number of profiles processed,
#' percent found, m/z range, retention time statistics, and intensity statistics.
#'
#' @param existing_row A vector representing an existing row in the known feature table with 18 elements:
#'   \itemize{
#'     \item [1-5]: Chemical identifiers and metadata
#'     \item [6]: m.z - Mean m/z value
#'     \item [7]: Number_profiles_processed - Total number of profiles processed
#'     \item [8]: Percent_found - Proportion of profiles where feature was found
#'     \item [9]: mz_min - Minimum m/z value observed
#'     \item [10]: mz_max - Maximum m/z value observed
#'     \item [11]: RT_mean - Mean retention time
#'     \item [12]: RT_sd - Standard deviation of retention time
#'     \item [13]: RT_min - Minimum retention time
#'     \item [14]: RT_max - Maximum retention time
#'     \item [15]: int_mean(log) - Mean log-intensity
#'     \item [16]: int_sd(log) - Standard deviation of log-intensity
#'     \item [17]: int_min(log) - Minimum log-intensity
#'     \item [18]: int_max(log) - Maximum log-intensity
#'   }
#' @param n The number of times the feature was found in previous experiments.
#' @param m The number of times the feature was found in the current experiment.
#' @param metadata_row A row from the aligned feature metadata table containing mz, mzmin, and mzmax.
#' @param rt_row A vector of retention times for the feature across samples in the current experiment.
#' @param ftrs_row A vector of log-transformed intensities for the feature across samples.
#'
#' @return A vector with the updated statistics for all 18 elements of the known feature table row.
#' @export
characterize <- function(existing_row, n, m, metadata_row, rt_row, ftrs_row) {
    existing_row[7] <- sum(existing_row[7], length(ftrs_row) - 1, na.rm = T)
    existing_row[8] <- (n + m) / existing_row[7]
    existing_row[9] <- min(existing_row[6], existing_row[9], metadata_row$mzmin, na.rm = T)
    existing_row[10] <- max(existing_row[6], existing_row[10], metadata_row$mzmax, na.rm = T)

    this <- merge.new(existing_row[11], existing_row[12], existing_row[13], existing_row[14], n, rt_row[2:length(rt_row)])
    existing_row[11:14] <- this

    this <- merge.new(existing_row[15], existing_row[16], existing_row[17], existing_row[18], n, ftrs_row[2:length(ftrs_row)])
    existing_row[15:18] <- this

    return(existing_row)
}

#' Internal function: Updates the information of a feature for the known feature table.
#'
#' @description
#' The function takes the information about the feature in the known feature table (if available), and updates it using the
#' information found in the current dataset.
#' @param existing_row The existing row in the known feature table.
#' @param ftrs_row The row of the matched feature in the new aligned feature table.
#' @param rt_row The row of the matched feature in the new retention time table of aligned features.
#' @return A vector, the updated row for the known feature table.
peak_characterize <- function(existing_row = NA, metadata_row, ftrs_row, rt_row) {
    ftrs_row[2:length(ftrs_row)] <- log10(ftrs_row[2:length(ftrs_row)] + 1)
    ftrs_row[ftrs_row == 0] <- NA
    if (length(existing_row) == 1) {
        existing_row <- rep(NA, 18)
        existing_row[6] <- metadata_row$mz
    }

    n <- round(as.numeric(existing_row[7]) * as.numeric(existing_row[8])) # times found in previous experiments
    if (is.na(n)) {
        n <- 0
    }
    m <- sum(!is.na(rt_row[2:length(rt_row)])) # times found in current experiment

    existing_row <- characterize(existing_row, n, m, metadata_row, rt_row, ftrs_row)

    return(tibble::as_tibble(existing_row))
}
