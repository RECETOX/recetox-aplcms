#' Draw basic plot with optional label.
#' @description
#' This function creates a basic plot with specified x and y ranges, labels, and an optional text label at the center.
#' @param x A numeric vector specifying the x-axis range.
#' @param y A numeric vector specifying the y-axis range.
#' @param xlab A character string for the x-axis label.
#' @param ylab A character string for the y-axis label.
#' @param main A character string for the main title of the plot.
#' @param axes A logical value indicating whether to draw the axes.
#' @param type A character string specifying the type of plot (default is "n" for no plotting).
#' @param label A character string for the text label to be placed at the center of the plot (default is NA, meaning no label).
#' @param cex A numeric value specifying the size of the text label (default is 1.2).
#' @export
draw_plot <- function(x = c(-1, 1), y = c(-1, 1),
                      xlab = "", ylab = "",
                      main = "", axes = FALSE,
                      type = "n", label = NA, cex = 1.2) {
  plot(x, y, type = type, xlab = xlab, ylab = ylab, main = main, axes = axes)
  if (!is.na(label)) {
    text(x = 0, y = 0, label, cex = cex)
  }
}

#' Plot tolerance level.
#' @description
#' This function plots the tolerance level for a given set of x and y values, along with expected y values and a selected index.
#' @param x A numeric vector of x values.
#' @param y A numeric vector of y values.
#' @param exp_y A numeric vector of expected y values.
#' @param selected An integer index indicating the selected point for drawing a line.
#' @param main A character string for the main title of the plot.
#' @export
tolerance_plot <- function(x, y, exp_y, selected, main) {
  plot(x, y, xlab = "Delta", ylab = "Density", main = main, cex = .25)
  lines(x, exp_y, col = "red")
  abline(v = x[selected], col = "blue")
}

#' Draw retention time normal peaks.
#' @description
#' This function draws the normal peaks for retention time data based on the provided parameters.
#' @param x A numeric vector of retention time values.
#' @param truth A numeric vector containing the parameters for the normal peaks:
#' \itemize{
#'   \item mean of the peak
#'   \item standard deviation for the left side of the peak
#'   \item standard deviation for the right side of the peak
#'   \item scaling factor for the peak height
#' }
#' @export
draw_rt_normal_peaks <- function(x, truth) {
  true.y1 <- dnorm(x[x < truth[1]], mean = truth[1], sd = truth[2]) * truth[2] * truth[4]
  true.y2 <- dnorm(x[x >= truth[1]], mean = truth[1], sd = truth[3]) * truth[3] * truth[4]
  lines(x, c(true.y1, true.y2), col = "green")
}

#' Plot raw profile histogram.
#' @description
#' This function plots various histograms and density plots for a given raw profile, including noise groups, selected groups, retention time range distribution, and signal presence distribution.
#' @param raw.prof A list containing raw profile data, including height records and minimum count run.
#' @param min_pres A numeric value indicating the minimum presence threshold.
#' @param baseline.correct A numeric value for baseline correction. If NA, it will be computed automatically.
#' @param baseline.correct.noise.percentile A numeric value indicating the percentile of noise group heights for baseline correction.
#' @param mz_tol A numeric value for the mass-to-charge ratio tolerance.
#' @param new.prof A list containing new profile data, including height records, time range records, and m/z presence records.
#' @export
plot_raw_profile_histogram <- function(raw.prof,
                                       min_pres,
                                       baseline.correct,
                                       baseline.correct.noise.percentile,
                                       mz_tol,
                                       new.prof) {
  h.1 <- log10(raw.prof$height.rec[raw.prof$height.rec[, 2] <= max(2, raw.prof$min.count.run * min_pres / 2), 3])
  h.2 <- log10(raw.prof$height.rec[raw.prof$height.rec[, 2] >= raw.prof$min.count.run * min_pres, 3])
  
  if (is.na(baseline.correct)) {
    baseline.correct <- 10 ^ quantile(h.1, baseline.correct.noise.percentile)
    message(c("maximal height cut is automatically set at the",
              baseline.correct.noise.percentile,
              "percentile of noise group heights: ",
              baseline.correct
             )
           )
  } else {
    message(c("maximal height cut is provided by user: ", baseline.correct))
  }
  par(mfrow = c(2, 2))
  
  draw_plot(main = "tolerance level loaded", label = mz_tol)
  
  if (length(h.1) > 50) {
    plot(density(h.1),
         xlab = "maximum height of group (log scale)",
         xlim = range(c(h.1, h.2)),
         main = "Black - noise groups \n Blue - selected groups")
  } else {
    plot(NA, NA, xlab = "maximum height of group (log scale)",
      xlim = range(c(h.1, h.2)), ylim = c(0, 1),
      main = "Black - noise groups \n Blue - selected groups"
        )
    if (length(h.1) > 0)
      abline(v = h.1)
  }
  
  abline(v = log10(baseline.correct), col = "red")
  lines(density(log10(new.prof$height.rec)), col = "blue")
  hist(
    new.prof$time.range.rec,
    xlab = "Range of retention time in the same group",
    ylab = "Density",
    freq = FALSE,
    nclass = 100,
    main = "Group retention time range distribution"
  )
  hist(
    new.prof$mz.pres.rec,
    xlab = "% signal present in the same group",
    ylab = "Density",
    freq = FALSE,
    nclass = 20,
    main = "Group % present signal distribution"
  )
}

#' Plot peak summary.
#' @description
#' This function plots a summary of peak characteristics, including m/z standard deviation, retention time standard deviation, and peak strength.
#' @param feature_groups A list of data frames, where each data frame represents a group of features with m/z values.
#' @param processed_features A data frame containing processed feature information with columns "sd1", "sd2", and "area".
#' @export
plot_peak_summary <- function(feature_groups, processed_features) {
  mz_sd <- compute_mz_sd(feature_groups)

  par(mfrow = c(2, 2))
  plot(c(-1, 1), c(-1, 1), type = "n", xlab = "", ylab = "", main = "", axes = FALSE)
  text(x = 0, y = 0, "Estimate peak \n area/location", cex = 1.5)
  hist(mz_sd, xlab = "m/z SD", ylab = "Frequency", main = "m/z SD distribution")
  hist(c(processed_features[, "sd1"], processed_features[, "sd2"]), xlab = "Retention time SD", ylab = "Frequency", main = "Retention time SD distribution")
  hist(log10(processed_features[, "area"]), xlab = "peak strength (log scale)", ylab = "Frequency", main = "Peak strength distribution")
}

#' Plot retention time profile.
#' @description
#' This function plots the retention time profile, including the base curve, intensity, and fitted components.
#' @param rt_profile A data frame containing the retention time profile with columns "base_curve" and "intensity".
#' @param bw The bandwidth used for the kernel density estimation.
#' @param fit A matrix containing the fitted components for the retention time profile.
#' @param m A numeric vector of positions where vertical lines should be drawn.
#' @export
plot_rt_profile <- function(rt_profile, bw, fit, m) {
  plot(rt_profile[, "base_curve"], rt_profile[, "intensity"], cex = .1, main = paste("bw=", bw))
  sum.fit <- apply(fit, 1, sum)
  lines(rt_profile[, "base_curve"], sum.fit)
  abline(v = m)
  cols <- c("red", "green", "blue", "cyan", "brown", "black", rep("grey", 100))
  for (i in 1:length(m))
  {
    lines(rt_profile[, "base_curve"], fit[, i], col = cols[i])
  }
}

#' Plot normalized mixture model with BIC.
#' @description
#' This function plots the data points and the fitted Gaussian mixture model components with different bandwidths.
#' @param x A numeric vector of data points on the x-axis.
#' @param y A numeric vector of data points on the y-axis.
#' @param bw The bandwidth used for the kernel density estimation.
#' @param aaa A matrix containing the parameters of the Gaussian mixture model components:
#' \itemize{
#'   \item mean of the Gaussian component
#'   \item standard deviation of the Gaussian component
#'   \item scaling factor for the Gaussian component
#' }
#' @export
plot_normix_bic <- function(x, y, bw, aaa) {
  plot(x, y, cex = .1, main = paste("bw=", bw))
  abline(v = aaa[, 1])
  cols <- c("red", "green", "blue", "cyan", "brown", "black", rep("grey", 100))
  for (i in 1:nrow(aaa))
  {
    lines(x, dnorm(x, mean = aaa[i, 1], sd = aaa[i, 2]) * aaa[i, 3], col = cols[i])
  }
}

#' Draw retention time correction plot.
#' @description
#' This function draws a plot showing the retention time correction for extracted features.
#' It plots the deviation of corrected retention times from the original retention times.
#' @param colors A character vector of colors to use for plotting each sample. If NA, default colors are used.
#' @param extracted_features A list of data frames, where each data frame contains the original retention times and m/z values of features.
#' @param corrected_features A list of data frames, where each data frame contains the corrected retention times of features.
#' @param rt_tol_relative A numeric value representing the relative retention time tolerance.
#' @export
draw_rt_correction_plot <- function(colors,
                                    extracted_features,
                                    corrected_features,
                                    rt_tol_relative) {
  number_of_samples <- length(extracted_features)
  if (is.na(colors[1])) {
    colors <- c(
      "red", "blue", "dark blue", "orange", "green", "yellow",
      "cyan", "pink", "violet", "bisque", "azure", "brown",
      "chocolate", rep("grey", number_of_samples)
    )
  }

  draw_plot(
    x = range(extracted_features[[1]]$rt),
    y = c(-rt_tol_relative, rt_tol_relative),
    xlab = "Original Retention time",
    ylab = "Retention time deviation",
    axes = TRUE
  )

  for (i in 1:number_of_samples) {
    extracted_features[[i]] <- extracted_features[[i]] |> dplyr::arrange_at(c("mz", "rt"))
    points(extracted_features[[i]]$rt, corrected_features[[i]]$rt - extracted_features[[i]]$rt,
      col = colors[i], cex = .2
    )
  }
}
