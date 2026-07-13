#' Compute densities for given masses.
#' @description
#' Compute the density estimation for a set of masses using kernel density estimation.
#' It allows for intensity-weighted densities and custom bandwidth functions.
#' @param masses A numeric vector of mass values.
#' @param mz_tol The mass-to-charge ratio tolerance.
#' @param intensity_weighted A logical value indicating whether to weight the densities by intensities.
#' @param intensity A numeric vector of intensity values corresponding to the masses.
#' @param bw_func A function to compute the bandwidth based on the masses.
#' @param n The number of equally spaced points at which the density is to be estimated. Default is 512.
#' @return A density object representing the estimated density of the masses.
#' @export
compute_densities <- function(masses, mz_tol, intensity_weighted, intensity, bw_func, n = 512) {
  bandwidth <- 0.5 * mz_tol * bw_func(masses)
  if (intensity_weighted) {
    weights <- intensity / sum(intensity)
    all.mass.den <- density(masses, weights = weights, bw = bandwidth, n = n)
  } else {
    all.mass.den <- density(masses, bw = bandwidth, n = n)
  }
  return(all.mass.den)
}

#' Compute mass values based on kernel density estimation.
#' @description
#' This function computes the mass values by performing kernel density estimation on the given masses.
#' It identifies the valleys in the density plot to determine the mass values.
#' @param mz_tol The mass-to-charge ratio tolerance.
#' @param masses A numeric vector of mass values.
#' @param intensity_binned A numeric vector of binned intensity values corresponding to the masses.
#' @param intensity_weighted A logical value indicating whether to weight the densities by intensities.
#' @return A numeric vector of mass values corresponding to the valleys in the density plot.
#' @export
compute_mass_values <- function(mz_tol, masses, intensity_binned, intensity_weighted) {
  n <- 2^min(15, floor(log2(length(masses))) - 2)

  all.mass.den <- compute_densities(masses, mz_tol, intensity_weighted, intensity_binned, max, n)

  all.mass.turns <- find.turn.point(all.mass.den$y)
  all.mass.vlys <- all.mass.den$x[all.mass.turns$vlys]
  return(all.mass.vlys)
}

#' @export
compute_breaks <- function(mz_tol, masses, intensity_binned, intensity_weighted) {
  all.mass.vlys <- compute_mass_values(mz_tol, masses, intensity_binned, intensity_weighted)
  breaks <- c(0, unique(round(approx(masses, 1:length(masses), xout = all.mass.vlys, rule = 2, ties = "ordered")$y))[-1])
  return(breaks)
}


#' Increment pointers in a list.
#' @description
#' This function increments the values of `prof.pointer`, `height.pointer`, and `curr.label` in the provided list of pointers.
#' @param pointers A list containing the pointers to be incremented.
#' @param that.n An integer value to increment the `prof.pointer`.
#' @return The updated list of pointers.
#' @keywords internal
#' @export
increment_counter <- function(pointers, that.n){
  pointers$prof.pointer <- pointers$prof.pointer + that.n
  pointers$height.pointer <- pointers$height.pointer + 1
  pointers$curr.label <- pointers$curr.label + 1

  return(pointers)
}

#' Adaptive binning
#' 
#' This is an internal function. It creates EICs using adaptive binning procedure
#' 
#' @param features A tibble with columns of m/z, retention time, intensity.
#' @param min_run Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be 
#'  considered a peak.
#' @param min_pres Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped 
#'  by m/z to be considered a peak.
#' @param mz_tol m/z tolerance level for the grouping of data points. This value is expressed as the fraction of the m/z value. 
#'  This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is the machine's nominal accuracy 
#'  level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended.
#' @param intensity_weighted Whether to weight the local density by signal intensities.
#' @return A list is returned.
#' \itemize{
#'   \item height.rec - The records of the height of each EIC.
#'   \item masses - The vector of m/z values after binning.
#'   \item rt - The vector of retention time after binning.
#'   \item intensity - The vector of intensity values after binning.
#'   \item grps - The EIC rt, i.e. which EIC each observed data point belongs to.
#'   \item times - All the unique retention time values, ordered.
#'   \item mz_tol - The m/z tolerance level.
#'   \item min.count.run - The minimum number of elution time points for a series of signals grouped by m/z to be considered a peak.
#' }
#' @export
adaptive.bin <- function(features,
                         min_run,
                         min_pres,
                         mz_tol,
                         intensity_weighted) {
  # order inputs after mz values
  features <- features |> dplyr::arrange_at("mz")


  cat(c("m/z tolerance is: ", mz_tol, "\n"))

  scan_times <- sort(unique(features$rt))

  num_scans <- length(scan_times)
  time_range <- span(scan_times)

  # calculate function parameters
  scans_per_second <- num_scans / time_range
  min_scans <- min_run * scans_per_second
  aver_cycle_time <- (time_range) / num_scans

  # init data
  newprof <- matrix(0, nrow = length(features$mz), ncol = 4)
  height.rec <- matrix(0, nrow = length(features$mz), ncol = 3)

  # init counters
  pointers <- list(curr.label = 1, prof.pointer = 1, height.pointer = 1)

  breaks <- compute_breaks(mz_tol, features$mz, features$intensity, intensity_weighted)

  for (i in 1:(length(breaks) - 1))
  {
    
    # get number of scans in bin
    start <- breaks[i] + 1
    end <- breaks[i + 1]

    this_table <- dplyr::slice(features, (start:end))

    if (length(unique(this_table$rt)) >= min_scans * min_pres) {
      # reorder in order of rt (scan number)
      this_table <- this_table |> dplyr::arrange_at("rt")
      mass.den <- compute_densities(this_table$mz, mz_tol, intensity_weighted, this_table$intensity, median)

      mass.den$y[mass.den$y < min(this_table$intensity) / 10] <- 0
      mass.turns <- find.turn.point(mass.den$y)
      mass.pks <- mass.den$x[mass.turns$pks]
      mass.vlys <- c(-Inf, mass.den$x[mass.turns$vlys], Inf)


      for (j in 1:length(mass.pks))
      {
        # compute boundaries
        boundaries <- compute_boundaries(mass.vlys, mass.pks[j])

        if (length(mass.pks) == 1){
          boundaries$lower <- boundaries$lower - 1
        }

        # get rows which fulfill condition
        that <- this_table |> dplyr::filter(dplyr::between(mz, boundaries$lower, boundaries$upper))

        if (nrow(that) > 0) {
          that <- aggregate_by_rt(that) |> dplyr::arrange_at("mz")

          that.range <- span(that$rt)

          if (that.range > 0.5 * time_range & length(that$rt) > that.range * min_pres & length(that$rt) / (that.range / aver_cycle_time) > min_pres) {
            that$intensity <- rm.ridge(that$rt, that$intensity, bw = max(10 *min_run, that.range / 2))

            that <- that |> dplyr::filter(intensity != 0)
          }

          num_pts_in_group <- length(that$mz)

          newprof[pointers$prof.pointer:(pointers$prof.pointer + num_pts_in_group - 1), ] <- cbind(that$mz, that$rt, that$intensity, rep(pointers$curr.label, num_pts_in_group))
          height.rec[pointers$height.pointer, ] <- c(pointers$curr.label, num_pts_in_group, max(that$intensity))

          # increment counters
          pointers <- increment_counter(pointers, num_pts_in_group)
        }
      }
    } else { # not enough points in profile
      if (runif(1) < 0.05) {
        this_table <- this_table |> dplyr::arrange_at("rt")

        that.merged <- aggregate_by_rt(this_table)
        num_pts_in_group <- nrow(that.merged)

        newprof[pointers$prof.pointer:(pointers$prof.pointer + num_pts_in_group - 1), ] <- cbind(that.merged$mz, that.merged$rt, that.merged$intensity, rep(pointers$curr.label, num_pts_in_group))
        height.rec[pointers$height.pointer, ] <- c(pointers$curr.label, num_pts_in_group, max(that.merged$intensity))

        # increment counters
        pointers <- increment_counter(pointers, num_pts_in_group)
      }
    }
  }

  newprof <- newprof[1:(pointers$prof.pointer - 1), ]
  height.rec <- height.rec[1:(pointers$height.pointer - 1), , drop = FALSE]

  newprof <- newprof[order(newprof[, 1], newprof[, 2]), ]

  newprof_tibble <- tibble::tibble(mz = newprof[, 1], rt = newprof[, 2], intensity = newprof[, 3], grps = newprof[, 4])

  raw.prof <- new("list")
  raw.prof$height.rec <- height.rec
  raw.prof$features <- newprof_tibble
  raw.prof$min.count.run <- min_scans

  return(raw.prof)
}
