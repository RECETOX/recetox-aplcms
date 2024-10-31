#' Computes unique groups
#' @param min_count_run filter parameter.
#' @param min_pres Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped
#'  by m/z to be considered a peak.
#' @param profile The matrix containing m/z, retention time, intensity, and EIC label as columns.
#' @return unique_grp.
#' @export
compute_uniq_grp <- function(profile, min_count_run, min_pres) {
  ttt <- table(profile)
  ttt <- ttt[ttt >= max(min_count_run * min_pres, 2)]
  unique_grp <- as.numeric(names(ttt))
  return(unique_grp)
}

#' Computes the smoothed retention times by using The Nadaraya-Watson kernel regression estimate function.
#' @param min_run Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak.
#' @param times. Retention times vector.
#' @return predicted rt.
#' @export
predict_smoothed_rt <- function(min_run, times) {
  # ksmooth(x, y, kernel, bandwidth, range, n.points, x.points)
  smooth <- ksmooth(
    seq(-min_run + 1, length(times) + min_run),
    c(
      rep(0, min_run),
      times,
      rep(0, min_run)
    ),
    kernel = "box",
    bandwidth = min_run,
    x.points = 1:length(times)
  )
  # vector of smoothed estimates for the regression at the corresponding x
  smooth <- smooth$y
  return(smooth)
}

#' This function labels the indices of values kept to perform further calculations
#' @param min_run Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak.
#' @param min_pres Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped
#'  by m/z to be considered a peak.
#' @param timeline.
#' @param this_times.
#' @param times. Retention times vector.
#' @return to_keep.
#' @export
label_val_to_keep <- function(min_run, timeline, min_pres, this_times, times) {
  this_timeline <- timeline
  this_timeline[this_times] <- 1
  to_keep <- this_times * 0

  # filtering based on the kernel regression estimate
  this_smooth <- predict_smoothed_rt(min_run, this_timeline)
  if (max(this_smooth, na.rm = TRUE) >= min_pres) {
    measured_points <- good_points <- timeline
    measured_points[this_times] <- 1

    good_sel <- which(this_smooth >= min_pres)
    good_points[good_sel] <- 1
    for (j in (-min_run):min_run) {
      curr_sel <- good_sel + j
      curr_sel <- curr_sel[curr_sel > 0 & curr_sel <= length(times)]
      good_points[curr_sel] <- 1
    }

    measured_points <- measured_points * good_points
    to_keep[which(this_times %in% which(measured_points == 1))] <- 1
  }
  return(to_keep)
}

min_num_datapoints <- function(duration, min_pres, scan_rate) {
  return(duration * min_pres * scan_rate)
}

calculate_scan_rate <- function(rt) {
  scan_times <- unique(rt)
  scan_rate <- 1.0 / abs(median(diff(scan_times)))
  return(scan_rate)
}

#' Continuity index.
#' @description
#' Internal function that removes noise in the retention time dimension. It uses continuity index (or "run filter") to select putative peaks from EIC.
#' @param newprof The matrix containing m/z, retention time, intensity, and EIC label as columns.
#' @param min_pres Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped
#' by m/z to be considered a peak.
#' @param min_run Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak.
#' @return A list is returned. new_rec - The matrix containing m/z, retention time, intensity, and EIC label as columns after applying the run filter.
#' @export
run_filter <- function(newprof,
                       min_pres,
                       min_run) {

  newprof <- dplyr::arrange_at(newprof, "rt")

  # calculates the minimun number of rt points to be considered a peak
  scan_rate <- calculate_scan_rate(newprof$rt)
  min_count_run <- round(min_pres * min_run * scan_rate)

  # computes unique groups
  uniq_grp <- compute_uniq_grp(newprof$grps, min_count_run, min_pres)

  # ordered by mz and grps data that are inside unigrps
  newprof_uniq <- dplyr::filter(newprof, grps %in% uniq_grp) |> dplyr::arrange(grps, mz)
  browser()

  results <- dplyr::group_by(newprof_uniq, grps) |>
    dplyr::filter(n() >= min_num_datapoints(span(rt), min_pres, scan_rate) && abs(span(rt)) >= min_run) |>
    dplyr::ungroup() |>
    dplyr::rename(group_number = grps)

  return(results)
}
