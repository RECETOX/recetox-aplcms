#' @import dplyr foreach
NULL
#> NULL

#' Combine template and sample features
#' @param template_features Tibble Template feature table (mz, rt, cluster, sample_id).
#' @param features Tibble Sample feature table (mz, rt, cluster, sample_id).
#' @return Tibble Combined feature table (rbind).
#' @export
compute_comb <- function(template_features, features) {
  combined <- dplyr::bind_rows(
    template_features,
    features |> dplyr::select(c(mz, rt, cluster, sample_id))
  ) |> dplyr::arrange_at(c("cluster","mz"))
  return(combined)
}

#' Select features to use for retention time alignment
#' @description This function selects features present in both the sample
#' feature table and template feature table given they have the same cluster,
#' are adjacent in the combined table.
#' @param combined Tibble Table with (mz, rt, cluster, sample_id).
#' @return List of bool Returns list of bools with TRUE at each index where this condition is met.
#' @export
compute_sel <- function(combined) {
  l <- nrow(combined)
  sel <- which(combined$cluster[1:(l - 1)] == combined$cluster[2:l] & 
               combined$sample_id[1:(l - 1)] != combined$sample_id[2:l])
  return(sel)
}

#' Create two column table with paired sample and template retention times.
#' @param combined Tibble Table with features from sample and template.
#' @param sel list of bools List of bools indiciating which features to pair.
#' See 'compute_sel'.
#' @param j string Template sample_id.
#' @export
compute_template_adjusted_rt <- function(combined, sel, j) {
  all_features <- cbind(combined$rt[sel], combined$rt[sel + 1])
  flip_indices <- which(combined$sample_id[sel] == j)
  temp <- all_features[flip_indices, 2]
  all_features[flip_indices, 2] <- all_features[flip_indices, 1]
  all_features[flip_indices, 1] <- temp

  # now the first column is the template retention time.
  # the second column is the to-be-adjusted retention time

  all_features <- all_features[order(all_features[, 2]), ]
  return(all_features)
}

#' Correct the rt in feature table based on paired feature rts and differences.
#' @description This is a newer implementation based on dplyr which might be more efficient than the other function.
#' @param features Tibble The feature table for which to correct rts.
#' @param template_rt List of floats Template retention times for the paired features.
#' @param delta_rt List of floats Differences between the paired rts.
#' @return Tibble A table with corrected retention times.
#' @export
compute_corrected_features_v2 <- function(features, template_rt, delta_rt) {
  features <- features |> dplyr::arrange_at(c("rt", "mz"))
  idx <- dplyr::between(features$rt, min(template_rt), max(template_rt))
  to_correct <- (features |> dplyr::filter(idx))$rt

  this.smooth <- ksmooth(
    template_rt,
    delta_rt,
    kernel = "normal",
    bandwidth = (max(delta_rt) - min(delta_rt)) / 5,
    x.points = to_correct
  )

  lower_bound_adjustment <- mean(this.smooth$y[this.smooth$x == min(this.smooth$x)])
  upper_bound_adjustment <- mean(this.smooth$y[this.smooth$x == max(this.smooth$x)])

  features <- features |>
    dplyr::mutate(rt = dplyr::case_when(
      rt < min(template_rt) ~ rt + lower_bound_adjustment,
      rt > max(template_rt) ~ rt + upper_bound_adjustment
    ))
  features[idx, "rt"] <- to_correct + this.smooth$y
  return(features |> dplyr::arrange_at(c("mz", "rt")))
}

#' Correct the rt in feature table based on paired feature rts and differences.
#' @param features Tibble The feature table for which to correct rts.
#' @param template_rt List of floats Template retention times for the paired features.
#' @param delta_rt List of floats Differences between the paired rts.
#' @return Tibble A table with corrected retention times.
#' @export
compute_corrected_features <- function(features, template_rt, delta_rt) {
  features <- features |> dplyr::arrange_at(c("rt", "mz"))

  corrected <- features$rt
  original <- features$rt

  idx <- dplyr::between(original, min(template_rt), max(template_rt))
  to_correct <- original[idx]
  this.smooth <- ksmooth(
    template_rt,
    delta_rt,
    kernel = "normal",
    bandwidth = (max(template_rt) - min(template_rt)) / 5,
    x.points = to_correct
  )

  corrected[idx] <- this.smooth$y + to_correct
  lower_bound_adjustment <- mean(this.smooth$y[this.smooth$x == min(this.smooth$x)])
  upper_bound_adjustment <- mean(this.smooth$y[this.smooth$x == max(this.smooth$x)])

  idx_lower <- original < min(template_rt)
  idx_upper <- original > max(template_rt)

  corrected[idx_lower] <- corrected[idx_lower] + lower_bound_adjustment
  corrected[idx_upper] <- corrected[idx_upper] + upper_bound_adjustment
  features$rt <- corrected
  features <- features[order(features$mz, features$rt), ]

  return(features)
}

#' Fill missing values based on original retention times.
#' @param orig.features Non-corrected feature table.
#' @param this.features Feature table with eventual missing values.
#' @return Tibble Feature table with filles values.
#' @export
fill_missing_values <- function(orig.feature, this.feature) {
  missing_values <- which(is.na(this.feature$rt))
  for (i in missing_values) {
    this.d <- abs(orig.feature$rt[i] - orig.feature$rt)
    this.d[missing_values] <- Inf
    this.s <- which.min(this.d)
    this.feature$rt[i] <- orig.feature$rt[i] + this.feature$rt[this.s] -
      orig.feature$rt[this.s]
  }
  return(this.feature)
}

#' Function to perform retention time correction
#' @param this.feature Tibble Feature table for which to correct rt.
#' @param template_features Tibble Template feature table to use for correction.
#' @return Tibble this.feature table with corrected rt values.
#' @export
correct_time <- function(this.feature, template_features) {
    orig.features <- this.feature
    template <- unique(template_features$sample_id)[1]
    j <- unique(this.feature$sample_id)[1]

    if (j != template) {
      this.comb <- compute_comb(template_features, this.feature)
      sel <- compute_sel(this.comb)

      if (length(sel) < 10) {
        stop("too few, aborted")
      } else {
        all.ftr.table <- compute_template_adjusted_rt(this.comb, sel, j)

        this.feature <- compute_corrected_features(
          this.feature,
          all.ftr.table[, 2],  # the to be adjusted time
          all.ftr.table[, 1] - all.ftr.table[, 2]  # the difference between the true time and the to-be-adjusted time
        )
      }
    }

    if (sum(is.na(this.feature$rt)) > 0) {
      this.feature <- fill_missing_values(
        orig.features,
        this.feature
      )
    }

  return(tibble::as_tibble(this.feature, column_name = c("mz", "rt", "sd1", "sd2", "area", "sample_id", "cluster")))
}

#' Select the template feature table.
#' @description The current implementation selects the table with the most features as the template.
#' @param extracted_features List of tables Tables from which to select the template.
#' @return Tibble Template feature table.
#' @export
compute_template <- function(extracted_features) {
  num.ftrs <- sapply(extracted_features, nrow)
  template_id <- which.max(num.ftrs)
  template <- extracted_features[[template_id]]$sample_id[1]
  message(paste("the template is sample", template))

  candi <- tibble::as_tibble(extracted_features[[template_id]]) |> dplyr::select(c(mz, rt, cluster))
  template_features <- dplyr::bind_cols(candi, sample_id = rep(template, nrow(candi)))
  return(tibble::as_tibble(template_features))
}

#' Rewritten version of 'correct_time'
#' @description This function uses dplyr to do the same as
#' 'correct_time', just with less code. Most functions used in the original
#' function are replaced with simple data transformations.
#' @param features Tibble Table with features to correct.
#' @param template Tibble Template feature table to use for correction.
#' @return Tibble Corrected feature table.
#' @export
correct_time_v2 <- function(features, template) {
  if (unique(features$sample_id) == unique(template$sample_id))
    return(tibble::as_tibble(features))

  subsets <- template |>
    dplyr::bind_rows(
      features |> dplyr::select(c(mz, rt, cluster, sample_id))
    ) |>
    dplyr::arrange_at(c("cluster", "mz")) |>
    dplyr::group_by(cluster) |>
    dplyr::mutate(count = dplyr::n_distinct(sample_id)) |>
    filter(count == 2) |>
    dplyr::add_count() |>
    filter(n == 2) |>
    dplyr::ungroup() |>
    dplyr::group_by(sample_id) |>
    dplyr::group_split()

  all_features_new <- cbind(subsets[[1]]$rt, subsets[[2]]$rt)
  all_features_new_order <- order(all_features_new[, 2])
  all_features_new_arranged <- all_features_new[all_features_new_order,]

  corrected <- compute_corrected_features_v2(
    features,
    all_features_new_arranged[, 2],
    all_features_new_arranged[, 1] - all_features_new_arranged[, 2]
  )
  return(tibble::as_tibble(corrected))
}

#' Adjust retention time across spectra.
#'
#' This function adjusts the retention time in each LC/MS profile to achieve better between-profile agreement.
#'
#' @param extracted_features A list object. Each component is a matrix which is the output from compute_clusters
#' @param mz_tol_relative The m/z tolerance level for peak alignment. This value is expressed as the
#'  percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.
#' @param rt_tol_relative The retention time tolerance level for peak alignment.
#' @param colors The vector of colors to be used for the line plots of time adjustments. The default is NA,
#'  in which case the program uses a set of default color set.
#' @param do.plot Indicates whether plot should be drawn.
#' @return A list object with the exact same structure as the input object features, i.e. one matrix per profile
#'  being processed. The only difference this output object has with the input object is that the retention time
#'  column in each of the matrices is changed to new adjusted values.
#' @export
adjust.time <- function(extracted_features,
                        colors = NA,
                        do.plot = TRUE) {
  number_of_samples <- length(extracted_features)

  if (number_of_samples <= 1) {
    message("Only one sample. No need to correct for time.")
  }

  if (do.plot) {
    par(mfrow = c(2, 2))
    draw_plot(label = "Retention time \n adjustment", cex = 2)
  }

  template_features <- compute_template(extracted_features)

  corrected_features <- foreach::foreach(features = extracted_features) %do% correct_time(
    features,
    template_features
  )

  if (do.plot) {
    draw_rt_correction_plot(
      colors,
      extracted_features,
      corrected_features,
    )
  }

  if (exists("corrected_features")) {
    return(corrected_features)
  }
}
