#' Aggregate the intensities and select median mz for features with identical rt.
#'
#' @description This functions computes median mz and sum of intensities over features with same rt.
#' @param features dataframe of retention time, m/z ratio, signal strength.
#' @return returns a tibble with the following columns
#' \itemize{
#'   \item mz - m/z ratio
#'   \item rt - retention time
#'   \item intensity - signal strength
#' }
#' @export
aggregate_by_rt <- function(features) {
    features |>
     dplyr::group_by(rt) |>
     dplyr::summarize(mass = median(mz[which.max(intensity)]), area = sum(intensity)) |>
     dplyr::rename(mz = mass, intensity = area)
}
