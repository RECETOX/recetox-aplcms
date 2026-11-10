# Expectations that depend on the version of a file reader library.
#
# The number of points `load.lcms.raw()` reports for a `.raw` file is a property of
# the Thermo reader behind rawrr, not of this package: the raw path applies a single
# filter to whatever the reader returns (`intensity > 0` in `process_chunk()`, see
# `R/load.lcms.R`), with no deduplication and no mslevel selection. Every point the
# native reader yields with a positive intensity therefore ends up in the table, and
# a new reader build that thresholds or centroids differently changes the count.
#
# Counts recorded from CI run 40897427126 for `rawrr::sampleFilePath()`:
#
#   rawrr  1.8.1  mono + rawrr.1.7.10.exe      -> 30674   R job, r-version 4.3
#   rawrr 1.10.2  mono + rawrr.1.9.2.exe       -> 30689   R Conda job, r-base 4.3.3
#   rawrr 1.20.0  self-contained .NET reader   -> 30689   R job, r-version 4.6
#
# Note that the conda job ran the same `r-base` as the passing 4.3 job and still saw
# 30689: the count follows the reader, not R. rawrr downloads the native binary
# matching its own version, so the R package version identifies the reader build.
#
# Versions between 1.8.1 and 1.10.2 were never observed here. They are reported as
# unknown rather than guessed at, and the calling test skips with the count it saw so
# that it can be added to the table above.
expected_raw_peak_count <- function(rawrr_version = packageVersion("rawrr")) {
  ver <- package_version(as.character(rawrr_version))
  if (ver <= package_version("1.8.1")) {
    return(30674L)
  }
  if (ver >= package_version("1.10.2")) {
    return(30689L)
  }
  NULL
}
