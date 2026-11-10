# Resolve the fixture holding an expectation that was captured from summary().
#
# summary.data.frame() returns a character matrix of *formatted* text (class
# "table"), so such a fixture is tied to the printing rules of the R version that
# produced it, not only to the data. R 4.6.0 changed two of those rules, both
# visible in the R 4.6 CI leg:
#
#   * summary() for character vectors is more informative and optionally resorts to
#     the factor method (R 4.6.0 NEWS), so a character column prints
#       Length / N.unique / N.blank / Min.nchar / Max.nchar
#     where R 4.3 printed
#       Length / Class / Mode
#     This adds two rows per character column and widens the right aligned column
#     headers of the whole matrix, so every column name shifts.
#   * numeric summaries are printed with more digits: "Min.   : 0.007709" instead
#     of "Min.   : 0.00771".
#
# Neither change alters the underlying data - row counts, minima and maxima all
# agree - so these fixtures are re-captured per R major.minor rather than treated
# as a difference in the analysis.
#
# Fixtures captured on R >= 4.6 live next to the original file with an "_R46"
# suffix; the originals are left untouched so that R < 4.6 keeps using them. A
# future formatting change needs another suffix rather than an edit of these files.
#
# Caveat for the two fixtures derived from Thermo .raw input (filtered/*.rds): they were
# captured on the R >= 4.6 stack, where rawrr is newer as well. remove_noise() on
# 8_qc_no_dil_milliq.raw reports 615603 rows there against 614091 in the older fixture, so
# those files differ in data as well as in formatting. That difference is not something
# summary() introduced and not run-to-run noise either: it reproduced exactly (615603,
# byte identical fixtures) across two independent runs on that stack, and the row count
# originates in the peaks the rawrr reader returns (R/load.lcms.R:78-89 keeps only
# intensity > 0), so it follows the reader build - see helper-reader.R for the same effect
# on load.lcms.raw(). Do not read these two fixtures as a pure formatting change.
#
# summary_fixture_path() gives the fixture belonging to the R version running the
# test, so uncommenting a saveRDS() line recaptures that version's fixture and never
# overwrites the one belonging to another.
summary_fixture_path <- function(path, r_version = getRversion()) {
  if (package_version(as.character(r_version)) < package_version("4.6.0")) {
    return(path)
  }
  sub("\\.([Rr])ds$", "_R46.\\1ds", path)
}

read_expected_summary <- function(path, r_version = getRversion()) {
  fixture <- summary_fixture_path(path, r_version)
  if (!file.exists(fixture)) {
    stop(sprintf(paste("no summary fixture for R >= 4.6 at '%s'.",
                       "Recapture it on R >= 4.6 with the saveRDS() line of the test."),
                 fixture), call. = FALSE)
  }
  readRDS(fixture)
}
