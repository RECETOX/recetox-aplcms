# Debug script for two.step.hybrid function
# Run this to test the function with sample data

# Make VS Code breakpoints work:
# - keep srcref so line breakpoints can bind
# - load package from workspace sources (not installed library)
options(keep.source = TRUE, keep.source.pkgs = TRUE)
Sys.setenv(R_KEEP_PKG_SOURCE = "yes")
print(getwd())

# Define test files
files <- c(
  "RCX_06_shortened.mzML",
  "RCX_07_shortened.mzML",
  "RCX_08_shortened.mzML",
  "RCX_06_shortened_copy.mzML"
)

# Set up paths
test_path <- file.path(".", "tests", "testdata")
test_files <- sapply(files, function(x) {
  file.path(test_path, "input", x)
})

# Load metadata
metadata <- read.table(
  file.path(test_path, "two_step_hybrid_info.csv"),
  sep = ",",
  header = TRUE,
  strip.white = TRUE
)

# Optional: load known table if needed
known_table_path <- file.path(test_path, "hybrid", "known_table.parquet")
known_table <- if (file.exists(known_table_path)) {
  known_table_path
} else {
  NA
}

# Set cluster size (adjust for your machine)
num_workers <- min(parallel::detectCores() - 1, 3)

# Run the function
message("Starting two.step.hybrid with ", length(test_files), " files")
message("Using ", num_workers, " workers")

result <- two.step.hybrid(
  filenames = test_files,
  metadata = metadata,
  work_dir = test_path,
  known.table = known_table,
  cluster = 1
  # do.plot = TRUE      # not working for now. Added 'draw_plot' to registered function names, but it seems that       
)

message("Completed successfully!")
message("Final features: ", nrow(result$final_features))

# # Optionally, compare with expected results
# expected_path <- file.path(test_path, "final_ftrs.Rda")
# if (file.exists(expected_path)) {
#   expected_final_features <- readRDS(expected_path)
#   message("Expected features loaded for comparison")
#   # Add your comparison logic here
# }

# Return result for inspection
result