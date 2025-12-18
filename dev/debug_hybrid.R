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
  "RCX_08_shortened.mzML"
)

# Set up paths
test_path <- file.path(".", "tests", "testdata")
test_files <- sapply(files, function(x) {
  file.path(test_path, "input", x)
})

# Load known table 
known_table_path <- file.path(test_path, "hybrid", "known_table.parquet")
known_table <- if (file.exists(known_table_path)) {
  known_table_path
} else {
  stop("Known_table file cannot be found!")
}

# Set cluster size (adjust for your machine)
num_workers <- min(parallel::detectCores() - 1, 3)

# Run the function
message("Starting two.step.hybrid with ", length(test_files), " files")
message("Using ", num_workers, " workers")

result <- hybrid(
  test_files,
  known_table,
  mz_tol_relative = NA,
  rt_tol_relative = NA,
  cluster = num_workers
)

message("Completed successfully!")
message("Final features: ", as_tibble(result$recovered_feature_sample_table))

# # Optionally, compare with expected results
# expected_path <- file.path(test_path, "final_ftrs.Rda")
# if (file.exists(expected_path)) {
#   expected_final_features <- readRDS(expected_path)
#   message("Expected features loaded for comparison")
#   # Add your comparison logic here
# }

# Return result for inspection
result