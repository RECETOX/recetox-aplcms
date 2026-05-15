#' Function to save table with metadata to parquet file
#'
write_arrow <- function(data_frame, path, metadata = NA) {
  tabular <- arrow::Table$create(data_frame)

  if (!is.na(metadata)) {
    table_schema <- arrow::schema(tabular)$WithMetadata(metadata)
    tabular <- tabular$cast(table_schema)
  }

  arrow::write_parquet(tabular, path)
}

read_arrow <- function(filepath) {
  arrow::read_parquet(filepath, as_data_frame = FALSE)
}