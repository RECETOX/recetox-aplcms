write_tibble <- function(tibble, path) {
  attrs <- attributes(tibble)
  attrs <- attrs[names(attrs) %notin% c("class", "row.names", "names")]
  metadata <- lapply(attrs, paste0, character(1), collapse = ",")
  write_arrow(tibble, path, metadata)
}

#' Function to save table with metadata to parquet file
#'
write_arrow <- function(data_frame, path, metadata = NA) {
  tabular <- arrow::Table$create(data_frame)

  if (!any(is.na(metadata))) {
    table_schema <- arrow::schema(tabular)$WithMetadata(metadata)
    tabular <- tabular$cast(table_schema)
  }

  arrow::write_parquet(tabular, path)
}

read_arrow <- function(filepath) {
  arrow::read_parquet(filepath, as_data_frame = FALSE)
}

read_run_id <- function(filepath) {
  if(is_mzml(filepath)) {
    doc <- xml2::read_xml(filepath)
    ns <- xml2::xml_ns(doc)
    run_node <- xml2::xml_find_first(doc, ".//d1:run", ns)
    return(xml2::xml_attr(run_node, "id"))
  } else if (tools::file_ext(filepath) == 'raw'){
    return(rawrr::readFileHeader(filepath)$'Sample id')
  } else {
    return(NA)
  }
}

#' @export
read_parquet_files <- function(filename, folder, pattern) {
  testdata <- file.path("..", "testdata")

  input <- lapply(filename, function(x) {
    tibble::as_tibble(arrow::read_parquet(file.path(testdata, folder, paste0(x, pattern))))
  })

  return(input)
}

#' @export
load_aligned_features <- function(metadata_file, intensities_file, rt_file) {
  metadata <- arrow::read_parquet(metadata_file)
  intensities <- arrow::read_parquet(intensities_file)
  rt <- arrow::read_parquet(rt_file)

  result <- list()
  result$metadata <- as_tibble(metadata)
  result$intensity <- as_tibble(intensities)
  result$rt <- as_tibble(rt)
  return(result)
}