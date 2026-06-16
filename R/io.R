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