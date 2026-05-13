save_parquet <- function(df, metadata, path) {
    t <- Table$create(df)
    s <- schema(t)$WithMetadata(metadata)
    n <- t$cast(s)
    arrow::write_parquet(n, path)
}