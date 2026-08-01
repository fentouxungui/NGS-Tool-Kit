validate_data <- function(df) {
  if (!is.data.frame(df)) stop("Input is not a data frame")
  if (ncol(df) < 2) stop("Need at least 2 columns")
  if (nrow(df) < 2) stop("Need at least 2 rows")
  TRUE
}
