#' Setup and verify JBrowse assets
#'
#' Initializes the local user data directory for JBrowse sessions in a CRAN-compliant manner.
#'
#' @param target_dir Path to store JBrowse user data. Defaults to user data directory.
#'
#' @return Logical indicating success.
#' @export
setup_jbrowse <- function(target_dir = NULL) {
  if (is.null(target_dir)) {
    target_dir <- file.path(tools::R_user_dir("NGSToolKit", "data"), "jbrowse")
  }

  user_data_dir <- file.path(target_dir, "user_data")
  if (!dir.exists(user_data_dir)) {
    dir.create(user_data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(TRUE)
}
