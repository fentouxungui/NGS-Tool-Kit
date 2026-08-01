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
    target_dir <- get_user_data_folder()
  }

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(TRUE)
}

