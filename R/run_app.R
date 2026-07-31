#' Run the NGS Tool Kit Shiny Application
#'
#' Launches the interactive Shiny web application for RNA-Seq data analysis,
#' differential expression, clustering, and genome visualization.
#'
#' @param launch.browser Logical, whether to open the application in a web browser. Default is TRUE.
#' @param port Optional port number for running the Shiny server. Default is NULL.
#' @param host Optional host IP address. Default is "127.0.0.1".
#'
#' @return Launches the Shiny application.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_ngs_toolkit()
#' }
run_ngs_toolkit <- function(launch.browser = TRUE, port = NULL, host = "127.0.0.1") {
  old_opts <- options(shiny.maxRequestSize = 5 * 1024^3)
  on.exit(options(old_opts), add = TRUE)

  # Verify or initialize local JBrowse storage
  setup_jbrowse()

  app_ui_obj <- app_ui()
  app_server_obj <- app_server

  app <- shiny::shinyApp(ui = app_ui_obj, server = app_server_obj)

  shiny::runApp(
    app,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}

#' Run the NGS Tool Kit Application (Alias)
#'
#' @inheritParams run_ngs_toolkit
#' @return Launches the Shiny application.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(launch.browser = TRUE, port = NULL, host = "127.0.0.1") {
  run_ngs_toolkit(launch.browser = launch.browser, port = port, host = host)
}

