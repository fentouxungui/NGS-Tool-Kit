#' NGSToolKit Shiny Server
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @noRd
app_server <- function(input, output, session) {
  
  # Global reactive state for analysis data
  analysis <- reactiveValues(
    raw_counts = NULL,
    metadata = NULL,
    deseq_obj = NULL,
    res = NULL,
    normalized_counts = NULL,
    vsd = NULL,
    is_processed = FALSE
  )

  # Default sample data path in package
  sample_counts_file <- system.file("sample_data", "sample_counts.csv.gz", package = "NGSToolKit")
  sample_meta_file <- system.file("sample_data", "sample_metadata.csv", package = "NGSToolKit")

  # Auto-load sample data if available
  if (file.exists(sample_counts_file) && file.exists(sample_meta_file)) {
    tryCatch({
      analysis$raw_counts <- read.csv(gzfile(sample_counts_file), row.names = 1, check.names = FALSE)
      analysis$metadata <- read.csv(sample_meta_file, row.names = 1, check.names = FALSE)
    }, error = function(e) NULL)
  }

  show_notification <- function(title, msg, type = "success") {
    tryCatch({
      showNotification(
        ui = tagList(
          icon(if(type == "success") "check-circle" else "exclamation-circle"),
          strong(title), br(), msg
        ),
        type = type,
        duration = 3
      )
    }, error = function(e) NULL)
  }

  # Modules server invocation
  qcServer("qc1", analysis)
  deServer("de1", analysis)
  maServer("ma1", analysis)
  pcaServer("pca1", analysis)
  heatmapServer("heat1", analysis)
  corrServer("corr1", analysis)
  volcanoServer("vol1", analysis)
  tsneServer("tsne1", analysis)
  umapServer("umap1", analysis)
  geneServer("gene1", analysis)
  topgenesServer("tg1", analysis)
  clusterServer("cl1", analysis)
  jbrowseServer("jb1", analysis)

  # Welcome notification
  observeEvent(TRUE, {
    show_notification(
      "Welcome to NGS Tool Kit",
      "Upload your RNA-Seq data or explore built-in samples to begin analysis",
      "success"
    )
  }, once = TRUE)
}
