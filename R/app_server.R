app_server <- function(input, output, session) {
  
  analysis <- reactiveValues(
    
    counts = NULL,
    meta = NULL,
    dds = NULL,
    vst = NULL,
    de = NULL,
    
    jbrowse = list(
      session = NULL,
      fasta = NULL,
      fai = NULL,
      gff = NULL,
      tbi = NULL,
      config = NULL,
      output = NULL,
      browser = NULL
    ),
    
    status = list(
      has_counts = FALSE,
      has_meta = FALSE,
      de_done = FALSE,
      normalized = FALSE
    )
  )
  
  # =========================
  # SAFE NOTIFICATION
  # =========================
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
  
  # =========================
  # MODULES
  # =========================
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
  
  
  # =========================
  # VISITOR COUNTER
  # =========================
  visitor_count <- get_and_increment_visitor_count()
  
  output$visitor_counter_card <- renderUI({
    div(class = "visitor-counter-card",
        div(class = "visitor-count-number", format(visitor_count, big.mark = ",")),
        div(class = "visitor-count-label", icon("users"), " Total Visitors")
    )
  })
  
  # =========================
  # FIXED WELCOME MESSAGE
  # =========================
  observeEvent(TRUE, {
    show_notification(
      "Welcome to NGS Tool Kit",
      "Upload your RNA-Seq data to begin analysis",
      "success"
    )
  }, once = TRUE)
}

