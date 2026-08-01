heatmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Heatmap"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Visualizes gene expression patterns across samples."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Normalized expression data (VST)"),
        tags$li("Optional uploaded CSV")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload dataset OR run DE Analysis"),
        tags$li("Select number of top variable genes"),
        tags$li("Enable scaling"),
        tags$li("Click 'Generate Heatmap'")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "250px"),
      
      hr()
    ),
    
    h3("Heatmap"),
    
    fileInput(ns("file"), "Upload Expression Matrix (CSV)"),
    
    checkboxInput(ns("scale"), "Scale data (row-wise)", TRUE),
    
    numericInput(ns("top_n"), "Top variable genes", 50, min = 10, max = 500),
    
    checkboxInput(ns("annot"), "Show metadata annotation", TRUE),
    
    actionButton(ns("run"), "Generate Heatmap"),   #  added
    
    plotOutput(ns("heatmapPlot"), height = "600px"),
    
    downloadButton(ns("download_heatmap"), "Download Heatmap Data")
  )
}
heatmapServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
# library(pheatmap)
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        Sample = paste0("Sample", 1:20),
        GeneA = rnorm(20, 100, 50),
        GeneB = rnorm(20, 200, 60),
        GeneC = rnorm(20, 150, 40),
        GeneD = rnorm(20, 300, 70)
      )
    })
    
    output$sample_table <- renderTable({
      sample_data()
    })
    
    # =========================
    # DEMO HEATMAP
    # =========================
    output$demoPlot <- renderPlot({
      
      df <- sample_data()
      rownames(df) <- df$Sample
      df <- df[, -1]
      
      df <- t(df)
      df <- t(scale(t(df)))
      
      pheatmap(df, show_rownames = FALSE, main = "Sample Heatmap")
    })
    
    # =========================
    # REAL DATA (NO FALLBACK)
    # =========================
    heatmap_data <- reactive({
      
      # USER FILE
      if (!is.null(input$file) && nzchar(input$file$datapath)) {
        
        df <- tryCatch({
          read.csv(input$file$datapath, stringsAsFactors = FALSE)
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(df) || ncol(df) < 2) return(NULL)
        
        rownames(df) <- make.unique(as.character(df[[1]]))
        df <- df[, -1, drop = FALSE]
        
        df[] <- lapply(df, function(x) as.numeric(as.character(x)))
        df <- as.matrix(df)
        
        return(df)
      }
      
      # DE DATA (SAFE CHECK)
      if (!is.null(analysis$status) &&
          !is.null(analysis$status$normalized) &&
          isTRUE(analysis$status$normalized) &&
          !is.null(analysis$vst)) {
        
        return(SummarizedExperiment::assay(analysis$vst))
      }
      
      return(NULL)
    })
    
    # =========================
    # MAIN HEATMAP
    # =========================
    output$heatmapPlot <- renderPlot({
      
      # BEFORE RUN
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Upload data and click 'Generate Heatmap'")
        return()
      }
      
      df <- heatmap_data()
      
      # NO DATA
      if (is.null(df)) {
        plot.new()
        text(0.5, 0.5, "No data available")
        return()
      }
      
      tryCatch({
        
        if (ncol(df) > nrow(df)) {
          df <- t(df)
        }
        
        df <- as.matrix(df)
        mode(df) <- "numeric"
        df <- df[complete.cases(df), ]
        
        if (nrow(df) < 2 || ncol(df) < 2) {
          stop("Not enough data")
        }
        
        # TOP VAR GENES
        vars <- apply(df, 1, var)
        top <- order(vars, decreasing = TRUE)[1:min(input$top_n, length(vars))]
        df <- df[top, , drop = FALSE]
        
        # SCALE
        if (isTRUE(input$scale)) {
          df <- t(scale(t(df)))
        }
        
        # SAFE METADATA
        annotation <- NULL
        
        if (isTRUE(input$annot) &&
            !is.null(analysis$status) &&
            !is.null(analysis$status$has_meta) &&
            isTRUE(analysis$status$has_meta) &&
            !is.null(analysis$meta) &&
            "condition" %in% colnames(analysis$meta)) {
          
          meta <- analysis$meta
          meta <- meta[match(colnames(df), meta$sample), ]
          
          annotation <- data.frame(condition = meta$condition)
          rownames(annotation) <- meta$sample
        }
        
        pheatmap(
          df,
          annotation_col = annotation,
          show_rownames = FALSE,
          main = "Heatmap (Top Variable Genes)"
        )
        
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message))
      })
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download_heatmap <- downloadHandler(
      filename = function() "heatmap_data.csv",
      content = function(file) {
        df <- heatmap_data()
        if (!is.null(df)) write.csv(df, file)
      }
    )
    
  })
}
