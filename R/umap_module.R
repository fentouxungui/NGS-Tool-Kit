umapUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About UMAP"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("UMAP reduces high-dimensional data to visualize sample similarity."),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload expression matrix OR use uploaded dataset"),
        tags$li("Ensure first column = gene names"),
        tags$li("Click 'Run UMAP'"),
        tags$li("Wait for result (few seconds)")
      ),
      
      h4("Sample Dataset"),
      verbatimTextOutput(ns("sample_text")),
      tableOutput(ns("sample_table")),
      
      
      
      
      hr()
    ),
    
    h3("UMAP Analysis"),
    
    fileInput(ns("file"), "Upload Expression Matrix"),
    numericInput(ns("neighbors"), "n_neighbors", 15),
    actionButton(ns("run"), "Run UMAP"),
    
    textOutput(ns("status")),
    
    plotOutput(ns("umapPlot"), height = "600px"),
    
    downloadButton(
      ns("download"),
      "Download UMAP Plot"
    )
  )
}
umapServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
# library(uwot)
    
    status_msg <- reactiveVal("Idle")
    
    output$status <- renderText({
      status_msg()
    })
    
    # ---------------- SAMPLE DATA ----------------
    sample_data <- reactive({
      data.frame(
        Gene = paste0("Gene", 1:20),
        Sample1 = rnorm(20, 100, 20),
        Sample2 = rnorm(20, 120, 25),
        Sample3 = rnorm(20, 200, 30),
        Sample4 = rnorm(20, 210, 35)
      )
    })
    
    output$sample_text <- renderText({
      paste(
        "Gene    Sample1 Sample2 Sample3 Sample4",
        "Gene1   100     120     200     210",
        "Gene2   50      60      20      25",
        "Gene3   300     320     400     420",
        sep = "\n"
      )
    })
    
    output$sample_table <- renderTable({
      head(sample_data())
    })
    
    # ---------------- DEMO ----------------
    output$demoPlot <- renderPlot({
      df <- sample_data()
      rownames(df) <- df$Gene
      df <- df[, -1]
      
      X <- scale(t(df))
      
      n_neighbors <- min(3, nrow(X) - 1)
      
      coords <- uwot::umap(X, n_neighbors = n_neighbors)
      
      plot(coords, col="darkgreen", pch=19, main="Sample UMAP")
    })
    
    # ---------------- DATA ----------------
    umap_data <- reactive({
      
      # User upload
      if (!is.null(input$file) &&
          nzchar(input$file$datapath)) {
        df <- tryCatch(
          
          read.csv(input$file$datapath),
          
          error = function(e){
            
            status_msg("Unable to read uploaded file.")
            
            return(NULL)
            
            
          }
          
        )
        
        if(is.null(df))
          return(NULL)
        if (all(c("log2FoldChange", "padj") %in% names(df))) {
          
          status_msg("Invalid: DE results uploaded. Please upload an expression matrix.")
          
          return(NULL)
          
        }
        
        rownames(df) <- df[[1]]
        df <- df[, -1]
        df[] <- lapply(
          df,
          function(x)
            suppressWarnings(as.numeric(x))
        )
        
        if (anyNA(df)) {
          
          status_msg("Non-numeric values detected.")
          
          return(NULL)
          
        }
        return(as.matrix(df))
      }
      
      # DESeq normalized
      if (!is.null(analysis$vst)) {
        return(assay(analysis$vst))
      }
      
      status_msg("No data available")
      return(NULL)
    })
    
    # ---------------- COMPUTE ----------------
    umap_res <- eventReactive(input$run, {
      
      withProgress(
        
        message = "Running UMAP",
        
        value = 0,
        
        {
      
      status_msg("Running UMAP...")
      
      df <- umap_data()
      
      incProgress(
        0.3,
        detail = "Loading data"
      )
      if (is.null(df))
        return(NULL)
      X <- if (ncol(df) > nrow(df)) t(df) else df
      
      # feature selection
      vars <- apply(
        X,
        2,
        var,
        na.rm = TRUE
      )
      
      vars[is.na(vars)] <- 0
      X <- X[, order(vars, decreasing = TRUE)[1:min(2000, ncol(X))]]
      
      X <- scale(X)
      incProgress(
        0.6,
        detail = "Preparing matrix"
      )
      X <- X[complete.cases(X), ]
      
      if (nrow(X) < 3) {
        status_msg("Too few samples")
        return(NULL)
      }
      
      n_neighbors <- min(input$neighbors, nrow(X) - 1)
      if (n_neighbors < 2) n_neighbors <- 2
      
      incProgress(
        0.9,
        detail = "Computing UMAP"
      )
      
      coords <- tryCatch(
        
        uwot::umap(
          
          X,
          
          n_neighbors = n_neighbors
          
        ),
        
        error = function(e){
          
          status_msg(e$message)
          
          return(NULL)
          
        }
        
      )
      
      if(is.null(coords))
        return(NULL)
      
      status_msg("Completed")
      incProgress(1)
      coords
    })
    })
    # ---------------- PLOT ----------------
    output$umapPlot <- renderPlot({
      
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Click Run UMAP")
        return()
      }
      
      coords <- umap_res()
      
      if (is.null(coords)) {
        plot.new()
        text(0.5, 0.5, "No result generated")
        return()
      }
      
      plot(
        coords[,1],
        coords[,2],
        pch = 19,
        col = "blue",
        main = "UMAP Visualization",
        xlab = "UMAP 1",
        ylab = "UMAP 2"
      )
    })
    output$download <- downloadHandler(
      
      filename = function() {
        "umap_plot.png"
      },
      
      content = function(file) {
        
        coords <- umap_res()
        
        if (is.null(coords))
          return()
        
        png(file,
            width = 1000,
            height = 700,
            res = 150)
        
        plot(
          coords[,1],
          coords[,2],
          pch = 19,
          col = "blue",
          main = "UMAP Visualization",
          xlab = "UMAP 1",
          ylab = "UMAP 2"
        )
        
        dev.off()
        
      }
      
    )
    
  })
}

