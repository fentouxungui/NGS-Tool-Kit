pcaUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About PCA Analysis"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Performs Principal Component Analysis (PCA) to visualize variation between samples."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Normalized expression data (VST)"),
        tags$li("Optional uploaded expression matrix")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload dataset OR run DE Analysis"),
        tags$li("Enable metadata coloring (optional)"),
        tags$li("Click 'Run PCA'"),
        tags$li("Interpret clustering patterns")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "250px"),
      
      hr()
    ),
    
    h3("PCA Analysis"),
    
    fileInput(ns("file"), "Upload Expression Matrix (CSV)"),
    
    checkboxInput(ns("color_meta"), "Color by metadata (if available)", TRUE),
    
    actionButton(ns("run"), "Run PCA"),
    
    withSpinner(
      plotOutput(ns("pcaPlot"), height = "600px")
    ),
    
    downloadButton(ns("download_pca"), "Download PCA Data")
  )
}
pcaServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    # =========================
    # SAMPLE DATA (ONLY FOR DEMO)
    # =========================
    sample_data <- reactive({
      
      data.frame(
        
        GeneID = c(
          "Gene0001",
          "Gene0002",
          "Gene0003",
          "Gene0004",
          "Gene0005"
        ),
        
        Sample1 = c(231,140,183,205,141),
        Sample2 = c(228,143,181,231,166),
        Sample3 = c(251,124,177,214,146),
        Sample4 = c(236,141,179,204,166),
        Sample5 = c(235,130,203,219,142),
        
        Sample6 = c(382,321,333,335,313),
        Sample7 = c(354,264,354,350,284),
        Sample8 = c(372,314,298,384,317),
        Sample9 = c(356,312,341,355,338),
        Sample10 = c(382,267,334,399,328)
        
      )
      
    })
    
    output$sample_table <- renderTable({
      sample_data()
    })
    
    # =========================
    # DEMO PCA (SAFE)
    # =========================
    output$demoPlot <- renderPlot({
      
      set.seed(123)
      
      pc1 <- c(
        rnorm(5,-5,0.5),
        rnorm(5,5,0.5),
        rnorm(5,0,0.5)
      )
      
      pc2 <- c(
        rnorm(5,0,0.5),
        rnorm(5,0,0.5),
        rnorm(5,6,0.5)
      )
      
      cols <- c(
        rep("red",5),
        rep("blue",5),
        rep("darkgreen",5)
      )
      
      plot(
        pc1,
        pc2,
        col=cols,
        pch=19,
        cex=2,
        xlab="PC1 (32.4%)",
        ylab="PC2 (31.8%)",
        main="Example PCA Plot"
      )
      
      legend(
        "topright",
        legend=c(
          "Control",
          "Treatment A",
          "Treatment B"
        ),
        col=c(
          "red",
          "blue",
          "darkgreen"
        ),
        pch=19
      )
      
    })
    
    # =========================
    # DATA SOURCE (SAFE, NO FALLBACK)
    # =========================
    pca_data <- reactive({
      
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
        df[] <- lapply(df, function(x)
          suppressWarnings(as.numeric(as.character(x)))
        )
        
        if(any(is.na(df))){
          stop("Expression matrix contains non-numeric values.")
        }
        
        df <- as.matrix(df)
        
        return(df)
      }
      
      # DE DATA
      if (!is.null(analysis$status) &&
          !is.null(analysis$status$normalized) &&
          isTRUE(analysis$status$normalized) &&
          !is.null(analysis$vst)) {
        
        return(SummarizedExperiment::assay(analysis$vst))
      }
      
      return(NULL)
    })
    
    # =========================
    # PCA COMPUTATION
    # =========================
    pca_res <- eventReactive(input$run, {
      
      df <- pca_data()
      if (is.null(df)) return(NULL)
      
      tryCatch({
        
        #----------------------------------------
        # RNA-Seq Count Matrix
        # Rows = Genes
        # Columns = Samples
        #----------------------------------------
        
        X <- t(df)
        
        if (nrow(X) < 2)
          return(NULL)
        
        pca <- prcomp(
          X,
          center = TRUE,
          scale. = TRUE
        )
        
        pca
        
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # =========================
    # MAIN PCA PLOT
    # =========================
    output$pcaPlot <- renderPlot({
      
      # BEFORE RUN
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Upload data and click 'Run PCA'")
        return()
      }
      
      pca <- pca_res()
      withProgress(
        
        message = "Running PCA",
        
        value = 0,
        
        {
          
          incProgress(
            0.3,
            detail = "Computing principal components"
          )
      
      # NO DATA
      if (is.null(pca)) {
        
        showNotification(
          "Please upload an expression matrix or complete DE Analysis first.",
          type = "warning"
        )
        
        plot.new()
        
        text(
          0.5,
          0.5,
          "No expression matrix available"
        )
        
        return()
        
      }
      
      coords <- as.data.frame(pca$x[,1:2])
      
      coords$Sample <- rownames(coords)
      
      var_exp <- round(
        100 * pca$sdev^2 /
          sum(pca$sdev^2),
        1
      )
      
      # SAFE metadata check
      use_meta <- FALSE
      
      if (!is.null(input$color_meta) && isTRUE(input$color_meta)) {
        if (!is.null(analysis$status) &&
            !is.null(analysis$status$has_meta) &&
            isTRUE(analysis$status$has_meta) &&
            !is.null(analysis$meta) &&
            "condition" %in% colnames(analysis$meta)) {
          
          use_meta <- TRUE
        }
      }
      incProgress(
        0.7,
        detail = "Generating plot"
      )
      if (use_meta) {
        
        groups <- as.factor(analysis$meta$condition)
        cols <- as.numeric(groups)
        
        plot(
          coords$PC1,
          coords$PC2,
          col = cols,
          pch = 19,
          cex = 1.5,
          main = "PCA (Colored by Condition)",
          xlab = paste0("PC1 (", var_exp[1], "%)"),
          ylab = paste0("PC2 (", var_exp[2], "%)")
        )
        
        text(
          coords$PC1,
          coords$PC2,
          labels = coords$Sample,
          pos = 3,
          cex = 0.7
        )
        
        legend("topright",
               legend = levels(groups),
               col = 1:length(levels(groups)),
               pch = 19)
        
      } else {
        
        plot(
          coords$PC1,
          coords$PC2,
          col = "blue",
          pch = 19,
          cex = 1.5,
          main = "Principal Component Analysis",
          xlab = paste0("PC1 (", var_exp[1], "%)"),
          ylab = paste0("PC2 (", var_exp[2], "%)")
        )
        
        text(
          coords$PC1,
          coords$PC2,
          labels = coords$Sample,
          pos = 3,
          cex = 0.7
        )
      }
      incProgress(
        1,
        detail = "Completed"
      )
      
      showNotification(
        "PCA completed successfully.",
        type = "message"
      )
        })
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download_pca <- downloadHandler(
      filename = function() "pca_coordinates.csv",
      content = function(file) {
        pca <- pca_res()
        if (!is.null(pca)) {
          write.csv(as.data.frame(pca$x), file)
        }
      }
    )
    
  })
}
