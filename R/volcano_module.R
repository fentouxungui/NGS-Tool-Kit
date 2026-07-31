volcanoUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Volcano Plot"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Displays differential expression results by plotting fold change against statistical significance."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("DESeq2 results (log2FoldChange & padj)"),
        tags$li("Optional uploaded dataset")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Run DE Analysis"),
        tags$li("Adjust thresholds"),
        tags$li("Click 'Generate Plot'"),
        tags$li("Interpret significant genes")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "250px"),
      
      hr()
    ),
    
    h3("Volcano Plot"),
    fileInput(
      ns("file"),
      "Upload DE Results (CSV, optional)",
      accept = c(".csv")
    ),
    
    numericInput(ns("padj"), "Adjusted p-value cutoff", 0.05),
    numericInput(ns("logfc"), "Log2 Fold Change cutoff", 1),
    numericInput(ns("top_n"), "Label top genes", 10, min = 0, max = 50),
    
    actionButton(ns("run"), "Generate Plot"),
    
    plotOutput(ns("volPlot"), height = "600px"),
    
    downloadButton(ns("download_plot"), "Download Plot"),
    downloadButton(ns("download_data"), "Download Data")
  )
}
volcanoServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        gene = paste0("Gene", LETTERS[1:20]),
        log2FoldChange = rnorm(20, 0, 2),
        padj = runif(20, 0.0001, 0.1)
      )
    })
    
    output$sample_table <- renderTable({
      sample_data()
    })
    
    # =========================
    # DEMO ONLY
    # =========================
    output$demoPlot <- renderPlot({
      df <- sample_data()
      incProgress(
        0.8,
        detail = "Drawing volcano plot"
      )
      plot(
        df$log2FoldChange,
        -log10(df$padj),
        col = "darkgreen",
        pch = 19,
        main = "Sample Volcano Plot"
      )
    })
    
    # =========================
    # REAL DATA (NO FALLBACK)
    # =========================
    vol_data <- eventReactive(input$run, {
      
      # Uploaded file has priority
      if (!is.null(input$file) && nzchar(input$file$datapath)) {
        
        df <- tryCatch(
          read.csv(input$file$datapath, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        
        if (is.null(df))
          return(NULL)
        
        required <- c("log2FoldChange", "padj")
        
        if (!all(required %in% names(df))) {
          
          showNotification(
            "Uploaded file must contain log2FoldChange and padj.",
            type = "error"
          )
          
          return(NULL)
        }
        
        if (!"gene" %in% names(df)) {
          df$gene <- paste0("Gene_", seq_len(nrow(df)))
        }
        
      } else if (!is.null(analysis$status) &&
                 isTRUE(analysis$status$de_done) &&
                 !is.null(analysis$de)) {
        
        df <- as.data.frame(analysis$de)
        df$gene <- rownames(df)
        
      } else {
        
        return(NULL)
        
      }
      
      df <- df[
        !is.na(df$log2FoldChange) &
          !is.na(df$padj),
      ]
      
      if (nrow(df) < 5)
        return(NULL)
      
      df
    })
    
    # =========================
    # MAIN PLOT
    # =========================
    output$volPlot <- renderPlot({
      
      withProgress(
        
        message = "Generating Volcano Plot",
        
        value = 0,
        
        {
      
      # BEFORE RUN
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Run DE Analysis and click 'Generate Plot'")
        incProgress(1)
        return()
      }
      
      df <- vol_data()
      incProgress(
        0.4,
        detail = "Loading differential expression results"
      )
      
      # NO DATA
      if (is.null(df)) {
        plot.new()
        text(0.5, 0.5, "No data available")
        return()
      }
      
      logFC <- df$log2FoldChange
      pval <- pmax(df$padj, 1e-300)
      
      fc_thresh <- input$logfc
      p_thresh  <- input$padj
      
      sig <-
        !is.na(logFC) &
        !is.na(pval) &
        abs(logFC) >= fc_thresh &
        pval <= p_thresh
      up <- sum(
        sig &
          logFC > fc_thresh,
        na.rm = TRUE
      )
      
      down <- sum(
        sig &
          logFC < -fc_thresh,
        na.rm = TRUE
      )
      
      plot(
        logFC,
        -log10(pval),
        pch = 19,
        col = ifelse(
          sig,
          "#D55E00",
          "grey75"
        ),
        xlab = "Log2 Fold Change",
        ylab = "-log10(padj)",
        main = "Volcano Plot"
      )
      
      abline(v = c(-fc_thresh, fc_thresh), col = "blue", lty = 2)
      abline(h = -log10(p_thresh), col = "blue", lty = 2)
      legend(
        
        "topright",
        
        legend = c(
          
          paste("Total:", nrow(df)),
          
          paste("Significant:", sum(sig, na.rm = TRUE)),
          
          paste("Up:", up),
          
          paste("Down:", down)
          
        ),
        
        bty = "n"
        
      )
      
      # TOP GENES
      if (input$top_n > 0) {
        df$score <- -log10(df$padj) * abs(df$log2FoldChange)
        top_genes <- head(df[order(df$score, decreasing = TRUE), ], input$top_n)
        
        text(
          top_genes$log2FoldChange,
          -log10(top_genes$padj),
          labels = top_genes$gene,
          cex = 0.7,
          pos = 3
        )
      }
    })
    })
    # =========================
    # DOWNLOADS
    # =========================
    output$download_plot <- downloadHandler(
      filename = function() "volcano_plot.png",
      content = function(file) {
        df <- vol_data()
        if (is.null(df)) return()
        
        png(file)
        plot(df$log2FoldChange, -log10(df$padj), pch = 19)
        dev.off()
      }
    )
    
    output$download_data <- downloadHandler(
      filename = function() "volcano_data.csv",
      content = function(file) {
        df <- vol_data()
        if (!is.null(df)) write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
