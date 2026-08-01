maUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About MA Plot"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Displays log fold change vs mean expression to assess differential expression patterns."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("DE results with log2FoldChange and padj"),
        tags$li("Optional: baseMean column"),
        tags$li("Optional uploaded CSV")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Run DE Analysis or upload DE results"),
        tags$li("Set thresholds (padj and log2FC)"),
        tags$li("Click 'Generate MA Plot'"),
        tags$li("Interpret up/down regulated genes")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "250px"),
      
      hr()
    ),
    
    h3("MA Plot"),
    
    fileInput(ns("file"), "Upload DE Results (TXT/CSV)"),
    
    numericInput(ns("padj"), "Adjusted p-value cutoff", 0.05),
    numericInput(ns("logfc"), "Log2FC cutoff", 1),
    
    checkboxInput(ns("highlight"), "Highlight significant genes", TRUE),
    
    actionButton(ns("run"), "Generate MA Plot"),
    
    plotOutput(ns("maPlot"), height = "600px"),
    
    downloadButton(ns("download_ma"), "Download Plot")
  )
}
maServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        gene = paste0("Gene", LETTERS[1:20]),
        baseMean = runif(20, 50, 1000),
        log2FoldChange = rnorm(20, 0, 2),
        padj = runif(20, 0.0001, 0.1)
      )
    })
    
    output$sample_table <- renderTable({
      head(sample_data())
    })
    
    # =========================
    # DEMO PLOT (ONLY SAMPLE HERE)
    # =========================
    output$demoPlot <- renderPlot({
      df <- sample_data()
      
      plot(
        log10(df$baseMean + 1),
        df$log2FoldChange,
        pch = 19,
        col = "blue",
        xlab = "Log10 Mean Expression",
        ylab = "Log2 Fold Change",
        main = "Sample MA Plot"
      )
    })
    
    # =========================
    # REAL DATA (NO SAMPLE FALLBACK)
    # =========================
    ma_data <- reactive({
      
      # USER FILE
      if (!is.null(input$file) && input$file$datapath != "") {
        
        df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
        
        if (!all(c("log2FoldChange", "padj") %in% colnames(df))) {
          showNotification("File must contain log2FoldChange and padj", type = "error")
          return(NULL)
        }
        
        if (!"gene" %in% colnames(df)) {
          df$gene <- paste0("Gene_", seq_len(nrow(df)))
        }
        
        if (!"baseMean" %in% names(df)) {
          showNotification(
            "The uploaded file does not contain a 'baseMean' column. An MA plot cannot be generated.",
            type = "error"
          )
          return(NULL)
        }
        
        return(df)
      }
      
      # DE DATA
      if (!is.null(analysis$status$de_done) && analysis$status$de_done) {
        
        df <- as.data.frame(analysis$de)
        df$gene <- rownames(df)
        
        df <- df[!is.na(df$padj) & !is.na(df$log2FoldChange), ]
        
        return(df)
      }
      
      return(NULL)   #  NO sample_data here
    })
    
    # =========================
    # MAIN PLOT
    # =========================
    output$maPlot <- renderPlot({
      withProgress(
        
        message = "Generating MA Plot",
        
        value = 0,
        
        {
      
      # BEFORE RUN
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Upload data and click 'Generate MA Plot'")
        return()
      }
      
      
      # NO DATA
      if (is.null(ma_data())) {
        plot.new()
        text(0.5, 0.5, "No data available")
        return()
      }
      
      df <- ma_data()
      incProgress(
        0.4,
        detail = "Loading data"
      )
      
      baseMean <- df$baseMean
      logFC <- df$log2FoldChange
      
      sig <- !is.na(df$padj) &
        !is.na(logFC) &
        df$padj < input$padj &
        abs(logFC) > input$logfc
      up <- sum(
        sig &
          logFC > input$logfc,
        na.rm = TRUE
      )
      
      down <- sum(
        sig &
          logFC < -input$logfc,
        na.rm = TRUE
      )
      
      incProgress(
        0.8,
        detail = "Drawing plot"
      )
      meanExp <- log10(baseMean + 1)
      
      
      plot(
        meanExp,
        logFC,
        pch = 19,
        col = if (input$highlight) {
          ifelse(sig, "red", "grey")
        } else {
          "blue"
        },
        xlab = expression(log[10](baseMean + 1)),
        ylab = "Log2 Fold Change",
        main = paste0(
          "MA Plot (Genes: ",
          nrow(df),
          " | Significant: ",
          sum(sig, na.rm = TRUE),
          ")"
        )
      )
      
      abline(
        h = 0,
        col = "black",
        lwd = 2
      )
      
      abline(
        h = c(-input$logfc, input$logfc),
        col = "blue",
        lty = 2
      )
      legend(
        
        "topright",
        
        legend = c(
          
          paste("Up:", up),
          
          paste("Down:", down)
          
        ),
        
        bty = "n"
        
      )
      
      incProgress(1)
        })
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download_ma <- downloadHandler(
      filename = function() {
        "MA_plot.png"
      },
      content = function(file) {
        
        df <- ma_data()
        required <- c(
          "baseMean",
          "log2FoldChange",
          "padj"
        )
        
        missing <- setdiff(required, names(df))
        
        if (length(missing) > 0) {
          
          plot.new()
          
          text(
            0.5,
            0.5,
            paste(
              "Missing:",
              paste(missing, collapse = ", ")
            )
          )
          
          return()
          
        }
        
        png(file, width = 800, height = 600)
        
        plot(
          log10(df$baseMean + 1),
          df$log2FoldChange,
          pch = 19,
          main = "MA Plot"
        )
        
        dev.off()
      }
    )
    
  })
}
