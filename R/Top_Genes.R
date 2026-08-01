# library(shiny)
# library(DT)

# =========================
# UI
# =========================
topgenesUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Top Genes"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Displays the most significant differentially expressed genes based on fold change and adjusted p-value."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("DESeq2 results OR uploaded CSV file"),
        tags$li("Columns required: log2FoldChange and padj")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Run DE Analysis OR upload file"),
        tags$li("Set thresholds"),
        tags$li("Click 'Show Top Genes'")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      tableOutput(ns("demoTable")),   #  FIXED
      
      hr()
    ),
    
    h3("Top Differentially Expressed Genes"),
    
    fileInput(ns("file"), "Upload DE Results (CSV, optional)"),
    
    numericInput(ns("padj"), "Adjusted p-value cutoff", 0.05),
    numericInput(ns("logfc"), "Log2FC cutoff", 1),
    selectInput(
      
      ns("top_n"),
      
      "Number of genes",
      
      choices = c(
        10,
        20,
        50,
        100,
        200
      ),
      
      selected = 20
      
    ),
    
    actionButton(ns("run"), "Show Top Genes"),
    
    h4("Results"),
    DTOutput(ns("table")),
    
    downloadButton(ns("download"), "Download Results")
  )
}

# =========================
# SERVER
# =========================
topgenesServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
# library(DT)
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        gene = paste0("Gene", LETTERS[1:30]),
        log2FoldChange = rnorm(30, 0, 2),
        padj = runif(30, 0.0001, 0.1),
        baseMean = runif(30, 50, 1000)
      )
    })
    
    output$sample_table <- renderTable({
      head(sample_data())
    })
    
    # =========================
    # SAMPLE OUTPUT (CORRECT)
    # =========================
    output$demoTable <- renderTable({
      
      df <- sample_data()
      df <- df[order(df$padj), ]
      
      head(df[, c("gene","log2FoldChange","padj","baseMean")], 5)
    })
    
    # =========================
    # DATA SOURCE
    # =========================
    get_data <- reactive({
      
      # DE RESULTS
      if (!is.null(analysis$de)) {
        df <- as.data.frame(analysis$de)
        df$gene <- rownames(df)
        return(df)
      }
      
      # USER FILE
      if (!is.null(input$file) && nzchar(input$file$datapath)) {
        
        df <- tryCatch({
          read.csv(input$file$datapath)
        }, error = function(e) return(NULL))
        
        if (!all(c("log2FoldChange","padj") %in% colnames(df))) {
          showNotification("Invalid file format", type = "error")
          return(NULL)
        }
        
        if (!"gene" %in% colnames(df)) {
          df$gene <- paste0("Gene_", seq_len(nrow(df)))
        }
        
        return(df)
      }
      
      return(NULL)
    })
    
    # =========================
    # FILTER LOGIC
    # =========================
    top_data <- eventReactive(input$run, {
      
      withProgress(
        
        message = "Finding Top Genes",
        
        value = 0,
        
        {
          
          incProgress(
            0.3,
            detail = "Loading results"
          )
      
      df <- get_data()
      
      
      if (is.null(df)) return(NULL)
      incProgress(
        0.6,
        detail = "Filtering genes"
      )
      
      df <- df[!is.na(df$padj) & !is.na(df$log2FoldChange), ]
      
      filtered <- df[
        !is.na(df$padj) &
          !is.na(df$log2FoldChange) &
          df$padj <= input$padj &
          abs(df$log2FoldChange) >= input$logfc,
      ]
      
      if (nrow(filtered) < 5) {
        showNotification(
          
          paste(
            
            "Only",
            
            nrow(filtered),
            
            "genes passed the thresholds. Showing the top ranked genes instead."
            
          ),
          
          type = "warning"
          
        )
        filtered <- df[order(df$padj), ]
      }
      
      filtered <- filtered[
        order(filtered$padj),
      ]
      incProgress(
        1,
        detail = "Done"
      )
      head(filtered, as.numeric(input$top_n))
    })
  })
    # =========================
    # TABLE OUTPUT
    # =========================
    output$table <- renderDT({
      
      if (input$run == 0) {
        return(datatable(data.frame(Status="Click 'Show Top Genes'"), options=list(dom='t')))
      }
      
      df <- top_data()
      
      if (is.null(df)) {
        return(datatable(data.frame(Status="Run DE first"), options=list(dom='t')))
      }
      
      cols <- intersect(
        
        c(
          "gene",
          "log2FoldChange",
          "padj",
          "baseMean"
        ),
        
        names(df)
        
      )
      
      df <- df[, cols]
      df <- df[order(df$padj), ]
      df$log2FoldChange <- round(
        df$log2FoldChange,
        3
      )
      
      df$padj <- signif(
        df$padj,
        3
      )
      
      df$baseMean <- round(
        df$baseMean,
        2
      )
      datatable(df, filter="top", rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE))
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download <- downloadHandler(
      filename = function() "top_genes.csv",
      content = function(file) {
        df <- top_data()
        content = function(file) {
          
          df <- top_data()
          
          if (is.null(df)) return(NULL)
          
          df <- df[order(df$padj), ]
          
          write.csv(df, file, row.names = FALSE)
          
        }
      }
    )
    
  })
}
