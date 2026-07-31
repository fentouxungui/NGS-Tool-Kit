geneUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Gene Expression Viewer"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Visualizes expression levels of one or more genes across samples. Useful for comparing expression patterns between conditions."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Normalized expression data (VST) from DESeq2 OR"),
        tags$li("User-uploaded expression matrix (genes ? samples)"),
        tags$li("Optional metadata with 'condition' column for coloring")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload your expression matrix (optional)"),
        tags$li("OR run DE Analysis to use normalized data"),
        tags$li("Search and select one or more genes"),
        tags$li("Enable 'Color by condition' if metadata is available"),
        tags$li("Click 'Plot Expression'"),
        tags$li("Download the plot if needed")
      ),
      
      h4("Input Format Example"),
      tags$pre(
        "Gene,Sample1,Sample2,Sample3
GeneA,120,130,300
GeneB,50,60,20"
      ),
      
      h4("Output"),
      tags$ul(
        tags$li("Boxplot of gene expression across samples"),
        tags$li("Multiple genes shown side-by-side"),
        tags$li("Optional coloring by condition"),
        tags$li("Downloadable plot")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "300px"),
      
      hr()
    ),
    
    h3("Gene Expression Viewer"),
    
    fileInput(ns("file"), "Upload Expression Matrix (CSV, optional)"),
    
    selectizeInput(
      ns("gene"),
      "Search & Select Gene(s)",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Type gene name...",
        maxItems = 10
      )
    ),
    
    checkboxInput(ns("color_meta"), "Color by condition", TRUE),
    
    actionButton(ns("run"), "Plot Expression"),
    
    plotOutput(ns("genePlot"), height = "600px"),
    
    downloadButton(ns("download_gene"), "Download Plot")
  )
}
geneServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        Gene = paste0("Gene", LETTERS[1:10]),
        Sample1 = rnorm(10, 100, 20),
        Sample2 = rnorm(10, 110, 20),
        Sample3 = rnorm(10, 200, 30),
        Sample4 = rnorm(10, 210, 30)
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
      values <- as.numeric(df[1, -1])
      
      boxplot(
        values,
        main = "Demo Expression",
        col = "steelblue"
      )
    })
    
    # =========================
    # REAL DATA (NO FALLBACK)
    # =========================
    expr_data <- reactive({
      
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
        return(as.matrix(df))
      }
      
      # DE DATA (SAFE)
      if (!is.null(analysis$status) &&
          !is.null(analysis$status$normalized) &&
          isTRUE(analysis$status$normalized) &&
          !is.null(analysis$vst)) {
        
        return(SummarizedExperiment::assay(analysis$vst))
      }
      
      return(NULL)
    })
    
    # =========================
    # UPDATE GENE LIST (SAFE)
    # =========================
    observe({
      
      df <- expr_data()
      
      if (!is.null(df)) {
        updateSelectizeInput(
          session,
          "gene",
          choices = rownames(df),
          server = TRUE
        )
      }
    })
    
    # =========================
    # MAIN PLOT
    # =========================
    output$genePlot <- renderPlot({
      
      # BEFORE RUN
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Select gene(s) and click 'Plot Expression'")
        return()
      }
      
      df <- expr_data()
      
      # NO DATA
      if (is.null(df)) {
        plot.new()
        text(0.5, 0.5, "No data available")
        return()
      }
      
      # NO GENE SELECTED
      if (is.null(input$gene) || length(input$gene) == 0) {
        plot.new()
        text(0.5, 0.5, "Please select gene(s)")
        return()
      }
      
      tryCatch({
        
        selected_genes <- input$gene
        
        expr <- df[selected_genes, , drop = FALSE]
        
        plot_df <- reshape2::melt(expr)
        
        colnames(plot_df) <- c(
          "Gene",
          "Sample",
          "Expression"
        )
        
        ## Metadata available?
        if (
          isTRUE(input$color_meta) &&
          !is.null(analysis$status) &&
          isTRUE(analysis$status$has_meta) &&
          !is.null(analysis$meta)
        ) {
          
          meta <- analysis$meta
          
          plot_df <- merge(
            plot_df,
            meta,
            by.x = "Sample",
            by.y = "sample",
            all.x = TRUE
          )
          
          ggplot(
            plot_df,
            aes(
              x = condition,
              y = Expression,
              fill = condition
            )
          ) +
            
            geom_boxplot(
              outlier.shape = NA,
              alpha = 0.7
            ) +
            
            geom_jitter(
              width = 0.15,
              size = 2
            ) +
            
            facet_wrap(
              ~Gene,
              scales = "free_y"
            ) +
            
            labs(
              title = "Gene Expression by Condition",
              x = "Condition",
              y = "Expression"
            ) +
            
            theme_bw() +
            
            theme(
              axis.text.x = element_text(
                angle = 45,
                hjust = 1
              )
            )
          
        } else {
          
          ggplot(
            plot_df,
            aes(
              x = Sample,
              y = Expression
            )
          ) +
            
            geom_col(fill = "steelblue") +
            
            facet_wrap(
              ~Gene,
              scales = "free_y"
            ) +
            
            labs(
              title = "Gene Expression",
              x = "Sample",
              y = "Expression"
            ) +
            
            theme_bw() +
            
            theme(
              axis.text.x = element_text(
                angle = 90,
                hjust = 1
              )
            )
          
        }
        
      }, error = function(e) {
        
        showNotification(
          paste("Plot Error:", e$message),
          type = "error"
        )
        
        plot.new()
        
        text(
          0.5,
          0.5,
          "Unable to generate plot."
        )
        
      })
      
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download_gene <- downloadHandler(
      filename = function() "gene_expression.png",
      content = function(file) {
        
        df <- expr_data()
        genes <- input$gene
        
        if (is.null(df) || is.null(genes)) return()
        
        png(file, width = 800, height = 600)
        
        data_list <- lapply(genes, function(g) as.numeric(df[g, ]))
        names(data_list) <- genes
        
        print(last_plot())
        
        dev.off()
      }
    )
    
  })
}
