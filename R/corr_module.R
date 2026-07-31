corrUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Correlation Analysis"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Calculates similarity between samples using correlation metrics."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Expression Matrix (CSV) or normalized expression matrix generated from DE Analysis"),
        tags$li("Optional uploaded dataset")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload dataset OR run DE Analysis"),
        tags$li("Select correlation method"),
        tags$li("Click 'Generate Correlation'"),
        tags$li("Interpret similarity between samples")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "250px"),
      
      hr()
    ),
    
    h3("Correlation Analysis"),
    
    fileInput(ns("file"), "Upload Expression Matrix (.csv)"),
    
    selectInput(
      ns("method"),
      "Correlation Method",
      choices = c("pearson", "spearman", "kendall"),
      selected = "pearson"
    ),
    helpText(
      HTML(
        "<b>Pearson:</b> Best for linear relationships in normally distributed data.<br>
     <b>Spearmans:</b> Best for ranked or non-normal data and is less affected by outliers.<br>
     <b>Kendall:</b> Best for small datasets or data with many tied values."
      )
    ),
    
    actionButton(ns("run"), "Generate Correlation"),   # ? added
    
    withSpinner(
      plotOutput(ns("corrPlot"), height = "600px")
    ),
    
    box(
      
      width=12,
      
      title=tagList(
        icon("download"),
        "Download Results"
      ),
      
      status="primary",
      
      solidHeader=TRUE,
      
      fluidRow(
        
        column(
          
          width=8,
          
          selectInput(
            
            ns("downloadType"),
            
            "Export Type",
            
            choices=c(
              
              "Correlation Matrix (CSV)",
              
              "Correlation Heatmap (PNG)"
              
            ),
            
            selectize=FALSE
            
          )
          
        ),
        
        column(
          
          width=4,
          
          div(
            
            style="margin-top:30px;text-align:right;",
            
            downloadButton(
              
              ns("downloadResult"),
              
              "Download"
              
            )
            
          )
          
        )
        
      )
      
    )
  )
}
corrServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        Sample = paste0("Sample", 1:20),
        GeneA = rnorm(20, 100, 30),
        GeneB = rnorm(20, 200, 40),
        GeneC = rnorm(20, 150, 20),
        GeneD = rnorm(20, 300, 50)
      )
    })
    
    output$sample_table <- renderTable({
      sample_data()
    })
    
    # =========================
    # DEMO PLOT (ONLY SAMPLE)
    # =========================
    output$demoPlot <- renderPlot({
      
      df <- sample_data()
      rownames(df) <- df$Sample
      df <- df[, -1]
      
      cor_mat <- cor(df)
      
      pheatmap(
        cor_mat,
        main = "Sample Correlation (Demo)"
      )
    })
    
    # =========================
    # REAL DATA (NO FALLBACK)
    # =========================
    corr_data <- reactive({
      
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
    # CORRELATION RESULT
    # =========================
    corr_res <- eventReactive(input$run, {
      
      df <- corr_data()
      
      if (is.null(df)) {
        
        showNotification(
          "Please upload an expression matrix or complete DE Analysis first.",
          type = "warning"
        )
        
        return(NULL)
        
      }
      
      tryCatch({
        
        withProgress(
          
          message = "Generating Correlation",
          
          value = 0,
          
          {
            
            incProgress(0.3,
                        detail = "Preparing data")
            
            df <- as.matrix(df)
            mode(df) <- "numeric"
            df <- df[complete.cases(df), ]
            
            if(nrow(df)<2 || ncol(df)<2)
              return(NULL)
            
            incProgress(0.7,
                        detail = "Calculating correlation")
            
            X <- t(df)
            
            cor_mat <- cor(
              X,
              method=input$method
            )
            
            
            incProgress(1,
                        detail = "Generating heatmap")
            showNotification(
              
              "Correlation generated successfully.",
              
              type="message"
              
            )
            
            cor_mat
            
          }
          
        )
        
      }, error=function(e){
        
        showNotification(
          
          paste(
            "Correlation Error:",
            e$message
          ),
          
          type="error"
          
        )
        
        return(NULL)
        
      })
    })
    
    # =========================
    # MAIN PLOT
    # =========================
    output$corrPlot <- renderPlot({
      
      # BEFORE RUN
      if (input$run == 0) {
        plot.new()
        text(0.5, 0.5, "Upload data and click 'Generate Correlation'")
        return()
      }
      
      cor_mat <- corr_res()
      
      # NO DATA
      if (is.null(cor_mat)) {
        plot.new()
        text(0.5, 0.5, "No valid expression matrix found.")
        return()
      }
      
      pheatmap(
        cor_mat,
        main = paste(
          "Sample Correlations (",
          tools::toTitleCase(input$method),
          ")",
          sep = ""
        ),
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        display_numbers = FALSE,
        fontsize = 10,
        border_color = NA
      )
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$downloadResult <- downloadHandler(
      
      filename = function(){
        
        if(input$downloadType == "Correlation Matrix (CSV)"){
          
          "correlation_matrix.csv"
          
        } else{
          
          "correlation_heatmap.png"
          
        }
        
      },
      
      content = function(file){
        
        cor_mat <- corr_res()
        
        req(cor_mat)
        
        if(input$downloadType == "Correlation Matrix (CSV)"){
          
          write.csv(cor_mat, file)
          
        } else{
          
          png(file,
              width=1200,
              height=1000,
              res=150)
          
          pheatmap(
            cor_mat,
            main=paste(
              "Sample Correlation (",
              input$method,
              ")",
              sep=""
            ),
            cluster_rows=TRUE,
            cluster_cols=TRUE,
            display_numbers=FALSE,
            fontsize=10,
            border_color=NA
          )
          
          dev.off()
          
        }
        
      }
      
    )
  })
}
