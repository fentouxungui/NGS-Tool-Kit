clusterUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Clustering"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Groups samples based on expression similarity using hierarchical clustering or k-means."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Normalized expression matrix (VST) or uploaded CSV"),
        tags$li("Genes ? Samples format")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload data or run DE Analysis"),
        tags$li("Choose clustering method"),
        tags$li("Set number of clusters (for k-means)"),
        tags$li("Click 'Run Clustering'")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoCluster"), height = "250px"),
      
      hr()
    ),
    
    h3("Clustering"),
    
    fileInput(ns("file"), "Upload Expression Matrix (CSV, optional)"),
    
    selectInput(ns("method"), "Method",
                choices = c("Hierarchical", "K-means")),
    
    numericInput(ns("k"), "Number of clusters (k-means)", 3, min = 2, max = 10),
    
    actionButton(ns("run"), "Run Clustering"),
    
    withSpinner(
      plotOutput(ns("clusterPlot"), height = "600px")
    ),
    downloadButton(
      ns("download_cluster"),
      "Download Plot"
    )
  )
}
clusterServer <- function(id, analysis) {
  
  moduleServer(id, function(input, output, session) {
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      
      data.frame(
        Gene = paste0("Gene", LETTERS[1:20]),
        Sample1 = rnorm(20, 100, 20),
        Sample2 = rnorm(20, 110, 20),
        Sample3 = rnorm(20, 200, 30),
        Sample4 = rnorm(20, 210, 30)
      )
      
    })
    
    output$sample_table <- renderTable({
      head(sample_data())
    })
    
    # =========================
    # DEMO PLOT
    # =========================
    output$demoCluster <- renderPlot({
      
      df <- sample_data()
      df <- df[, -1]
      
      X <- t(scale(df))
      
      hc <- hclust(dist(X))
      
      plot(
        hc,
        main = "Sample Clustering Output"
      )
      
    })
    
    # =========================
    # REAL DATA
    # =========================
    expr_data <- reactive({
      
      ## Uploaded file
      if (!is.null(input$file) && nzchar(input$file$datapath)) {
        
        df <- tryCatch({
          
          read.csv(
            input$file$datapath,
            stringsAsFactors = FALSE
          )
          
        }, error = function(e) {
          
          return(NULL)
          
        })
        
        if (is.null(df) || ncol(df) < 2)
          return(NULL)
        
        rownames(df) <- make.unique(as.character(df[[1]]))
        
        df <- df[, -1, drop = FALSE]
        
        df[] <- lapply(df, function(x)
          as.numeric(as.character(x)))
        
        return(as.matrix(df))
        
      }
      
      ## DESeq2 normalized data
      if (!is.null(analysis$status) &&
          !is.null(analysis$status$normalized) &&
          isTRUE(analysis$status$normalized) &&
          !is.null(analysis$vst)) {
        
        return(
          SummarizedExperiment::assay(analysis$vst)
        )
        
      }
      
      NULL
      
    })
    
    # =========================
    # MAIN PLOT
    # =========================
    output$clusterPlot <- renderPlot({
      
      if (input$run == 0) {
        
        plot.new()
        
        text(
          0.5,
          0.5,
          "Upload data and click 'Run Clustering'"
        )
        
        return()
        
      }
      
      df <- expr_data()
      
      if (is.null(df)) {
        
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
      
      tryCatch({
        
        withProgress(
          
          message = "Running Clustering",
          
          value = 0,
          
          {
            
            incProgress(
              0.3,
              detail = "Loading data"
            )
            
            X <- t(scale(df))
            
            incProgress(
              0.7,
              detail = "Performing clustering"
            )
            
            if (input$method == "Hierarchical") {
              
              hc <- hclust(dist(X))
              
              plot(
                hc,
                main = paste(
                  "Hierarchical Clustering (",
                  input$k,
                  " Clusters)",
                  sep = ""
                )
              )
              
              rect.hclust(
                hc,
                k = input$k,
                border = rainbow(input$k)
              )
              
            } else {
              
              set.seed(123)
              
              km <- kmeans(
                X,
                centers = input$k
              )
              
              plot(
                X[, 1:2],
                col = km$cluster,
                pch = 19,
                main = "K-means Clustering",
                xlab = "Dimension 1",
                ylab = "Dimension 2"
              )
              
            }
            
            incProgress(
              1,
              detail = "Completed"
            )
            
            showNotification(
              "Clustering completed successfully.",
              type = "message"
            )
            
          }
          
        )
        
      }, error = function(e) {
        
        showNotification(
          paste("Clustering Error:", e$message),
          type = "error"
        )
        
        plot.new()
        
        text(
          0.5,
          0.5,
          "Unable to perform clustering."
        )
        
      })
      
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download_cluster <- downloadHandler(
      
      filename = function() {
        
        "cluster_plot.png"
        
      },
      
      content = function(file) {
        
        df <- expr_data()
        
        if (is.null(df))
          return()
        
        png(
          file,
          width = 900,
          height = 700
        )
        
        X <- t(scale(df))
        
        if (input$method == "Hierarchical") {
          
          hc <- hclust(dist(X))
          
          plot(
            hc,
            main = paste(
              "Hierarchical Clustering (",
              input$k,
              " Clusters)",
              sep = ""
            )
          )
          
          rect.hclust(
            hc,
            k = input$k,
            border = rainbow(input$k)
          )
          
        } else {
          
          set.seed(123)
          
          km <- kmeans(
            X,
            centers = input$k
          )
          
          plot(
            X[, 1:2],
            col = km$cluster,
            pch = 19,
            main = "K-means Clustering",
            xlab = "Dimension 1",
            ylab = "Dimension 2"
          )
          
        }
        
        dev.off()
        
      }
      
    )
    
  })
  
}
