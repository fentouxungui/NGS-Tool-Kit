tsneUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About t-SNE"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("t-SNE visualizes similarity between samples using dimensionality reduction."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Normalized data (VST) OR expression matrix"),
        tags$li("Genes  Samples format")
      ),
      
      h4("Steps"),
      tags$ol(
        tags$li("Upload dataset OR run DE"),
        tags$li("Set perplexity"),
        tags$li("Click Run t-SNE")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot"), height = "250px"),
      
      hr()
    ),
    
    h3("t-SNE Analysis"),
    
    fileInput(ns("file"), "Upload Expression Matrix"),
    
    numericInput(ns("perplexity"), "Perplexity", 10),
    
    actionButton(ns("run"), "Run t-SNE"),
    
    textOutput(ns("status")),
    
    plotOutput(ns("tsnePlot"), height = "600px"),
    downloadButton(
      ns("download"),
      "Download Plot"
    )
  )
}
tsneServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
# library(Rtsne)
# library(SummarizedExperiment)
    
    status_msg <- reactiveVal("Idle")
    
    output$status <- renderText({
      status_msg()
    })
    
    # SAMPLE
    sample_data <- reactive({
      
      data.frame(
        
        GeneID=c(
          "Gene0001",
          "Gene0002",
          "Gene0003",
          "Gene0004",
          "Gene0005"
        ),
        
        Sample1=c(231,140,183,205,141),
        Sample2=c(228,143,181,231,166),
        Sample3=c(251,124,177,214,146),
        Sample4=c(236,141,179,204,166),
        Sample5=c(235,130,203,219,142),
        
        Sample6=c(382,321,333,335,313),
        Sample7=c(354,264,354,350,284),
        Sample8=c(372,314,298,384,317),
        Sample9=c(356,312,341,355,338),
        Sample10=c(382,267,334,399,328)
        
      )
      
    })
    output$sample_table <- renderTable({
      sample_data()
    })
    
    
    
    output$demoPlot <- renderPlot({
      
      set.seed(123)
      
      x <- c(
        rnorm(5,-5,0.5),
        rnorm(5,5,0.5),
        rnorm(5,0,0.5)
      )
      
      y <- c(
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
        x,
        y,
        col=cols,
        pch=19,
        cex=2,
        main="Example t-SNE"
      )
      
    })
    
    # DATA
    tsne_data <- reactive({
      
      if (!is.null(input$file) &&
          nzchar(input$file$datapath)) {
        df <- tryCatch(
          
          read.csv(input$file$datapath),
          
          error = function(e){
            
            status_msg("Unable to read uploaded file.")
            
            return(NULL)
            
          }
          
        )
        if (is.null(df))
          return(NULL)
        
        if (all(c("log2FoldChange","padj") %in% colnames(df))) {
          status_msg("Invalid: DE results uploaded")
          return(NULL)
        }
        
        rownames(df) <- df[[1]]
        df <- df[, -1]
        df[] <- lapply(df,function(x)
          suppressWarnings(as.numeric(x)))
        
        if (ncol(df) < 2) {
          
          status_msg("Expression matrix must contain at least two samples.")
          
          return(NULL)
          
        }
        
        if (anyNA(df)) {
          
          status_msg("Non-numeric values detected.")
          
          return(NULL)
          
        }
        return(as.matrix(df))
      }
      
      if (!is.null(analysis$vst)) {
        return(assay(analysis$vst))
      }
      
      status_msg("No data available")
      return(NULL)
    })
    
    # COMPUTE
    tsne_res <- eventReactive(input$run, {
      withProgress(
        
        message = "Running t-SNE",
        
        value = 0,
        
        {
      
      status_msg("Running t-SNE...")
      
      df <- tsne_data()
      incProgress(
        0.3,
        detail = "Loading data"
      )
      if (is.null(df)) return(NULL)
      
      # RNA-seq count matrix
      # Rows = genes
      # Columns = samples
      X <- t(df)
      
      print(dim(X))
      incProgress(
        0.6,
        detail = "Preparing expression matrix"
      )
      
      # Check minimum number of samples
      if (nrow(X) < 5) {
        
        status_msg("Too few samples. At least 5 samples are required for t-SNE.")
        
        return(NULL)
        
      }
      
      vars <- apply(
        X,
        2,
        var,
        na.rm = TRUE
      )
      
      vars[is.na(vars)] <- 0
      
      X <- X[, vars > 0, drop = FALSE]
      top <- min(
        2000,
        ncol(X)
      )
      
      X <- X[, order(vars, decreasing = TRUE)[1:top]]
      
      X <- scale(X)
      X <- X[, colSums(is.na(X)) == 0, drop = FALSE]
      
      X <- X[complete.cases(X), , drop = FALSE]
      
      max_perplexity <- floor((nrow(X) - 1) / 3)
      
      if (max_perplexity < 1) {
        status_msg("Too few samples for t-SNE.")
        return(NULL)
      }
      
      perplexity <- min(
        input$perplexity,
        max_perplexity
      )
      incProgress(
        0.9,
        detail = "Computing t-SNE"
      )
      res <- tryCatch(
        
        Rtsne(
          X,
          perplexity = perplexity,
          max_iter = 300
        ),
      
      
      
        
        error = function(e){
          
          status_msg(e$message)
          
          return(NULL)
          
        }
        
      )
      print(res)
      str(res)
      
      status_msg("Completed")
      incProgress(1)
      
      res
    })
    })
    
    output$tsnePlot <- renderPlot({
      
      if (input$run == 0) {
        plot.new()
        text(0.5,0.5,"Click Run t-SNE")
        return()
      }
      
      ts <- tsne_res()
      
      print(ts)
      
      if (is.null(ts)) {
        
        plot.new()
        text(0.5, 0.5, "t-SNE result is NULL")
        
        return()
        
      }
      
      print(dim(ts$Y))
      print(head(ts$Y))
      print(class(ts))
      print(names(ts))
      print(dim(ts$Y))
      
      plot(
        ts$Y[,1],
        ts$Y[,2],
        pch = 19,
        cex = 1.6,
        col = "blue",
        main = "t-SNE Visualization",
        xlab = "t-SNE Dimension 1",
        ylab = "t-SNE Dimension 2"
      )
      
      
    })
    output$download <- downloadHandler(
      filename = function(){
        "tsne_plot.png"
      },
      content = function(file){
        
        ts <- tsne_res()
        
        if(is.null(ts)) return()
        
        png(file,
            width=1000,
            height=700,
            res=150)
        
        plot(
          ts$Y[,1],
          ts$Y[,2],
          pch=19,
          cex=1.6,
          col="blue",
          main="t-SNE Visualization",
          xlab="t-SNE Dimension 1",
          ylab="t-SNE Dimension 2"
        )
        
        dev.off()
        
      }
    )
    
  })
}
