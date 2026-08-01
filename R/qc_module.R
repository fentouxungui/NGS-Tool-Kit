qcUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Quality Control"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Performs quality control checks on RNA-Seq count data, including distribution analysis, library size comparison, PCA visualization, and outlier detection."),
      
      h4("Required Inputs"),
      tags$ul(
        tags$li("Count matrix (genes  samples)"),
        tags$li("First column: gene names"),
        tags$li("Remaining columns: numeric counts")
      ),
      
      h4("How to use"),
      tags$ol(
        tags$li("Upload count matrix CSV"),
        tags$li("Check distribution (boxplot)"),
        tags$li("Compare library sizes"),
        tags$li("Inspect PCA clustering"),
        tags$li("Identify outlier samples")
      ),
      
      h4("Sample Dataset"),
      tableOutput(ns("sample_table")),
      
      h4("Sample Output"),
      plotOutput(ns("demoPlot1"), height = "200px"),
      plotOutput(ns("demoPlot2"), height = "200px"),
      plotOutput(ns("demoPlot3"), height = "250px"),
      
      hr()
    ),
    
    h3("Quality Control"),
    
    fileInput(
      ns("file"),
      "Upload Count Matrix (CSV)",
      accept = ".csv"
    ),
    
    tabsetPanel(
      tabPanel("Distribution", withSpinner(
        plotOutput(ns("qcPlot"), height = "400px")
      )),
      tabPanel("Library Size",withSpinner(
        plotOutput(ns("libPlot"), height = "400px")
      )),
      tabPanel("PCA", withSpinner(
        plotOutput(ns("pcaPlot"), height = "400px")
      )),
      tabPanel("Outliers", tableOutput(ns("outliers"))
    )
      ),
    br(),
    
    box(
      width = 12,
      title = tagList(icon("download"), "Download Results"),
      status = "primary",
      solidHeader = TRUE,
      
      fluidRow(
        
        column(
          width = 10,
          
          selectInput(
            ns("downloadType"),
            "Export Type",
            choices = c(
              "QC Summary (CSV)",
              "Processed Count Matrix (CSV)",
              "Distribution Plot (PNG)",
              "Library Size Plot (PNG)",
              "PCA Plot (PNG)"
            ),
            selectize = FALSE
          )
        ),
        
        column(
          width = 2,
          
          div(
            style = "margin-top:30px; text-align:right;",
            
            downloadButton(
              ns("downloadResult"),
              "Download",
              class = "btn-primary"
            )
          )
          
        )
        
      )
      
    ),
  )
}
qcServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_data <- reactive({
      data.frame(
        Gene = paste0("Gene", LETTERS[1:30]),
        Sample1 = sample(100:1000, 30),
        Sample2 = sample(100:1000, 30),
        Sample3 = sample(100:1000, 30),
        Sample4 = sample(100:1000, 30)
      )
    })
    # =========================
    # SAMPLE OUTPUT (PREVIEW)
    # =========================
    
    output$demoPlot1 <- renderPlot({
      df <- sample_data()
      df <- df[,-1]
      
      boxplot(df,
              col = "lightblue",
              main = "Sample Distribution")
    })
    
    output$demoPlot2 <- renderPlot({
      df <- sample_data()
      df <- df[,-1]
      
      barplot(colSums(df),
              col = "orange",
              main = "Sample Library Size")
    })
    # =========================
    # PCA SAMPLE OUTPUT (NEW)
    # =========================
    output$demoPlot3 <- renderPlot({
      
      df <- sample_data()
      df <- df[,-1]
      
      df <- log2(df + 1)
      pca <- prcomp(t(df), scale. = TRUE)
      
      # Detect outliers using PC1
      pc1 <- pca$x[,1]
      z <- scale(pc1)
      
      outliers <- abs(z) > 2
      
      colors <- ifelse(outliers, "red", "blue")
      
      plot(
        pca$x[,1], pca$x[,2],
        col = colors,
        pch = 19,
        xlab = "PC1",
        ylab = "PC2",
        main = "PCA (Outliers in Red)"
      )
      
      text(
        pca$x[,1], pca$x[,2],
        labels = colnames(df),
        pos = 3,
        cex = 0.7
      )
    })
    output$sample_table <- renderTable({
      head(sample_data())
    })
    
    # =========================
    # LOAD DATA
    # =========================
    observeEvent(input$file, {
      
      req(input$file)
      
      tryCatch({
        
        withProgress(
          message = "Loading Count Matrix",
          value = 0, {
            
            incProgress(0.3, detail = "Reading CSV")
            
            df <- read.csv(
              input$file$datapath,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
            
            if (ncol(df) < 2)
              stop("Invalid file")
            
            genes <- as.character(df[[1]])
            
            # Fix empty gene names
            genes[is.na(genes) | genes == ""] <-
              paste0("Gene_", seq_len(length(genes)))
            
            # Fix duplicate gene names
            if (any(duplicated(genes))) {
              showNotification(
                "Duplicate genes fixed",
                type = "warning"
              )
              genes <- make.unique(genes)
            }
            
            rownames(df) <- genes
            df <- df[, -1, drop = FALSE]
            
            df[] <- lapply(df, function(x)
              as.numeric(as.character(x)))
            
            df <- as.matrix(df)
            
            incProgress(0.9, detail = "Finalizing")
            
            analysis$counts <- df
            analysis$status$has_counts <- TRUE
            
            incProgress(1)
            
          }   # closes withProgress()
          
        )
        
        showNotification(
          paste(
            "Loaded",
            nrow(df),
            "genes and",
            ncol(df),
            "samples."
          ),
          type = "message"
        )
        
      }, error = function(e) {
        
        showNotification(
          paste("QC Error:", e$message),
          type = "error"
        )
        
      })
      
    })
    # =========================
    # DISTRIBUTION
    # =========================
    output$qcPlot <- renderPlot({
      
      if (!analysis$status$has_counts) {
        plot.new()
        text(
          
          0.5,
          
          0.5,
          
          "Upload a count matrix to begin Quality Control",
          
          cex = 1.2
          
        )
        return()
      }
      
      boxplot(
        
        analysis$counts,
        
        col = "lightblue",
        
        border = "steelblue",
        
        las = 2,
        
        outline = TRUE,
        
        main = "Expression Distribution",
        
        xlab = "Samples",
        
        ylab = "Raw Counts"
        
      )
    })
    
    # =========================
    # LIBRARY SIZE
    # =========================
    output$libPlot <- renderPlot({
      
      req(analysis$status$has_counts)
      
      lib_sizes <- colSums(analysis$counts)
      
      barplot(
        
        lib_sizes,
        
        col = "orange",
        
        border = "darkorange",
        
        las = 2,
        
        main = "Library Size per Sample",
        
        xlab = "Samples",
        
        ylab = "Total Read Counts"
        
      )
    })
    
    # =========================
    # PCA PREVIEW
    # =========================
    output$pcaPlot <- renderPlot({
      
      req(analysis$status$has_counts)
      
      df <- analysis$counts
      
      df <- log2(df + 1)
      pca <- prcomp(t(df), scale. = TRUE)
      
      plot(
        pca$x[,1], pca$x[,2],
        col = "blue",
        pch = 19,
        xlab = "PC1",
        ylab = "PC2",
        main = "PCA (QC Check)"
      )
    })
    
    # =========================
    # OUTLIER DETECTION
    # =========================
    output$outliers <- renderTable({
      
      req(analysis$status$has_counts)
      
      df <- log2(analysis$counts + 1)
      
      sample_means <- colMeans(df)
      z_scores <- scale(sample_means)
      
      outliers <- names(sample_means)[abs(z_scores) > 2]
      
      if (length(outliers) == 0) {
        return(data.frame(Message = "No outliers detected"))
      }
      
      data.frame(Outlier_Samples = outliers)
    })
    
    output$downloadResult <- downloadHandler(
      
      filename = function() {
        
        switch(
          
          input$downloadType,
          
          "QC Summary (CSV)" =
            paste0("QC_Summary_", Sys.Date(), ".csv"),
          
          "Processed Count Matrix (CSV)" =
            paste0("Processed_Count_Matrix_", Sys.Date(), ".csv"),
          
          "Distribution Plot (PNG)" =
            paste0("Distribution_", Sys.Date(), ".png"),
          
          "Library Size Plot (PNG)" =
            paste0("Library_Size_", Sys.Date(), ".png"),
          
          "PCA Plot (PNG)" =
            paste0("PCA_", Sys.Date(), ".png")
          
        )
        
      },
      
      content = function(file) {
        
        req(analysis$status$has_counts)
        
        if (input$downloadType == "QC Summary (CSV)") {
          
          df <- analysis$counts
          
          summary_df <- data.frame(
            
            Sample = colnames(df),
            
            Library_Size = colSums(df),
            
            Mean = round(colMeans(df),2),
            
            Median = round(apply(df,2,median),2),
            
            SD = round(apply(df,2,sd),2),
            
            Minimum = apply(df,2,min),
            
            Maximum = apply(df,2,max)
            
          )
          
          write.csv(summary_df, file, row.names = FALSE)
          
        }
        
        else if (input$downloadType == "Processed Count Matrix (CSV)") {
          
          write.csv(
            analysis$counts,
            file,
            row.names = TRUE
          )
          
        }
        
        else if (input$downloadType == "Distribution Plot (PNG)") {
          
          png(file, width = 1200, height = 800)
          
          boxplot(
            
            analysis$counts,
            
            col = "lightblue",
            
            border = "steelblue",
            
            las = 2,
            
            outline = TRUE,
            
            main = "Expression Distribution",
            
            xlab = "Samples",
            
            ylab = "Raw Counts"
            
          )
          
          dev.off()
          
        }
        
        else if (input$downloadType == "Library Size Plot (PNG)") {
          
          png(file, width = 1200, height = 800)
          
          barplot(
            
            colSums(analysis$counts),
            
            col = "orange",
            
            border = "darkorange",
            
            las = 2,
            
            main = "Library Size per Sample",
            
            xlab = "Samples",
            
            ylab = "Total Read Counts"
            
          )
          
          dev.off()
          
        }
        
        else if (input$downloadType == "PCA Plot (PNG)") {
          
          png(file, width = 1200, height = 800)
          
          df <- log2(analysis$counts + 1)
          
          pca <- prcomp(
            t(df),
            scale. = TRUE
          )
          
          plot(
            
            pca$x[,1],
            
            pca$x[,2],
            
            pch = 19,
            
            col = "blue",
            
            xlab = "PC1",
            
            ylab = "PC2",
            
            main = "PCA (QC Check)"
            
          )
          
          text(
            
            pca$x[,1],
            
            pca$x[,2],
            
            labels = colnames(df),
            
            pos = 3,
            
            cex = 0.8
            
          )
          
          dev.off()
          
        }
        
      }
      
    )
    
    
        
    
    
    
    
  })
  
}
