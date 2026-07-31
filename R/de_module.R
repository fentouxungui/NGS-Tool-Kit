deUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      title = tagList(icon("circle-info"), "About Differential Expression"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4("What does this module do?"),
      p("Runs DESeq2 on count data with flexible metadata handling."),
      
      h4("Required Inputs"),
      tags$ul(
        
        tags$li("Count matrix (CSV )"),
        tags$li("Metadata (any column names)")
      ),
      
      h4("How to use"),
      tags$ol(
        
        tags$li("Upload counts + metadata"),
        tags$li("Select sample & condition columns"),
        tags$li("Click Run")
      ),
      h4("Sample Dataset"),
      tableOutput(ns("sample_counts")),
      tableOutput(ns("sample_meta")),
      
      h4("Sample Output"),
      tableOutput(ns("demo_results")),
      
      hr()
    ),
    
    h3("DESeq2 Analysis"),
    
    fileInput(ns("counts"), "Upload Count Matrix (CSV/TSV)"),
    fileInput(ns("meta"), "Upload Metadata"),
    
    selectInput(ns("sample_col"), "Sample Column", choices = NULL),
    selectInput(ns("condition_col"), "Condition Column", choices = NULL),
    
    actionButton(ns("run"), "Run DE"),
    
    tableOutput(ns("results")),
    
    downloadButton(ns("download_de"), "Download Results")
  )
}
deServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    
    
    # =========================
    # SAMPLE DATA
    # =========================
    sample_counts <- reactive({
      df <- data.frame(
        Gene = paste0("Gene", LETTERS[1:20]),
        Sample1 = sample(50:500, 20),
        Sample2 = sample(50:500, 20),
        Sample3 = sample(50:500, 20),
        Sample4 = sample(50:500, 20),
        stringsAsFactors = FALSE
      )
      
      rownames(df) <- make.unique(df$Gene)
      df <- df[, -1]
      df
    })
    
    sample_meta <- reactive({
      data.frame(
        sample = c("Sample1","Sample2","Sample3","Sample4"),
        condition = c("Control","Control","Treatment","Treatment")
      )
    })
    
    # =========================
    # SHOW SAMPLE TABLES
    # =========================
    output$sample_counts <- renderTable({
      head(sample_counts())
    }, rownames = TRUE)
    
    output$sample_meta <- renderTable({
      sample_meta()
    })
    
    # =========================
    # SAMPLE OUTPUT (DEMO)
    # =========================
    output$demo_results <- renderTable({
      
      df <- data.frame(
        gene = paste0("Gene", LETTERS[1:10]),
        log2FoldChange = rnorm(10, 0, 2),
        padj = runif(10, 0.001, 0.1)
      )
      
      df[order(df$padj), ]
    })
    # =========================
    # AUTO DETECT METADATA COLS
    # =========================
    observeEvent(input$meta, {
      
      req(input$meta)
      
      meta <- read.csv(input$meta$datapath, stringsAsFactors = FALSE)
      cols <- colnames(meta)
      
      updateSelectInput(session, "sample_col", choices = cols)
      updateSelectInput(session, "condition_col", choices = cols)
      
      # smart guess
      sample_guess <- grep("sample|id", cols, ignore.case = TRUE, value = TRUE)[1]
      cond_guess   <- grep("condition|group|treat", cols, ignore.case = TRUE, value = TRUE)[1]
      
      if (!is.na(sample_guess)) updateSelectInput(session, "sample_col", selected = sample_guess)
      if (!is.na(cond_guess)) updateSelectInput(session, "condition_col", selected = cond_guess)
    })
    
    # =========================
    # RUN DE
    # =========================
      observeEvent(input$run, {
      
      withProgress(
        message = "Running DESeq2 Analysis",
        value = 0,
        {
          
          tryCatch({
            
            req(input$counts, input$meta)
            req(input$sample_col, input$condition_col)
            
            incProgress(
              0.1,
              detail = "Reading input files"
            )
        
        # -------------------------
        # READ COUNTS (AUTO DETECT)
        # -------------------------
        if (grepl("\\.csv$", input$counts$name)) {
          raw <- read.csv(input$counts$datapath, check.names = FALSE)
        } else {
          raw <- read.delim(input$counts$datapath, check.names = FALSE)
        }
        
        # -------------------------
        # EXTRACT GENES
        # -------------------------
        genes <- raw[[1]]
        genes[is.na(genes) | genes == ""] <- paste0("Gene_", seq_len(nrow(raw)))
        genes <- make.unique(as.character(genes))
        
        # -------------------------
        # KEEP ONLY NUMERIC COLUMNS
        # -------------------------
        counts <- raw[, -1, drop = FALSE]
        colnames(counts) <- trimws(colnames(counts))
        
        counts <- as.data.frame(lapply(counts, function(x) {
          suppressWarnings(as.numeric(as.character(x)))
        }))
        
        # remove non-numeric cols
        counts <- counts[, colSums(!is.na(counts)) > 0, drop = FALSE]
        
        rownames(counts) <- genes
        
        # -------------------------
        # REMOVE ALL-ZERO GENES
        # -------------------------
        counts <- counts[rowSums(counts, na.rm = TRUE) > 0, ]
        incProgress(
          0.3,
          detail = "Filtering genes"
        )
        
        keep <- rowSums(counts >= 10) >= 2
        
        counts <- counts[keep, ]
        
        if (nrow(counts) == 0)
          stop("No genes remain after filtering.")
        
        if (nrow(counts) == 0) {
          stop("All genes have zero counts after cleaning")
        }
        
        # -------------------------
        # LOAD META
        # -------------------------
        meta <- read.csv(input$meta$datapath, stringsAsFactors = FALSE)
        
        colnames(meta)[colnames(meta) == input$sample_col] <- "sample"
        
        meta$sample <- trimws(as.character(meta$sample))
        colnames(meta)[colnames(meta) == input$condition_col] <- "condition"
        
        # -------------------------
        # VALIDATION
        # -------------------------
        print("Counts column names:")
        print(colnames(counts))
        
        print("Metadata sample names:")
        print(meta$sample)
        if (!all(colnames(counts) %in% meta$sample)) {
          
          missing_samples <- setdiff(
            colnames(counts),
            meta$sample
          )
          
          stop(
            paste(
              "Metadata missing sample(s):",
              paste(missing_samples, collapse = ", ")
            )
          )
          
        }
        
        meta <- meta[match(colnames(counts), meta$sample), ]
        
        # -------------------------
        # DESEQ2
        # -------------------------
        incProgress(
          0.5,
          detail = "Creating DESeq2 dataset"
        )
        dds <- DESeqDataSetFromMatrix(
          countData = round(as.matrix(counts)),
          colData   = meta,
          design    = ~ condition
        )
        incProgress(
          0.8,
          detail = "Running DESeq2"
        )
        
        dds <- DESeq(dds)
        
        res <- results(dds)
        incProgress(
          0.9,
          detail = "Preparing results"
        )
        # -------------------------
        # STORE
        # -------------------------
        analysis$counts <- counts
        analysis$meta   <- meta
        analysis$dds    <- dds
        analysis$de     <- res
        analysis$vst    <- vst(dds)
        
        analysis$status$de_done    <- TRUE
        analysis$status$normalized <- TRUE
        analysis$summary <- list(
          
          genes = nrow(counts),
          
          samples = ncol(counts),
          
          significant = sum(
            res$padj < 0.05,
            na.rm = TRUE
          )
          
        )
        incProgress(1)
        
        showNotification(
          "DESeq2 analysis completed successfully.",
          type = "message"
        )
        
        
      }, error = function(e) {
        showNotification(paste("DE Error:", e$message), type = "error")
      })
    })
      })
    
    # =========================
    # OUTPUT
    # =========================
    output$results <- renderTable({
      
      if (!analysis$status$de_done) {
        return(data.frame(Status = "Run DE Analysis first"))
      }
      
      df <- as.data.frame(analysis$de)
      df$gene <- rownames(df)
      
      head(df[order(df$padj), ])
    })
    
    # =========================
    # DOWNLOAD
    # =========================
    output$download_de <- downloadHandler(
      filename = function() "DE_results.csv",
      content = function(file) {
        df <- as.data.frame(analysis$de)
        df$gene <- rownames(df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
