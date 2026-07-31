#===========================================================
# jbrowse_module.R
# NGS Tool Kit v2.0
# Genome Browser Module
#===========================================================


#===========================================================
# UI
#===========================================================

jbrowseUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    #=======================================================
    # About
    #=======================================================
    
    box(
      
      width = 12,
      
      title = tagList(
        icon("dna"),
        "Genome Browser"
      ),
      
      status = "primary",
      
      solidHeader = TRUE,
      
      collapsible = TRUE,
      
      collapsed = FALSE,
      
      h4("Overview"),
      
      p(
        "The Genome Browser module enables interactive exploration ",
        "of genomes and annotations using JBrowse."
      ),
      
      br(),
      
      h4("Supported Input"),
      
      tags$ul(
        
        tags$li(
          strong("Reference Genome"),
          " : FASTA (.fa, .fasta, .fna)"
        ),
        
        tags$li(
          strong("Annotation"),
          " : GFF3 (.gff, .gff3)"
        )
        
      ),
      
      br(),
      
      h4("Workflow"),
      
      tags$ol(
        
        tags$li("Upload a FASTA file."),
        
        tags$li("Upload a GFF3 annotation."),
        
        tags$li("Click 'Load Genome'."),
        
        tags$li("Wait while genome files are prepared."),
        
        tags$li("Browse your genome interactively.")
        
      )
      
    ),
    
    #=======================================================
    # Upload Panel
    #=======================================================
    
    box(
      
      width = 12,
      
      title = tagList(
        
        icon("upload"),
        
        "Upload Genome"
        
      ),
      
      status = "success",
      
      solidHeader = TRUE,
      
      fileInput(
        
        ns("fasta"),
        
        "Reference Genome (FASTA)",
        
        accept = VALID_FASTA_EXTENSIONS
        
      ),
      
      fileInput(
        
        ns("gff"),
        
        "Annotation (GFF3)",
        
        accept = VALID_GFF_EXTENSIONS
        
      ),
      
      fluidRow(
        
        column(
          
          4,
          
          actionButton(
            
            ns("load"),
            
            "Load Genome",
            
            icon = icon("play"),
            
            class = "btn-primary btn-block"
            
          )
          
        ),
        
        column(
          
          4,
          
          actionButton(
            
            ns("reset"),
            
            "Reset",
            
            icon = icon("rotate-left"),
            
            class = "btn-warning btn-block"
            
          )
          
        )
        
      ),
      
      br(),
      
      verbatimTextOutput(
        
        ns("status")
        
      )
      
    ),
    
    #=======================================================
    # Genome Browser
    #=======================================================
    
    box(
      
      width = 12,
      
      title = tagList(
        
        icon("globe"),
        
        "JBrowse"
        
      ),
      
      status = "info",
      
      solidHeader = TRUE,
      
      uiOutput(
        
        ns("browser")
        
      )
      
    )
    
  )
  
}
#===========================================================
# Server
#===========================================================

jbrowseServer <- function(id, analysis){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    #=======================================================
    # Reactive Values
    #=======================================================
    
    browserURL <- reactiveVal(
      
      DEFAULT_BROWSER_PAGE
      
    )
    
    currentSession <- reactiveVal(
      
      NULL
      
    )
    
    currentGenome <- reactiveVal(
      
      NULL
      
    )
    
    statusMessage <- reactiveVal(
      
      STATUS_READY
      
    )
    
    #=======================================================
    # Status Output
    #=======================================================
    
    output$status <- renderText({
      
      statusMessage()
      
    })
    
    #=======================================================
    # Browser Output
    #=======================================================
    
    output$browser <- renderUI({
      
      tags$iframe(
        
        id = ns("jbrowse_iframe"),
        
        src = browserURL(),
        
        width = "100%",
        
        height = DEFAULT_BROWSER_HEIGHT,
        
        style = "
          border:none;
          background:white;
        "
        
      )
      
    })
    
    #=======================================================
    # Initialize Analysis Object
    #=======================================================
    
    observe({
      
      if(is.null(analysis$jbrowse)){
        
        analysis$jbrowse <- list(
          
          session = NULL,
          
          fasta = NULL,
          
          fai = NULL,
          
          gff = NULL,
          
          tbi = NULL,
          
          config = NULL,
          
          output = NULL,
          
          browser = NULL
          
        )
        
      }
      
    })
    
    #=======================================================
    # Reset Browser
    #=======================================================
    
    observeEvent(input$reset,{
      
      info <- currentSession()
      
      if(!is.null(info)){
        
        cleanupSessionFolder(info$path)
        
      }
      
      browserURL(DEFAULT_BROWSER_PAGE)
      
      currentGenome(NULL)
      
      currentSession(NULL)
      
      analysis$jbrowse <- list(
        session = NULL,
        fasta = NULL,
        fai = NULL,
        gff = NULL,
        tbi = NULL,
        config = NULL,
        output = NULL,
        browser = NULL
      )
      
      statusMessage(STATUS_READY)
      
      writeLog("Genome browser reset.")
      
    })
    
    #=======================================================
    # Validate Upload
    #=======================================================
    
    uploadReady <- reactive({
      
      !is.null(input$fasta) &&
        
        !is.null(input$gff)
      
    })
    
    observe({
      
      shinyjs::toggleState(
        "load",
        condition = uploadReady() && !processing()
      )
      
      shinyjs::toggleState(
        "reset",
        condition = !processing()
      )
      
    })
    
    #=======================================================
    # Disable During Processing
    #=======================================================
    
    processing <- reactiveVal(FALSE)
    
    
    
    #=======================================================
    # Load Genome
    #=======================================================
    
    observeEvent(input$load,{
      
      req(input$fasta)
      req(input$gff)
      
      processing(TRUE)
      
      statusMessage(STATUS_VALIDATING)
      
      writeLog("Genome loading started.")
      
      tryCatch({
        
        
        #---------------------------------------------------
        # Create Session
        #---------------------------------------------------
        
        sessionInfo <- createSessionFolder(
          USER_DATA_FOLDER
        )
        
        currentSession(
          sessionInfo
        )
        
        writeLog(
          
          paste(
            "Session:",
            sessionInfo$id
          )
          
        )
        
        #---------------------------------------------------
        # Copy Uploaded Files
        #---------------------------------------------------
        
        fasta_dest <- file.path(
          
          sessionInfo$path,
          
          basename(
            input$fasta$name
          )
          
        )
        
        gff_dest <- file.path(
          
          sessionInfo$path,
          
          basename(
            input$gff$name
          )
          
        )
        
        copyUpload(
          
          input$fasta$datapath,
          
          fasta_dest
          
        )
        
        copyUpload(
          
          input$gff$datapath,
          
          gff_dest
          
        )
        
        writeLog(
          "Uploaded files copied."
        )
        writeLog(
          "Preparing genome..."
        )
        #---------------------------------------------------
        # Prepare Genome
        #---------------------------------------------------
        
        statusMessage(
          STATUS_PREPARING
        )
        
        genome <- withProgress(
          
          message = "Preparing Genome",
          
          value = 0,
          
          {
            
            prepareGenome(
              
              fasta_file = fasta_dest,
              
              gff_file = gff_dest,
              
              session_id = sessionInfo$id,
              
              progress = function(msg){
                
                incProgress(
                  
                  0.20,
                  
                  detail = msg
                  
                )
                
              }
              
            )
            
          }
          
        )
        
        currentGenome(
          
          genome
          
        )
        
        #---------------------------------------------------
        # Store Analysis
        #---------------------------------------------------
        
        analysis$jbrowse <- genome
        
        #---------------------------------------------------
        # Browser URL
        #---------------------------------------------------
        
        browserPath <- paste0(
          
          DEFAULT_BROWSER_PAGE,
          
          "?config=user_data/",
          
          sessionInfo$id,
          
          "/",
          
          CONFIG_FILE_NAME
          
        )
        
        cat("\nBrowser URL:", browserPath, "\n")
        cat("\n=============================\n")
        cat("Browser Path:\n")
        cat(browserPath, "\n")
        cat("=============================\n")
        browserURL(
          
          browserPath
          
        )
        
        statusMessage(
          
          STATUS_COMPLETE
          
        )
        
        writeLog(
          
          "Genome loaded successfully."
          
        )
        
      },
      
      error = function(e){
        
        statusMessage(
          
          paste(
            
            STATUS_ERROR,
            
            "\n\n",
            
            e$message
            
          )
          
        )
        
        writeLog(
          
          paste(
            
            "ERROR:",
            
            e$message
            
          )
          
        )
        
        
        
        info <- currentSession()
        
        if(!is.null(info)){
          
          # cleanupSessionFolder(info$path)
          
          currentSession(NULL)
          
          currentGenome(NULL)
          
          analysis$jbrowse <- list(
            session = NULL,
            fasta = NULL,
            fai = NULL,
            gff = NULL,
            tbi = NULL,
            config = NULL,
            output = NULL,
            browser = NULL
          )
        }
        
      },
      
      finally = {
        
        processing(FALSE)
        
      })
      
    })
    #=======================================================
    # Session Cleanup
    #=======================================================
    
    #=======================================================
    # Session Cleanup
    #=======================================================
    
    session$onSessionEnded(function() {
      
      isolate({
        
        info <- currentSession()
        
        if (!is.null(info)) {
          
          writeLog(
            paste("Cleaning session:", info$id)
          )
          
          cleanupSessionFolder(info$path)
          
        }
        
        writeLog("JBrowse session closed.")
        
      })
      
    })
    
    #=======================================================
    # Browser Auto Reset
    #=======================================================
    
    observe({
      
      if(is.null(currentGenome())){
        
        browserURL(
          
          DEFAULT_BROWSER_PAGE
          
        )
        
      }
      
    })
    
    
    
    #=======================================================
    # Show Genome Information
    #=======================================================
    
    observeEvent(currentGenome(),{
      
      req(currentGenome())
      
      writeLog(
        
        paste(
          
          "Genome loaded:",
          
          basename(
            
            currentGenome()$fasta
            
          )
          
        )
        
      )
      
    })
    
  })
  
}
