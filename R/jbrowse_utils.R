#===========================================================
# jbrowse_utils.R
# NGS Tool Kit v2.0
# Genome Preparation Utilities
# NOTE: Uses native R/Rsamtools - no WSL required
#===========================================================


#===========================================================
# Helper Functions
#===========================================================

#-----------------------------------------------------------
# Check dependency availability (R-package based)
#-----------------------------------------------------------
verifyDependencies <- function(){
  
  deps <- checkDependencies()
  
  if(!deps$valid){
    
    missing <- names(
      deps$status
    )[!deps$status]
    
    stop(
      
      paste(
        
        "Missing required R packages:",
        
        paste(
          missing,
          collapse = ", "
        )
        
      ),
      
      call. = FALSE
      
    )
    
  }
  
  TRUE
  
}

#===========================================================
# FASTA Index Generation (native Rsamtools - no WSL)
#===========================================================

generateFAI <- function(fasta_file){
  
  checkFile(fasta_file)
  
  writeLog(
    
    paste(
      
      "Generating FASTA index:",
      
      basename(fasta_file)
      
    )
    
  )
  
  tryCatch({
    
    Rsamtools::indexFa(fasta_file)
    
    writeLog("FASTA indexing completed.")
    
  }, error = function(e){
    
    stop(
      paste("FASTA indexing failed:", e$message),
      call. = FALSE
    )
    
  })
  
  fai_file <- paste0(
    
    fasta_file,
    
    FASTA_INDEX_EXTENSION
    
  )
  
  checkFile(fai_file)
  
  return(fai_file)
  
}
#===========================================================
# Sort GFF3 (pure R - no WSL)
#===========================================================

sortGFF <- function(gff_file){
  
  checkFile(gff_file)
  
  writeLog(
    paste(
      "Sorting GFF:",
      basename(gff_file)
    )
  )
  
  sorted_gff <- file.path(
    dirname(gff_file),
    "annotation.sorted.gff"
  )
  
  tryCatch({
    
    lines <- readLines(gff_file, warn = FALSE)
    
    # Separate header/comment lines from feature lines
    header_lines  <- lines[grepl("^#", lines)]
    feature_lines <- lines[!grepl("^#", lines)]
    feature_lines <- feature_lines[nzchar(feature_lines)]
    
    if(length(feature_lines) > 0){
      
      # Parse seqname (col 1) and start (col 4) for sorting
      split_lines <- strsplit(feature_lines, "\t")
      
      seqnames <- vapply(split_lines,
                         function(x) if(length(x) >= 1) x[1] else "",
                         character(1))
      
      starts <- suppressWarnings(
        vapply(split_lines,
               function(x) if(length(x) >= 4) as.integer(x[4]) else NA_integer_,
               integer(1))
      )
      starts[is.na(starts)] <- 0L
      
      ord <- order(seqnames, starts)
      feature_lines <- feature_lines[ord]
      
    }
    
    writeLines(c(header_lines, feature_lines), sorted_gff)
    
    writeLog("GFF sorting completed.")
    
  }, error = function(e){
    
    stop(
      paste("GFF sorting failed:", e$message),
      call. = FALSE
    )
    
  })
  
  checkFile(sorted_gff)
  
  return(sorted_gff)
}
#===========================================================
# GFF Compression (native Rsamtools::bgzip - no WSL)
#===========================================================

compressGFF <- function(gff_file){
  
  checkFile(gff_file)
  
  writeLog(
    
    paste(
      
      "Compressing GFF:",
      
      basename(gff_file)
      
    )
    
  )
  
  gz_file <- paste0(
    
    gff_file,
    
    COMPRESSED_GFF_EXTENSION
    
  )
  
  tryCatch({
    
    Rsamtools::bgzip(gff_file, dest = gz_file, overwrite = TRUE)
    
    writeLog("GFF compression completed.")
    
  }, error = function(e){
    
    stop(
      paste("GFF compression failed:", e$message),
      call. = FALSE
    )
    
  })
  
  checkFile(gz_file)
  
  return(gz_file)
  
}
#===========================================================
# Tabix Index Generation (native Rsamtools - no WSL)
#===========================================================

indexGFF <- function(gz_file){
  
  checkFile(gz_file)
  
  writeLog(
    
    paste(
      
      "Creating Tabix index:",
      
      basename(gz_file)
      
    )
    
  )
  
  tryCatch({
    
    Rsamtools::indexTabix(
      gz_file,
      format  = "gff",
      seq     = 1L,
      start   = 4L,
      end     = 5L
    )
    
    writeLog("Tabix indexing completed.")
    
  }, error = function(e){
    
    stop(
      paste("Tabix indexing failed:", e$message),
      call. = FALSE
    )
    
  })
  
  tbi_file <- paste0(
    
    gz_file,
    
    TABIX_EXTENSION
    
  )
  
  checkFile(tbi_file)
  
  return(tbi_file)
  
}

#===========================================================
# Standardize Uploaded Files
#===========================================================

standardizeGenomeFiles <- function(
    fasta_file,
    gff_file
){
  
  checkFile(fasta_file)
  
  checkFile(gff_file)
  
  output_dir <- dirname(fasta_file)
  
  
  std_fasta <- file.path(
    output_dir,
    "genome.fasta"
  )
  
  std_gff <- file.path(
    output_dir,
    "annotation.gff"
  )
  
  if(normalizePath(
    fasta_file,
    winslash="/"
  ) != normalizePath(
    std_fasta,
    winslash="/",
    mustWork=FALSE
  )){
    
    if (file.exists(std_fasta)) {
      unlink(std_fasta)
    }
    
    if (file.exists(std_fasta)) {
      unlink(std_fasta)
    }
    
    
    ok <- file.rename(
      fasta_file,
      std_fasta
    )
    
    if (!ok) {
      
      stop(
        "Unable to rename FASTA file.",
        call. = FALSE
      )
      
    }
    
    writeLog(paste("FASTA standardized to:", basename(std_fasta)))
    
    if(!ok){
      
      stop(
        
        "Unable to rename FASTA file.",
        
        call.=FALSE
        
      )
      
    }
    
  }
  
  if(normalizePath(
    gff_file,
    winslash="/"
  ) != normalizePath(
    std_gff,
    winslash="/",
    mustWork=FALSE
  )){
    
    message("gff_file = ", gff_file)
    message("std_gff = ", std_gff)
    message("gff exists = ", file.exists(gff_file))
    message("std exists = ", file.exists(std_gff))
    
    
    if (file.exists(std_gff)) {
      unlink(std_gff)
    }
    
    ok <- file.rename(
      gff_file,
      std_gff
    )
    
    if (!ok) {
      
      stop(
        "Unable to rename GFF file.",
        call. = FALSE
      )
      
    }
    
    writeLog(paste("GFF standardized to:", basename(std_gff)))
    
  }
  
  list(
    
    fasta = std_fasta,
    
    gff = std_gff,
    
    output = output_dir
    
  )
  
}

#===========================================================
# Verify Generated Files
#===========================================================

verifyGenomeOutputs <- function(
    
  fasta_file,
  
  fai_file,
  
  gz_file,
  
  tbi_file,
  
  config_file
  
){
  
  files <- c(
    
    fasta_file,
    
    fai_file,
    
    gz_file,
    
    tbi_file,
    
    config_file
    
  )
  
  missing <- files[
    !file.exists(files)
  ]
  
  if(length(missing) > 0){
    
    stop(
      
      paste(
        
        "Missing generated files:\n",
        
        paste(
          basename(missing),
          collapse="\n"
        )
        
      ),
      
      call.=FALSE
      
    )
    
  }
  
  TRUE
  
}

#===========================================================
# Clean Temporary Files
#===========================================================

cleanupTemporaryFiles <- function(
    
  output_dir
  
){
  
  tmp <- list.files(
    
    output_dir,
    
    pattern="\\.tmp$|\\.bak$",
    
    full.names=TRUE
    
  )
  
  if(length(tmp)){
    
    unlink(
      
      tmp,
      
      force=TRUE
      
    )
    
  }
  
  invisible(TRUE)
  
}
#===========================================================
# Genome Preparation Pipeline
#===========================================================

prepareGenome <- function(
    fasta_file,
    gff_file,
    session_id = NULL,
    progress = NULL
){
  
  writeLog("==========================================")
  writeLog("Genome Preparation Started")
  writeLog("==========================================")
  
  #---------------------------------------------------------
  # Check Dependencies
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress("Checking dependencies...")
  
  verifyDependencies()
  
  #---------------------------------------------------------
  # Validate Uploaded Files
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress(STATUS_VALIDATING)
  
  validation <- validateGenomeFiles(
    fasta_file,
    gff_file
  )
  
  if(!validation$valid)
    stop(validation$message, call. = FALSE)
  
  #---------------------------------------------------------
  # Standardize File Names
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress("Preparing uploaded files...")
  
  files <- standardizeGenomeFiles(
    fasta_file,
    gff_file
  )
  
  fasta_file <- files$fasta
  gff_file   <- files$gff
  output_dir <- files$output
  
  writeLog("Files standardized.")
  
  #---------------------------------------------------------
  # Generate FASTA Index
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress(STATUS_INDEXING)
  
  fai_file <- generateFAI(
    fasta_file
  )
  
  #---------------------------------------------------------
  # Compress Annotation
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress(STATUS_COMPRESSING)
  
  sorted_gff <- sortGFF(
    gff_file
  )
  
  gz_file <- compressGFF(
    sorted_gff
  )
  standard_gz <- file.path(
    dirname(gz_file),
    "annotation.gff.gz"
  )
  
  if (normalizePath(gz_file, winslash = "/", mustWork = FALSE) !=
      normalizePath(standard_gz, winslash = "/", mustWork = FALSE)) {
    
    if (file.exists(standard_gz))
      unlink(standard_gz)
    
    file.rename(gz_file, standard_gz)
    
    
    gz_file <- standard_gz
  }
  
  #---------------------------------------------------------
  # Generate Tabix Index
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress("Generating Tabix index...")
  
  tbi_file <- indexGFF(
    gz_file
  )
  
  #---------------------------------------------------------
  # Create JBrowse Configuration
  #---------------------------------------------------------
  
  if(!is.null(progress))
    progress(STATUS_CREATING_CONFIG)
  
  config_file <- generateConfig(
    
    output_dir = output_dir,
    
    assembly_name =
      if(is.null(session_id))
        "Genome"
    else
      session_id,
    
    fasta_file = basename(fasta_file),
    
    gff_file = basename(gz_file)
    
  )
  
  checkFile(config_file)
  
  writeLog("Configuration generated.")
  
  #---------------------------------------------------------
  # Validate Generated Output
  #---------------------------------------------------------
  
  validation <- validateOutputFiles(
    output_dir
  )
  
  if(!validation$valid)
    stop(validation$message, call. = FALSE)
  
  verifyGenomeOutputs(
    
    fasta_file,
    
    fai_file,
    
    gz_file,
    
    tbi_file,
    
    config_file
    
  )
  
  #---------------------------------------------------------
  # Cleanup
  #---------------------------------------------------------
  
  cleanupTemporaryFiles(
    output_dir
  )
  if(file.exists(sorted_gff))
    unlink(sorted_gff)
  
  writeLog("Genome preparation completed successfully.")
  
  writeLog("==========================================")
  
  #---------------------------------------------------------
  # Return Results
  #---------------------------------------------------------
  
  return(
    
    list(
      
      session = session_id,
      
      output = output_dir,
      
      fasta = fasta_file,
      
      fai = fai_file,
      
      gff = gz_file,
      
      tbi = tbi_file,
      
      config = config_file,
      
      browser = file.path(
        "user_data",
        basename(output_dir),
        CONFIG_FILE_NAME
      )
      
    )
    
  )
  
}
