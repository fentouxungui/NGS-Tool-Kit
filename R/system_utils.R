#===========================================================
# system_utils.R
# Utility functions for WSL execution, logging, and session
# management used by the JBrowse module.
#===========================================================

#-----------------------------------------------------------
# Execute a command inside WSL
#-----------------------------------------------------------
runWSL <- function(command,
                   intern = TRUE,
                   ignore.stderr = FALSE) {
  
  full_cmd <- paste("wsl", command)
  
  tryCatch({
    
    cat("\n=============================\n")
    cat("Executing:\n")
    cat(full_cmd)
    cat("\n=============================\n")
    
    output <- system(
      full_cmd,
      intern = intern,
      ignore.stderr = FALSE
    )
    
    print(output)
    
    return(output)
    
  }, error = function(e) {
    
    stop(
      sprintf(
        "WSL command failed:\n%s\n\n%s",
        full_cmd,
        e$message
      ),
      call. = FALSE
    )
    
  })
  
}

#-----------------------------------------------------------
# Check if a required R package is available
#-----------------------------------------------------------
checkDependency <- function(pkg) {
  
  requireNamespace(pkg, quietly = TRUE)
  
}

#-----------------------------------------------------------
# Check all required R packages
#-----------------------------------------------------------
checkSystemRequirements <- function() {
  
  pkgs <- c(
    
    "Rsamtools",
    "Biostrings"
    
  )
  
  status <- sapply(
    
    pkgs,
    
    checkDependency,
    
    USE.NAMES = TRUE
    
  )
  
  status
  
}

#-----------------------------------------------------------
# Create a unique session directory
#-----------------------------------------------------------
createSessionFolder <- function(base_dir) {
  
  session_id <- paste0(
    
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    
    "_",
    
    paste(sample(c(letters, LETTERS, 0:9),
                 8,
                 replace = TRUE),
          collapse = "")
    
  )
  
  session_path <- file.path(
    
    base_dir,
    
    session_id
    
  )
  
  dir.create(
    
    session_path,
    
    recursive = TRUE,
    
    showWarnings = FALSE
    
  )
  
  list(
    
    id = session_id,
    
    path = normalizePath(
      session_path,
      winslash = "/",
      mustWork = FALSE
    )
    
  )
  
}

#-----------------------------------------------------------
# Delete session directory
#-----------------------------------------------------------
cleanupSessionFolder <- function(path) {
  
  if (dir.exists(path)) {
    
    unlink(
      
      path,
      
      recursive = TRUE,
      
      force = TRUE
      
    )
    
  }
  
}

#-----------------------------------------------------------
# Write log messages
#-----------------------------------------------------------
writeLog <- function(message,
                     logfile = "jbrowse.log") {
  
  timestamp <- format(
    
    Sys.time(),
    
    "%Y-%m-%d %H:%M:%S"
    
  )
  
  cat(
    
    sprintf(
      
      "[%s] %s\n",
      
      timestamp,
      
      message
      
    ),
    
    file = logfile,
    
    append = TRUE
    
  )
  
}

#-----------------------------------------------------------
# Copy uploaded file
#-----------------------------------------------------------
copyUpload <- function(source,
                       destination) {
  
  success <- file.copy(
    
    source,
    
    destination,
    
    overwrite = TRUE
    
  )
  
  if (!success) {
    
    stop(
      
      sprintf(
        
        "Failed to copy %s",
        
        basename(source)
        
      ),
      
      call. = FALSE
      
    )
    
  }
  
  invisible(destination)
  
}

#-----------------------------------------------------------
# Check file existence
#-----------------------------------------------------------
checkFile <- function(file) {
  
  if (!file.exists(file)) {
    
    stop(
      
      sprintf(
        
        "File not found:\n%s",
        
        file
        
      ),
      
      call. = FALSE
      
    )
    
  }
  
  TRUE
  
}

#-----------------------------------------------------------
# Get file size (human readable)
#-----------------------------------------------------------
humanFileSize <- function(file) {
  
  size <- file.info(file)$size
  
  units <- c("B", "KB", "MB", "GB", "TB")
  
  power <- floor(log(size, 1024))
  
  power <- max(0, min(power, length(units) - 1))
  
  sprintf(
    
    "%.2f %s",
    
    size / (1024^power),
    
    units[power + 1]
    
  )
  
}

#-----------------------------------------------------------
# Print system information
#-----------------------------------------------------------
systemSummary <- function() {
  
  list(
    
    R = R.version.string,
    
    OS = Sys.info()["sysname"],
    
    WSL = checkSystemRequirements()
    
  )
  
}
