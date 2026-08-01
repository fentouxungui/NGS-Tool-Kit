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
# Delete session directory or file safely
#-----------------------------------------------------------
cleanupSessionFolder <- function(path) {
  
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))
  
  # Trigger garbage collection to release open handles (e.g. Rsamtools/Tabix)
  suppressWarnings(gc(verbose = FALSE))
  
  if (dir.exists(path)) {
    
    unlink(
      path,
      recursive = TRUE,
      force = TRUE
    )
    
    if (dir.exists(path) && .Platform$OS.type == "windows") {
      target <- chartr("/", "\\", normalizePath(path, mustWork = FALSE))
      system(paste('cmd /c rmdir /s /q', shQuote(target)), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    
  } else if (file.exists(path)) {
    
    unlink(
      path,
      force = TRUE
    )
    
    if (file.exists(path) && .Platform$OS.type == "windows") {
      target <- chartr("/", "\\", normalizePath(path, mustWork = FALSE))
      system(paste('cmd /c del /f /q /a', shQuote(target)), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    
  }
  
  invisible(!dir.exists(path) && !file.exists(path))
  
}

#-----------------------------------------------------------
# Clean up stale/orphan sessions and loose files in user_data
#-----------------------------------------------------------
cleanupStaleSessions <- function(base_dir, max_age_hours = 2) {
  
  if (missing(base_dir) || is.null(base_dir) || !dir.exists(base_dir)) {
    return(invisible(0))
  }
  
  # Force garbage collection to free file locks
  suppressWarnings(gc(verbose = FALSE))
  
  items <- list.files(base_dir, full.names = TRUE, include.dirs = TRUE)
  
  if (length(items) == 0) return(invisible(0))
  
  cleaned_count <- 0
  now <- Sys.time()
  
  for (item in items) {
    
    # Loose files directly in user_data folder (not subdirectories)
    if (!dir.exists(item)) {
      if (cleanupSessionFolder(item)) {
        cleaned_count <- cleaned_count + 1
      }
      next
    }
    
    # Session directories - try parsing session folder timestamp format (YYYYMMDD_HHMMSS)
    folder_name <- basename(item)
    ts_str <- substr(folder_name, 1, 15)
    folder_time <- suppressWarnings(as.POSIXct(strptime(ts_str, "%Y%m%d_%H%M%S", tz = Sys.timezone())))
    
    if (is.na(folder_time)) {
      info <- file.info(item)
      folder_time <- as.POSIXct(info$ctime)
    }
    
    if (!is.na(folder_time)) {
      
      age_hours <- as.numeric(difftime(now, folder_time, units = "hours"))
      
      if (!is.na(age_hours) && age_hours >= max_age_hours) {
        
        if (cleanupSessionFolder(item)) {
          cleaned_count <- cleaned_count + 1
        }
        
      }
      
    }
    
  }
  
  if (cleaned_count > 0 && exists("writeLog", mode = "function")) {
    
    writeLog(
      sprintf("Cleaned %d stale session items from %s", cleaned_count, base_dir)
    )
    
  }
  
  invisible(cleaned_count)
  
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

#-----------------------------------------------------------
# Persistent Visitor Counter
#-----------------------------------------------------------
get_and_increment_visitor_count <- function(file = "visitor_count.txt") {
  
  count <- 0
  
  if (file.exists(file)) {
    
    val <- suppressWarnings(as.integer(readLines(file, warn = FALSE)[1]))
    
    if (!is.na(val) && val >= 0) count <- val
    
  }
  
  count <- count + 1
  
  tryCatch({
    
    writeLines(as.character(count), file)
    
  }, error = function(e) NULL)
  
  return(count)
  
}
