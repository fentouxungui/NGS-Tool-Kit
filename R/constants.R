#===========================================================
# constants.R
# Global constants used throughout the JBrowse subsystem
#===========================================================

#-----------------------------------------------------------
# Application
#-----------------------------------------------------------

APP_NAME <- "NGS Tool Kit"

APP_VERSION <- "2.0"

#-----------------------------------------------------------
# Supported File Extensions
#-----------------------------------------------------------

VALID_FASTA_EXTENSIONS <- c(
  
  ".fa",
  
  ".fasta",
  
  ".fna"
  
)

VALID_GFF_EXTENSIONS <- c(
  
  ".gff",
  
  ".gff3"
  
)

VALID_GTF_EXTENSIONS <- c(
  
  ".gtf"
  
)

#-----------------------------------------------------------
# Required External Tools
#-----------------------------------------------------------

REQUIRED_TOOLS <- c(
  
  "Rsamtools",
  
  "Biostrings"
  
)

#-----------------------------------------------------------
# JBrowse
#-----------------------------------------------------------

JBROWSE_ROOT <- file.path(
  
  "www",
  
  "jbrowse"
  
)

USER_DATA_FOLDER <- file.path(
  
  JBROWSE_ROOT,
  
  "user_data"
  
)

DEFAULT_BROWSER_PAGE <- "jbrowse/index.html"

DEFAULT_BROWSER_HEIGHT <- "850px"

#-----------------------------------------------------------
# Generated Files
#-----------------------------------------------------------

FASTA_INDEX_EXTENSION <- ".fai"

COMPRESSED_GFF_EXTENSION <- ".gz"

TABIX_EXTENSION <- ".tbi"

CONFIG_FILE_NAME <- "config.json"

#-----------------------------------------------------------
# Validation
#-----------------------------------------------------------

MIN_FASTA_LINES <- 2

MIN_GFF_COLUMNS <- 9

VALID_FASTA_HEADER <- ">"

#-----------------------------------------------------------
# Feature Types
#-----------------------------------------------------------

SUPPORTED_FEATURES <- c(
  
  "gene",
  
  "mRNA",
  
  "transcript",
  
  "CDS",
  
  "exon",
  
  "five_prime_UTR",
  
  "three_prime_UTR",
  
  "ncRNA",
  
  "tRNA",
  
  "rRNA"
  
)

#-----------------------------------------------------------
# Session
#-----------------------------------------------------------

SESSION_PREFIX <- "JBROWSE"

LOG_FILE <- "jbrowse.log"

#-----------------------------------------------------------
# Upload Limits
#-----------------------------------------------------------

MAX_FASTA_SIZE_GB <- 20

MAX_GFF_SIZE_GB <- 5

#-----------------------------------------------------------
# Status Messages
#-----------------------------------------------------------

STATUS_READY <- "Ready"

STATUS_VALIDATING <- "Validating input files..."

STATUS_PREPARING <- "Preparing genome..."

STATUS_INDEXING <- "Generating genome index..."

STATUS_COMPRESSING <- "Compressing annotation..."

STATUS_CREATING_CONFIG <- "Creating JBrowse configuration..."

STATUS_LOADING <- "Loading genome browser..."

STATUS_COMPLETE <- "Genome loaded successfully."

STATUS_ERROR <- "Genome preparation failed."

#-----------------------------------------------------------
# Supported Genome Formats
#-----------------------------------------------------------

SUPPORTED_GENOME_FORMATS <- c(
  
  "FASTA + GFF3",
  
  "FASTA + GTF"
  
)

#-----------------------------------------------------------
# Genome Statistics Labels
#-----------------------------------------------------------

STAT_LABELS <- list(
  
  chromosomes = "Chromosomes",
  
  genome_size = "Genome Size",
  
  genes = "Genes",
  
  mrna = "mRNA",
  
  cds = "CDS",
  
  exons = "Exons"
  
)
