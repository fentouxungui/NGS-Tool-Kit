#===========================================================
# jbrowse_validation.R
# Validation utilities for NGS Tool Kit
#===========================================================

validateFASTA <- function(file) {
  if (missing(file) || is.null(file) || !nzchar(file))
    return(list(valid=FALSE,message="No FASTA file supplied."))
  if (!file.exists(file))
    return(list(valid=FALSE,message="FASTA file does not exist."))
  ext <- paste0(".",tolower(tools::file_ext(file)))
  if (!ext %in% VALID_FASTA_EXTENSIONS)
    return(list(valid=FALSE,message="Unsupported FASTA extension."))
  if (file.info(file)$size == 0)
    return(list(valid=FALSE,message="FASTA file is empty."))
  x <- readLines(file,n=20,warn=FALSE)
  first <- x[nzchar(trimws(x))][1]
  if (is.na(first) || !startsWith(first,">"))
    return(list(valid=FALSE,message="Invalid FASTA header."))
  list(valid=TRUE,
       sequences=sum(grepl("^>",readLines(file,warn=FALSE))),
       message="FASTA validation successful.")
}

validateGFF <- function(file){
  if (missing(file)||is.null(file)||!file.exists(file))
    return(list(valid=FALSE,message="GFF file not found."))
  ext <- paste0(".",tolower(tools::file_ext(file)))
  if (!ext %in% VALID_GFF_EXTENSIONS)
    return(list(valid=FALSE,message="Unsupported GFF extension."))
  x <- readLines(file,warn=FALSE)
  d <- x[!grepl("^#",x)][1]
  if (is.na(d))
    return(list(valid=FALSE,message="No feature records found."))
  if (length(strsplit(d,"\t")[[1]]) < MIN_GFF_COLUMNS)
    return(list(valid=FALSE,message="GFF must contain 9 columns."))
  list(valid=TRUE,message="GFF validation successful.")
}

validateGenomeFiles <- function(fasta_file,gff_file){
  f <- validateFASTA(fasta_file)
  if(!f$valid) return(f)
  g <- validateGFF(gff_file)
  if(!g$valid) return(g)
  list(valid=TRUE,message="Genome files validated.")
}

validateOutputFiles <- function(folder){
  required_files <- c(
    "genome.fasta",
    "genome.fasta.fai",
    "annotation.gff.gz",
    "annotation.gff.gz.tbi",
    "config.json"
  )
  miss <- required_files[!file.exists(file.path(folder,required_files))]
  if(length(miss))
    return(list(valid=FALSE,missing=miss,
                message=paste("Missing:",paste(miss,collapse=", "))))
  list(valid=TRUE,message="Output validation successful.")
}

checkDependencies <- function(){
  s <- checkSystemRequirements()
  list(valid=all(s),status=s)
}

validateSessionFolder <- function(path){
  if(!dir.exists(path))
    dir.create(path,recursive=TRUE,showWarnings=FALSE)
  list(valid=dir.exists(path),message="Session folder checked.")
}

validateConfig <- function(config_file){
  if(!file.exists(config_file))
    return(list(valid=FALSE,message="config.json missing."))
  jsonlite::fromJSON(config_file)
  list(valid=TRUE,message="Configuration valid.")
}

countGenomeStats <- function(fasta_file,gff_file){
  fa <- readLines(fasta_file,warn=FALSE)
  gf <- readLines(gff_file,warn=FALSE)
  features <- gf[!grepl("^#",gf)]
  types <- vapply(strsplit(features,"\t"),
                  function(z) if(length(z)>=3) z[3] else "",
                  character(1))
  list(
    chromosomes=sum(grepl("^>",fa)),
    genome_size=sum(nchar(fa[!grepl("^>",fa)])),
    genes=sum(types=="gene"),
    mrna=sum(types %in% c("mRNA","transcript")),
    cds=sum(types=="CDS"),
    exons=sum(types=="exon")
  )
}
