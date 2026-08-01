#===========================================================
# jbrowse_config.R
# Create JBrowse configuration for a prepared genome
#===========================================================

createReferenceAssembly <- function(name,
                                    fasta_uri,
                                    fai_uri){

  list(
    name = name,
    sequence = list(
      type = "ReferenceSequenceTrack",
      trackId = paste0(name, "_ref"),
      adapter = list(
        type = "IndexedFastaAdapter",
        fastaLocation = list(uri = fasta_uri),
        faiLocation = list(uri = fai_uri)
      )
    )
  )
}

createAnnotationTrack <- function(name,
                                  gff_uri,
                                  tbi_uri){

  list(
    type = "FeatureTrack",
    trackId = paste0(name, "_gff"),
    name = paste(name, "Annotations"),
    assemblyNames = list(name),
    adapter = list(
      type = "Gff3TabixAdapter",
      gffGzLocation = list(uri = gff_uri),
      index = list(
        location = list(uri = tbi_uri)
      )
    )
  )
}

createDefaultSession <- function(name){

  list(
    name = "Default Session",
    view = list(
      id = "linearGenomeView",
      type = "LinearGenomeView",
      tracks = list(
        list(
          type = "FeatureTrack",
          configuration = paste0(name, "_gff")
        )
      )
    )
  )
}

createConfig <- function(output_dir,
                         assembly_name = "Genome",
                         fasta_file = "genome.fasta",
                         gff_file = "annotation.gff.gz"){

  if(!dir.exists(output_dir))
    stop("Output directory does not exist.", call. = FALSE)

  cfg <- list(

    assemblies = list(
      createReferenceAssembly(
        assembly_name,
        fasta_file,
        paste0(fasta_file, ".fai")
      )
    ),

    tracks = list(
      createAnnotationTrack(
        assembly_name,
        gff_file,
        paste0(gff_file, ".tbi")
      )
    ),

    defaultSession = createDefaultSession(
      assembly_name
    )
  )

  cfg
}

writeConfig <- function(config,
                        output_dir){

  if(!dir.exists(output_dir))
    dir.create(output_dir,
               recursive = TRUE,
               showWarnings = FALSE)

  outfile <- file.path(
    output_dir,
    CONFIG_FILE_NAME
  )

  jsonlite::write_json(
    config,
    outfile,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  invisible(outfile)
}

generateConfig <- function(output_dir,
                           assembly_name = "Genome",
                           fasta_file = "genome.fasta",
                           gff_file = "annotation.gff.gz"){

  cfg <- createConfig(
    output_dir = output_dir,
    assembly_name = assembly_name,
    fasta_file = fasta_file,
    gff_file = gff_file
  )

  writeConfig(cfg, output_dir)
}

