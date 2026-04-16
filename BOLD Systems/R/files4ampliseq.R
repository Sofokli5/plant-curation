# Libraries ------------------------------------------------------------
library(data.table)
library(stringr)
library(seqinr)
library(optparse)

# Function -------------------------------------------------------------
add_taxonomy_headers <- function(fasta_file, tax_file, output_dir) {
  
  # Read taxonomy using data.table (fast & efficient)
  tax <- fread(
    tax_file,
    header = FALSE, sep = "\t",
    col.names = c("ID", "taxonomy")
  )
  
  # Read FASTA using seqinr
  # seqinr reads headers and sequences into a list
  fasta_list <- read.fasta(fasta_file, seqtype = "DNA", as.string = TRUE, forceDNAtolower = FALSE)
  
  # Convert list to data.table
  fasta_dt <- data.table(
    header = names(fasta_list),
    sequence = as.character(unlist(fasta_list))
  )
  
  # Extract ID (matching the logic in your original script)
  fasta_dt[, ID := str_extract(header, "^[^|]+")]
  
  # Merge taxonomy (standard data.table merge)
  merged <- merge(fasta_dt, tax, by = "ID", all.x = TRUE)
  
  # Create output directory
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # --- Output 1: Taxonomy as Header ---
  tax_out <- file.path(output_dir, "taxonomy_headers.fasta")
  
  # Create the FASTA format content using data.table's speed
  # We use paste0 to format and write directly
  writeLines(
    merged[!is.na(taxonomy), paste0(">", taxonomy, ";", "\n", sequence)],
    con = tax_out
  )
  R.utils::gzip(tax_out, overwrite = TRUE)
  
  # --- Output 2: Sample ID and only species ---
  # Use stringr 'word' as requested
  merged[, species := word(taxonomy, -1, sep = fixed(";"))]
  
  # Filter out NAs
  fasta_species <- merged[!is.na(species) & species != "NA"]
  
  sp_out <- file.path(output_dir, "id_species.fasta")
  writeLines(
    fasta_species[, paste0(">", ID, " ", species, "\n", sequence)],
    con = sp_out
  )
  R.utils::gzip(sp_out, overwrite = TRUE)
}

# optparse -------------------------------------------------------------
parser <- OptionParser() |>
  add_option(c("-f", "--fasta"), type = "character",
             help = "Input FASTA file [required]") |>
  add_option(c("-t", "--tax"), type = "character",
             help = "Input taxonomy file (.tax) [required]") |>
  add_option(c("-o", "--output_dir"), type = "character", default = ".",
             help = "Output directory [default: current directory]")

arguments <- parse_args(parser)

# Validate -------------------------------------------------------------
if (is.null(arguments$fasta) || is.null(arguments$tax)) {
  print_help(parser)
  stop("\nError: both --fasta and --tax are required.\n", call. = FALSE)
}

# Run ------------------------------------------------------------------
add_taxonomy_headers(arguments$fasta, arguments$tax, arguments$output_dir)