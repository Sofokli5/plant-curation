# load libraries --------------------
library(data.table)
library(stringr)
library(seqinr)

fasta2tax <- function(fasta_file, output_file){
  # read fasta ------------------------------------------------
  fasta <- read.fasta(fasta_file, as.string = T, whole.header = T)
  headers <- names(fasta)
  
  # parse headers --------------------------------------------------------------
  ids <- str_extract(headers, "^[^.]+")
  taxa <- str_replace(headers, "^[^.]+\\.[^_]*_", "")
  
  # combine and write ------------------------------------
  out <- data.table(id = ids, taxonomy = taxa)
  fwrite(out, output_file, sep = "\t", col.names = FALSE)
}

#optparse----------------------------------------------------------------------------------------

parser <- OptionParser() |>
  add_option(c("-i", "--input"), type = "character",
              help = "Input FASTA file (e.g., ITS.fasta, ITS1.fasta, ITS2.fasta) [required]") |>
  add_option(c("-o", "--output"), type = "character", default = NULL,
              help = "Output .tax file path [default: same name as input with .tax extension]")

arguments <- parse_args(parser)

# Validate -------------------------------------------------------------
if (is.null(opt$input)) {
  print_help(opt_parser)
  stop("\nError: You must specify an input FASTA file with --input.\n", call. = FALSE)
}

# Determine output filename --------------------------------------------
if (is.null(opt$output)) {
  base <- str_replace(basename(opt$input), "\\.fasta$", "")
  opt$output <- file.path(dirname(opt$input), paste0(base, ".tax"))
}

#run------------------------------------------------------------------
fasta2tax(opt$input, opt$output)