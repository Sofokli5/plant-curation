#load libraries
library(stringr)
library(seqinr)
library(data.table)
# produce new .tax files

filter_tax <- function(input_fasta, input_tax, out_tax, out_dir){
  
  fasta <- read.fasta(input_fasta)
  
  ids <- sub("\\..*", "", names(fasta))
  
  tax <- fread(input_tax, sep="\t", header=FALSE)
  
  filtered <- tax[tax[[1]] %in% ids, ]

  write.table(filtered, paste0(out_dir, out_tax), sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
}

library(optparse)

parser <- OptionParser() |>
  add_option(c("-i", "--input_fasta"), type = "character", help = "Input fasta as created by MAFFT aligner, mandatory") |>
  add_option(c("-t", "--input_tax"), type = "character", help = "Input .tax file with two columns (entry IDs and taxonomy)") |>
  add_option(c("-o", "--out_dir"), type = "character", help = "Output directory [default: input_fasta directory]")|>
  add_option("--out_tax", type = "character", help = "Output .tax file name, be sure to know what taxonomy file describes what .fasta [default: input_fasta name]")

arguments <- parse_args(parser)

if (is.null(arguments$input_fasta) || is.null(arguments$input_tax)) {
  print_help(parser)
  stop("\nError: You must specify --input_fasta and --input_tax. \n", call. = FALSE)
}

# Determine output filename --------------------------------------------
if (is.null(arguments$out_tax)) {
  base <- str_replace(basename(arguments$input_fasta), "\\.fasta$", "")
  arguments$out_tax <- paste0(base, ".tax")
}

if (is.null(arguments$out_dir)) {
  arguments$out_dir <- paste0(file.path(dirname(arguments$input_fasta),"/"))
}

filter_tax(arguments$input_fasta, arguments$input_tax, arguments$out_tax, arguments$out_dir)
