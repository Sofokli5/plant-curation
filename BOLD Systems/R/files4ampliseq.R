#!/usr/bin/env Rscript

# Libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(stringr)
library(Biostrings)
library(R.utils)
library(optparse)

# Function -------------------------------------------------------------
add_taxonomy_headers <- function(fasta_file, tax_file, output_dir) {
  # Read taxonomy ------------------------------------------------------
  tax <- fread(
    tax_file,
    header = FALSE, sep = "\t",
    col.names = c("ID", "taxonomy")
  )
  
  # Read FASTA ---------------------------------------------------------
  fasta <- readDNAStringSet(fasta_file)
  fasta_dt <- data.table(
    header = names(fasta),
    sequence = as.character(fasta)
  )
  
  fasta_dt[, ID := str_extract(header, "^[^.]+")]
  
  # Merge taxonomy -----------------------------------------------------
  merged <- merge(fasta_dt, tax, by = "ID", all.x = TRUE)
  
  dir.create(output_dir, showWarnings = FALSE)
  
  # fasta 1 taxonomy as header ---------------------------------------
  fasta_taxonomy <- merged %>%
    select(header = taxonomy, sequence)
  
  tax_out <- file.path(output_dir, "taxonomy_headers.fasta")
  write_lines(
    paste0(">", fasta_taxonomy$header, "\n", fasta_taxonomy$sequence),
    tax_out
  )
  gzip(tax_out)
  
  # fasta 2 sample ID and only species --------------------------------------
  merged <- merged %>%
    mutate(species = word(taxonomy, -1, sep = fixed(";"))) %>%
    filter(!is.na(species), species != "NA")
  
  fasta_species <- merged %>%
    mutate(header = paste(ID, species)) %>%
    select(header, sequence)
  
  sp_out <- file.path(output_dir, "id_species.fasta")
  write_lines(
    paste0(">", fasta_species$header, "\n", fasta_species$sequence),
    sp_out
  )
  gzip(sp_out)
}

# optparse -------------------------------------------------------------
parser <- OptionParser() |>
  add_option(c("-f", "--fasta"), type = "character",
              help = "Input FASTA file [required]")|>
  add_option(c("-t", "--tax"), type = "character",
              help = "Input taxonomy file (.tax) [required]")|>
  add_option(c("-o", "--output_dir"), type = "character", default = ".",
              help = "Output directory [default: current directory]")

arguments <- parse_args(parser)

# Validate -------------------------------------------------------------
if (is.null(opt$fasta) || is.null(opt$tax)) {
  print_help(opt_parser)
  stop("\nError: both --fasta and --tax are required.\n", call. = FALSE)
}

# Run ------------------------------------------------------------------
add_taxonomy_headers(opt$fasta, opt$tax, opt$output_dir)
