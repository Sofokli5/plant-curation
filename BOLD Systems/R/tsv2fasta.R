# load libraries --------------------

library(data.table)
library(stringr)
library(seqinr)

# helper function ------------------

tsv2fasta <- function(input_sequences_table, output_dir) {
  # input data ----------------------

  df <- fread(input_sequences_table, sep = "\t", header = TRUE, na.strings = c("", "NA"), fill = Inf)

  # filtering out --------------------------

  df <- df |> subset(!is.na(nuc))

  df$nuc <- df$nuc |> str_remove_all("\\-") |> str_to_upper()

  # split per marker ------------------

  its0 <- df |> subset(marker_code %in% c("ITS", "ITS1", "ITS2"))
  trnl <- df |> subset(marker_code == "trnL")

  # Make headers -----------------------

  its0[, tax_col := paste(kingdom, phylum, class, order, family, genus, species, sep = ";")]
  trnl[, tax_col := paste(kingdom, phylum, class, order, family, genus, species, sep = ";")]
  
  #its0 <- its0[, by = .(tax_col, nuc), head(.SD, 1)]
  #trnl <- trnl[, by = .(tax_col, nuc), head(.SD, 1)]
  
  #its0[, fasta_col := paste0(record_id, "_", tax_col)]
  #trnl[, fasta_col := paste0(record_id, "_", tax_col)]
  
  # workflow -----------------------

  dir.create(output_dir, showWarnings = FALSE)

  write.fasta(sequences = as.list(its0$nuc), names = as.list(its0$record_id), file.out = paste0(output_dir, "/ITS0.fasta"), nbchar = 120)
  write.fasta(sequences = as.list(trnl$nuc), names = as.list(trnl$record_id), file.out = paste0(output_dir, "/trnL.fasta"), nbchar = 120)

  #R.utils::gzip(paste0(output_dir, "/ITS0.fasta"))
  #R.utils::gzip(paste0(output_dir, "/trnL.fasta"))

  its0_tax <- its0[, .(processid, tax_col)]
  trnl_tax <- trnl[, .(processid, tax_col)]

  fwrite(its0_tax, paste0(output_dir, "/ITS0.tax"), sep = "\t", row.names = FALSE, col.names = F, quote = FALSE)
  fwrite(trnl_tax, paste0(output_dir, "/trnL.tax"), sep = "\t", row.names = FALSE, col.names = F, quote = FALSE)
}

# optparse ----------------------------


library(optparse)

parser <- OptionParser() |>
  add_option(c("-i", "--input_seq"), action = "store", default = "download", help = "Input sequences as downloaded from BOLD Systems") |>
  add_option(c("-o", "--output"), action = "store", default = "r-curation", help = "Output folder")

arguments <- parse_args(parser)


# call function --------------

tsv2fasta(arguments$input_seq, arguments$output)
