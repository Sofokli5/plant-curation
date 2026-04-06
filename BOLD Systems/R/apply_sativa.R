library(data.table)

correct_taxonomy <- function(tax_file, mis_file, out_file){
  tax <- fread(tax_file, header = F, sep = "\t")
  mis <- fread(mis_file, header = T, sep = "\t")
  
  setnames(tax, c("ID", "Taxonomy"))
  setnames(mis, old = ";SeqID", new = "ID")
  
  oldtax <- copy(tax)
  
  tax[mis, on = "ID", Taxonomy := ProposedTaxonomyPath]
  
  changed <- oldtax[Taxonomy != tax$Taxonomy]
  
  fwrite(tax, out_file, sep = "\t", quote = FALSE)
}

# optparse ---------------------------------------------------------------------

parser <- OptionParser() |>
  add_option(c("-t", "--tax"), type = "character",
              help = "Input taxonomy file (.tax) [required]")|>
  add_option(c("-m", "--mis"), type = "character",
              help = "Input misidentification file (.mis) [required]")|>
  add_option(c("-o", "--output"), type = "character", default = NULL,
              help = "Output corrected taxonomy file [default: <input>_corrected.tax]")

arguments <- parse_args(parser)

# validate ---------------------------------------------------------------------
if (is.null(opt$tax) || is.null(opt$mis)) {
  print_help(opt_parser)
  stop("\nError: You must specify both --tax and --mis files.\n", call. = FALSE)
}

# default output name ---------------------------
if (is.null(opt$output)) {
  base <- sub("\\.tax$", "", basename(opt$tax))
  opt$output <- paste0(base, "_corrected.tax")
}

# run -----------------------------------------
correct_taxonomy(opt$tax, opt$mis, opt$output)
