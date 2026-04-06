library(seqinr)
library(stringr)

concatITS <- function(its1, s58, its2) {
  s1 <- its1 |> read.fasta()
  s2 <- s58 |> read.fasta()
  s3 <- its2 |> read.fasta()

  names(s1) <- s1 |>
    names() |>
    str_split_i("\\|", 1)
  names(s2) <- s2 |>
    names() |>
    str_split_i("\\|", 1)
  names(s3) <- s3 |>
    names() |>
    str_split_i("\\|", 1)

  overlap <- intersect(names(s1), names(s2))
  overlap <- intersect(overlap, names(s3))

  s1 <- s1[overlap]
  s2 <- s2[overlap]
  s3 <- s3[overlap]

  concat <- mapply(function(x, y, z) {
    o <- c(x, y, z)
  }, s1, s2, s3)

  return(concat)
}

# optparse -------------------------------------------------------------
parser <- OptionParser() |>
  add_option(c("--its1"), type = "character", help = "ITS1 FASTA file [required]")|>
  add_option(c("--s58"), type = "character", help = "5.8S FASTA file [required]")|>
  add_option(c("--its2"), type = "character", help = "ITS2 FASTA file [required]")|>
  add_option(c("-o", "--output_dir"), type = "character", default = ".",
              help = "Output directory [default: current directory]")|>
  add_option(c("-p", "--prefix"), type = "character", default = "concat",
              help = "Output file prefix [default: concat]")


arguments <- parse_args(parser)

#validation---------------------------------------------------------------------
if (is.null(opt$its1) || is.null(opt$s58) || is.null(opt$its2)) {
  print_help(opt_parser)
  stop("\nError: You must specify --its1, --s58, and --its2.\n", call. = FALSE)
}

#run--------------------------------------------------------------------
full_its <- concatITS(opt$its1, opt$s58, opt$its2)

out_file <- file.path(opt$output_dir, paste0(opt$prefix, ".ITS.fasta"))
dir.create(opt$output_dir, showWarnings = FALSE)

write.fasta(
  full_its, names(full_its) |> as.list(),
  nbchar = 120,
  file.out = paste0(outdir, "/", db, ".ITS.fasta")
)
