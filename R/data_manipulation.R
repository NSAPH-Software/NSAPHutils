### common string + data manipulation functions

#' Reverse a vector of strings
#'
#' @param str A vector of strings to be reversed
#'
#' @return a character vector
#' @export
reverse_string <- function(str) {
  return(sapply(lapply(strsplit(str, NULL), rev), paste, collapse=""))
}

#' Parse a .fts file to produce a data dictionaryh.
#'
#' @param path The path to the .fts file.
#'
#' @export
#'
parse_fts <- function(path) {
  txt <- readr::read_lines(path)
  start <- stringr::str_which(txt, stringr::fixed("----")) + 1
  cnames <- stringr::str_split(txt[start - 2], " +")[[1]]
  txt <- stringr::str_c(txt[start:(length(txt) - 2)], collapse = "\n")
  dict <- readr::read_fwf(txt, readr::fwf_empty(txt, col_names = cnames))
  dict$Rtype <- stringr::str_replace_all(dict$Type, "CHAR|DATE", "character")
  dict[dict$Width < 5 & dict$Type == "NUM", "Rtype"] <- "integer"
  dict[dict$Width >= 5 & dict$Type == "NUM", "Rtype"] <- "double"
  dict
}

#' Convert a directory of CMS Medicaid MAX data .csv files to R's fst storage format.
#'
#' CMS ships Medicaid data as a set of `.csv`` files, one for each state/year. These `.csv``
#' files are distributed along with `.fts` (File Transport Summary) files containing file
#' and variable level metadata. This function converts these to `.fst` format to allow
#' faster selective IO from R. It does not write over any existing `.fst` files, so
#' if you want to replace existing `.fst` files you must delete them first.
#'
#' @param path The path to the Medicaid data directory for a single year.
#' @param dict A dictionary as produced by \code{\link{parse_fts}}.
#' @param outdir The directory to write the `.fst` files to.
#' @param pattern Regular expression to match `.csv` files to convert.
#' Useful for separating `ps` (patient summary) from `ip` (inpatient) data,
#' as these are stored in the same directory in the origina CMS data.
#'
#' @export
#'
medicaid_csv_dir_to_fst <- function(path, dict, outdir, pattern) {
  files <- list.files(path,
                      pattern = pattern,
                      full.names = TRUE)
  walk(files,
       function(file) {
         outpath <- str_c(outdir,
                          "/",
                          str_replace(basename(file), ".csv$", ".fst"),
                          sep = "")
         if(!file.exists(outpath)) {
           f <- fread(file,
                      col.names = dict[["Column"]],
                      colClasses = dict[["Rtype"]])
           write_fst(f, outpath)
           rm(f)
           gc()
         }
       }
  )
}

#' Convert zips stored as ints to 5 digit strings
#'
#' @param zip zipcode represented as an integer
#'
#' @details This function initially assumes that the input is intended to be  a 5 digit zipcode.
#'     If the initial conversion is more than 5 characters, the input is assumed to be intended to
#'     be a 9 digit zipcode. The function still returns the 5 digit equivalent of the 9 digit
#'     zip code.
#'
#' @export
#'
int_to_zip_str <- function(zip) {

  out <- sprintf("%.5d",zip)
  if (any(nchar(out) > 5)) {
    out <- sprintf("%.9d",zip)
    out <- substr(out,1,5)
  }
  return(out)
}
