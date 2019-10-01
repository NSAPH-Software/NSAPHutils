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


#' Convert zips stored as ints to 5 digit strings
#'
#' @param zip zipcode represented as an integer
#'
#' @export
#'
int_to_zip_str <- function(zip) {
  return(sprintf("%.5d",zip))
}
