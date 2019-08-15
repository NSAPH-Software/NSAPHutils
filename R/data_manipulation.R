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

#' Convert zips stored as ints to 5 digit strings
#'
#' @param zip zipcode represented as an integer
#'
#' @export
#'
int_to_zip_str <- function(zip) {
  return(sprintf("%.5d",zip))
}
