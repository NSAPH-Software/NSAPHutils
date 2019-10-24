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
#' @details This function initially assumes that the input is intended to be  a 5 digit zipcode.
#'     If the initial conversion is more than 5 characters, the input is assumed to be intended to
#'     be a 9 digit zipcode. The function still returns the 5 digit equivalent of the 9 digit
#'     zip code.
#'
#' @export
#'
int_to_zip_str <- function(zip) {

  out <- sprintf("%.5d",zip)
  if (nchar(out) > 5) {
    out <- sprintf("%.9d",zip)
    out <- substr(out,1,5)
  }
  return(out)
}
