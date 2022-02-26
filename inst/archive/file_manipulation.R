# file manipulation

#' refactor multiple files at once
#'
#' @param path directory containing files to be refactored
#' @param orig original text to be refactored
#' @param replace text to replace the refactored phrase with
#' @param pattern pattern for further identifying files to be refactored
#'
#' @importFrom readr read_file write_file
#' @export
refactor_files <- function(path, orig, replace, pattern = NULL) {
  files <- list.files(path, full.names = T, pattern = as.character(pattern))
  for (file in files) {
    code <- read_file(file)
    code <- gsub(orig, replace, code)
    write_file(code, file)
  }
}
