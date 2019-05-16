## read_data.R

#' Read Shareded Data
#'
#' @param path path to the directory containing the sharded data
#' @param years vector of years of data to read
#' @param columns vector of strings listing the columns of data to select
#' @param nthreads number of threads to use in processes. Default detects requested cpus on rce to force good behavior
#'
#' Prototype assumes fst data and that data is broken out by year, year used as suffix
#'
#' @import data.table
#' @import fst
#' @importFrom tools file_path_sans_ext
#' @export
read_data <- function(path, years, columns, nthreads = get_cpus()) {
  set_threads(nthreads)
  files <- list.files(path, pattern = ".fst", full.names = TRUE)

  ## extract suffix, ensure is in years
  files <- files[substr(file_path_sans_ext(files),
                        nchar(file_path_sans_ext(files)) - 3,
                        nchar(file_path_sans_ext(files)))
                 %in% years]
  out <- rbindlist(lapply(files, read_fst, as.data.table = T, columns =  columns))

  return(out)

}

#' Get number of requested CPUs
#'
#'
#' @export
get_cpus <- function() {
  ncpus <- system("grep -i ^requestcpus $_CONDOR_JOB_AD", intern = T)
  ncpus <- as.numeric(gsub("\\D+", "", ncpus))
  return(ncpus)
}

set_threads <- function(nthreads = get_cpus()) {
  setDTthreads(nthreads)
  threads_fst(nr_of_threads = nthreads)
}
