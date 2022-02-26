## condor_tools.R
## R functions to help with setting up + running batch jobs

#' Create a default batch file infrastructure in a directory
#'
#' @param path path to the directory where the structure should be created
#'
#' @export
prep_batch <- function(path = ".") {
   gen_submit(path)
   dir.create(file.path(path, "batch_output"))

}

#' Generate submit file template in a directory
#' @param path path to the directory where the submit file should be placed
#' @param out_name file name for the submit file, should end in ".submit"
#'
#' @export
gen_submit <- function(path = ".", out_name = "r.submit") {
  file.copy(file.path(path.package("NSAPHutils"), "default_submit_file.submit"), file.path(path, out_name))
}
