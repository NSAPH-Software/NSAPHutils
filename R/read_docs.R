## read_docs.R
## code to output documentation

#' Get list of available variables
#'
#' @param src the type of data to list
#' @param filter.vars named list of variables and values to prefilter the data list
#'
#'
#' @export
list_available_data <- function(src, filter.vars = NULL) {
  if (!(src  %in% c("health", "exposure", "other"))) {
    stop("src must be 'health', 'exposure', or 'other'")
  }

  if (src == "health") {
    path <- "/nfs/nsaph_ci3/users/ci3_mbsabath/data_documentation/rce_data_list/health_data.csv"
  } else if (src == "exposure"){
    path <- "/nfs/nsaph_ci3/users/ci3_mbsabath/data_documentation/rce_data_list/exposure_data.csv"
  } else if (src == "other") {
    path <- "/nfs/nsaph_ci3/users/ci3_mbsabath/data_documentation/rce_data_list/confounder_data.csv"
  }

  out <- read.csv(path)
  if(!is.null(filter.vars)){
    for (i in 1:length(filter.vars)) {
      out <- out[out[[names(filter.vars)[i]]] == filter.vars[i]]
    }
  }

  return(out)
}
