#' Download data request csv from qualtrics
#' @param out_path path to save data in
#'
#' @details
#' Your qualtrics API access token must be exported as QUALTRICSTOKEN to access the survey results.
#' Also your quatrics user id needs to be exported as QUALTRICSUSERID
#'
#' @import httr
#' @export
download_data_requests <- function(out_path=".") {
  token <- system("echo $QUALTRICSTOKEN", intern = T)
  user <- system("echo $QUALTRICSUSERID", intern = T)
  base_url      <- "https://harvard.az1.qualtrics.com"
  api_path <- "API/v3"
  my_headers <- add_headers('X-API-TOKEN' = token,
                          'Content-Type' =  "application/json")

  listofsurveys <- content(GET(base_url, path = c(api_path, "surveys"),
                             my_headers))

  x <- POST(base_url, path = c(api_path, "responseexports"),
            my_headers,
            body = list("surveyId" = "SV_3sXoEALKS7gn9E9",
                       # "embeddedDataIds" = "['data_s_CEDc1zob3w']",
                        "format" = "csv"),
            encode = "json")
  if(http_error(x)) {
    print(x)
    stop()
  }

  survey_id <- content(x)$result$id
  download_pct <- 0
  download_ntries <- 0

  while (download_pct < 100 & download_ntries < 1000) {
    download_pct <- content(
      GET(base_url, path = c(api_path, "responseexports", survey_id),
          my_headers))$result$percentComplete
    download_ntries <- download_ntries + 1
    Sys.sleep(0.5)
  }
  if (download_ntries < 1000) {
    GET(base_url, path = c(api_path, "responseexports", survey_id, "file"),
        my_headers,
        write_disk(file.path(out_path, "out.zip"), overwrite = T))
    unzip(file.path(out_path,"out.zip"))
    file.remove(file.path(out_path,"out.zip"))
  }
}

#' Convert a downloaded Qualtrics csv to a project list markdown file
#'
#' @param file path to the csv file containing data requests
#' @param outfile name of the file to be produced
#'
#'
#' @import pander
#' @export
data_requests_to_markdown <- function(file = "NSAPH Health Data Request Form.csv", outfile = "project_list.md") {
  data <- read.csv(file, stringsAsFactors=F)
  data <- data[3:nrow(data),]
  out <- list()
  for (i in 1:nrow(data)) {
    req <- list()
    obs <- data[i,]
    req[[obs$name]] <- list()
    req[[obs$name]][["email"]] <- obs$email
    if(as.numeric(obs$all_years)){
      req[[obs$name]][["years"]] <- "1999-2016"
    } else {
      req[[obs$name]][["years"]] <- obs$years
    }
    req[[obs$name]][["outcome"]] <- obs$outcome
    req[[obs$name]][["comorbidities"]] <- obs$comorbidities
    req[[obs$name]][["data_source"]] <- obs$data_source
    req[[obs$name]][["geography"]] <- obs$geography
    req[[obs$name]][["notes"]] <- obs$notes
    req[[obs$name]][["analysis"]] <- obs$analysis
    req[[obs$name]][["grants"]] <- obs$grants
    req[[obs$name]][["data_path"]] <- obs$data_path
    out[[i]] <- req
  }
  print(pandoc.list(out[[1]]))
  return(req)
}

