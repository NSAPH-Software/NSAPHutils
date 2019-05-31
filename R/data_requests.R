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

