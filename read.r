## get necessary libraries
library(dplyr)
library(readr)
library(tidyr)

## function to read in the data (credentials revised)
readInODK <- function(svcLink, dataIndex, download) {
  Sys.setenv(ODKC_URL = "https://surveygat.info/")
  Sys.setenv(ODKC_SVC = svcLink)
  Sys.setenv(ODKC_UN = "gat3@gmail.com")
  Sys.setenv(ODKC_PW = "gat23456789@")
  
  ruODK::ru_setup(
    svc = Sys.getenv("ODKC_SVC"),
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = "Asia/Karachi",
    verbose = TRUE
  )
  
  fq_svc <- ruODK::odata_service_get()
  loc <- fs::path("media")
  
  fq_data <- ruODK::odata_submission_get(
    table = fq_svc$name[dataIndex],
    local_dir = loc,
    wkt = TRUE,
    download = download
  )
  
  return(fq_data)
}

## read in the benif data
listing_summary <-
  readInODK("https://surveygat.info/v1/projects/2/forms/1-Listing_App.svc",
            1,
            F)

listing <-
  readInODK("https://surveygat.info/v1/projects/2/forms/1-Listing_App.svc",
            2,
            F)

listing_summary$str_count <- as.numeric(listing_summary$str_count)
listing_summary$hh_count <- as.numeric(listing_summary$hh_count)

listing$str_index <- as.numeric(listing$str_index)
listing$str_stat <- as.numeric(listing$str_stat)
listing$str_stat_nan <- as.numeric(listing$str_stat_nan)
listing$hh_index <- as.numeric(listing$hh_index)
listing$hh_stat <- as.numeric(listing$hh_stat)
listing$hh_stat_log <- as.numeric(listing$hh_stat_log)

as.POSIXct(paste(as.Date(listing_summary$survey_day), listing_summary$survey_time))

timeUpdated <- format(Sys.time(), "%a %b %d  %Y %X")
saveRDS(object = timeUpdated, file = 'who-dash/timeUpdated.RDS')
