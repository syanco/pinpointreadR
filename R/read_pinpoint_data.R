#' readPinpoint
#'
#' @param file String, filename to read in.
#' @param birdID String, optional ID of bird to associate with records
#' @param band String, optional band number of bird to associate with records
#' @param database Optional, object name of existing database to append with
#' read in data.
#' @param start String, starting date/time of tracking data to retain.  Format:
#' "y/m/d h:m:s"
#' @param stop String, ending date/time of tracking data to retain.  Format:
#' "y/m/d h:m:s"
#' @param breedyear String, optional year of data to associate with records
#'
#' @return Data.frame of pinpoint data\
#' @export
#'
readPinpoint <- function(file, birdID = NULL, band = NULL, database = NULL,
                         start, stop, breedyear = NULL) {
  '%notin%' <- Negate('%in%')
  if(reader::get.delim(file) == ","){
    data <- read.csv(file, stringsAsFactors = F)
  } else {
    data <- readr::read_table(file)
  }

  #add bird ID
  data$bird_id <- rep(as.character(birdID), nrow(data))
  #add band number
  data$band <- rep(as.character(band), nrow(data))
  #add breeding year
  data$breedyr <- breedyear
  if("Temperature(C)" %in% colnames(data)) {
    data <- dplyr::select(data, -c('Temperature(C)'))
    }

  #add date/time stamps column
  #check which data column names were used
  #add date/time stamps column
  if("RTC-date" %in% colnames(data)){
    data$t_ <- lubridate::ymd_hms(paste0(data$`RTC-date`, " ", data$`RTC-time`),
                                  tz="UTC")
  }

  if("Date" %in% colnames(data)){
    data$t_ <- lubridate::ymd_hms(paste0(data$Date, " ", data$`RTC time`),
                                  tz="UTC")
  }

  if("RTC.date" %in% colnames(data) & "`RTC time`" %in% colnames(data)){
    data$t_ <- lubridate::ymd_hms(paste0(as.character(data$RTC.date),
                                         " ", as.character(data$`RTC time`)),
                                  tz="UTC")
  }

  if("RTC.date" %in% colnames(data) & "RTC.time" %in% colnames(data)){
    data$t_ <- lubridate::ymd_hms(paste0(as.character(data$RTC.date),
                                         " ", as.character(data$RTC.time)),
                                  tz="UTC")
  }

  if("date" %in% colnames(data) & "time" %in% colnames(data)){
    data$t_ <- lubridate::ymd_hms(paste0(as.character(data$date),
                                         " ", as.character(data$time)),
                                  tz="UTC")
  }
  if("time" %in% colnames(data) & "date"  %notin% colnames(data)){
    data$t_ <- lubridate::ymd_hms(data$time)
  }
  #remove points pre and post deployment
  data <- dplyr::filter(data, t_ > lubridate::ymd_hms(start) &
                          t_ < lubridate::ymd_hms(stop))

  #if not the first file loaded, add to the running database
  if(!is.null(database)){
    data <- dplyr::bind_rows(database, data)
  }

  #return full and cleaned dataset
  return(data)
}
