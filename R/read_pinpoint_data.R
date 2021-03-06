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
readPinpoint <- function(file, birdID = NA, band = NA, database = NULL,
                         start = NULL, stop = NULL, breedyear = NA,
                         rowskip = F) {
  '%notin%' <- Negate('%in%')
  if(reader::get.delim(file) == ","){
    if(rowskip == T) {
      data <- read.csv(file, stringsAsFactors = F, skip = 8)
    } else {
      data <- read.csv(file, stringsAsFactors = F)
    }
  } else {
    if(reader::get.delim(file) == "\t") {
      data <- readr::read_table2(file)
    }else{
      if(rowskip == T) {
        data <- read.csv(file, stringsAsFactors = F, skip = 3)
      }else {
        data <- readr::read_table(file)
      }
    }

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

  if("GMT.Time" %in% colnames(data)){
    data$t_ <- lubridate::mdy_hms(data$GMT.Time)
  }

  #remove points pre and post deployment
  if(!is.null(start) & !is.null(stop)) {
    data <- dplyr::filter(data, t_ > lubridate::ymd_hms(start) &
                            t_ < lubridate::ymd_hms(stop))
  }


  #modify column classed for compatibility
  data <- data %>%
    mutate_all(as.character)

  #if not the first file loaded, add to the running database
  if(!is.null(database)){
    data <- dplyr::bind_rows(database, data)
  }


  #return full and cleaned dataset
  return(data)
}

