#birdID, band, start, and stop should all be fed in as strings
#start and stop should follow: "y/m/d h:m:s"
#start should be 90 minues after tag is affixed,
#stop is time the capture recovery attempt begins (e.g. net up)

readPinpoint <- function(file, birdID, band, database = NULL, start, stop, breedyear) {
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