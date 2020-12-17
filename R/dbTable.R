# Wrapper function for dbReadTable that converts "NA" to <NA> and changes some data types.
# Written by Kaija Gahm on 9 November 2020
# GH issue #32

dbTable <- function(con = con, tableName){
  library(RSQLite)
  library(lubridate)
  tab <- dbReadTable(con, tableName) # read in the table. All cols will be character because of the way we encoded them. 
  
  # Change things to NA's ---------------------------------------------------
  tab[tab == "NA"] <- NA_character_ 
  tab[tab == ""] <- NA_character_
  tab[tab == " "] <- NA_character_
  
  # Parse dates -------------------------------------------------------------
  if("dateReleased" %in% names(tab)){
    tab$dateReleased <- lubridate::ymd(tab$dateReleased)
  }
  if("dateClosed" %in% names(tab)){
    tab$dateClosed <- lubridate::ymd(tab$dateClosed)
  }
  if("dateTimeStart" %in% names(tab)){
    tab$dateTimeStart <- lubridate::parse_date_time(tab$dateTimeStart, orders = c("ymd_HMS", "mdy_HM", "mdy_HMS", "ymd_HM"))
  }
  if("dateTimeEnd" %in% names(tab)){
    tab$dateTimeEnd <- lubridate::parse_date_time(tab$dateTimeEnd, orders = c("ymd_HMS", "mdy_HM", "mdy_HMS", "ymd_HM"))
  }
  if("date" %in% names(tab)){
    tab$date <- lubridate::ymd(tab$date)
  }
  
  # Return finished table ---------------------------------------------------
  return(tab)
}
