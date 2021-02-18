#' Save questionID's
#'
#' This function separates out questionID's and column names from the raw survey data and returns them, for use in other functions.
#' @param df Raw survey data as a data frame.
#' @export
saveQuestionIDs <- function(df){
  qids <- df[2,] %>% unname() %>% t() %>%
    as.data.frame() %>%
    stats::setNames("qid") %>%
    mutate(extr = str_extract(qid, "(?<=ImportId\"\\:\")[^\"]+(?=\")")) %>%
    pull(extr)
  newNames <- df[1,] %>% unname() %>% t() %>%
    as.data.frame() %>%
    setNames("new") %>%
    pull(new)
  outdf <- data.frame(colName = newNames, questionIDRaw = qids)
  return(outdf)
}

#' Fix the names from the qualtrics table
#'
#' Since the Qualtrics output has a weird three-line header, this function cleans up the names. Sets the first row of the data frame as names, then removes the first and second rows of the data frame. Then it renames a bunch of still-messy names from Qualtrics.
#' @param df Raw survey data as a data frame. Must have column 'responseID'.
#' @export

# Naming function ---------------------------------------------------------
fixNames <- function(df){
  # Save first row for names
  newNames <- df[1,] %>% unname() %>% t() %>%
    as.data.frame() %>%
    setNames("new") %>%
    pull(new) # first row of the df will become names

  # Remove first and second row
  df <- df[-c(1:2),] # remove first and second row of the df

  # Set first row as names
  names(df) <- newNames

  # Check whether "Response ID" and "CompCode" exist, and throw an error if not
  required <- c("Response ID", "CompCode")
  if(!all(required %in% names(df))){
    inds <- which(!(required %in% names(df)))
    stop(paste0("Question names 'Response ID' and 'CompCode' are both required for data cleanup. Could not find the following: ",
                paste(required[inds], collapse = ","),
                "."))
  }

  # Check whether each of the other columns exists, and warn if not
  targets <- c("Start Date", "End Date", "IP Address", "Progress", "Duration (in seconds)", "Finished", "Recorded Date", "Location Latitude", "Location Longitude", "User Language", "race - Other - Text", "race - Selected Choice", "Browser Meta Info - Version", "Browser Meta Info - Browser", "Browser Meta Info - Operating System", "Browser Meta Info - Resolution")
  targetNames <- c("dateTimeStart", "dateTimeEnd", "ipAddress", "progress", "durationSeconds", "finished", "recordedDate", "locationLat", "locationLong", "userLanguage", "raceOther", "race", "version", "browser", "operatingSystem", "resolution")
  for(i in 1:length(targets)){
    if(!(targets[i] %in% names(df))){
      warning(paste0("Need to rename ", targets[i], " to ",
                     targetNames[i], ", but can't find ", targets[i],
                     ". Creating ", targetNames[i], " as NA."))
    }
  }

  # Clean up the names, setting info to NA if column is not found.
  df <- df %>%
    {if("Start Date" %in% names(.)) rename(., dateTimeStart = "Start Date") else mutate(., dateTimeStart = NA)} %>%
    {if("End Date" %in% names(.)) rename(., dateTimeEnd = "End Date") else mutate(., dateTimeEnd = NA)} %>%
    {if("IP Address" %in% names(.)) rename(., ipAddress = "IP Address") else mutate(., ipAddress = NA)} %>%
    {if("Progress" %in% names(.)) rename(., progress = "Progress") else mutate(., progress = NA)} %>%
    {if("Duration (in seconds)" %in% names(.)) rename(., durationSeconds = "Duration (in seconds)") else mutate(., durationSeconds = NA)} %>%
    {if("Finished" %in% names(.)) rename(., finished = "Finished") else mutate(., finished = NA)} %>%
    {if("Recorded Date" %in% names(.)) rename(., recordedDate = "Recorded Date") else mutate(., recordedDate = NA)} %>%
    {if("Location Latitude" %in% names(.)) rename(., locationLat = "Location Latitude") else mutate(., locationLat = NA)} %>%
    {if("Location Longitude" %in% names(.)) rename(., locationLong = "Location Longitude") else mutate(., locationLong = NA)} %>%
    {if("User Language" %in% names(.)) rename(., userLanguage = "User Language") else mutate(., userLanguage = NA)} %>%
    {if("race - Other - Text" %in% names(.)) rename(., raceOther = "race - Other - Text") else mutate(., raceOther = NA)} %>%
    {if("race - Selected Choice" %in% names(.)) rename(., race = "race - Selected Choice") else mutate(., race = NA)} %>%
    {if("Browser Meta Info - Version" %in% names(.)) rename(., version = "Browser Meta Info - Version") else mutate(., version = NA)} %>%
    {if("Browser Meta Info - Browser" %in% names(.)) rename(., browser = "Browser Meta Info - Browser") else mutate(., browser = NA)} %>%
    {if("Browser Meta Info - Operating System" %in% names(.)) rename(., operatingSystem = "Browser Meta Info - Operating System") else mutate(., operatingSystem = NA)} %>%
    {if("Browser Meta Info - Resolution" %in% names(.)) rename(., resolution = "Browser Meta Info - Resolution") else mutate(., resolution = NA)} %>%
    dplyr::rename(responseID = "Response ID") %>% # must have 'Response ID', and we already checked for it above.
    dplyr::rename(completionCode = "CompCode") # must have 'CompCode', and we already checked for it above.
  return(df)
}

#' Remove any obsolete geography columns
#'
#' Before Kaija, Jason Zentz had rigged something up in Qualtrics that would geocode the localities--except it doesn't really work, and you end up with a lot of Worcester. This function removes the old geo stuff.
#' @param df Raw survey data as a data frame.
#' @export

removeGeo <- function(df){
  df <- df %>%
    select(-contains("ZIP")) %>%
    select(-contains("City-US"))
  return(df)
}

