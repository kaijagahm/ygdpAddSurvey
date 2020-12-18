#' Save questionID's
#'
#' This function separates out questionID's and column names from the raw survey data and returns them, for use in other functions.
#' @param df Raw survey data as a data frame.
#' @export
saveQuestionIDs <- function(df){
  qids <- df[2,] %>% unname() %>% t() %>%
    as.data.frame() %>%
    setNames("qid") %>%
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
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
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

  # Check whether "Response ID" exists, and throw an error if not
  if("Response ID" %in% names(df)){
    df <- df
  }else{
    stop("Data must contain column 'Response ID'.")
  }

  # Clean up names that we couldn't assign
  df <- df %>%
    {if("Response Type" %in% names(.)) select(-c("Response Type")) else .} %>%
    {if("External Data Reference" %in% names(.)) select(-c("External Data Reference")) else .} %>%
    {if("Distribution Channel" %in% names(.)) select(-c("Distribution Channel")) else .} %>%
    {if("Start Date" %in% names(.)) rename(., dateTimeStart = "Start Date") else .} %>%
    {if("End Date" %in% names(.)) rename(., dateTimeEnd = "End Date") else .} %>%
    {if("IP Address" %in% names(.)) rename(., ipAddress = "IP Address") else .} %>%
    {if("Progress" %in% names(.)) rename(., progress = "Progress") else .} %>%
    {if("Duration (in seconds)" %in% names(.)) rename(., durationSeconds = "Duration (in seconds)") else .} %>%
    {if("Finished" %in% names(.)) rename(., finished = "Finished") else .} %>%
    {if("Recorded Date" %in% names(.)) rename(., recordedDate = "Recorded Date") else .} %>%
    {if("Location Latitude" %in% names(.)) rename(., locationLat = "Location Latitude") else .} %>%
    {if("Location Longitude" %in% names(.)) rename(., locationLong = "Location Longitude") else .} %>%
    {if("User Language" %in% names(.)) rename(., userLanguage = "User Language") else .} %>%
    {if("race - Other - Text" %in% names(.)) rename(., raceOther = "race - Other - Text") else .} %>%
    {if("race - Selected Choice" %in% names(.)) rename(., race = "race - Selected Choice") else .} %>%
    dplyr::rename("responseID" = "Response ID") %>% # must have responseID
    {if("Browser Meta Info - Version" %in% names(.)) rename(., version = "Browser Meta Info - Version") else .} %>%
    {if("Browser Meta Info - Browser" %in% names(.)) rename(., browser = "Browser Meta Info - Browser") else .} %>%
    {if("Browser Meta Info - Operating System" %in% names(.)) rename(., operatingSystem = "Browser Meta Info - Operating System") else .} %>%
    {if("Browser Meta Info - Resolution" %in% names(.)) rename(., resolution = "Browser Meta Info - Resolution") else .} %>%
    {if("CompCode" %in% names(.)) rename(., completionCode = "CompCode") else .}
  return(df)
}

#' Remove any obsolete geography columns
#'
#' Before Kaija, Jason Zentz had rigged something up in Qualtrics that would geocode the localities--except it doesn't really work, and you end up with a lot of Worcester. This function removes the old geo stuff.
#' @param df Raw survey data as a data frame.
#' @export

worcesterRemove <- function(df){
  df <- df %>%
    select(-contains("ZIP")) %>%
    select(-contains("City-US"))
  return(df)
}

#' Preprocess data
#'
#' Wrapper function that combines `fixNames` and `worcesterRemove` into a single function
#' @param df Raw survey data as a data frame. Must have column 'responseID'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export
#'

preprocessData <- function(df, updateID){
  df1 <- fixNames(df)
  df2 <- worcesterRemove(df1)
  return(df2)
}

