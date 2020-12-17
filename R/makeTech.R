#' Make the TECH table
#'
#' This function creates the TECH database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have columns 'responseID', 'consent', and 'completionCode'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

makeTech <- function(df, updateID){
  # check for essential columns
  ## Must have responseID, consent, completionCode.
  required <- c("responseID", "consent", "completionCode")
  if(any(!(required %in% names(df)))){
    stop("One or more of the columns 'responseID', 'consent', or 'completionCode' is missing or misspelled. Please check column names and try again.")
  }

  # turn all cols to character, for consistency
  df <- df %>%
    mutate_all(.funs = as.character)

  # initialize df
  tech <- df %>%
    select(any_of(c("responseID", "dateTimeStart", "dateTimeEnd", "ipAddress", "progress",
                    "durationSeconds", "finished", "recordedDate", "consent", "browser",
                    "version", "operatingSystem", "resolution", "hasAmazonID", "completionCode",
                    "locationLat", "locationLong", "userLanguage"))) %>%
    mutate(updateID = updateID) # user defines the updateID

  # set hasAmazonID to "No" if it's NA
  tech <- tech %>%
    mutate(hasAmazonID = case_when(is.na(hasAmazonID) ~ "No",
                                   TRUE ~ hasAmazonID))

  # Only distinct rows, in case of any repeats
  tech <- tech %>%
    distinct() %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the TECH table.")

  # Return
  return(tech)
}
