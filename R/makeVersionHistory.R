#' Make the VERSION_HISTORY table
#' @export
#' @import dplyr
#' @import stringr
#' 
#'
#' This function creates the row of the VERSION_HISTORY table that corresponds to the update we're making for this survey
#' @param con A connection to the existing database, created with RSQLite.
#' @param date Date of the update, in the format YYYY-MM-DD
#' @param description Description of the version. For a standard database update, it's just "added [surveyID]".
#' 
# Make VERSION_HISTORY table ---------------------------------------------
makeVersionHistory <- function(con, date, description){
  # Get VERSION_HISTORY
  dbVersionHistory <- dbTable(con, "version_history")
  
  # Get the most recent version number
  lastVersion <- dbVersionHistory[nrow(dbVersionHistory), "versionNumber"]
  
  # Create new version number: adding a survey updates the middle number, so we add 1 to the current middle number and then make the last number into 0.
  newVersion <- lastVersion %>%
    str_replace(., "(?<=\\.)\\d+(?=\\.)", as.character(as.numeric(str_extract(lastVersion, "(?<=\\.)\\d+(?=\\.)")) + 1)) %>%
    str_replace(., "(?<=\\.)\\d+$", "0")
  
  # Make the new row for versionHistory
  versionHistory <- data.frame(versionNumber = newVersion,
                               date = date, 
                               description = description) %>%
    mutate_all(., .funs = as.character) 
  
  message("Successfully created VERSION_HISTORY")
  return(versionHistory)
}