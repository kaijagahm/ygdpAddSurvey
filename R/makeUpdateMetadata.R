#' Make the UPDATE_METADATA table
#'
#' This function creates the row of the UPDATE_METADATA table that corresponds to the updateID that we've been using for all the other database tables.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param date Date of the update, in the format YYYY-MM-DD
#' @param updater Name of the person doing the update (first and last), e.g. Kaija Gahm
#' @param description Description of the update. For a standard database update, it's just "added [surveyID]".
#'@export
#'
# Make UPDATE_METADATA table ----------------------------------------------
makeUpdateMetadata <- function(updateID, date, updater, description){
  # Make the table with the user-generated information.
  updateMetadata <- data.frame(updateID = updateID,
                               date = date,
                               updater = updater,
                               metadata = description) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created UPDATE_METADATA")
  return(updateMetadata)
}
