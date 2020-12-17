#' Make the SURVEY_COMMENTS table
#' @export
#' @import dplyr
#' @import stringr
#'
#' This function creates the SURVEY_COMMENTS database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have columns 'responseID' and 'generalComments'.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param qids Data frame containing questionID/column name mappings for this survey. Create this using the `saveQuestionIDs()` function.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#'
makeSurveyComments <- function(df, surveyID, qids, updateID){
  # check for required cols
  required <- c("responseID", "generalComments")
  if(any(!(required %in% names(df)))){
    stop("One or more of the columns 'responseID' or 'generalComments' are missing or misspelled. Please check column names and try again.")
  }

  # Initialize table
  surveyComments <- df %>%
    select(responseID, generalComments) %>%
    rename(comment = generalComments) %>%
    distinct()

  # Get questionID
  if("generalComments" %in% qids$colName){
    a <- qids %>%
      filter(colName == "generalComments") %>%
      pull(questionIDRaw) %>%
      str_extract("QID\\d+")
    b <- paste0(surveyID, a) # make the full questionID string
  }else{
    b <- NA
  }

  # Add cols to surveyComments
  surveyComments <- surveyComments %>%
    mutate(surveyID = surveyID,
           questionID = b,
           updateID = updateID)

  # Remove any where the comments field is blank
  surveyComments <- surveyComments %>%
    filter(!is.na(comment)) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the SURVEY_COMMENTS table.")

  # Return
  return(surveyComments)
}
