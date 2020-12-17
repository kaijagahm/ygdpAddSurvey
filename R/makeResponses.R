#' Make the RESPONSES table
#'
#' This function creates the RESPONSES database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have column 'responseID'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @export

makeResponses <- function(df, updateID, surveyID){
  # check for required cols
  required <- c("responseID")
  if(any(!(required %in% names(df)))){
    stop("The 'responseID' column is missing or misspelled. Please check your column names and try again.")
  }

  # initialize the responses table
  responses <- df %>%
    select(responseID, amazonID) %>%
    mutate(surveyID = surveyID,
           updateID = updateID) %>%
    distinct() %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the RESPONSES table.")

  # return
  return(responses)
}
