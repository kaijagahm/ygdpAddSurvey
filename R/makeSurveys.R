#' Make the SURVEYS table
#' @export
#' @import dplyr
#'
#' This function creates the SURVEYS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param admin Character string: name of the person who administered the survey. For example, "Jim Wood".
#' @param surveyName Character string naming the survey (this is an opportunity to be more descriptive than `surveyID` allows.) For example, the `surveyName` for Survey 11 might be "Survey 11". Or, if you did a survey focused on a particular construction, you might call it "Needs Washed Survey".

# Make SURVEYS table ------------------------------------------------------
makeSurveys <- function(df, surveyID, updateID, admin, surveyName){
  # Get start and end dates
  forDates <- df %>%
    select(dateTimeStart, dateTimeEnd) %>%
    mutate_all(.funs = parse_datetime) # parse_datetime guesses the format
  dateReleased <- min(forDates$dateTimeStart)
  dateClosed <- max(forDates$dateTimeEnd)

  # make the data frame
  surveys <- data.frame(surveyID = surveyID,
                        surveyName = surveyName,
                        dateReleased = dateReleased,
                        dateClosed = dateClosed,
                        administrator = admin,
                        updateID = updateID) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the SURVEYS table.")

  # return
  return(surveys)
}
