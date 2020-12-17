#' Make the QUESTIONS table
#' @export
#' @import dplyr
#'
#' This function creates the QUESTIONS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param qids Data frame containing questionID/column name mappings for this survey. This is created using the `saveQuestionIDs()` function. Must have columns 'colName' and 'questionIDRaw'.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#'
# Make QUESTIONS table ----------------------------------------------------
# `qids` is a saved df of the format output by saveQuestionIDs
makeQuestions <- function(df, qids, surveyID, updateID){
  ## Check whether `qids` has the correct format
  if(!is.data.frame(qids)){
    stop("Argument `qids` must be a data frame. Use `saveQuestionIDs()` to create `qids`.")
  }
  if(any(names(qids) != c("colName", "questionIDRaw"))){
    stop("Argument `qids` must have columns `colName` and `questionIDRaw`. Use `saveQuestionIDs()` to create `qids`.")
  }

  # Make the df
  questions <- qids %>%
    rename(questionText = colName,
           questionID = questionIDRaw) %>%
    mutate(questionID = paste0(surveyID, "_", questionID)) %>%
    mutate(surveyID = surveyID,
           stimulusType = NA,
           scaleOptions = NA,
           problems = NA,
           updateID = updateID)

  # To do later:
  ## update stimulus types
  ## Fix the questionText column--sometimes it has the text, sometimes not.
  # Reorder the cols
  questions <- questions %>%
    select(questionID, questionText, surveyID, stimulusType, scaleOptions, problems, updateID) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the QUESTIONS table.")

  # Return
  return(questions)
}
