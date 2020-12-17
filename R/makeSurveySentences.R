#' Make the SURVEY_SENTENCES table
#'
#' This function creates the SURVEY_SENTENCES database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make the SURVEY_SENTENCES table -----------------------------------------
makeSurveySentences <- function(df, surveyID, updateID){
  sentence_nums <- df %>%
    select(matches("CU\\_|CG\\_|TS\\_")) %>%
    select(!matches("comments\\_")) %>%
    names() %>%
    str_extract(., "(?<=[A-Z]\\_)[0-9,\\.]+$") # only the sentence number

  # initialize
  surveySentences <- data.frame(
    sentenceID = sentence_nums,
    surveyID = surveyID,
    updateID = updateID
  ) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the SURVEY_SENTENCES table.")
}
