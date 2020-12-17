#' Make the RATINGS table
#'
#' This function creates the RATINGS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param questions The QUESTIONS portion of the database tables, created by makeQuestions.R
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make RATINGS table ------------------------------------------------------
makeRatings <- function(df, questions, surveyID, updateID){
  justSentences <- df %>%
    select(responseID, contains("TS", ignore.case = F), contains("CU", ignore.case = F), contains("CG", ignore.case = F)) %>%
    select(-contains("comments"))

  # pivot longer
  ratings <- justSentences %>%
    pivot_longer(cols = -responseID, names_to = "sentenceID", values_to = "rating") %>%
    left_join(questions %>%
                select(questionID, questionText),
              by = c("sentenceID" = "questionText")) %>%
    mutate(sentenceID = str_extract(sentenceID, "(?<=\\_).*$"),
           surveyID = surveyID,
           updateID = updateID)

  # put cols in the right order
  ratings <- ratings %>%
    select(responseID, sentenceID, surveyID, questionID, rating, updateID)

  # Remove NA ratings
  ratings <- ratings %>%
    filter(!is.na(ratings)) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the RATINGS table.")

  # Return
  return(ratings)
}
