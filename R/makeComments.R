#' Make the COMMENTS table
#' @export
#' @import dplyr
#' @import stringr
#'
#' This function creates the COMMENTS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param questions The QUESTIONS portion of the database tables, created by makeQuestions.R
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.

# Make COMMENTS table -----------------------------------------------------
makeComments <- function(df, questions, updateID){
  justComments <- df %>%
    select(responseID, contains("comments", ignore.case = F))

  # pivot longer
  comments <- justComments %>%
    pivot_longer(cols = -responseID, names_to = "sentenceID", values_to = "comment") %>%
    left_join(questions %>%
                select(questionID, questionText),
              by = c("sentenceID" = "questionText")) %>%
    mutate(sentenceID = str_extract(sentenceID, "(?<=^comments\\_).*$"),
           updateID = updateID)

  # remove NA comments
  comments <- comments %>%
    filter(!is.na(comment))

  # Replace double quotation marks
  comments <- comments %>%
    mutate(comment = str_replace_all(comment, "\"", "'"),
           comment = str_replace_all(comment, "\n", "")) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the COMMENTS table.")

  # Return
  return(comments)
}
