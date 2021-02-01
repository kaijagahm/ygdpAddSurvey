#' Check survey completion and annotate the AMT table
#'
#' This function takes three inputs: the amt output table, the (cleaned) raw survey data table, and a parameter to define how many un-rated sentences are acceptable (defaults to 2). For each response, the function computes how many unanswered questions that person had and marks the survey as complete or not based on that and the parameter. Then it joins that result, by completion code, to the AMT table.
#' @param amt The raw AMT output data frame.
#' @param qt The (somewhat cleaned) Qualtrics output, after having passed through `preprocessData()`.
#' @param n The number of un-rated sentences acceptable in a "complete" survey. Default is 2, although settings on newer surveys to force answers to questions, this may actually be too liberal.
#' @export

checkCompletion <- function(amt, qt, n = 2){
  ## Check that qt has a "completionCode" column
  if(!("completionCode" %in% names(qt))){
    stop("Qualtrics data does not include a 'completionCode' column.")
  }

  ## Check that amt has an "Answer.surveycode" column
  if(!("Answer.surveycode" %in% names(amt))){
    stop("AMT data does not include an 'Answer.surveycode' column.")
  }

  ## Extract sentence columns and compute how many sentences were unanswered
  a <- qt %>%
    dplyr::select(completionCode,
           matches("^TS|^CG|^CU", ignore.case = F)) %>%
    tidyr::pivot_longer(cols = -completionCode, names_to = "sentence", values_to = "rating") %>%
    group_by(completionCode) %>%
    summarize(nUnanswered = sum(is.na(rating))) %>%
    mutate(matchesCompleteSurvey = case_when(nUnanswered < n ~ T,
                                             TRUE ~ F)) %>%
    select(-nUnanswered)

  ## Warn if there are duplicate completion codes in the amt file
  if(any(duplicated(amt$Answer.surveycode) & !is.na(amt$Answer.surveycode))){
    d <- which(!is.na(amt$Answer.surveycode) & (duplicated(amt$Answer.surveycode)|duplicated(amt$Answer.surveycode, fromLast = T)))
    warning(paste0("The following rows have duplicate completion codes in the Qualtrics survey output: ", paste(d, collapse = ","), ". You might want to manually investigate these."))
  }

  ## Warn if there are duplicate completion codes in the qt file
  if(any(duplicated(qt$completionCode) & !is.na(qt$completionCode))){
    d <- which(!is.na(qt$completionCode) & (duplicated(qt$completionCode)|duplicated(qt$completionCode, fromLast = T)))
    rids <- qt[d, "responseID"] # which responseID's have the duplicate completion codes
    warning(paste0("The following responseID's have duplicate completionCodes in the Qualtrics survey output: ", paste(rids, collapse = ", "), ". You might want to manually investigate these."))
  }

  ## Join the two tables:
  b <- amt %>%
    left_join(a, by = c("Answer.surveycode" = "completionCode")) %>%
    mutate(matchesCompleteSurvey = case_when(is.na(matchesCompleteSurvey) ~ F,
                                             TRUE ~ matchesCompleteSurvey))

  return(b) # return the result
}
