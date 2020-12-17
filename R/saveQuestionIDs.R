#' Save questionID's
#'
#' This function separates out questionID's and column names from the raw survey data and returns them, for use in other functions.
#' @param df Raw survey data as a data frame.
#' @export
saveQuestionIDs <- function(df){
  qids <- df[2,] %>% unname() %>% t() %>%
    as.data.frame() %>%
    setNames("qid") %>%
    mutate(extr = str_extract(qid, "(?<=ImportId\"\\:\")[^\"]+(?=\")")) %>%
    pull(extr)
  newNames <- df[1,] %>% unname() %>% t() %>%
    as.data.frame() %>%
    setNames("new") %>%
    pull(new)
  outdf <- data.frame(colName = newNames, questionIDRaw = qids)
  return(outdf)
}
