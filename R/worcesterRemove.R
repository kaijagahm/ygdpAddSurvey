#' Remove any obsolete geography columns
#' @export
#' @import dplyr
#'
#' Before Kaija, Jason Zentz had rigged something up in Qualtrics that would geocode the localities--except it doesn't really work, and you end up with a lot of Worcester. This function removes the old geo stuff.
#' @param df Raw survey data as a data frame.

worcesterRemove <- function(df){
  df <- df %>%
    select(-contains("ZIP")) %>%
    select(-contains("City-US"))
  return(df)
}
