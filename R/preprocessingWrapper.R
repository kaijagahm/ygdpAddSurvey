#' Preprocess data
#'
#' Wrapper function that combines `fixNames` and `worcesterRemove` into a single function
#' @param df Raw survey data as a data frame. Must have column 'responseID'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

preprocessData <- function(df){
  df1 <- fixNames(df) # apply the fixNames function. See documentation for `fixNames()`
  df2 <- removeGeo(df1) # remove any extra geographic columns. See documentation for `removeGeo()`.

  ## Replace "" and " " with NA
  df2 <- df2 %>%
    naniar::replace_with_na_all(.,
                                condition = ~.x %in% c("", " ", "NA"))

  ## Return the completed data frame
  return(df2)
}
