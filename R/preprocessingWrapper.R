#' Preprocess data
#'
#' Wrapper function that combines `fixNames` and `worcesterRemove` into a single function
#' @param df Raw survey data as a data frame. Must have column 'responseID'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

preprocessData <- function(df, updateID){
  df1 <- fixNames(df)
  df2 <- removeGeo(df1)
  print('test')
  return(df2)
}
