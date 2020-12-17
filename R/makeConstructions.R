#' Make the CONSTRUCTIONS table
#'
#' Function to create the CONSTRUCTIONS database table.
#' @param constructions A csv file, which you need to create separately before running this script. The table should have three columns: constructionID (existing or new ID's for all of the constructions contained in this survey), constructionName (the name of each construction, corresponding to the ID's), and pageYGDP (the URL of the YGDP page corresponding to this construction, if applicable.)
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make CONSTRUCTIONS table ------------------------------------------------
makeConstructions <- function(constructions, con, updateID){
  # Pull in CONSTRUCTIONS
  dbConstructions <- dbTable(con, "constructions")

  # Test that the `constructions` input has the correct columns
  if(!("constructionID" %in% names(constructions))){
    stop("Input table `constructions` must have a column called 'constructionID'.")
  }
  if(!("constructionName" %in% names(constructions))){
    stop("Input table `constructions` must have a column called 'constructionName'.")
  }
  if(!("pageYGDP" %in% names(constructions))){
    stop("Input table `constructions` must have a column called 'pageYGDP'. If none of the constructions have associated YGDP pages, set all the values in this column to 'NA' or leave the cells blank.")
  }

  # If there are any rows that match by constructionID but not by name, warn the user
  problems <- constructions %>%
    filter(constructionID %in% dbConstructions$constructionID &
             !(constructionName %in% dbConstructions$constructionName))
  if(nrow(problems) > 0){
    warning(
      paste0("The following constructionID's are already in the database, but with different constructionNames:\n",
             paste(problems$constructionID, collapse = ",")),
      "\nThese constructionID's will not be added to the database. If these are new constructions that you would like to add, change the constructionID to something unique."
    )
  }

  # Remove any constructions that are already in the database, by ID and by name separately (in case there are discrepancies)
  constructions <- constructions %>%
    filter(!(constructionID %in% dbConstructions$constructionID),
           !(constructionName %in% dbConstructions$constructionName))

  # Add updateID
  constructions <- constructions %>%
    mutate(updateID = updateID) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created CONSTRUCTIONS.")

  # return
  return(constructions)
}
