#' Append a newly-created table to the existing database table
#'
#' This function returns a complete version of the database table in question, as a data frame--i.e. the result contains both the rows from the new survey you are trying to add and all previous rows.
#' @param tableName A character string: name of the database table. Not case sensitive. E.g. "responses" for the RESPONSES table.
#' @param tableNew The name of a data frame object (not in quotes), corresponding to the newly-created portion of the table in question. E.g. responsesNew.
#' @param overwrite FALSE by default. If FALSE, the function checks whether parameter `u` is already contained in the updateID column of the database table. If it's not, then the function appends the data from `tableNew`. If it is, the function does nothing and returns the table as is. If `overwrite` == TRUE, then if `u` is already in the database table, all data with updateID `u` will be *removed* and replaced with the data from `tableNew`.
#' @param cn SQLite connection to the database.
#' @param u the updateID you're using for this database update. When using the template script, `u` should be updateIDString.
#' @export
#'
updateTable <- function(tableName, tableNew, overwrite = F, cn = con, u = updateIDString){
  tab <- dbReadTable(con, tableName) # get the version of this table that's in the database.

  # Warn if the names don't match. Bind_rows will still work intelligently, but will cause weird results when the sql script is run.
  if(!(all(names(tab)) %in% names(tableNew))|!(all(names(tableNew)) %in% names(tab))){
    warning("Names do not agree between the database table and the new data that you are trying to append. You may end up with NA's and/or extra columns in the database, which may cause conflicts with the SQL script.")
  }

  # Add to the table, or overwrite the data, or do nothing.
  if(!(u %in% tab$updateID)){
    message("Appending new data")
    fullTab <- bind_rows(tab %>%
                           mutate_all(., .funs = as.character),
                         tableNew %>%
                           mutate_all(., .funs = as.character)) # add the new data
  }else if(u %in% tab$updateID & overwrite == T){
    message("updateID already present in the database table. Since `overwrite` == TRUE, removing existing data and replacing it with your new data.")
    fullTab <- tab %>%
      filter(updateID != u) %>% # remove the old data with this updateID
      mutate_all(., .funs = as.character) %>%
      bind_rows(tableNew %>%
                  mutate_all(., .funs = as.character)
                ) # add the new data
  }else{ # if u %in% tab$updateID & overwrite == F...
    message("updateID already present in the database table. Since `overwrite` == FALSE, making no changes to the existing database table.")
    fullTab <- tab %>% mutate_all(., .funs = as.character) # ...leave the table as is.
  }

  # Basic check. Give an error if data isn't in the expected format.
  if(is.data.frame(fullTab) & nrow(fullTab) > 0){
    message(paste("Finished table has ", nrow(fullTab), " rows."))
    return(fullTab)
  }else{
    stop("Error: output is not a data frame and/or has 0 rows.")
  }
}
