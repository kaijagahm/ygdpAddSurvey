#' Append a newly-created table to the existing database table
#'
#' This function returns a complete version of the database table in question, as a data frame--i.e. the result contains both the rows from the new survey you are trying to add and all previous rows.
#' @param tableName A character string: name of the database table. Not case sensitive. E.g. "responses" for the RESPONSES table.
#' @param tableNew The name of a data frame object (not in quotes), corresponding to the newly-created portion of the table in question. E.g. responsesNew.
#' @param overwrite FALSE by default. If FALSE, the function checks whether parameter `u` is already contained in the updateID column of the database table. If it's not, then the function appends the data from `tableNew`. If it is, the function does nothing and returns the table as is. If `overwrite` == TRUE, then if `u` is already in the database table, all data with updateID `u` will be *removed* and replaced with the data from `tableNew`.
#' @param con SQLite connection to the database.
#' @param u the updateID you're using for this database update. When using the template script, `u` should be updateIDString.
#' @export
#'
updateTable <- function(tableName, tableNew, overwrite = F, con = con, u = updateIDString){
  tab <- dbReadTable(con, tableName) # get the version of this table that's in the database.

  # Add to the table, or overwrite the data, or do nothing.
  if(!(u %in% tab$updateID)){
    fullTab <- bind_rows(tab, tableNew) # add the new data
  }else if(u %in% tab$updateID & overwrite == T){
    fullTab <- tab %>%
      filter(updateID != u) %>% # remove the old data with this updateID
      bind_rows(tableNew) # add the new data
  }else{ # if u %in% tab$updateID & overwrite == F...
    fullTab <- tab # ...leave the table as is.
  }
  return(fullTab)
}
