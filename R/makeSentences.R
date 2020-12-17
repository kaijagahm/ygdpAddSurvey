#' Make the SENTENCES table
#' @export
#' @import dplyr
#' @import stringr
#'
#' This function creates the SENTENCES database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param masterList Master list of all sentences. ### XXX COME BACK TO THIS: HOW BEST TO LOAD AND UPDATE?
#' @param con Connection to the current version of the database. Create the connection using RSQLite. For example: con <- dbConnect(RSQLite::SQLite(), here("database", "currentDB", "ygdpDB.db")) (for the database file ygdpDB.db, stored in "database/currentDB".)
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#'
# Make SENTENCES table ----------------------------------------------------
makeSentences <- function(df, masterList, con, updateID){
  # Get sentenceIDs for CU sentences
  cuIDs <- df %>%
    select(contains("CU_", ignore.case = F)) %>%
    names() %>%
    str_extract(., "(?<=CU\\_).*")
  if(!(all(grepl("^[0-9,\\.]*$", cuIDs)))){
    message(paste0("CU sentenceID's contain characters other than numbers and '.' The offenders are: ", paste(cuIDs[!(grepl("^[0-9,\\.]*$", cuIDs))], collapse = ", "),  ". Consider whether this is a problem, and fix your sentence column names if need be."))
  }

  # Get sentenceIDs for CG sentences
  cgIDs <- df %>%
    select(contains("CG_", ignore.case = F)) %>%
    names() %>%
    str_extract(., "(?<=CG\\_).*")
  if(!(all(grepl("^[0-9,\\.]*$", cgIDs)))){
    message(paste0("CG sentenceID's contain characters other than numbers and '.' The offenders are: ",
                   paste(cgIDs[!(grepl("^[0-9,\\.]*$", cgIDs))], collapse = ", "),
                   ". Consider whether this is a problem, and fix your sentence column names if need be."))
  }

  # Get sentenceIDs for test sentences
  tsIDs <- df %>%
    select(contains("TS_", ignore.case = F)) %>%
    names() %>%
    str_extract(., "(?<=TS\\_).*")
  if(!(all(grepl("^[0-9,\\.]*$", tsIDs)))){
    message(paste0("TS sentenceID's contain characters other than numbers and '.' The offenders are: ",
                   paste(tsIDs[!(grepl("^[0-9,\\.]*$", tsIDs))], collapse = ", "),
                   ". Consider whether this is a problem, and fix your sentence column names if need be."))
  }

  # Look up the sentence text in the master list for each set of sentenceIDs
  sentences <- data.frame(sentenceID = c(cuIDs, cgIDs, tsIDs),
                          sentenceType = c(rep("CU", length(cuIDs)),
                                           rep("CG", length(cgIDs)),
                                           rep("TS", length(tsIDs))),
                          stringsAsFactors = F) %>%
    left_join(masterList %>% select(sentenceID, sentenceText, constructionID), by = "sentenceID") %>%
    select(sentenceID, sentenceText, sentenceType, constructionID) %>% # reorder the columns
    mutate(updateID = updateID) # add the updateID

  # Throw an error if there are NA's
  if(any(is.na(sentences$sentenceText))){
    stop(paste0("Some sentences have NA for sentenceText. Check that your sentenceID's match up properly between the master list and the survey results. Also check that the sentenceText field in the master list isn't blank. The offending sentences are: ", paste(sentences$sentenceID[is.na(sentences$sentenceText)], collapse = ", ")))
  }

  # Check the new sentences table against the database version: we only want to keep new rows.
  s <- dbTable(con, "sentences")

  n <- sum(sentences$sentenceID %in% s$sentenceID) # how many sentences were already in the db?
  tot <- nrow(sentences) # how many total sentences were in this survey

  sentences <- sentences %>% # remove sentenceID's that are already represented in the database
    filter(!(sentenceID %in% s$sentenceID)) %>%
    mutate_all(., .funs = as.character)

  message(paste0(n, " sentences out of ", tot, " were already in the database, so your final SENTENCES table contains ", tot-n, " sentences."))
  message("Successfully created the SENTENCES table.")

  # Return
  return(sentences)
}
