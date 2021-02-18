#' Check for duplicate column-names-to-be in the raw data
#'
#' This function checks the first row of the raw data (which will become the column names after cleanup) to see if there are any duplicate column-names-to-be. If there are, it throws an error, and you'll need to go and fix those duplicates.
#' @param rawData The raw Qualtrics output.
#' @export

checkRawDupNames <- function(rawData){
  namesToBe <- rawData %>%
    dplyr::slice(1) %>%
    purrr::flatten_chr()
  if(any(duplicated(namesToBe))){
    stop(paste0("The first row of the raw data, which will become its column names, contains duplicates. The duplicates are at columns ", paste(which(duplicated(namesToBe)|duplicated(namesToBe, fromLast = T)), collapse = ", "), "."))
  }else{
    message("SUCCESS! No duplicates detected in the first row of the raw data, which will become its column names.")
  }
}

#' Check how many TS, CU, CG, and comments columns the data contains
#'
#' This function checks the first row of the raw data, which will become the column names after cleaning. It reports how many sentence columns (marked with "TS_", "CU_", and "CG_") it finds, as well as how many comments columns (marked with "comments"). You need to manually look at these numbers to make sure they make sense.
#' @param rawData The raw Qualtrics output.
#' @export

howManySentences <- function(rawData){
  message("Checking sentence and comment column names: \n")
  namesToBe <- rawData %>%
    dplyr::slice(1) %>%
    purrr::flatten_chr()

  # Find the names corresponding to each of TS_, CG_, CU_, and comments_
  ts <- which(grepl("TS\\_", namesToBe))
  cg <- which(grepl("CG\\_", namesToBe))
  cu <- which(grepl("CU\\_", namesToBe))
  comments <- which(grepl("comments\\_", namesToBe))

  message(paste0("Found ", length(ts), " test sentence columns at the following positions: ",
                 paste(ts, collapse = ", ")))

  message(paste0("Found ", length(cg), " control grammatical (CG) sentence columns at the following positions: ",
                 paste(cg, collapse = ", ")))

  message(paste0("Found ", length(cu), " control ungrammatical (CU) columns at the following positions: ",
                 paste(cu, collapse = ", ")))

  message(paste0("Found ", length(comments), " comments columns at the following positions: ",
                 paste(comments, collapse = ", ")))
  message("\n Do these column counts make sense? If not, check your raw data to make sure that you correctly labeled the questions in Qualtrics according to the instructions in the survey format guidelines.")
}

#' Check that the number of comments columns matches the number of sentence columns
#'
#' This function checks that the number of columns whose name-to-be (i.e. the first row of the raw data) starts with "comments_" matches the number that start with "TS_", "CU_", or "CG_", as per the instructions in the survey format guidelines document. If there's a mismatch, the function throws a warning that you may end up with some missing data.
#' @param rawData The raw Qualtrics output.
#' @export

checkSentenceCommentNumbers <- function(rawData){
  # Extract names-to-be
  namesToBe <- rawData %>%
    dplyr::slice(1) %>%
    purrr::flatten_chr()

  # Find the names corresponding to each of TS_, CG_, CU_, and comments_
  ts <- which(grepl("TS\\_", namesToBe))
  cg <- which(grepl("CG\\_", namesToBe))
  cu <- which(grepl("CU\\_", namesToBe))
  comments <- which(grepl("comments\\_", namesToBe))

  # Compare lengths: is the number of comments equal to the number of sentences?
  if(length(comments) != length(ts) + length(cg) + length(cu)){
    warning(paste0("\n WARNING! The number of comments columns does not match the number of sentence columns. Found ", length(comments), " comments columns and ", length(ts) + length(cg) + length(cu), " sentence columns. If there is sentence or comment data that was not correctly mislabeled, it will not end up in the database!"))
  }
}

#' Check whether the master list contains the required columns
#'
#' In order for the survey processing functions to work, the master list must contain columns "sentenceID", "sentenceText", and "constructionID". This function checks that all columns are present and throws an informative error if not, telling you which ones are missing. If all columns are present, the function gives you a SUCCESS message.
#' @param masterList The master list of YGDP sentences.
#' @export
checkMasterListColumns <- function(masterList){
  n <- names(masterList)
  targetNames <- c("sentenceID", "sentenceText", "constructionID")
  if(!all(targetNames %in% names(masterList))){
    missingNames <- targetNames[which(!(targetNames %in% names(masterList)))]
    currentNames <- names(masterList)
    stop(paste0("masterList does not have the correct column names. The column names are: ",
                paste(currentNames, collapse = ","),
                ". The following names are required, but currently missing: ",
                paste(missingNames, collapse = ","),
                "."))
  }else{
    message("SUCCESS: The master list has the correct column names!")
  }
}

#' Check whether the master list has any duplicate sentenceID's in the 'sentenceID' column.
#'
#' In order for joins to work properly in the survey processing script, the master list must not have any duplicate sentenceID's. This function throws informative errors if the "sentenceID" column is missing or if there are duplicate sentenceID's, and prints a success message if no duplicates are found.
#' @param masterList The master list of YGDP sentences.
#' @export
#'
checkMasterListSentenceIDs <- function(masterList){
  if(!("sentenceID" %in% names(masterList))){
    stop("masterList does not contain the column `sentenceID`.")
  }else{
    ndups <- masterList %>%
      dplyr::group_by(sentenceID) %>%
      dplyr::filter(n() > 1) %>%
      nrow()
    if(ndups > 0){
      inds <- which(duplicated(masterList$sentenceID)|duplicated(masterList$sentenceID, fromLast = T))
      stop(paste0("Found duplicate sentenceIDs in masterList at the following rows: ",
                  paste(inds, collapse = ","),
                  ". Duplicate sentenceID's will cause problems when creating the database. Please resolve duplicates and try again."))
    }else{
      message("SUCCESS! No duplicate sentenceID's detected in the master list.")
    }
  }
}
