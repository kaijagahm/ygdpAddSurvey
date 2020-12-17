#' Make the SPOKEN_LANGS table
#' @export
#' @import dplyr
#' @import stringr
#'
#'
#' This function creates the SPOKEN_LANGS database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have columns 'responseID', 'consent', and 'completionCode'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.

# Make the SPOKEN_LANGS table ---------------------------------------------
makeSpokenLangs <- function(df, updateID){
  # grab responseID and languages spoken
  df <- df %>%
    select(responseID, spokenLangs) %>%
    mutate(spokenLangs = tolower(spokenLangs)) %>%
    filter(!is.na(spokenLangs)) # remove any NA languages

  # Separate rows
  df <- df %>%
    separate_rows(spokenLangs, sep = ",|;|\\sand|\\&|\\.\\.\\.|\\/") %>%
    rename(language = spokenLangs)

  # Strip out leading and trailing spaces
  df <- df %>%
    mutate(language = str_replace(language, "^\\s+", ""),
           language = str_replace(language, "\\s+$", ""))

  # Turn "na" or "n/a" or misspellings of "english" to NA
  df <- df %>%
    mutate(language = fct_recode(language,
                                 NULL = "english",
                                 NULL = "englilsh",
                                 NULL = "eng",
                                 NULL = "englsh",
                                 NULL = "englishs",
                                 NULL = "american",
                                 NULL = "american english",
                                 NULL = "british english",
                                 NULL = "englsih",
                                 NULL = "anglish",
                                 NULL = "englihs",
                                 NULL = "eniglsh",
                                 NULL = "eniglish",
                                 NULL = "enlgish",
                                 NULL = "enlgihs",
                                 NULL = "na",
                                 NULL = "englihg",
                                 NULL = "nelgihs",
                                 NULL = "enghlsh",
                                 NULL = "englush",
                                 NULL = "n/a",
                                 NULL = "none",
                                 NULL = "nothing")) %>%
    mutate(language = as.character(language)) %>%
    mutate(language = case_when(language %in% c("english", "engliah", "englaih",
                                                "englush", "ebglish", "englishe",
                                                "englaihe", "englshe", "ebglishe",
                                                "englisb", "engoish", "englsih",
                                                "enflsih", "englsh", "enflsib",
                                                "englsib") ~ NA_character_,
                                TRUE ~ language)) %>%
    filter(!is.na(language))
  # I'm going to stop with the processing there because there's not a heck of a lot else I can do automatically.
  # When I was doing manual processing, I kept running into situations where people wrote things like "french hausa", instead of "french, hausa", so the languages didn't get separated. However, I don't know how to automatically distinguish cases like that from cases like "egyptian arabic" where the two words legitimately refer to the same language. So we're going to leave it, and allow that some two-language values will definitely make it in.

  ## add updateID column
  spokenLangs <- df %>%
    mutate(updateID = updateID) %>%
    mutate_all(., .funs = as.character)

  message("Successfully created the SPOKEN_LANGS table.")

  ## Return
  return(spokenLangs)
}
