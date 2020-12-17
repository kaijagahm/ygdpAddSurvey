#' Make the DEMO_GEO table
#' @export
#' @import dplyr
#' @import stringr
#' @import hereR
#'
#' This function returns a **list**, not a **data frame**. The list contains the DEMO_GEO, CITIES, and CITIES_REF portions of the database. Note that for CITIES to be complete, it has to be further passed through the makeCensusCountyDemo.R function, which also outputs a list.
#' @param df Raw survey data as a data frame.
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param overwrite Whether or not to overwrite existing geocoded localities, if `geocoded` already exists in the environment. This parameter was mainly included for function testing and development--it pretty much never makes sense to set it to F when using the function.

# Make DEMO_GEO table -----------------------------------------------------
makeDemoGeo <- function(df, updateID, con, overwrite = T){
  # Pull in CITIES and CITIES_REF from the database
  citiesRef <- dbTable(con, "cities_ref")
  cities <- dbTable(con, "cities")

  # Create joinFailed var
  joinFailed <- F

  # 1. get info in wide format
  info <- df %>%
    select(responseID, currentCity, currentState,
           raisedCity, raisedState,
           motherCity, motherState, motherCountry,
           fatherCity, fatherState, fatherCountry,
           currentYears, raisedYears,
           gender, age, income, race, education, nLangs)

  # 2. Put locations into long format
  locs <- info %>%
    select(responseID, currentCity, currentState) %>%
    rename(city = currentCity, state = currentState) %>%
    mutate(country = NA, type = "current") %>%
    bind_rows(info %>%
                select(responseID, raisedCity, raisedState) %>%
                rename(city = raisedCity, state = raisedState) %>%
                mutate(type = "raised")) %>%
    bind_rows(info %>%
                select(responseID, motherCity, motherState, motherCountry) %>%
                rename(city = motherCity, state = motherState, country = motherCountry) %>%
                mutate(type = "mother")) %>%
    bind_rows(info %>%
                select(responseID, fatherCity, fatherState, fatherCountry) %>%
                rename(city = fatherCity, state = fatherState, country = fatherCountry) %>%
                mutate(type = "father"))

  # 2.5 Clean up the locations and create the `togeocode` column
  locs <- locs %>%
    mutate(country = str_replace(country, "\\s\\-\\s.*$", "")) %>%
    mutate(city = tolower(city),
           state = tolower(state),
           country = tolower(country),
           togeocode = paste(city, state, country, sep = " ")) %>%
    mutate(togeocode = str_replace_all(togeocode, "\\sNA|\\sNA\\s|NA\\s", "")) %>%
    mutate_all(as.character) %>%
    mutate(state = str_replace_all(state, "outside the united states", ""),
           togeocode = str_replace_all(togeocode, "outside the united states", "")) %>%
    mutate(togeocode = str_replace_all(togeocode, "^\\s|\\s$", "")) %>%
    mutate(togeocode = str_replace(togeocode, "united states", ""), # removing "united states" because for some reason that tends to make the geocoding mess up.
           togeocode = str_replace(togeocode, "united state", ""),
           togeocode = str_replace(togeocode, "usa", ""),
           togeocode = str_replace_all(togeocode, "\\d", ""),
           togeocode = str_replace_all(togeocode, "^\\s|\\s$", ""))

  # 3. Check whether any cities already exist in CITIES_REF from the database
  ## make sure we only have unique cities in citiesRef
  citiesRef <- citiesRef %>%
    group_by(city, state, country) %>%
    slice(1) # just keep the first cityID for each city/state/country combo

  ## test the join
  test <- locs %>%
    left_join(distinct(citiesRef) %>%
                select(city, state, country, cityID),
              by = c("city", "state", "country"))

  if(nrow(test) != nrow(locs)){
    joinFailed <- T
    warning("Join to CITIES_REF failed: may have duplicate entries in CITIES_REF table. Continuing without the CITIES_REF join.")
    locs <- locs %>%
      mutate(cityID = NA)
  }

  ## perform the join
  if(!joinFailed){
    locs <- locs %>%
      left_join(distinct(citiesRef) %>%
                  select(city, state, country, cityID),
                by = c("city", "state", "country"))
  }

  # 4. Grab the unique localities that weren't already geocoded
  unique_locs <- locs %>%
    filter(is.na(cityID)) %>% # only the ones that didn't already match
    select(togeocode) %>%
    distinct() %>% # this is important to avoid repeat geocoding!
    filter(!is.na(togeocode), togeocode != " ", togeocode != "", togeocode != "  ") %>% # remove blank strings
    mutate(id = 1:nrow(.)) # add ID: very important for joining back after geocoding!

  # 5. Geocode
  if((!exists("geocoded"))|(exists("geocoded") & overwrite == T)){
    set_key("4dWewuABghwiuv3xmctuplLYSxw4DfzevnGVX5AIalk")
    geocoded <- hereR::geocode(unique_locs$togeocode, sf = FALSE) %>%
      rename(cityName = city, stateID = state, countryID = country,
             long = lng, countyName = county) %>%
      select(cityName, stateID, countryID, lat, long, postalCode, id, countyName)
  }

  # Join back to unique_locs by ID
  unique_locs <- unique_locs %>%
    left_join(geocoded, by = "id") %>%
    select(-id) %>%
    mutate_at(vars(lat, long), .funs = as.character)

  # 7. Find matches in CITIES
  ## Are there any that match by lat/long?
  llmatch <- left_join(unique_locs,
                       cities %>% # get only the unique entries in CITIES
                         group_by(cityName, stateID, countryID) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(lat, long, cityID), by = c("lat", "long")) %>%
    select(togeocode, cityID) %>%
    filter(!is.na(cityID))

  ## Any that match by city, state, country, but not lat/long?
  cscmatch <- left_join(unique_locs,
                        cities %>%
                          group_by(cityName, stateID, countryID) %>%
                          slice(1) %>%
                          ungroup() %>%
                          select(cityName, stateID, countryID, cityID), by = c("cityName", "stateID", "countryID")) %>%
    select(togeocode, cityID) %>%
    filter(!(togeocode %in% llmatch$togeocode),
           !is.na(cityID))

  ## The rest are actually new
  nomatch <- unique_locs %>%
    filter(!(togeocode %in% cscmatch$togeocode), !(togeocode %in% llmatch$togeocode))

  # 8. Add new cities to CITIES
  maxID <- cities %>%
    pull(cityID) %>%
    str_replace("C", "") %>%
    as.numeric() %>%
    max()

  ## Add cityID's to nomatch
  nomatch <- nomatch %>%
    mutate(cityID = paste0("C", as.character(seq(from = maxID + 1, to = maxID + 1 + nrow(.)-1))))

  ## get everything to add back to locs
  backToLocs <- nomatch %>%
    select(togeocode, cityID) %>%
    bind_rows(llmatch, cscmatch) %>%
    group_by(togeocode) %>%
    slice(1) %>% # just in case there are multiples for the same togeocode value
    ungroup()

  ## Join back to locs by togeocode
  locs <- locs %>%
    left_join(backToLocs %>%
                rename(cityIDNew = cityID),
              by = "togeocode")

  ### Patch in the cityID's
  locs <- locs %>%
    # Add new cityID's to the ones that still had NA's
    mutate(cityID = case_when(is.na(cityID) ~ cityIDNew,
                              TRUE ~ cityID)) %>%
    select(-cityIDNew) # remove the new cityID columns.

  # Now we can add the new cities to CITIES
  toAddToCities <- nomatch %>%
    select(cityID, cityName, stateID, countyName, countryID,
           lat, long, postalCode) %>%
    mutate(updateID = updateID)

  ## add stateNames
  toAddToCities <- toAddToCities %>%
    left_join(lookupTable, by = "stateID") %>%
    # Remove state names for non-US states: e.g. India has a state abbreviated TN.
    mutate(stateName = case_when(countryID != "USA" ~ NA_character_,
                                 TRUE ~ as.character(stateName)))

  # Make CITIES output
  citiesOut <- toAddToCities  %>%
    mutate_all(., .funs = as.character)

  # Get the new rows to add to CITIES_REF
  citiesRefToAdd <- locs %>%
    select(city, state, country, cityID)

  citiesRefToAdd <- anti_join(citiesRefToAdd, citiesRef, by = c("city", "state", "country")) %>%
    mutate(updateID = updateID) %>%
    distinct() # take only distinct rows

  ### just to really make sure we're only getting one row each, let's group by city/state/country and slice
  citiesRefToAdd <- citiesRefToAdd %>%
    group_by(city, state, country) %>%
    slice(1) %>%
    ungroup()

  citiesRefOut <- citiesRefToAdd %>%
    mutate_all(., .funs = as.character)

  # Make DEMO_GEO output
  dg <- locs %>%
    select(responseID, type, cityID) %>%
    distinct() %>%
    mutate(type = fct_recode(type,
                             "currentCityID" = "current",
                             "raisedCityID" = "raised",
                             "motherCityID" = "mother",
                             "fatherCityID" = "father"),
           type = as.character(type)) %>%
    pivot_wider(id_cols = "responseID", names_from = "type", values_from = "cityID")

  ## get the rest of the demographic info
  dgRest <- df %>%
    select(responseID, currentYears, raisedYears, gender, age, income, race, raceOther, education, nLangs) %>%
    mutate(updateID = updateID,
           income = as.numeric(income)) %>%
    mutate_at(vars(gender, race, education, raceOther), .funs = tolower)

  ## add raceCats column
  dgRest <- dgRest %>%
    mutate(raceCats = case_when(grepl(",", race) ~ "other",
                                race == "white" ~ "white",
                                race == "other" ~ "other",
                                race == "asian" ~ "asian",
                                race == "hispanic" ~ "hispanic/latinx",
                                race == "black" ~ "black",
                                race == "amerindian" ~ "native american",
                                race == "pacific" ~ "pacific islander",
                                race == "NA" ~ NA_character_),
           race = case_when(is.na(raceOther) ~ race,
                            !is.na(raceOther) & !is.na(race) ~ paste(race, raceOther, sep = ","),
                            !is.na(raceOther) & is.na(race) ~ raceOther),
           race = str_replace(race, "other,", "")) %>%
    select(-raceOther) %>%
    mutate(education = fct_recode(education,
                                  "associate" = "associates",
                                  "bachelor's" = "bachelors",
                                  "graduate" = "graduate",
                                  "high school diploma" = "hs_diploma",
                                  "some college" = "no_degree",
                                  "some high school" = "no_hs_diploma"),
           updateID = updateID)

  # More cleaning, and put cols in the right order
  dg <- dg %>%
    left_join(dgRest, by = "responseID") %>%
    select(responseID, currentCityID, raisedCityID, motherCityID, fatherCityID, currentYears, raisedYears, gender, age, income, race, raceCats, education, nLangs, updateID) %>%
    mutate(raisedYears = as.numeric(raisedYears), # *** Add to sql script: these cols should be numeric
           currentYears = as.numeric(currentYears)) %>%
    mutate_all(., .funs = as.character)

  # Output demo_geo, cities, and cities_ref as a list
  listOut <- list("CITIES" = citiesOut,
                  "CITIES_REF" = citiesRefOut,
                  "DEMO_GEO" = dg)

  message("Successfully created the CITIES, CITIES_REF, and DEMO_GEO tables: output is a list.")

  return(listOut)
}
