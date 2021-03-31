#' Make the CENSUS_COUNTY_DEMO table
#'
#' This function returns a list, not a data frame. The list contains the CENSUS_COUNTY_DEMO and CITIES portions of the database. This modifies CITIES from the makeDemoGeo function and returns the finished version of CITIES.
#' @param cities The CITIES element of the list returned by makeDemoGeo.R. It will be passed into this function to be further modified before being finalized.
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export


# Make CENSUS_COUNTY_DEMO table -------------------------------------------
makeCensusCountyDemo <- function(cities, con, updateID){
  # Pull in CENSUS_COUNTY_DEMO
  censusCountyDemo <- dbTable(con, "census_county_demo")

  # Check that cities has lat and long cols
  if(!("lat" %in% names(cities) & "long" %in% names(cities))){
    stop("Argument `cities` must have 'lat' and 'long' columns.")
  }

  # make the lat/long in cities into an sf point object
  ll <- cities %>%
    filter(!is.na(lat)) %>%
    select(lat, long, cityID) %>%
    sf::st_as_sf(., coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
    sf::st_transform(., crs = 2163)

  # Join to county data (loaded at the beginning of this script). Drop geometry.
  citiesCountyData <- st_join(ll, counties %>% # transform counties to planar
                                st_transform(., crs = 2163)) %>%
    st_drop_geometry()

  # Now we're going to correct the county names in CITIES, since we've decided to privilege the county values from ESRI over the ones from the geocoder.
  cities <- cities %>%
    left_join(citiesCountyData %>%
                select(NAME, STATE_NAME, cityID) %>%
                rename(countyNameCensus = NAME,
                       stateNameCensus = STATE_NAME),
              by = "cityID") %>%
    mutate(across(c(stateName, stateNameCensus, countyName, countyNameCensus), as.character))

  # Keep ESRI state and county names
  cities <- cities %>%
    mutate(stateName = case_when(stateName != "Washington D.C." &
                                   stateName != stateNameCensus ~ stateNameCensus,
                                 TRUE ~ stateName)) %>%
    select(-stateNameCensus)

  # Re-do the state abbreviations
  cities <- cities %>%
    select(-stateID) %>%
    left_join(reftable_states, by = c("stateName" = "state")) %>%
    rename(stateID = abbreviation)

  # Keep the census county names
  cities <- cities %>%
    select(-countyName) %>%
    rename(countyName = countyNameCensus)

  # Reorder the columns
  citiesOut <- cities %>%
    select(cityID, cityName, stateID, stateName, countyName, countryID, lat, long, postalCode, updateID)%>%
    mutate(across(everything(), as.character))

  # --- Now for the actual census info
  censusCountyDemoOut <- citiesCountyData %>%
    select(-c("lat", "long")) %>%
    rename(countyName = NAME,
           stateName = STATE_NAME) %>%
    relocate(cityID) %>% # move cityID to the first position
    mutate(updateID = updateID) %>%
    filter(!is.na(FIPS)) # remove any that don't have county info attached (did not intersect; e.g. outside the united states)

  # remove any rows that have cityID's already present in ccd, just to make sure we don't introduce repeats.
  censusCountyDemoOut <- censusCountyDemoOut %>%
    filter(!(cityID %in% censusCountyDemo$cityID)) %>%
    mutate(across(everything(), as.character))

  # List output
  listOut <- list("CITIES" = citiesOut,
                  "CENSUS_COUNTY_DEMO" = censusCountyDemoOut)

  # Perform a basic check and send a success or error message.
  if(length(listOut) == 2 &
     !is.null(listOut[[1]]) &
     !is.null(listOut[[2]])){
    message(paste0("Successfully created the CENSUS_COUNTY_DEMO table with ", nrow(listOut[[2]]), " rows, and an updated version of the CITIES table with ", nrow(listOut[[1]]), " rows. Output is a list with the CITIES portion as element 1 and the CENSUS_COUNTY_DEMO portion as element 2."))
    return(listOut)
  }else{
    stop("Something's wrong with the makeCensusCountyDemo function. Either the output list does not have two elements, or one of the elements is NULL.")
  }
}

#' Make the CENSUS_URBAN_AREAS table
#'
#' This function returns a data frame, the CENSUS_URBAN_AREAS table of the database.
#' @param cities The CITIES element of the list returned by makeCensusCountyDemo.R (i.e. its final form)
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make CENSUS_URBAN_AREAS table -------------------------------------------
makeCensusUrbanAreas <- function (cities, con, updateID) {
  message("Binding urban areas geometry and info columns...") # message so people know why this is taking so long
  ua <- cbind(uaCols, uaGeom) %>% sf::st_as_sf(., crs = 4326) %>%  # bind together the two smaller datasets into one larger one
    sf::st_transform(., crs = 2163)
  if (ncol(ua) > 2) { # success message
    message("Successfully joined urban areas datasets. Preparing to create table...")
  }
  censusUrbanAreas <- dbTable(con, "census_urban_areas") # pull in the CENSUS_URBAN_AREAS table
  if (!("lat" %in% names(cities) & "long" %in% names(cities))) { # check for lat/long cols
    stop("Argument `cities` must have 'lat' and 'long' columns.")
  }
  ll <- cities %>% filter(!is.na(lat)) %>% select(lat, long,
                                                  cityID) %>% sf::st_as_sf(., coords = c("long", "lat"),
                                                                           crs = 4326, remove = FALSE) %>% sf::st_transform(., crs = 2163)
  citiesUrbanAreaData <- sf::st_join(ll, ua) %>% sf::st_drop_geometry() %>%
    select(-c("lat", "long"))
  cua <- citiesUrbanAreaData %>% mutate(updateID = updateID) %>% # remove NA's
    filter(!is.na(UACE10) & !is.na(GEOID10)) %>% filter(!(cityID %in%
                                                            censusUrbanAreas$cityID)) %>% mutate(across(everything(), as.character))
  if (!is.null(cua) & is.data.frame(cua) & nrow(cua) > 0) {
    message(paste0("Successfully created the CENSUS_URBAN_AREAS table with ",
                   nrow(cua), " rows."))
    return(cua)
  }else if(nrow(cua) == 0){
    message("Either all rows are already present in CENSUS_URBAN_AREAS or your cities did not intersect the US urban areas polygons (e.g. international cities). Output will have 0 rows.")
    return(cua)
  }
  else {
    stop("Something's wrong with the makeCensusUrbanAreas function. Unable to create the output table.")
  }
}

#' Make the COMMENTS table
#'
#' This function creates the COMMENTS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param questions The QUESTIONS portion of the database tables, created by makeQuestions.R
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make COMMENTS table -----------------------------------------------------
makeComments <- function(df, questions, updateID){
  justComments <- df %>%
    select(responseID, contains("comments", ignore.case = F))

  # pivot longer
  comments <- justComments %>%
    pivot_longer(cols = -responseID, names_to = "sentenceID", values_to = "comment") %>%
    left_join(questions %>%
                select(questionID, questionText),
              by = c("sentenceID" = "questionText")) %>%
    mutate(sentenceID = stringr::str_extract(sentenceID, "(?<=^comments\\_).*$"),
           updateID = updateID)

  # remove NA comments
  comments <- comments %>%
    filter(!is.na(comment))

  # Replace double quotation marks
  comments <- comments %>%
    mutate(comment = stringr::str_replace_all(comment, "\"", "'"),
           comment = stringr::str_replace_all(comment, "\n", "")) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(comments) & is.data.frame(comments) & nrow(comments) > 0){
    message(paste0("Successfully created the COMMENTS table with ",
                   nrow(comments), " rows."))
    return(comments)
  }else if(nrow(comments) == 0){
    warning("Your output has 0 rows.")
    return(comments)
  }
  else{
    stop("Something's wrong with the makeComments function. Unable to create the output table.")
  }
}

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
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(constructions) & is.data.frame(constructions) & nrow(constructions) > 0){
    message(paste0("Successfully created the CONSTRUCTIONS table with ",
                   nrow(constructions), " rows."))
    return(constructions)
  }else if(nrow(constructions) == 0){
    warning("Your output has 0 rows.")
    return(constructions)
  }else{
    stop("Something's wrong with the makeConstructions function. Unable to create the output table.")
  }
}

#' Make the DEMO_GEO table
#'
#' This function returns a list, not a data frame. The list contains the DEMO_GEO, CITIES, and CITIES_REF portions of the database. Note that for CITIES to be complete, it has to be further passed through the makeCensusCountyDemo.R function, which also outputs a list.
#' @param df Raw survey data as a data frame.
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param key The API key for the hereR API
#' @param overwrite Whether or not to overwrite existing geocoded localities, if `geocoded` already exists in the environment. This parameter was mainly included for function testing and development--it pretty much never makes sense to set it to F when using the function.
#' @import forcats
#' @export

# Make DEMO_GEO table -----------------------------------------------------
makeDemoGeo <- function(df, updateID, key, con, overwrite = T){
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
    mutate(country = stringr::str_replace(country, "\\s\\-\\s.*$", "")) %>%
    mutate(city = tolower(city),
           state = tolower(state),
           country = tolower(country),
           togeocode = paste(city, state, country, sep = " ")) %>%
    mutate(togeocode = stringr::str_replace_all(togeocode, "\\sNA|\\sNA\\s|NA\\s", "")) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(state = stringr::str_replace_all(state, "outside the united states", ""),
           togeocode = stringr::str_replace_all(togeocode, "outside the united states", "")) %>%
    mutate(togeocode = stringr::str_replace_all(togeocode, "^\\s|\\s$", "")) %>%
    mutate(togeocode = stringr::str_replace(togeocode, "united states", ""), # removing "united states" because for some reason that tends to make the geocoding mess up.
           togeocode = stringr::str_replace(togeocode, "united state", ""),
           togeocode = stringr::str_replace(togeocode, "usa", ""),
           togeocode = stringr::str_replace_all(togeocode, "\\d", ""),
           togeocode = stringr::str_replace_all(togeocode, "^\\s|\\s$", ""))

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
    {if("cityID" %in% names(.)) filter(., is.na(cityID)) else .} %>% # only the ones that didn't already match
    select(togeocode) %>%
    distinct() %>% # this is important to avoid repeat geocoding!
    filter(!is.na(togeocode), togeocode != " ", togeocode != "", togeocode != "  ") %>% # remove blank strings
    mutate(id = 1:nrow(.)) # add ID: very important for joining back after geocoding!

  # 5. Geocode
  if((!exists("geocoded"))|(exists("geocoded") & overwrite == T)){
    hereR::set_key(key)
    geocoded <- hereR::geocode(unique_locs$togeocode, sf = FALSE) %>%
      rename(cityName = city, stateID = state_code, stateName = state,
             countryID = country_code, long = lng_position, lat = lat_position,
             countyName = county, postalCode = postal_code) %>%
      select(cityName, stateID, stateName, countryID, lat, long, postalCode, id, countyName)
  }

  # Join back to unique_locs by ID
  unique_locs <- unique_locs %>%
    left_join(geocoded, by = "id") %>%
    select(-id) %>%
    mutate(across(c(lat, long), as.character))

  # 7. Find matches in CITIES
  ## Are there any that match by lat/long?
  llmatch <- left_join(unique_locs,
                       cities %>% # get only the unique entries in CITIES
                         group_by(cityName, stateName, countryID) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(lat, long, cityID), by = c("lat", "long")) %>%
    select(togeocode, cityID) %>%
    filter(!is.na(cityID))

  ## Any that match by city, state, country, but not lat/long?
  cscmatch <- left_join(unique_locs,
                        cities %>%
                          group_by(cityName, stateName, countryID) %>%
                          slice(1) %>%
                          ungroup() %>%
                          select(cityName, stateName, countryID, cityID), by = c("cityName", "stateName", "countryID")) %>%
    select(togeocode, cityID) %>%
    filter(!(togeocode %in% llmatch$togeocode),
           !is.na(cityID))

  ## The rest are actually new
  nomatch <- unique_locs %>%
    filter(!(togeocode %in% cscmatch$togeocode), !(togeocode %in% llmatch$togeocode))

  # 8. Add new cities to CITIES
  maxID <- cities %>%
    pull(cityID) %>%
    stringr::str_replace("C", "") %>%
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
    {if(!"cityID" %in% names(.)) mutate(., cityID = NA_character_) else .} %>%
    # Add new cityID's to the ones that still had NA's
    mutate(cityID = case_when(is.na(cityID) ~ cityIDNew,
                              TRUE ~ cityID)) %>%
    select(-cityIDNew) # remove the new cityID columns.

  # Now we can add the new cities to CITIES
  toAddToCities <- nomatch %>%
    select(cityID, cityName, stateID, stateName, countyName, countryID,
           lat, long, postalCode) %>%
    mutate(updateID = updateID) %>%
    mutate(stateName = case_when(countryID != "USA" ~ NA_character_,
                                 TRUE ~ stateName)) # remove state names for non-US states, purely to maintain backwards compatibility with older versions of the database (i.e. checking for duplicate city entries)

  # Make CITIES output
  citiesOut <- toAddToCities  %>%
    mutate(across(everything(), as.character))

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
    mutate(across(everything(), as.character))

  # Make DEMO_GEO output
  dg <- locs %>%
    select(responseID, type, cityID) %>%
    distinct() %>%
    mutate(type = forcats::fct_recode(type,
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
           income = suppressWarnings(as.numeric(as.character(income)))) %>%
    mutate(across(c(gender, race, education, raceOther), tolower))

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
           race = stringr::str_replace(race, "other,", "")) %>%
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
    mutate(across(everything(), as.character))

  # Output demo_geo, cities, and cities_ref as a list
  listOut <- list("CITIES" = citiesOut,
                  "CITIES_REF" = citiesRefOut,
                  "DEMO_GEO" = dg)

  # Perform a basic check and send a success or error message.
  if(length(listOut) == 3 & !is.null(listOut[[1]]) & !is.null(listOut[[2]]) & !is.null(listOut[[3]])){
    message(paste0("Successfully created the CITIES/CITIES_REF/DEMO_GEO table list with ", nrow(listOut[[1]]), ", ", nrow(listOut[[2]]), ", and ", nrow(listOut[[3]]), " rows, respectively."))
    return(listOut)
  }else{
    stop("Something's wrong with the makeDemoGeo function. There may have been a problem creating the output list, or one of the list elements is null.")
  }
}

#' Make the DIALECT_REGIONS table
#'
#' Function to create the DIALECT_REGIONS database table.
#' @param cities The CITIES table, created with makeDemoGeo.R and makeCensusCountyDemo.R.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make DIALECT_REGIONS table ----------------------------------------------
makeDialectRegions <- function(cities, updateID){
  # Check that `cities` contains the correct columns
  targetCols <- c("lat", "long", "cityID")
  if(!all(targetCols %in% names(cities))){
    stop(paste0("`cities` must contain the following column names: ",
                paste(targetCols, collapse = ","),
                ". The following column names are missing: ",
                paste(targetCols[which(!(targetCols %in% names(cities)))], collapse = ","),
                "."))
  }

  # Make the table
  ll <- cities %>%
    select(lat, long, cityID) %>%
    st_as_sf(., coords = c("long", "lat")) %>%
    st_set_crs(4326) %>% # coords are originally set to 4326, so we have to define it as such
    st_transform(., 2163) # but need to transform coords to planar for spatial extractions

  # Perform each of the spatial joins.
  ## Occasionally, a point overlaps with more than one dialect region. I don't know how to choose, so I've opted to just take the first instance of overlap each time.
  nsw_join <- st_join(ll, nsw %>% # transform to planar
                        st_transform(., crs = 2163)) %>%
    rename(regionNSW = Dialect_Re) %>%
    select(cityID, regionNSW) %>%
    group_by(cityID) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry()

  anae_join <- st_join(ll, anae %>% # transform to planar
                         st_transform(., crs = 2163)) %>%
    rename(regionANAE = Dialect_Re) %>%
    select(cityID, regionANAE) %>%
    group_by(cityID) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry()

  carver_join <- st_join(ll, carver %>% # transform to planar
                           st_transform(., crs = 2163)) %>%
    rename(regionCarver = Region,
           subRegionCarver = Sub_Region,
           subSubRegionCarver = SubSub_Reg) %>%
    select(cityID, regionCarver, subRegionCarver, subSubRegionCarver) %>%
    group_by(cityID) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry()

  # Join together all of these regions.
  dialect_regions <- left_join(nsw_join, anae_join, by = "cityID") %>%
    left_join(., carver_join, by = "cityID")
  nrow(dialect_regions)

  # Make a column for the most specific Carver region
  dialect_regions <- dialect_regions %>%
    mutate(mostSpecificRegionCarver = case_when(!is.na(regionCarver) ~
                                                  as.character(regionCarver),
                                                is.na(regionCarver) & !is.na(subRegionCarver) ~
                                                  as.character(subRegionCarver),
                                                is.na(regionCarver) & is.na(subRegionCarver) & !is.na(subSubRegionCarver) ~
                                                  as.character(subSubRegionCarver),
                                                TRUE ~ NA_character_))

  # Add updateID
  dialect_regions <- dialect_regions %>%
    mutate(updateID = updateID)

  # Perform a basic check and send a success or error message.
  if(!is.null(dialect_regions) & is.data.frame(dialect_regions) & nrow(dialect_regions) > 0){
    message(paste0("Successfully created the DIALECT_REGIONS table with ",
                   nrow(dialect_regions), " rows."))
    return(dialect_regions)
  }else if(nrow(dialect_regions) == 0){
    message("Your output has 0 rows. Perhaps none of the cities intersected with the given dialect regions?")
    return(dialect_regions)
  }
  else{
    stop("Something's wrong with the makeDialectRegions function. Unable to create the output table.")
  }
}

#' Make the QUESTIONS table
#'
#' This function creates the QUESTIONS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param qids Data frame containing questionID/column name mappings for this survey. This is created using the `saveQuestionIDs()` function. Must have columns 'colName' and 'questionIDRaw'.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export
#'
# Make QUESTIONS table ----------------------------------------------------
# `qids` is a saved df of the format output by saveQuestionIDs
makeQuestions <- function(df, qids, surveyID, updateID){
  ## Check whether `qids` has the correct format
  if(!is.data.frame(qids)){
    stop("Argument `qids` must be a data frame. Use `saveQuestionIDs()` to create `qids`.")
  }
  if(any(names(qids) != c("colName", "questionIDRaw"))){
    stop("Argument `qids` must have columns `colName` and `questionIDRaw`. Use `saveQuestionIDs()` to create `qids`.")
  }

  # Make the df
  questions <- qids %>%
    rename(questionText = colName,
           questionID = questionIDRaw) %>%
    mutate(questionID = paste0(surveyID, "_", questionID)) %>%
    mutate(surveyID = surveyID,
           stimulusType = NA,
           scaleOptions = NA,
           problems = NA,
           updateID = updateID)

  # To do later:
  ## update stimulus types
  ## Fix the questionText column--sometimes it has the text, sometimes not.
  # Reorder the cols
  questions <- questions %>%
    select(questionID, questionText, surveyID, stimulusType, scaleOptions, problems, updateID) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(questions) & is.data.frame(questions) & nrow(questions) > 0){
    message(paste0("Successfully created the QUESTIONS table with ",
                   nrow(questions), " rows."))
    return(questions)
  }else if(nrow(questions) == 0){
    warning("Your output has zero rows.")
    return(questions)
  }else{
    stop("Something's wrong with the makeQuestions function. Unable to create the output table.")
  }
}

#' Make the RATINGS table
#'
#' This function creates the RATINGS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param questions The QUESTIONS portion of the database tables, created by makeQuestions.R
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make RATINGS table ------------------------------------------------------
makeRatings <- function(df, questions, surveyID, updateID){
  justSentences <- df %>%
    select(responseID,
           contains("TS", ignore.case = F),
           contains("CU", ignore.case = F),
           contains("CG", ignore.case = F)) %>%
    select(-contains("comments"))

  # pivot longer
  ratings <- justSentences %>%
    pivot_longer(cols = -responseID,
                 names_to = "sentenceID",
                 values_to = "rating") %>%
    left_join(questions %>%
                select(questionID, questionText),
              by = c("sentenceID" = "questionText")) %>%
    mutate(sentenceID = stringr::str_extract(sentenceID, "(?<=\\_).*$"),
           surveyID = surveyID,
           updateID = updateID)

  # put cols in the right order
  ratings <- ratings %>%
    select(responseID, sentenceID, surveyID, questionID, rating, updateID)

  # Remove NA ratings
  ratings <- ratings %>%
    filter(!is.na(ratings)) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(ratings) & is.data.frame(ratings) & nrow(ratings) > 0){
    message(paste0("Successfully created the RATINGS table with ",
                   nrow(ratings), " rows."))
    return(ratings)
  }else if(nrow(ratings) == 0){
    warning("Your output has 0 rows.")
    return(ratings)
  }else{
    stop("Something's wrong with the makeRatings function. Unable to create the output table.")
  }
}

#' Make the RESPONSES table
#'
#' This function creates the RESPONSES database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have column 'responseID'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @export

makeResponses <- function(df, updateID, surveyID){
  # check for required cols
  required <- c("responseID")
  if(any(!(required %in% names(df)))){
    stop("The 'responseID' column is missing or misspelled. Please check your column names and try again.")
  }

  # initialize the responses table
  responses <- df %>%
    select(responseID, amazonID) %>%
    mutate(surveyID = surveyID,
           updateID = updateID) %>%
    distinct() %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(responses) & is.data.frame(responses) & nrow(responses) > 0){
    message(paste0("Successfully created the RESPONSES table with ",
                   nrow(responses), " rows."))
    return(responses)
  }else if(nrow(responses) == 0){
    warning("Your output has 0 rows.")
    return(responses)
  }else{
    stop("Something's wrong with the makeResponses function. Unable to create the output table.")
  }
}

#' Make the SENTENCES table
#'
#' This function creates the SENTENCES database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param masterList Master list of all sentences. ### XXX COME BACK TO THIS: HOW BEST TO LOAD AND UPDATE?
#' @param con Connection to the current version of the database. Create the connection using RSQLite. For example: con <- dbConnect(RSQLite::SQLite(), here("database", "currentDB", "ygdpDB.db")) (for the database file ygdpDB.db, stored in "database/currentDB".)
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export
#'
# Make SENTENCES table ----------------------------------------------------
makeSentences <- function(df, masterList, con, updateID){
  # Get sentenceIDs for CU sentences
  cuIDs <- df %>%
    select(contains("CU_", ignore.case = F)) %>%
    names() %>%
    stringr::str_extract(., "(?<=CU\\_).*") # select everything after the "CU_"

  # Check whether the CU sentenceID's contain characters other than numbers and '.'
  if(!(all(grepl("^[0-9,\\.]*$", cuIDs)))){
    message(paste0("CU sentenceID's contain characters other than numbers and '.' The offenders are: ", paste(cuIDs[!(grepl("^[0-9,\\.]*$", cuIDs))], collapse = ", "),  ". Consider whether this is a problem, and fix your sentence column names if need be."))
  }

  # Get sentenceIDs for CG sentences
  cgIDs <- df %>%
    select(contains("CG_", ignore.case = F)) %>%
    names() %>%
    stringr::str_extract(., "(?<=CG\\_).*") # select everything after the "CG_"

  # Check whether the CG sentenceID's contain characters other than numbers and '.'
  if(!(all(grepl("^[0-9,\\.]*$", cgIDs)))){
    message(paste0("CG sentenceID's contain characters other than numbers and '.' The offenders are: ",
                   paste(cgIDs[!(grepl("^[0-9,\\.]*$", cgIDs))], collapse = ", "),
                   ". Consider whether this is a problem, and fix your sentence column names if need be."))
  }

  # Get sentenceIDs for test sentences
  tsIDs <- df %>%
    select(contains("TS_", ignore.case = F)) %>%
    names() %>%
    stringr::str_extract(., "(?<=TS\\_).*") # select everything after the "TS_"

  # Check whether the TS sentenceID's contain characters other than numbers and '.'
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
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(sentences) & is.data.frame(sentences) & nrow(sentences) > 0){
    message("Successfully created the SENTENCES table.")
    message(paste0(n, " sentences out of ", tot, " were already in the database, so your final SENTENCES table contains ", tot-n, " sentences."))
    return(sentences)
  }else if(nrow(sentences) == 0){
    message("All sentences are already in the SENTENCES table; your newly-created table will have 0 rows.")
    return(sentences)
  }else{
    stop("Something's wrong with the makeSentences function. Unable to create the output table.")
  }
}

#' Make the SPOKEN_LANGS table
#'
#' This function creates the SPOKEN_LANGS database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have columns 'responseID', 'consent', and 'completionCode'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make the SPOKEN_LANGS table ---------------------------------------------
makeSpokenLangs <- function(df, updateID){
  # Check for the `responseID` and `spokenLangs` columns
  targets <- c("responseID", "spokenLangs")
  if(!all(targets %in% names(df))){
    stop("df must contain columns `responseID` and `spokenLangs`.")
  }

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
    mutate(language = stringr::str_replace(language, "^\\s+", ""),
           language = stringr::str_replace(language, "\\s+$", ""))

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
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(spokenLangs) & is.data.frame(spokenLangs) & nrow(spokenLangs) > 0){
    message(paste0("Successfully created the SPOKEN_LANGS table with", nrow(spokenLangs), " rows."))
    return(spokenLangs)
  }else if(nrow(spokenLangs == 0)){
    warning("Your output has 0 rows; perhaps no participants spoke additional languages?")
    return(spokenLangs)
  }else{
    stop("Something's wrong with the makeSpokenLangs function. Unable to create the output table.")
  }
}

#' Make the SURVEY_COMMENTS table
#'
#' This function creates the SURVEY_COMMENTS database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have columns 'responseID' and 'generalComments'.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param qids Data frame containing questionID/column name mappings for this survey. Create this using the `saveQuestionIDs()` function.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#'
#'@export
#'
makeSurveyComments <- function(df, surveyID, qids, updateID){
  # check for required cols
  required <- c("responseID", "generalComments")
  if(any(!(required %in% names(df)))){
    stop("One or more of the columns 'responseID' or 'generalComments' are missing or misspelled. Please check column names and try again.")
  }

  # Initialize table
  surveyComments <- df %>%
    select(responseID, generalComments) %>%
    rename(comment = generalComments) %>%
    distinct()

  # Get questionID
  if("generalComments" %in% qids$colName){
    a <- qids %>%
      filter(colName == "generalComments") %>%
      pull(questionIDRaw) %>%
      stringr::str_extract("QID\\d+")
    b <- paste0(surveyID, a) # make the full questionID string
  }else{
    b <- NA
  }

  # Add cols to surveyComments
  surveyComments <- surveyComments %>%
    mutate(surveyID = surveyID,
           questionID = b,
           updateID = updateID)

  # Remove any where the comments field is blank
  surveyComments <- surveyComments %>%
    filter(!is.na(comment)) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(surveyComments) & is.data.frame(surveyComments) & nrow(surveyComments) > 0){
    message(paste0("Successfully created the SURVEY_COMMENTS table with", nrow(surveyComments), " rows."))
    return(surveyComments)
  }else if(nrow(surveyComments) == 0){
    warning("Your output has 0 rows. Perhaps nobody had any comments?")
    return(surveyComments)
  }else{
    stop("Something's wrong with the makeSurveyComments function. Unable to create the output table.")
  }
}

#' Make the SURVEYS table
#'
#' This function creates the SURVEYS database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param admin Character string: name of the person who administered the survey. For example, "Jim Wood".
#' @param surveyName Character string naming the survey (this is an opportunity to be more descriptive than `surveyID` allows.) For example, the `surveyName` for Survey 11 might be "Survey 11". Or, if you did a survey focused on a particular construction, you might call it "Needs Washed Survey".
#' @export

# Make SURVEYS table ------------------------------------------------------
makeSurveys <- function(df, surveyID, updateID, admin, surveyName){
  # Get start and end dates
  forDates <- df %>%
    select(dateTimeStart, dateTimeEnd) %>%
    mutate(across(everything(), ~lubridate::parse_date_time(.x, orders = c("ymd_HMS"))))
  dateReleased <- min(forDates$dateTimeStart)
  dateClosed <- max(forDates$dateTimeEnd)

  # make the data frame
  surveys <- data.frame(surveyID = surveyID,
                        surveyName = surveyName,
                        dateReleased = dateReleased,
                        dateClosed = dateClosed,
                        administrator = admin,
                        updateID = updateID) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(surveys) & is.data.frame(surveys) & nrow(surveys) > 0){
    message(paste0("Successfully created the SURVEYS table with", nrow(surveys), " rows."))
    return(surveys)
  }else if(nrow(surveys) == 0){
    warning("Your output has 0 rows.")
    return(surveys)
  }else{
    stop("Something's wrong with the makeSurveys function. Unable to create the output table.")
  }
}

#' Make the SURVEY_SENTENCES table
#'
#' This function creates the SURVEY_SENTENCES database table from raw survey data.
#' @param df Raw survey data as a data frame.
#' @param surveyID Character string that will become the `surveyID` column for this table. For example, "S11" for Survey 11.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export

# Make the SURVEY_SENTENCES table -----------------------------------------
makeSurveySentences <- function(df, surveyID, updateID){
  sentence_nums <- df %>%
    select(matches("CU\\_|CG\\_|TS\\_")) %>%
    select(!matches("comments\\_")) %>%
    names() %>%
    stringr::str_extract(., "(?<=[A-Z]\\_)[0-9,\\.]+$") # only the sentence number

  # initialize
  surveySentences <- data.frame(
    sentenceID = sentence_nums,
    surveyID = surveyID,
    updateID = updateID
  ) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(surveySentences) & is.data.frame(surveySentences) & nrow(surveySentences) > 0){
    message(paste0("Successfully created the SURVEY_SENTENCES table with", nrow(surveySentences), " rows."))
    return(surveySentences)
  }else if(nrow(surveySentences) == 0){
    warning("Your output has 0 rows.")
    return(surveySentences)
  }else{
    stop("Something's wrong with the makeSurveySentences function. Unable to create the output table.")
  }
}

# Make the TECH table -----------------------------------------------------
#' This function creates the TECH database table from raw survey data.
#' @param df Raw survey data as a data frame. Must have columns 'responseID', 'consent', and 'completionCode'.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @export
makeTech <- function(df, updateID){
  # check for essential columns
  ## Must have responseID, consent, completionCode.
  required <- c("responseID", "consent", "completionCode")
  if(any(!(required %in% names(df)))){
    stop("One or more of the columns 'responseID', 'consent', or 'completionCode' is missing or misspelled. Please check column names and try again.")
  }

  # turn all cols to character, for consistency
  df <- df %>%
    mutate(across(everything(), as.character))

  # initialize df
  tech <- df[,names(df)[names(df) %in% c("responseID", "dateTimeStart", "dateTimeEnd", "ipAddress", "progress",
                                   "durationSeconds", "finished", "recordedDate", "consent", "browser",
                                   "version", "operatingSystem", "resolution", "hasAmazonID", "completionCode",
                                   "locationLat", "locationLong", "userLanguage")]] %>%
    mutate(updateID = updateID) # user defines the updateID

  # set hasAmazonID to "No" if it's NA
  tech <- tech %>%
    mutate(hasAmazonID = case_when(is.na(hasAmazonID) ~ "No",
                                   TRUE ~ hasAmazonID))

  # Only distinct rows, in case of any repeats
  tech <- tech %>%
    distinct() %>%
    mutate(across(everything(), as.character))

  # Convert any "" to NA
  tech <- tech %>%
    mutate(across(everything(), .fns = ~na_if(.x, "")))

  # Perform a basic check and send a success or error message.
  if(!is.null(tech) & is.data.frame(tech) & nrow(tech) > 0){
    message(paste0("Successfully created the TECH table with", nrow(tech), " rows."))
    return(tech)
  }else if(nrow(tech) == 0){
    warning("Your output has 0 rows.")
    return(tech)
  }else{
    stop("Something's wrong with the makeTech function. Unable to create the output table.")
  }
}

#' Make the UPDATE_METADATA table
#'
#' This function creates the row of the UPDATE_METADATA table that corresponds to the updateID that we've been using for all the other database tables.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.
#' @param date Date of the update, in the format YYYY-MM-DD
#' @param updater Name of the person doing the update (first and last), e.g. Kaija Gahm
#' @param description Description of the update. For a standard database update, it's just "added [surveyID]".
#' @param con A connection to the existing database, created with RSQLite.
#' @param sourceCode The name of the R script used to perform this update.
#'@export
#'
# Make UPDATE_METADATA table ----------------------------------------------
makeUpdateMetadata <- function(updateID, date, updater, description, con, sourceCode){
  # Get the existing VERSION_HISTORY TABLE
  dbVersionHistory <- dbTable(con, "version_history")

  # Get the most recent version number
  lastVersion <- dbVersionHistory[nrow(dbVersionHistory), "versionNumber"]

  # Create new version number: adding a survey updates the middle number, so we add 1 to the current middle number and then make the last number into 0.
  newVersion <- lastVersion %>%
    stringr::str_replace(., "(?<=\\.)\\d+(?=\\.)", as.character(as.numeric(stringr::str_extract(lastVersion, "(?<=\\.)\\d+(?=\\.)")) + 1)) %>%
    stringr::str_replace(., "(?<=\\.)\\d+$", "0")

  # Make the table with the user-generated information.
  updateMetadata <- data.frame(updateID = updateID,
                               date = date,
                               updater = updater,
                               metadata = description,
                               versionNumber = newVersion,
                               sourceCode = sourceCode) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(updateMetadata) & is.data.frame(updateMetadata) & nrow(updateMetadata) > 0){
    message(paste0("Successfully created the UPDATE_METADATA table with", nrow(updateMetadata), " rows."))
    return(updateMetadata)
  }else{ # note that there's no case here where a zero-row output makes sense, since the updateID should always be added.
    stop("Something's wrong with the makeUpdateMetadata function. Unable to create the output table.")
  }
}

#' Make the VERSION_HISTORY table
#'
#' This function creates the row of the VERSION_HISTORY table that corresponds to the update we're making for this survey
#' @param con A connection to the existing database, created with RSQLite.
#' @param date Date of the update, in the format YYYY-MM-DD
#' @param description Description of the version. For a standard database update, it's just "added [surveyID]".
#'
#'@export
# Make VERSION_HISTORY table ---------------------------------------------
makeVersionHistory <- function(con, date, description){
  # Get VERSION_HISTORY
  dbVersionHistory <- dbTable(con, "version_history")

  # Get the most recent version number
  lastVersion <- dbVersionHistory[nrow(dbVersionHistory), "versionNumber"]

  # Create new version number: adding a survey updates the middle number, so we add 1 to the current middle number and then make the last number into 0.
  newVersion <- lastVersion %>%
    stringr::str_replace(., "(?<=\\.)\\d+(?=\\.)", as.character(as.numeric(stringr::str_extract(lastVersion, "(?<=\\.)\\d+(?=\\.)")) + 1)) %>%
    stringr::str_replace(., "(?<=\\.)\\d+$", "0")

  # Make the new row for versionHistory
  versionHistory <- data.frame(versionNumber = newVersion,
                               date = date,
                               description = description) %>%
    mutate(across(everything(), as.character))

  # Perform a basic check and send a success or error message.
  if(!is.null(versionHistory) & is.data.frame(versionHistory) & nrow(versionHistory) > 0){
    message(paste0("Successfully created the VERSION_HISTORY table with", nrow(versionHistory), " rows."))
    return(versionHistory)
  }else{ # note that there's no case here where a zero-row output makes sense, since the version number should always be added.
    stop("Something's wrong with the makeVersionHistory function. Unable to create the output table.")
  }
}

