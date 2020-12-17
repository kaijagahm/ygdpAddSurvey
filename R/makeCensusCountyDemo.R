#' Make the CENSUS_COUNTY_DEMO table
#' @export
#' @import dplyr
#' @import sf
#'
#' This function returns a **list**, not a **data frame**. The list contains the CENSUS_COUNTY_DEMO and CITIES portions of the database. This modifies CITIES from the makeDemoGeo function and returns the finished version of CITIES.
#' @param cities The CITIES element of the list returned by makeDemoGeo.R. It will be passed into this function to be further modified before being finalized.
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.


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
    st_as_sf(., coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
    st_transform(., crs = 2163)

  # Join to county data (loaded at the beginning of this script). Drop geometry.
  citiesCountyData <- st_join(ll, counties) %>%
    st_drop_geometry()

  # Now we're going to correct the county names in CITIES, since we've decided to privilege the county values from ESRI over the ones from the geocoder.
  cities <- cities %>%
    left_join(citiesCountyData %>%
                select(NAME, STATE_NAME, cityID) %>%
                rename(countyNameCensus = NAME,
                       stateNameCensus = STATE_NAME),
              by = "cityID") %>%
    mutate_at(vars(stateName, stateNameCensus, countyName, countyNameCensus),
              .funs = as.character)

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
    mutate_all(., .funs = as.character)

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
    mutate_all(., .funs = as.character)

  # List output
  listOut <- list("CITIES" = citiesOut,
                  "CENSUS_COUNTY_DEMO" = censusCountyDemoOut)

  message("Successfully created the CENSUS_COUNTY_DEMO table and an updated version of the CITIES table: output is a list.")

  return(listOut)
}
