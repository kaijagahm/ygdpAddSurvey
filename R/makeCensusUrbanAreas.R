#' Make the CENSUS_URBAN_AREAS table
#' @export
#' @import dplyr
#' @import sf
#'
#' This function returns a data frame, the CENSUS_URBAN_AREAS table of the database.
#' @param cities The CITIES element of the list returned by makeCensusCountyDemo.R (i.e. its final form)
#' @param con A connection to the existing database, created with RSQLite.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.

# Make CENSUS_URBAN_AREAS table -------------------------------------------
makeCensusUrbanAreas <- function(cities, con, updateID){
  # Pull in CENSUS_URBAN_AREAS
  censusUrbanAreas <- dbTable(con, "census_urban_areas")

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

  # Join to urban areas and drop geometry
  citiesUrbanAreaData <- st_join(ll, ua) %>%
    st_drop_geometry() %>%
    select(-c("lat", "long"))

  # Add updateID and filter out NA's
  cua <- citiesUrbanAreaData %>%
    mutate(updateID = updateID) %>%
    filter(!is.na(UACE10) & !is.na(GEOID10)) %>%
    filter(!(cityID %in% censusUrbanAreas$cityID)) %>% # remove any cities that might already be in CENSUS_URBAN_AREAS.
    mutate_all(., .funs = as.character)

  message("Successfully created CENSUS_URBAN_AREAS.")

  return(cua)
}
