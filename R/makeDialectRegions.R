#' Make the DIALECT_REGIONS table
#' @export
#' @import dplyr
#' @import sf
#'
#' Function to create the DIALECT_REGIONS database table.
#' @param cities The CITIES table, created with makeDemoGeo.R and makeCensusCountyDemo.R.
#' @param updateID Character string that will become the `updateID` column for this table. For example, "survey11Add" for Survey 11.

# Make DIALECT_REGIONS table ----------------------------------------------
makeDialectRegions <- function(cities, updateID){
  ll <- cities %>%
    select(lat, long, cityID) %>%
    st_as_sf(., coords = c("long", "lat")) %>%
    st_set_crs(4326) %>% # coords are originally set to 4326, so we have to define it as such
    st_transform(., 2163) # but need to transform coords to planar for spatial extractions

  # Perform each of the spatial joins.
  ## Occasionally, a point overlaps with more than one dialect region. I don't know how to choose, so I've opted to just take the first instance of overlap each time.
  nsw_join <- st_join(ll, nsw) %>%
    rename(regionNSW = Dialect_Re) %>%
    select(cityID, regionNSW) %>%
    group_by(cityID) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry()

  anae_join <- st_join(ll, anae) %>%
    rename(regionANAE = Dialect_Re) %>%
    select(cityID, regionANAE) %>%
    group_by(cityID) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry()

  carver_join <- st_join(ll, carver) %>%
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

  # Message
  message("Successfully created DIALECT_REGIONS")

  # return
  return(dialect_regions)
}
