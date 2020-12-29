# This script loads in the raw shapefile data, processes it with sf, and saves it using usethis::use_data() to the data/ folder of this package.

# Load packages we'll need for this processing
library(dplyr)
library(sf)
library(rgdal)
library(usethis)
library(here)

# Read in the shapefiles
anae <- st_read(here("data-raw", "ANAE"))
carver <- st_read(here("data-raw", "Carver"))
ua <- st_read(here("data-raw", "censusUrbanAreas", "tl_2019_us_uac10"))
nsw <- st_read(here("data-raw", "NSW"))
counties <- st_read(here("data-raw", "USA_Counties", "v107", "counties.gdb"))

use_data(anae, overwrite = TRUE)
use_data(carver, overwrite = TRUE)
use_data(ua, overwrite = TRUE)
use_data(nsw, overwrite = TRUE)
use_data(counties, overwrite = TRUE)
