# This script loads in the raw shapefile data, processes it with sf, and saves it using usethis::use_data() to the data/ folder of this package.

# Load packages we'll need for this processing
library(dplyr)
library(sf)
library(usethis)
library(here)

# Read in the shapefiles
anae <- st_read(here("data-raw", "ANAE")) %>% st_as_sf(., crs = 4326)
carver <- st_read(here("data-raw", "Carver")) %>% st_as_sf(., crs = 4326)
nsw <- st_read(here("data-raw", "NSW")) %>% st_as_sf(., crs = 4326)
counties <- st_read(here("data-raw", "USA_Counties", "v107", "counties.gdb")) %>% st_as_sf(., crs = 4326)

# Processed UA separately, outside of the package in shrinkUAShapefile.R and saved it, compressed and with most columns removed, to the data/ folder of the package.

use_data(anae, overwrite = TRUE, version = 3, compress = "xz")
use_data(carver, overwrite = TRUE, version = 3, compress = "xz")
use_data(nsw, overwrite = TRUE, version = 3, compress = "xz")
use_data(counties, overwrite = TRUE, version = 3, compress = "xz")
