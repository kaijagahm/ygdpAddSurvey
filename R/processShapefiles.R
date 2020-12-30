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
nsw <- st_read(here("data-raw", "NSW"))
counties <- st_read(here("data-raw", "USA_Counties", "v107", "counties.gdb"))

# Processed UA separately, outside of the package in shrinkUAShapefile.R and saved it, compressed and with most columns removed, to the data/ folder of the package.

use_data(anae, overwrite = TRUE, version = 3, compress = "xz")
use_data(carver, overwrite = TRUE, version = 3, compress = "xz")
use_data(nsw, overwrite = TRUE, version = 3, compress = "xz")
use_data(counties, overwrite = TRUE, version = 3, compress = "xz")
