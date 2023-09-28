
# S2 Prototype Land Cover 20m Map of Africa 2016
# Source: https://2016africalandcover20m.esrin.esa.int/
# Provided to the public by the ESA Climate Change Initiative and 
#      its Land Cover project as the source of the CCI-LC database
# Copyright Notice:
# © Contains modified Copernicus data (2015/2016)
# © ESA Climate Change Initiative - Land Cover project 2017

# format: GeoTIFF

# value codes:
# 0   No data
# 1   Tree cover areas
# 2   Shrubs cover areas
# 3   Grassland
# 4   Cropland
# 5   Vegetation aquatic or regularly flooded
# 6   Lichens Mosses / Sparse vegetation
# 7   Bare areas
# 8   Built-up areas
# 9   Snow and/or Ice
# 10  Open Water

# =========================================================

# create a temporary directory to extract the download land cover data
temp_dir <- "/tmp/africa2016landcover/"
dir.create(temp_dir)
setwd(temp_dir)

# path to the downloaded file
downloaded_file <- 
  "~/Downloads/ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.zip"

# extract the downloaded file
unzip(zipfile=downloaded_file, exdir=temp_dir)

# read the Africa 2016 land cover using the 'terra' package
library("terra")
africa_land_cover <- 
  rast(paste0(temp_dir, 
                "ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.tif"))

# plot the Africa 2016 land cover map
plot(africa_land_cover, type="classes")

# function to extract land cover type by coordinates
get_land_cover <- function(x, y)
{
  extract(africa_land_cover, cbind(x, y))
}

# =========================================================
