
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
  terra::extract(africa_land_cover, cbind(x, y))
}

# =========================================================

# World Wildlife Fund. Africa: 
# Void-filled digital elevation mode, 2007 [ArcGRID]
# resolution: 15s resolution
# U.S. Geological Survey. 
# Retrieved from https://maps.princeton.edu/catalog/stanford-jm998sr5227 


# set connection timeout
options(timeout=1000)

# download the digital elevation model
download.file(
  url="https://stacks.stanford.edu/file/druid:jm998sr5227/data.zip",
  destfile="africa_elevation.zip")


# extract the downloaded file
unzip(zipfile="africa_elevation.zip", exdir=temp_dir)

# read the Africa dem 15 ars second map
africa_elevation <- rast("af_dem_15s/")

# plot the elevation map
plot(africa_elevation)

# function to extract land cover type by coordinates
get_elevation2 <- function(x, y)
{
  terra::extract(africa_elevation, cbind(x, y))
}

# =========================================================

if (FALSE)
{
  # download shapefile of Mozambique boundary
  download.file(
    url="https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_MOZ_shp.zip",
    destfile="Moz_map.zip")
  
  # extract the shapefile
  unzip(zipfile="Moz_map.zip", exdir=temp_dir)
  # read the shapefile
  Moz_map <- vect(paste0(temp_dir, "gadm41_MOZ_0.shp"))
  
  # crop the land cover data to the polygone in the shapefile
  Moz_land_cover <- 
    terra::crop(africa_land_cover, ext(Moz_map))
  
  plot(Moz_land_cover, fun=lines(Moz_map))
  
  aa <- focal(Moz_land_cover, w=3, fun=mean)
  
  aa <- africa_land_cover
  ext(aa) <- c(xmin=35.565, xmax=35.6, ymin=-17.345, ymax=-17.29)
  test <- crop(africa_land_cover, 
               ext(aa))
  plot(test)
  bb <- adaptive_table_covars %>% 
    distinct(`House ID`, .keep_all=TRUE)
  points(bb$Longitude, 
         bb$Latitude, pch="+")
}

# =========================================================
# Shuttle Radar Topography Mission (SRTM) 30 metres 
# Elevation data
# 
# link: https://rcmrd.africageoportal.com/datasets/rcmrd::mozambique-srtm30meters/about
download.file(
  "https://s3.amazonaws.com/rcmrd-open-data/downloadable_files/Mozambique_SRTM30meters.zip",
  destfile="Moz_STRM30m.zip")

unzip(zipfile="Moz_STRM30m.zip", exdir=temp_dir)

Moz_strm30 <- rast("Mozambique_SRTM30meters.tif")

plot(Moz_strm30)

# function to extract elevation by coordinates
get_elevation <- function(x, y)
{
  terra::extract(Moz_strm30, cbind(x, y))
}
# =========================================================
# read adaptive_table_covars data
data_path <- "~/Downloads/Mozambique/"
adaptive_table_covars <- 
  readRDS(paste0(data_path, "adaptive_table_coavrs.rds"))

library("tidyverse")
# add land cover and elevation to the covariates
adaptive_table_covars <- 
  adaptive_table_covars %>%
  as_tibble() %>%
  mutate(land_cover=
           get_land_cover(Longitude, Latitude),
         elevation=
           get_elevation(Longitude, Latitude))

# check land cover
adaptive_table_covars %>%
  distinct(`House ID`, .keep_all=TRUE) %>%
  group_by(Province, District, `Collection method`) %>%
  count(land_cover)

# check elevation
adaptive_table_covars %>%
  distinct(`House ID`, .keep_all=TRUE) %>%
  group_by(Province, District, `Collection method`) %>%
  select(elevation)

# save the updated data
saveRDS(adaptive_table_covars, 
        file=paste0(data_path, "adaptive_table_coavrs.rds"))
