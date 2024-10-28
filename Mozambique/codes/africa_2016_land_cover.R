
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

# Gridded Population of the World (GPW), v4
# resolution: 30 arc second
# provided by Socioeconomic Data and Applications Center, NASA
# https://sedac.ciesin.columbia.edu/data/collection/gpw-v4

# =========================================================

# World Wildlife Fund. Africa: 
# Void-filled digital elevation mode, 2007 [ArcGRID]
# resolution: 15s resolution
# U.S. Geological Survey. 
# Retrieved from https://maps.princeton.edu/catalog/stanford-jm998sr5227 

# =========================================================

get_covars <- local({
  # function to read and extract raster data
  extract_raster_covars <- function(downloaded_file,
                                    data_file,
                                    temp_dir)
  {
    # create a temporary directory to extract the downloaded file
    if (is.null(temp_dir))
      temp_dir <- tempdir()
    if (!dir.exists(temp_dir))
    {
      dir.create(temp_dir)
    }
    
    file_path <- paste0(temp_dir, "/", data_file)
    if (!file.exists(file_path))
    {
      # extract the downloaded file
      unzip(zipfile=downloaded_file, exdir=temp_dir)
    }
    
    # read the raster data using the 'terra' package
    raster <- terra::rast(paste0(temp_dir, "/", data_file))
    
    return(raster)
  }
  
  # function to extract land cover type by coordinates
  files <- list(
    land_cover=c(
      "ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.zip",
      "ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.tif",
      "categorical"
    ),
    population_density=c(
      "gpw-v4-population-density-rev11_2020_30_sec_tif.zip",
      "gpw_v4_population_density_rev11_2020_30_sec.tif",
      "numeric"
    ),
    elevation=c(
      "data.zip",
      "af_dem_15s/",
      "numeric"
    )
  )
  
  
  get_land_covers_points <- function(coords, 
                                     path,
                                     temp_dir=NULL)
  {
    covars <- vector("list", length=length(files))
    for (j in 1:length(files))
    {
      r <- extract_raster_covars(paste0(path, files[[j]][1]),
                                 files[[j]][2],
                                 temp_dir)
      covars[[j]] <- terra::extract(r, coords)
    }
    data.frame(covars)
  }
  
  get_land_covars_polygon <- function(map,
                                      path,
                                      temp_dir=NULL)
  {
    covars <- vector("list", length=length(files))
    for (j in 1:length(files))
    {
      r <- extract_raster_covars(paste0(path, files[[j]][1]),
                                 files[[j]][2],
                                 temp_dir)
      out <- vector("list", length=nrow(map))
      for (i in 1:nrow(map))
      {
        cat(i, ": ")
        cp <- terra::crop(r, map[i, ], mask=FALSE, touches=TRUE)
        cp <- terra::mask(cp, map[i, ])
        cp <- terra::values(cp)
        cp <- cp[!is.na(cp)]
        
        out[[i]] <- switch(files[[j]][3], numeric={
          c(mean=mean(cp), min=min(cp), max=max(cp), sd=sd(cp))
        }, categorical={
          prop.table(table(cp))
        })
      }
      out <- data.frame(out)
      if (files[[j]][3] == "categorical")
        out[is.na(out)] <- 0
    }
    data.frame(covars)
  }
  
  get_land_covars <- function(input, path, temp_dir=NULL)
  {
    if (is(input, "matrix"))
    {
      get_land_covers_points(input, path, temp_dir)
    } else{
      if (is(input, "sf"))
        get_land_covars_polygon(input, path, temp_dir)
      else
        print("wrong input type")
    }
  }
})

get_covars(cbind(runif(10, -10, 10), runif(10, 10, 20)),
           path="~/Downloads/Africa_covars/")

get_covars(eth_map,
           path="~/Downloads/Africa_covars/")


get_pop_density <- 
  extract_raster_covars(
    paste0("~/Downloads/Africa_covars/",
           "gpw-v4-population-density-rev11_2020_30_sec_tif.zip"),
    "gpw_v4_population_density_rev11_2020_30_sec.tif",
    map=eth_map, stat="numeric"
  )



# function to extract elevation type by coordinates
get_elevation2 <- 
  function(x, y,
           url="https://stacks.stanford.edu/file/druid:jm998sr5227/data.zip",
           data_file="af_dem_15s/",
           temp_dir=tempdir(),
           plot.it=FALSE)
  {
    file_path <- paste0(temp_dir, "/", data_file)
    if (!file.exists(file_path))
    {
      # set connection timeout
      options(timeout=1000)
      # download the digital elevation model
      download.file(
        url="https://stacks.stanford.edu/file/druid:jm998sr5227/data.zip",
        destfile="africa_elevation.zip")
      
      # extract the downloaded file
      unzip(zipfile="africa_elevation.zip", exdir=temp_dir)
    }
    # read the Africa dem 15 ars second map
    africa_elevation <- terra::rast(file_path)
    
    if (plot.it)
    {
      # plot the elevation map
      plot(africa_elevation)
      points(x, y)
    }
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
# read adaptive table with covariates
data_path <- "~/Downloads/Mozambique/"
adaptive_table_covars <- 
  readRDS(paste0(data_path, "adaptive_table_coavrs.rds"))

# read adaptive grid with covariates
adaptive_grid_covars <- 
  readRDS(paste0(data_path, "adaptive_grid_coavrs.rds"))

library("tidyverse")
# add land cover and elevation to the covariates
adaptive_table_covars <- 
  adaptive_table_covars %>%
  as_tibble() %>%
  mutate(land_cover=
           get_land_cover(Longitude, Latitude),
         elevation=
           get_elevation(Longitude, Latitude))

adaptive_grid_covars <- 
  adaptive_grid_covars %>%
  as_tibble() %>%
  mutate(land_cover=
           get_land_cover(Longitude, Latitude),
         elevation=
           get_elevation(Longitude, Latitude))


# check land cover
adaptive_table_covars %>%
  distinct(`House ID`, .keep_all=TRUE) %>%
  group_by(Province, District, `Collection method`) %>%
  count(land_cover) %>% print(n=100)

adaptive_grid_covars %>%
  distinct(Longitude, Latitude, .keep_all=TRUE) %>%
  group_by(Province, District) %>%
  count(land_cover) %>% print(n=100)

# check elevation
adaptive_table_covars %>%
  distinct(`House ID`, .keep_all=TRUE) %>%
  group_by(Province, District, `Collection method`) %>%
  select(elevation) %>% print(n=100)

adaptive_grid_covars %>%
  distinct(Longitude, Latitude, .keep_all=TRUE) %>%
  group_by(Province, District) %>%
  select(elevation) %>% print(n=100)

if (FALSE)
{
  library("rasterVis")
  gplot(Moz_strm30) + 
    geom_tile(aes(fill = value)) + 
    coord_equal() +
    geom_point(data=adaptive_grid_covars %>% 
                 distinct(Longitude, Latitude),
               aes(x=Longitude, y=Latitude))
  
  # remove grid points with missing (NA) elevation
  adaptive_grid_covars <-
    adaptive_grid_covars %>%
    filter(!is.na(elevation))  
}

if (FALSE)
{
  # remove grid points with missing (NA) values in any column
  adaptive_grid_covars <- 
    adaptive_grid_covars %>% 
    na.omit()
}


# save the updated data
saveRDS(adaptive_table_covars, 
        file=paste0(data_path, "adaptive_table_coavrs.rds"))

saveRDS(adaptive_grid_covars, 
        file=paste0(data_path, "adaptive_grid_coavrs.rds"))
