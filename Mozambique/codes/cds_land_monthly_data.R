
# Create a temporary directory to download map data
temp_dir <- "/tmp/cds"
dir.create(temp_dir)
setwd(temp_dir)

# =========================================================
library("ecmwfr")

# European Centre for Medium-Range Weather Forecasts (ECMWF) 
# Copernicus's Climate Data Store (CDS)
# https://cds.climate.copernicus.eu
# DOI: 10.24381/cds.68d2bb30

# spatial resolution: 0.1x0.1 degrees
# temporal resolution: monthly
# temporal availability: from January 1981 to 2-3 months before the present

# user credentials
user <- "****************"
cds.key <- "********************************"

# set secret ECMWF token
wf_set_key(user=user, key=cds.key, service="cds")

# request for getting land data
request <- list(
  # ERA5-Land monthly averaged data from 1950 to present
  # data_id: reanalysis-era5-land-monthly-means:monthly_averaged_reanalysis
  product_type = "monthly_averaged_reanalysis",
  variable = c("leaf_area_index_high_vegetation", 
               "leaf_area_index_low_vegetation", 
               "skin_temperature", 
               "total_precipitation", 
               "volumetric_soil_water_layer_1"),
  time = "00:00",
  year = c("2021", "2022", "2023"),
  month = c("01", "02", "03", "04", "05", "06", 
            "07", "08", "09", "10", "11", "12"),
  # North, West, South, East
  area = c(-10, 30, -27, 41),
  format = "netcdf.zip",
  dataset_short_name = "reanalysis-era5-land-monthly-means",
  target = "landvars.zip"
)

# check the validity of a data request and login credentials
wf_check_request(user=user, request=request)

# download the data request
wf_request(user=user, request=request,
           transfer=TRUE, path=getwd(),
           verbose=TRUE)

# extract downloaded Zip file
unzip(zipfile="landvars.zip", exdir="landvars/")

# =========================================================

library("ncdf4")
library("ncdf4.helpers")

cds_land_data <- nc_open("landvars/data.nc")
print(cds_land_data)

# date and time
nc.get.time.series(cds_land_data)
# dimension axes
nc.get.dim.axes(cds_land_data)
# list of names of data variables
nc.get.variable.list(cds_land_data)

dim(ncvar_get(cds_land_data, "skt"))
# Skin temperature
temp <- ncvar_get(cds_land_data, "skt")
# Total precipitation
perc <- ncvar_get(cds_land_data, "tp")
# Volumetric soil water layer 1
swvl <- ncvar_get(cds_land_data, "swvl1")
# Leaf area index, high vegetation
laih <- ncvar_get(cds_land_data, "lai_hv")
# Leaf area index, low vegetation
lail <- ncvar_get(cds_land_data, "lai_lv")

# =========================================================
library("tidyverse")

# convert data to an R data.frame
to_df_fun <- function(nc_data)
{
  # extract longitude
  lon <- ncvar_get(nc_data, "longitude")
  # extract latitude
  lat <- ncvar_get(nc_data, "latitude")
  # extract date and time
  dt <- nc.get.time.series(cds_land_data)
  # list of names of data variables
  vars <- nc.get.variable.list(cds_land_data)
  
  dat <- vector("list", length=length(vars))
  for (i in 1:length(vars))
  {
    vals <- ncvar_get(nc_data, vars[i])[, , 1, ]
    vals <- array(vals, dim=dim(vals),
                  dimnames=list(longitude=1:length(lon), 
                                latitude=1:length(lat),
                                time=1:length(dt)))
    
    dat[[i]] <- as.data.frame.table(vals)
    colnames(dat[[i]]) <- c(colnames(dat[[i]])[-4], vars[i])
    dat[[i]] <- dat[[i]] %>%
      mutate(longitude=lon[longitude],
             latitude=lat[latitude],
             time=dt[time])
    
  }
  
  dat %>% reduce(full_join, 
                 by = join_by(longitude, latitude, time))
}

dat <- to_df_fun(cds_land_data)

plot_fun <- function(var, dt)
{
  dat %>% 
    filter(as.character(time) == dt) %>%
    ggplot(aes_string(x="longitude", 
                      y="latitude", 
                      fill=var)) +
    geom_tile() +
   # borders("world", 
  #          xlim=range(dat %>% pull(longitude)), 
  #          ylim=range(dat %>% pull(latitude))) +
    coord_quickmap()
}

plot_fun("tp", "2022-12-01")

# ===============================================
# save land data as an R object of class data.frame
saveRDS(dat, file="cds_land_data.rds")
