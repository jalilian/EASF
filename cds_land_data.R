
# Create a temporary directory to download map data
temp_dir <- "/tmp/cds"
dir.create(temp_dir)
setwd(temp_dir)

# =========================================================
library("ecmwfr")

# European Centre for Medium-Range Weather Forecasts (ECMWF) 
# Copernicus's Climate Data Store (CDS)
# https://cds.climate.copernicus.eu

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


plot_nc <- function(nc_data, var, i, implt=TRUE)
{
  vals <- ncvar_get(nc_data, var)[, , 1, ]
  dd <- dim(vals)
  if (implt)
  {
    fields::image.plot(ncvar_get(nc_data, "longitude"), 
                       rev(ncvar_get(nc_data, "latitude")),
                       vals[, dd[2]:1, i], 
                       main=paste(var, as.character(nc.get.time.series(nc_data))[i]),
                       asp=1, xlab="longitude", ylab="latitude")
    maps::map("world", add=TRUE, col="black")
  } else{
    image(ncvar_get(nc_data, "longitude"), 
          rev(ncvar_get(nc_data, "latitude")),
          vals[, dd[2]:1, i], 
          main=paste(var, as.character(nc.get.time.series(nc_data))[i]),
          asp=1, xlab="longitude", ylab="latitude", axes=FALSE)
    contour(ncvar_get(nc_data, "longitude"), 
            rev(ncvar_get(nc_data, "latitude")),
            vals[, dd[2]:1, i], add=TRUE, 
            col="grey50", lwd=0.5)
    maps::map("world", add=TRUE, col="blue")
  }
}
plot_nc(cds_land_data, "skt", 2)
plot_nc(cds_land_data, "tp", 2)
# ===============================================
