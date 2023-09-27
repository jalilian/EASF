
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
# temporal resolution: Hourly
# temporal availability: from January 1981 to 2-3 months before the present

# user credentials
user <- "****************"
cds.key <- "********************************"

# set secret ECMWF token
wf_set_key(user=user, key=cds.key, service="cds")

ym <- rbind(
  cbind("2022", c("05", "06", "07", "08", 
                  "09", "10", "11", "12")),
  cbind("2023", c("01", "02", "03", "04", 
                  "05", "06", "07", "08", "09"))
)


for (i in split(ym, row(ym)))
{
  # request for getting land data
  request <- list(
    variable = c("leaf_area_index_high_vegetation", 
                 "leaf_area_index_low_vegetation", 
                 "skin_temperature", 
                 "total_precipitation", 
                 "volumetric_soil_water_layer_1"),
    year = i[1],
    month = i[2],
    day = c("01", "02", "03", "04", "05", "06", 
            "07", "08", "09", "10", "11", "12", 
            "13", "14", "15", "16", "17", "18", 
            "19", "20", "21", "22", "23", "24", 
            "25", "26", "27", "28", "29", "30", "31"),
    time = c("00:00", "06:00", "12:00", "18:00"),
    area = c(-10, 30, -27, 41),
    format = "netcdf.zip",
    dataset_short_name = "reanalysis-era5-land",
    target = paste0("landvars_hourly_", i[1], "_", i[2], ".zip"))
  
  # check the validity of a data request and login credentials
  wf_check_request(user=user, request=request)
  
  # download the data request
  wf_request(user=user, request=request,
             transfer=TRUE, path=getwd(),
             verbose=TRUE)
}



# =========================================================

library("ncdf4")
library("ncdf4.helpers")
library("tidyverse")


# convert nc data to an R data.frame
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
    vals <- ncvar_get(nc_data, vars[i])
    dimnames(vals) <- 
      list(longitude=1:length(lon), 
           latitude=1:length(lat),
           time=1:length(dt))
    
    dat[[i]] <- as.data.frame.table(vals)
    dat[[i]] <- dat[[i]] %>%
      mutate(longitude=lon[longitude],
             latitude=lat[latitude],
             time=dt[time]) %>%
      mutate(year=substr(time, 1, 4), 
             month=substr(time, 6, 7)) %>%
      group_by(longitude, latitude, year, month) %>%
      summarise(mean=mean(Freq),
                min=min(Freq),
                max=max(Freq),
                sd=sd(Freq)) 
    dat[[i]] <- dat[[i]] %>%
      setNames(c(
        colnames(dat[[i]])[1:4],
        paste(vars[i], 
              c("mean", "min", "max", "sd"), sep="_")
      ))
  }
  
  dat %>% reduce(full_join, 
                 by = join_by(longitude, latitude, 
                              year, month))
}

land_data <- list()
for (i in split(ym, row(ym)))
{
  # extract downloaded Zip file
  dn <- paste0("landvars_hourly_", i[1], "_", i[2])
  unzip(zipfile=paste0(dn, ".zip"), 
        exdir=paste0(dn ,"/"))
  
  cds_land_data <- nc_open(paste0(dn, "/data.nc"))
  print(cds_land_data)
  
  # dimension axes
  nc.get.dim.axes(cds_land_data)
  land_data[[paste(i, collapse="_")]] <- to_df_fun(cds_land_data)
}

# merge 
land_data <- 
  land_data %>% 
  bind_rows() %>% 
  as_tibble()

land_data  %>%
  count(year, month)

# =========================================================

plot_fun <- function(var, year, month)
{
  dat %>% 
    filter(year == year & month == month) %>%
    ggplot(aes_string(x="longitude", 
                      y="latitude", 
                      fill=var)) +
    geom_tile() +
    # borders("world", 
    #          xlim=range(dat %>% pull(longitude)), 
    #          ylim=range(dat %>% pull(latitude))) +
    coord_quickmap()
}

plot_fun("tp_mean", "2022", "05")

# ===============================================
# save land data as an R object of class data.frame
saveRDS(land_data, file="cds_land_data.rds")
