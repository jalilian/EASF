
# S2 prototype Land Cover 20m map of Africa 2016
# https://2016africalandcover20m.esrin.esa.int/
# available to the public by the ESA Climate Change Initiative and 
#     its Land Cover project as the source of the CCI-LC database
# copyright
# © Contains modified Copernicus data (2015/2016)
# © ESA Climate Change Initiative - Land Cover project 2017

# format: GeoTIFF
# =========================================================

# Create a temporary directory to download map data
temp_dir <- "/tmp/africa2016landcover"
dir.create(temp_dir)
setwd(temp_dir)

# path to the downloaded file
downloaded_file <- 
  "~/Downloads/ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.zip"

# extract the downloaded file
unzip(zipfile=downloaded_file, exdir=tmp_dir)