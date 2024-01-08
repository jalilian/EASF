
library("sf")
library("tidyverse")
library("readxl")

gha_data <- 
  read_excel("~/Downloads/Ghana EASF data_03Jan24.xlsx", 
             sheet="Sheet 1 - events",
             range=cell_limits(c(2, 1), 
                               c(26170, 20)), 
             col_names = TRUE,
             na = "")

gha_data %>% count(event)

gha_data %>% count(`Org unit name`)

setwd(tempdir())
download.file(
  url="https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_GHA_shp.zip",
  destfile="gha.zip")
unzip("gha.zip", exdir="gha/")
gha_map <- read_sf("gha/", layer="gadm41_GHA_2")
