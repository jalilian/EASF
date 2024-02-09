
library("sf")
library("tidyverse")
library("readxl")

gha_coor <- read_excel(
  "~/Downloads/Ghana_coordinates.xlsx",
  sheet="Sheet1",
  range="A1:Q81",
  )
  

coortfun <- function(x)
{
  y <- strsplit(x, "°")
  y <- lapply(y, function(o){ gsub("[^0-9.]", "", o) })
  out <- unlist(lapply(y, function(o){
    if (length(o) == 1)
    {
      as.numeric(o)
    } else{
      y1 <- o[1]
      y2 <- o[2]
      if (y2 == "")
      {
        as.numeric(y1)
      } else{
        as.numeric(y1) + as.numeric(y2) / 60
      }
    }
  }))
  out <- ifelse(grepl("(-|W|S)", x), -out, out)
  return(out)
}

gh_coor <- gha_coor %>% 
  #filter(str_detect(latitude, "N ")) %>%
  mutate(lat=coortfun(latitude),
         long=coortfun(longitude),
         )

gh_coor %>%
  select(`house ID`, latitude, lat, longitude, long)%>%
  print(n=500)

gh_coor %>% 
  write_csv(file="~/Desktop/GHA_coordinates.csv")

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

gha_map %>% count(NAME_1)
gha_map %>% count(NAME_2)


gha_data %>% select(longitude, latitude) %>% distinct() %>% points()
