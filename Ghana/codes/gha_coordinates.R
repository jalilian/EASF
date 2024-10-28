
library("sf")
library("tidyverse")

gha_coor <- read_csv("~/Downloads/Ghana/GHA_coordinates.csv")

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
  write_csv(file="~/Downloads/Ghana/gha_coords.csv")

