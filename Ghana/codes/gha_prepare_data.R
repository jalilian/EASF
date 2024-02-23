
library("sf")
library("tidyverse")
library("readxl")

# =========================================================

gha_data <- list()
gha_data$HLC <- read_csv(
  "~/Downloads/Ghana/Ghana EASF_HLC data (Jul-Dec23).csv"
  ) %>%
  mutate(providedElsewhere=as.logical(
    case_match(providedElsewhere,
               "0" ~ "FALSE",
               .default=providedElsewhere)
    ))
gha_data$PSC <- read_csv(
  "~/Downloads/Ghana/Ghana EASF_PSC data (Jul-Dec23).csv"
  ) %>%
  rename("Program stage name" = "enrollment")
all.equal(colnames(gha_data$HLC), colnames(gha_data$PSC))

gha_data <- bind_rows(gha_data)
saveRDS(gha_data, file="~/Downloads/Ghana/gha_data.rds")

# =========================================================

# districts
gha_data %>% count(orgUnit, `Org unit name`)

## house ID
gha_data %>% 
  group_by(`Org unit name`) %>%
  filter(`Datat element name` == "HH Number") %>%
  filter(str_detect(value, "HLC|hlc")) %>%
   count(value) %>% print(n=500)

gha_data %>% count(`Org unit name`, eventDate) %>%
  print(n=500)
gha_data %>% 
  count(`Datat element name`) %>%
  print(n=500)

gha_data %>% filter(`Datat element name` == "Collection Place") %>%
  count(value)

gha_data %>%
  pivot_wider(names_from=`Datat element name`,
              values_from=value)

gha_data %>% 
  filter(`Org unit name` == "Glitame EASF Site") %>%
  count(`Datat element name`)

gha_data %>% 
  filter(str_detect(`Datat element name`, "Tota")) %>%
  count(`Datat element name`) %>% print(n=500)

gha_data %>% 
  filter(`Org unit name` == "Glitame EASF Site") %>%
  filter(`Datat element name` == "Total Collected: Anopheles Female") %>%
  select(`Datat element name`, value,) %>%
  count(value)

gha_hlc <- gha_data %>%
  filter(`Program name` == "HLC") %>%
  group_by(`Org unit name`, eventDate) %>%
  filter(str_detect(`Datat element name`, "An|HH Num")) %>%
  filter(!str_detect(`Datat element name`, "caught")) %>%
  mutate(value=str_remove_all(str_to_upper(value), " "),
         value=str_replace_all(value, "/", "-")) %>%
  mutate(value=str_replace(value, "HCL", "HLC")) %>%
  mutate(value=str_replace(value, "PSC", "HLC")) %>%
  mutate(value=case_match(value,
                          "ANT-HKC-04" ~ "ANT-HLC-04",
                          "KARHLC-03" ~ "KAR-HLC-03",
                          "KLI-HLC01" ~ "KLI-HLC-01",
                          "ODC-HLC-05" ~ "ODU-HLC-05",
                          "0KU-HLC-02" ~ "OKU-HLC-02",
                          "D0B-HLC-06" ~ "DOB-HLC-06",
                          .default=value)) %>%
  select(`Datat element name`, value) %>%
  ungroup()

gha_psc <- gha_data %>%
  filter(`Program name` == "CID") %>%
  group_by(`Org unit name`, eventDate) %>%
  filter(str_detect(`Datat element name`, "Anoph|HH Num")) %>%
  filter(!str_detect(`Datat element name`, "caught")) %>%
  mutate(value=str_remove_all(str_to_upper(value), " "),
         value=str_replace_all(value, "/", "-")) %>%
  mutate(value=str_replace(value, "HLC", "PSC")) %>%
  mutate(value=str_replace(value, "PSC-OKU", "OKU-PSC")) %>%
  mutate(value=case_match(value,
                          "D0B-PSC-09" ~ "DOB-PSC-09",
                          "DOB=PSC-11" ~ "DOB-PSC-11",
                          "KAR-PSC05" ~ "KAR-PSC-05",
                          "KAR-PSC18" ~ "KAR-PSC-18",
                          "KARPSC-11" ~ "KAR-PSC-11",
                          "ODU-PSC-001" ~ "ODU-PSC-01",
                          "0KU-PSC-03" ~ "OKU-PSC-03",
                          "0KU-PSC-06" ~ "OKU-PSC-06",
                          #"OKU-HLC-16" ~ "OKU-HLC-03",
                          #"OKU-HLC-09" ~ "OKU-HLC-05",
                          .default=value)) %>%
  select(`Datat element name`, value) %>%
  ungroup()

myfun <- function(x)
{
  y <- rep(NA, length(x))
  i_start <- 0
  for (i in 1:length(x))
  {
    if (is.na(x[i]))
    {
      y[i] <- sum(x[(i_start +1):(i - 1)])
      i_start <- i
    }
  }
  return(y)
}

gha_hlc <- gha_hlc %>%
  group_by(`Org unit name`, eventDate) %>%
  mutate(nn=as.numeric(value),
         mm=myfun(nn)) %>%
  ungroup()

gha_hlc <- gha_hlc %>% 
  filter(!is.na(mm)) %>%
  select(`Org unit name`, eventDate, value, mm) %>%
  arrange(value, eventDate) %>% 
  rename("House ID"="value", "Number of An."="mm",
         "unit"="Org unit name") %>% 
  mutate(year=year(eventDate), month=month(eventDate)) %>%
  mutate(unit=str_replace(unit, " EASF Site| EASF site", "")) %>% 
  relocate(year, month, .after=eventDate)

gha_hlc %>%
  print(n=800)


gha_psc <- gha_psc %>%
  group_by(`Org unit name`, eventDate) %>%
  mutate(nn=as.numeric(value),
         mm=myfun(nn)) %>%
  ungroup()

gha_psc <- gha_psc %>% 
  filter(!is.na(mm)) %>%
  select(eventDate, value, mm) %>%
  arrange(value, eventDate) %>% 
  rename("House ID"="value", "Number of An."="mm") %>% 
  mutate(year=year(eventDate), month=month(eventDate)) %>%
  relocate(year, month, .after=eventDate)

gha_psc %>%
  print(n=900)

gha_psc %>% write_csv(file="~/Downloads/Ghana/gha_psc.csv")

# =========================================================
gha_coords <- read_csv("~/Downloads/Ghana/gha_coords.csv")

gha_coords <- gha_coords %>%
  mutate(`house ID`=str_remove_all(str_to_upper(`house ID`), " "))

gha_hlc <- gha_hlc %>% 
  group_by(unit, year, month, `House ID`) %>%
  summarise(`Number of An.` = sum(`Number of An.`)) %>%
  ungroup()

gha_hlc <- gha_hlc %>%
  left_join(
    gha_coords %>% 
      select(`house ID`, long, lat) %>%
      rename(`House ID`=`house ID`),
    by=join_by(`House ID`)
  )

gha_hlc %>% write_csv(file="~/Downloads/Ghana/gha_hlc.csv")

gha_hlc %>%
  distinct(unit, `House ID`, long, lat) %>%
  st_as_sf(., coords=c("long", "lat"), crs="WGS84") %>%
  ggplot(aes(colour=unit)) +
  geom_sf() +
  geom_sf_label(aes(label=unit)) +
  theme_light()

library("mapview")
mapview(gha_hlc %>%
          distinct(unit, `House ID`, long, lat) %>%
          st_as_sf(., coords=c("long", "lat"), crs="WGS84"), 
        zcol="unit")

gha_hlc %>% 
  filter(unit %in% c("Antokrom", "Dadeiso", "Karlo", "Kwasuo"))  %>%
  summarise(summary(long), summary(lat))

gha_hlc %>% 
  filter(unit %in% c("Doblo Gono", "Oduman", "Okushibiade"))  %>%
  summarise(summary(long), summary(lat))

gha_hlc %>% 
  filter(unit %in% c("Duta", "Glitame", "Klikor"))  %>%
  summarise(summary(long), summary(lat))

gha_grid <- bind_rows(
  expand_grid(long=seq(-3.2, -2.7, l=50),
              lat=seq(5.9, 6.4, l=50)),
  expand_grid(long=seq(-0.6, -0.1, l=50),
              lat=seq(5.45, 5.95, l=50)),
  expand_grid(long=seq(0.7, 1.2, l=50),
              lat=seq(5.9, 6.4, l=50))
)

gha_map <- 
  read_sf("https://geodata.ucdavis.edu/gadm/gadm4.1/kmz/gadm41_GHA_0.kmz")

gha_grid <- gha_grid %>% 
  mutate(long1=long, lat1=lat) %>%
  st_as_sf(., coords=c("long1", "lat1"), crs="WGS84") %>%
  st_filter(., gha_map) %>%
  as_tibble() %>%
  select(-geometry)

bind_rows(
  gha_hlc %>% select(long, lat, unit),
  gha_grid %>% mutate(unit="grid")
)  %>%
  st_as_sf(., coords=c("long", "lat"), crs="WGS84") %>% 
  mapview(zcol="unit")


# =========================================================
# land cover, population density and elevation data

source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_land_covars_africa.R")

covars <- get_covars(gha_hlc %>% select(long, lat) %>% as.matrix(),
           path="~/Downloads/Africa_covars/")
names(covars) <- c("land_cover", "pop_density", "elevation")
gha_hlc <- bind_cols(gha_hlc, covars)

covars <- get_covars(gha_grid %>% select(long, lat) %>% as.matrix(),
                     path="~/Downloads/Africa_covars/")
names(covars) <- c("land_cover", "pop_density", "elevation")
gha_grid <- bind_cols(gha_grid, covars)

# =========================================================
# Copernicus climate data

source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_Copernicus_climate_data.R")

user <- "****************"
cds.key <- "********************************"

covars <- get_cds(user, cds.key, 
                  year=2023, month=sprintf("%02d", 4:12), 
                  what=cbind(gha_grid$long, gha_grid$lat))

covars <- covars %>%
  mutate(skt = skt -273.15)

collapfun <- function(dat)
{
  dat %>%
    mutate(time=as.Date(as.POSIXct(time)),
           year=year(time), month=month(time)) %>% 
    group_by(longitude, latitude, year, month) %>%
    summarise(across(u10:swvl1, list(mean=\(x) mean(x, na.rm=TRUE),
                                     sd=\(x) sd(x, na.rm=TRUE),
                                     min=\(x) min(x, na.rm=TRUE),
                                     max=\(x) max(x, na.rm=TRUE)),
                     .names="{.col}_{.fn}")) %>%
    ungroup()
}

covars <- collapfun(covars) 

covars %>%
  group_by(year, month) %>% 
  summarise_all(~sum(is.na(.)))

lagfun <- function(dat)
{
  out <- list()
  m <- 7:12
  for (i in 1:length(m))
  {
    out[[i]] <- bind_cols(
      dat %>% 
        filter(month == m[i]) %>%
        rename_at(vars(-(longitude:month)),function(x) paste0(x, "_0")),
      dat %>% 
        filter(month == m[i] - 1) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_1")),
      dat %>% 
        filter(month == m[i] - 2) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_2")),
      dat %>% 
        filter(month == m[i] - 3) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_3"))
    )
  }
  return(bind_rows(out))
}
covars <- lagfun(covars)
gha_grid <- gha_grid %>%
  rename(longitude=long, latitude=lat) %>%
  full_join(covars, 
            by=join_by(longitude, latitude)) %>%
  relocate(year, month, .after=latitude)

saveRDS(gha_grid, file="~/Downloads/Ghana/gha_grid.rds")

# data
covars <- get_cds(user, cds.key, 
                  year=2023, month=sprintf("%02d", 4:12), 
                  what=cbind(gha_hlc$long, gha_hlc$lat))

covars <- covars %>%
  mutate(skt = skt -273.15)

covars <- collapfun(covars) 

covars %>%
  group_by(year, month) %>% 
  summarise_all(~sum(is.na(.)))

covars <- lagfun(covars)
gha_hlc <- gha_hlc %>%
  rename(longitude=long, latitude=lat) %>%
  left_join(covars, 
            by=join_by(year, month, longitude, latitude)) 

saveRDS(gha_hlc, file="~/Downloads/Ghana/gha_hlc.rds")

