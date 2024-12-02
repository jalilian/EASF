
library("readxl")
library("tidyverse")
library("sf")

# ===============================================
# GPS coordinates of sampling locations in both programs

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")

egps <- bind_rows(
  read_excel(
    "~/Downloads/Ghana/EASF GPS COORDINATES.xlsx",
    sheet="HLC") %>%
    rename("Latitude" = `Latitude `) %>%
    select("house ID", "Longitude", "Latitude") %>%
    mutate(method="HLS"),
  read_excel(
    "~/Downloads/Ghana/EASF GPS COORDINATES.xlsx",
    sheet="PSC") %>%
    rename("house ID" = "Hosehold Number") %>%
    select("house ID", "Longitude", "Latitude") %>%
    mutate(method="PSC")
) %>%
  na.omit() %>%
  mutate(`house ID`=str_replace_all(`house ID`, "\\s", ""),
         `house ID`=str_replace_all(`house ID`, "-", "_"),
         `house ID`=str_replace_all(`house ID`, "SUA", "SUI"),
         `house ID`=str_replace_all(`house ID`, "OUT", "OTU")) %>%
  mutate(Longitude=case_match(Longitude, 
                              "-4.2976E-2" ~ "-0.42976", # OBO-HLC-03 & OBO-HLC-04
                              "001°100.120 W" ~ "001°00.120 W", # EHI-HLC-04
                              "001°03.700." ~ "001°03.700", # ATI-PSC-06
                              "001°03.800." ~ "001°03.800", # ATI-PSC-11
                              "-258269" ~ "-2.58269", # SUI-PSC-11
                              "-258244" ~ "-2.58244", # SUI-PSC-12 & SUI-PSC-14
                              "-258231" ~ "-2.58231", # SUI-PSC-13
                              "-258365" ~ "-2.58365", # SUI-PSC-16
                              "-3.476500" ~ "-3.0476500", # ANT-PSC-08
                              .default=Longitude),
         Longitude=case_when(
           str_detect(`house ID`, "SUI_HLC") ~ 
             paste0("-", Longitude), # for SUA-HLC
           .default=Longitude),
         Latitude=case_match(Latitude,
                             "N 05.42.300'" ~ "N 05 42.300'", # DOB-PSC-20
                             "6.797399" ~ "6.0797399", # KAR-PSC-02
                             "610330" ~ "6.10330", # SUI-PSC-11
                             "610333" ~ "6.10333", # SUI-PSC-12	
                             "610363" ~ "6.10363", # SUI-PSC-13
                             "610366" ~ "6.10366", # SUI-PSC-14
                             "610442" ~ "6.10442", # SUI-PSC-16
                             "5.2882910000000001" ~ "5.88291", # OBO-PSC-04
                             "5.2883988999999998" ~ "5.883989", # OBO-PSC-05
                             "5.5377070000000002" ~ "5.7377070000000002", # OBO-HLC-04
                             .default=Latitude)) %>%
  mutate(program="EASF")

aa <- convert_coords(egps$Longitude, egps$Latitude)
egps <- egps %>%
  mutate(LONGITUDE=aa[, 1], LATITUDE=aa[, 2]) %>%
  select(-c(Longitude, Latitude))


sgps <- bind_rows(
  read_excel(
    "~/Downloads/Ghana/ROUTINE (CONTROL) SITES .xlsx",
    sheet="HLC") %>%
    rename("house ID" = "HOUSEHOLD NUMBER") %>%
    select("house ID", "LONGITUDE", "LATITUDE") %>%
    mutate(method="HLS"),
  read_excel(
    "~/Downloads/Ghana/ROUTINE (CONTROL) SITES .xlsx",
    sheet="PSC") %>%
    rename("house ID" = "ROOM NUMBER") %>%
    select("house ID", "LONGITUDE", "LATITUDE") %>%
    mutate(method="PSC")
) %>%
  mutate(`house ID`=str_replace_all(`house ID`, "\\s", ""),
         `house ID`=str_replace_all(`house ID`, "-", "_")) %>%
  mutate(program="Routine")

gps <- bind_rows(egps, sgps)
write_csv(gps, file="~/Downloads/Ghana/gha_gps.csv")

st_as_sf(gps, coords=c( "LONGITUDE", "LATITUDE"),
         crs=4326) %>%
  ggplot() +
  geom_sf(aes(colour=program, shape=method)) +
  theme_light() +
  theme(legend.position="bottom")

library("mapview")
mp <- st_as_sf(gps, coords=c( "LONGITUDE", "LATITUDE"),
               crs=4326) %>%
  mutate(met=paste(program, method, sep=":")) %>%
  mapview(., zcol="met", label="house ID")
mapshot2(mp, url="~/Downloads/Ghana/map.html")
# =========================================================

# EASF data
edata <- list()
edata[[1]] <- 
  read_excel("~/Downloads/Ghana/Ghana EASF_HLC data (Jul-Dec23).xlsx",
             sheet="Sheet 1 - events1",
             col_types="text")
edata[[2]] <- 
  read_excel("~/Downloads/Ghana/Ghana EASF_PSC data (Jul-Dec23).xlsx",
             sheet="Sheet 1 - events", skip=1,
             col_types="text") %>%
  rename("Program stage name"="enrollment")
edata[[3]] <- 
  read_excel("~/Downloads/Ghana/HLC_data_EASF_Jan_Jul.xlsx",
             sheet="Sheet 1 - events_hlc_csv",
             col_types="text") %>%
  rename("Program stage name" = "programstagename",
         "Org unit name" = "orgunitname") %>%
  select(-attributeOptionCombo)
edata[[4]] <- 
  read_excel("~/Downloads/Ghana/CID_data_EASF_Jan_Jul.xlsx",
             sheet="Sheet 1 - events_cid_csv",
             col_types="text") %>%
  select(-attributeOptionCombo)

edata <- bind_rows(edata)

prepfun <- function(dat)
{
  dat %>%
    # convert eventDate to Date type and extract year and month
    mutate(eventDate = as.Date(eventDate),
           year=substr(eventDate, 1, 4), 
           month=month.name[as.numeric(substr(eventDate, 6, 7))]) %>%
    group_by(event) %>%
    # extract 'houseID'
    mutate(houseID=
             ifelse(`Datat element name` == "HH Number", 
                    value, 
                    NA)) %>%
    mutate(houseID=toupper(houseID),
           houseID=ifelse(sum(!is.na(houseID)) > 0,
                          houseID[!is.na(houseID)],
                          NA),
           houseID=str_remove_all(houseID, " "),
           houseID=str_replace_all(houseID, "-|=|/", "_"),
           houseID=gsub("_+", "_", houseID)) %>%
    mutate(houseID2=paste(
      toupper(substr(`Org unit name`, 1, 3)),
      `Program stage name`,
      str_pad(as.numeric(gsub("[^0-9]", "", 
                              sub("^(([^_]*_){2}[^_]*)_.*", 
                                  "\\1", houseID))), 
              width=2, pad="0"), 
      sep="_"))
}

edata <- prepfun(edata)

edata <- edata %>%
  mutate(houseID2=str_replace_all(houseID2, "CID", "PSC"),
         houseID2=case_match(houseID2, 
                             "ANT_PSC_00" ~ "ANT_PSC_15",
                             "DOB_PSC_NA" ~ "DOB_PSC_05",
                             "DUT_HLC_NA" ~ "DUT_HLC_02",
                             .default=houseID2))

names(table(edata$houseID2[!(edata$houseID2 %in% egps$`house ID`)]))
egps$`house ID`[!(egps$`house ID` %in% edata$houseID2)]

edata %>% 
  count(`Program name`, eventDate, houseID, 
        longitude, latitude, geometry) %>%
  arrange(eventDate, houseID) %>%
  select(-n) %>%
  write_csv(file="~/Desktop/gha_easf_sites.csv")

# sentinel site data
sdata <- list()
sdata[[1]] <- 
  read_excel("~/Downloads/Ghana/Control sites HLC.xlsx",
             sheet="Sheet 1 - events 2",
             col_types="text") %>%
  rename("Org unit name" = "orgunitname",
         "Program stage name" = "programstagename")
sdata[[2]] <- 
  read_excel("~/Downloads/Ghana/Control sites CID.xlsx",
             sheet="Sheet 1 - events", skip=1,
             col_types="text") %>%
  rename("Program name" = "programname")
sdata[[3]] <- 
  read_excel("~/Downloads/Ghana/Control sites HLC_Jan-Jul.xlsx",
             sheet="Sheet 1 - events_hlc_csv",
             col_types="text") %>%
  rename("Org unit name" = "orgunitname",
         "Program stage name" = "programstagename")
sdata[[4]] <- 
  read_excel("~/Downloads/Ghana/Control sites CID_Jan-Jul.xlsx",
             sheet="Sheet 1 - events_cid_csv",
             col_types="text")


sdata <- bind_rows(sdata) 

sdata <- prepfun(sdata)

sdata <- sdata %>%
  mutate(houseID2=str_replace_all(houseID2, "CID", "PSC"),
         houseID2=str_replace_all(houseID2, "FOD", "HEL"),
         houseID2=str_replace_all(houseID2, "OLD", "OTET"),
         houseID2=str_replace_all(houseID2, "WIA", "SEFW"))

sdata %>% 
  count(`Program name`, eventDate, houseID, 
        longitude, latitude, geometry) %>%
  arrange(eventDate, houseID) %>%
  select(-n) %>%
  write_csv(file="~/Desktop/gha_control_sites.csv")

names(table(sdata$houseID2[!(sdata$houseID2 %in% sgps$`house ID`)]))
sgps$`house ID`[!(sgps$`house ID` %in% sdata$houseID2)]
