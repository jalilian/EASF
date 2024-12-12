
library("readxl")
library("tidyverse")
library("sf")

# ===============================================
# GPS coordinates of sampling locations in both programs

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")


gps <- read_excel(
  "~/Downloads/Ghana/GHA_GPS EB review 12_6_24.xlsx",
  sheet="gha_gps"
  ) 

aa <- convert_coords(gps$LONGITUDE, gps$LATITUDE)

gps <- gps %>%
  mutate(LONGITUDE=aa[, 1],
         LATITUDE=aa[, 2]) %>% 
  mutate(method=case_match(method,
                           "HLS" ~ "HLC",
                           .default=method)) %>%
  distinct()

write_csv(gps, file="~/Downloads/Ghana/gha_gps.csv")

egps <- gps %>% filter(program == "EASF")
sgps <- gps %>% filter(program == "Routine")


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
      sep="_")) %>%
    # Add 'An. gambiae' variable with conditions
    mutate(
      `An. gambiae` = case_when(
        `Program name` == "CID" & 
          `Datat element name` %in% 
          c("Total Collected: Anopheles Female", 
            "Total Collected: Anopheles Male",
            "An. gambiae s.l.: Collected") ~ value,
        `Program name` == "CID" & 
          `Datat element name` %in%
          c("An. gambiae s.l. caught") ~ value,
        .default=NA
      )
    )
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


edata <- edata %>% 
  rename(`house ID`=houseID2) %>%
  left_join(
    egps %>% select(`house ID`, LONGITUDE, LATITUDE),
    by=join_by(`house ID`)
  )

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

# =========================================================
# combine all data

adata <- bind_rows(
  edata %>%
    mutate(`An. gambiae`=as.numeric(replace_na(`An. gambiae`, "0"))) %>%
    group_by(event, eventDate, `Program name`, `Org unit name`, houseID2) %>%
    summarise(`An. gambiae`=max(`An. gambiae`)) %>%
    mutate(program="EASF"),
  sdata %>%
    mutate(`An. gambiae`=as.numeric(replace_na(`An. gambiae`, "0"))) %>%
    group_by(event, eventDate, `Program name`, `Org unit name`, houseID2) %>%
    summarise(`An. gambiae`=max(`An. gambiae`)) %>%
    mutate(program="Routine")
  )  %>%
  rename(method=`Program name`,
         `house ID`=houseID2) %>%
  ungroup() %>%
  mutate(method=case_match(method,
                           "CID" ~ "PSC",
                           .default=method)) %>%
  left_join(gps,
            by=join_by(`house ID`, method, program)) %>%
  na.omit()

table(sub("_.*", "", gps$`house ID`))
table(sub("_.*", "", adata$`house ID`))

# =========================================================
# molecular analysis of EASF mosquito samples 
# from July 2023 – July 2024
mdata <- read_excel(
  #"~/Downloads/Ghana/EASF Molecular Data July 2023 - July 2024.xlsx",
  "~/Downloads/Ghana/EASF Molecular Data July 2023 - July 2024_updated.xlsx",
  sheet="All collections"
) %>%
  slice(-1) %>%
  mutate(`FIELD ID`=gsub("-|/", "_", `FIELD ID`)) %>%
  mutate(`house ID`=paste(toupper(substr(COMMUNITY, 1, 3)),
                          `TYPE OF COLLECTION`, 
                          str_pad(as.numeric(gsub("[^0-9]", "", `FIELD ID`)), 
                                  width=2, pad=0),
                          sep="_"),
         `house ID`=str_replace_all(`house ID`, "TOG", "TOR")) %>%
  relocate(`house ID`, .before=`FIELD ID`)

mdata %>% count(`FIELD ID`, `house ID`) %>% print(n=800)

mdata %>% 
  filter(str_detect(`house ID`, "NA")) %>%
  count(`FIELD ID`, `house ID`) %>% print(n=800)

mdata %>% 
  count(COMMUNITY, `TYPE OF COLLECTION`, `FIELD ID`) %>%
  print(n=800)
mdata %>% count(COMMUNITY, sub("_.*", "", `FIELD ID`)) %>% print(n=500)

mdata %>%
  mutate(`house ID`=gsub("-|/", "_", `FIELD ID`)) %>%
  pull(`house ID`) %>% sub("_.*", "", .) %>% table(.)

