
library("readxl")
library("tidyverse")
library("sf")

sdata <- list()
sdata[[1]] <- read_excel("~/Downloads/Ghana/Control sites HLC.xlsx",
                         sheet="Sheet 1 - events 2",
                         col_types="text") %>%
  rename("programname" = "Program name",
         "Org unit name" = "orgunitname",
         "Program stage name" = "programstagename")
sdata[[2]] <- read_excel("~/Downloads/Ghana/Control sites CID.xlsx",
                         sheet="Sheet 1 - events", skip=1,
                         col_types="text")

sdata[[1]] %>% filter(str_detect(`Datat element name`, ": An. gambiae")) %>%
  count(`Datat element name`) %>% print(n=500)

sdata[[2]] %>% filter(str_detect(`Datat element name`, "An. gambiae s.l.: Collected")) %>%
  count(`Datat element name`) %>% print(n=500)

sdata <- bind_rows(sdata)  %>%
  mutate(eventDate = as.Date(eventDate),
         year=substr(eventDate, 1, 4), 
         month=month.name[as.numeric(substr(eventDate, 6, 7))]) %>% 
  mutate(vv=str_replace_all(value, " ", ""),
         vv=str_replace_all(vv, "-", "_"),
         vv=str_replace(vv, "_LC_", "_HLC_"),
         vv=str_replace(vv, "_SC_", "_PSC_"),
         vv=str_replace(vv, "^HL_|^HLC_", "HEL_"),
         vv=str_replace(vv, "0TET_", "OTET_"), 
         vv=str_replace(vv, "_14$|_0014$", "_014"),
         vv=str_replace(vv, "_0011", "_011"),
         vv=case_match(vv, "HEL_PSC_08" ~ "HEL_PSC_008", .default=vv),
         vv=ifelse(nchar(vv) == 14, substr(vv, 1, 11), vv),
         vv=ifelse(str_detect(vv, "^SW"), 
                   paste(programname, 
                         str_replace(vv, "(?=[:digit:])", "_"), 
                         sep="_"), vv)) %>%
  group_by(programname, `Org unit name`, month)

sdata <- sdata %>%
  mutate(hh=ifelse(`Datat element name` == "HH Number", 
                           vv, NA))
idx <- which(!is.na(sdata$hh))
sdata$`house ID` <- NA
for (i in 1:nrow(sdata))
{
  ll <- sum(idx < i) + 1
  sdata$`house ID`[i] <- sdata$hh[idx][ll]
}

sdata %>% 
  filter(`Datat element name` == "HH Number") %>% 
  count(value, vv) %>% print(n=700)

sdata %>% 
  filter(`Datat element name` == "HH Number") %>% 
  count(vv, geometry) %>% print(n=700)

sdata <- sdata %>% 
  group_by(programname, `Org unit name`, `house ID`, month)
# ===============================================

sent_coords <- as_tibble(matrix(c(
  "HEL_HLC_01", 0.51125, 7.0709, 
  "HEL_HLC_02", 0.51072, 7.07006,
  "HEL_HLC_03", 0.51072, 7.06965,
  "HEL_HLC_04", 0.51089, 7.07195,
  "HEL_HLC_05", 0.5097, 7.06965,
  "HEL_HLC_06", 0.51005, 7.07195,
  "HEL_HLC_07", 0.51014, 7.07195,
  "HEL_HLC_08", 0.51005, 7.07175,
  "OTET_HLC_01", -0.30888, 5.54196,
  "OTET_HLC_02", -0.30853, 5.54149,
  "OTET_HLC_03", -0.31041, 5.54086,
  "OTET_HLC_04", -0.31086, 5.54083,
  "OTET_HLC_05", -0.3134, 5.5396,
  "OTET_HLC_06", -0.31336, 5.53995,
  "OTET_HLC_07", -0.31129, 5.53178,
  "OTET_HLC_08", -0.31155, 5.53143,
  "HEL_PSC_001", 0.510948, 7.070647,
  "HEL_PSC_002", 0.510978, 7.070618,
  "HEL_PSC_003", 0.51092, 7.070617,
  "HEL_PSC_004", 0.510908, 7.070909,
  "HEL_PSC_005", 0.510996, 7.070658,
  "HEL_PSC_006", 0.510634, 7.069969,
  "HEL_PSC_007", 0.510716, 7.069776,
  "HEL_PSC_008", 0.510634, 7.069745,
  "HEL_PSC_009", 0.510772, 7.069799,
  "HEL_PSC_010", 0.510762, 7.069788,
  "HEL_PSC_011", 0.510215, 7.070134,
  "HEL_PSC_012", 0.511656, 7.070245,
  "HEL_PSC_013", 0.510301, 7.070071,
  "HEL_PSC_014", 0.510067, 7.070102,
  "HEL_PSC_015", 0.509972, 7.071589,
  "HEL_PSC_016", 0.510246, 7.069983,
  "HEL_PSC_017", 0.510254, 7.069981,
  "HEL_PSC_018", 0.51021, 7.069988,
  "HEL_PSC_019", 0.510258, 7.069982,
  "HEL_PSC_020", 0.510781, 7.070421,
  "OTET_PSC_01", -0.309438, 5.542005,
  "OTET_PSC_02", -0.309393, 5.542103,
  "OTET_PSC_03", -0.309625, 5.542676,
  "OTET_PSC_04", -0.311746, 5.541052,
  "OTET_PSC_05", -0.309319, 5.543349,
  "OTET_PSC_06", -0.310141, 5.540407,
  "OTET_PSC_07", -0.310156, 5.540017,
  "OTET_PSC_08", -0.311229, 5.541541,
  "OTET_PSC_09", -0.310141, 5.540407,
  "OTET_PSC_10", -0.310141, 5.540407,
  "OTET_PSC_11", -0.318238, 5.551906,
  "OTET_PSC_12", -0.311595, 5.540842,
  "OTET_PSC_13", -0.311229, 5.541541,
  "OTET_PSC_14", -0.318238, 5.55196,
  "OTET_PSC_15", -0.309075, 5.542215,
  "OTET_PSC_16", -0.309461, 5.543356
), ncol=3, byrow=TRUE)) %>%
  mutate(V2=as.numeric(V2), V3=as.numeric(V3)) %>%
  rename("HH"="V1", "longitude"="V2", "latitude"="V3")

easf_coords <- read_csv("~/Downloads/Ghana/gha_coords.csv") %>% 
  mutate(`house ID`=str_remove_all(`house ID`, " ")) %>%
  distinct(`house ID`, long, lat)

library("mapview")
sent_coords %>% 
  st_as_sf(., coords=c("longitude", "latitude"), crs="WGS84") %>%
  mapview()
  
easf_coords %>%
  st_as_sf(., coords=c("long", "lat"), crs="WGS84") %>%
  mapview()

bind_rows(
  sent_coords %>% 
    rename("house ID" = "HH") %>%
    mutate(program="sentinel"),
  easf_coords %>%
    rename("longitude"="long", "latitude"="lat") %>%
    mutate(program="EASF")
  ) %>% 
  mutate(method="HLC") %>%
  mutate(method=ifelse(str_detect(`house ID`, "PSC"), "PSC", method)) %>%
  write_csv(file="~/Downloads/Ghana/gha_coords_all.csv")

# ===============================================



sent_hlc <- sdata %>% 
  filter(programname == "HLC", 
         str_detect(`Datat element name`, ": An. gambiae")) %>%
  summarise(gambiae=sum(as.numeric(value), na.rm=TRUE)) %>%
  ungroup()

sent_hlc <- sent_hlc %>% 
  left_join(
    sdata %>% 
      filter(programname == "HLC", 
             str_detect(`Datat element name`, "An. funestus")) %>%
      summarise(funestus=sum(as.numeric(value), na.rm=TRUE)) %>% 
      ungroup()
    ) %>%
  mutate(funestus=ifelse(is.na(funestus), 0, funestus))

sdata %>% 
  filter(programname == "HLC", 
         str_detect(`Datat element name`, "An. funestus")) %>%
  summarise(funestus=sum(as.numeric(value), na.rm=TRUE)) %>%
  ungroup()


sdata %>%
  group_by(programname, `Org unit name`, `house ID`, month) %>% 
  filter(programname == "CID",
         str_detect(`Datat element name`, "An. gambiae s.l.: Collected")) %>%
  summarise(gam=sum(as.numeric(value)))

sdata %>% count(eventDate) %>% print(n=500)
sdata %>% count(geometry) %>% print(n=500)
sdata %>% count(longitude, latitude) %>% print(n=500)
sdata %>% 
  filter(`Datat element name` == "HH Number") %>%
  count(value) %>% print(n=500)

sdata %>% count(`Org unit name`, programname, eventDate)

sdata  %>% count(`Datat element name`) %>% print(n=500)

sdata  %>% 
  group_by(`Org unit name`) %>%
  filter(`Datat element name` == "HH Number") %>%
  count(value) %>% print(n=500)

sdata %>%
  filter(`Datat element name` == "Collection Method Indoor") %>% 
  count(value)

sdata[[1]] %>%
  filter(`Datat element name` == "HH Name (HLC)") %>%
  count(value)

sdata[[2]] %>%
  filter(`Datat element name` == "HH Name (CID)") %>%
  count(value)
