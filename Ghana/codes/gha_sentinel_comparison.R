

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
  mutate(lat=as.numeric(latitude), 
         lat=ifelse(lat > 100, lat/1e5, lat)) %>%
  group_by(programname, `Org unit name`, month)

sdata %>% 
  filter(`Datat element name` == "HH Number") %>% 
  count(value, vv) %>% print(n=700)

sdata %>% 
  filter(`Datat element name` == "HH Number") %>% 
  count(vv, geometry) %>% print(n=700)

sdata %>% 
  filter(programname == "HLC", 
         str_detect(`Datat element name`, ": An. gambiae")) %>%
  summarise(gam=sum(as.numeric(value)))

sdata %>%
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
