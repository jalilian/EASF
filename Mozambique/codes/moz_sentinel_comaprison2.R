
library("tidyverse")
library("readxl")

setwd("~/Downloads/Mozambique/Merged/")
ndata  <- read_excel(
  "Niassa province_EASF_Routine_data_cuamba_mandimba_AZM_09122024 _Year 1 & Year 2.xlsx",
  sheet="Cuamba-Mandimba-EASF-Routine",
  col_types="text", na = c("", "N/A")
  ) %>% rename_all(tolower) %>%
  rename("collection date"="date")

zdata  <- read_excel(
  "Zambezia province_EASF-Routine-Gurue_Morrumbala-Year 1 & 2-12122024.xlsx",
  sheet="EASF-Routine-Y1-Y2",
  col_types="text", na = c("", "N/A")
  ) %>% rename_all(tolower) %>%
  rename("street"="village")

ndata %>% 
  count(programme, year)

adata <- bind_rows(
  ndata %>%
    select(intersect(names(ndata), names(zdata))),
  zdata %>%
    select(intersect(names(ndata), names(zdata)))
  ) %>%
  mutate(date=dmy(`collection date`))

adata <- adata %>%
  mutate(
    date2=case_when(
      is.na(date) ~ as.Date(as.numeric(`collection date`), origin="1899-04-30"),
      .default=date
    )
  ) %>%
  mutate(date3=paste(year(date2), month(date2), sep="-")) %>%
  mutate(species=gsub("\\s+", " ", species),
         species=gsub("Anopheles", "An.", species),
         species=gsub("An\\.(?! )", "An. ", species, perl = TRUE),
         species=gsub("\\.$", "", species),
         species=paste0(toupper(substr(species, 1, 1)), 
                        tolower(substr(species, 2, nchar(species))))) %>%
  mutate(`trap type`=case_match(`trap type`, 
                                "AL-CDC" ~ "CDC",
                                "CDC-LT" ~ "CDC",
                                "Isca Humana" ~ "HLC",
                                "Isca humana" ~ "HLC", 
                                "IH" ~ "HLC",
                                .default=`trap type`)
         ) %>%
  mutate(`house id`=if_else(is.na(`house id`),
                                  paste(district, `administrative post`, 
                                        locality, street, `house no`, sep="-"),
                                  `house id`),
         `house id`=gsub("Franqueza", "Fraqueza", `house id`))

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")

aa <- cbind(adata$longitude, adata$latitude)
bb <- cbind(adata$`gps-garmin s`, adata$`gps-garmin e`)
idx <- is.na(aa)
idx <- apply(idx, 1, any)
idx <- sapply(strsplit(aa[, 1], split="\\."), function(o) nchar(o[1]))
idx <- which(idx > 2)
aa[idx, ]
aa <- convert_coords(adata$`gps-garmin e`, adata$`gps-garmin s`)
adata %>%
  count(programme, year, district, `administrative post`, 
        locality, street, `house no`) %>%
  count(programme, district, year)

adata %>% 
  filter(programme == "Routine", district == "Morrumbala") %>%
  distinct(`administrative post`, locality, street, `house no`, `house id`, `trap type`)

adata %>% 
  count(programme, year, district, street, `house id`) %>%
  count(programme, year, district, street)

adata %>%
  filter(`house id` == "N/A") %>%
  distinct(`house id`, `gps-garmin s`, `gps-garmin e`) %>%
  left_join(adata %>%
              filter(`house id` != "N/A") %>%
              distinct(`gps-garmin s`, `gps-garmin e`, `house id`) %>%
              rename("house id2" = "house id")
            )

tt <- adata %>% group_by(`trap type`, programme, `house id`, date3) %>% 
  summarise(An=sum(str_detect(species, "An. ")))
t.test(
  tt %>% filter(`trap type`=="CDC", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="CDC", programme=="easf") %>%
    pull(An)
)

t.test(
  tt %>% filter(`trap type`=="Prokopack", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="Prokopack", programme=="easf") %>%
    pull(An)
)
