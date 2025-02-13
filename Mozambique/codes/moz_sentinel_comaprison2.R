
library("tidyverse")
library("readxl")

setwd("~/Downloads/Mozambique/Merged/")
# read and preprocess Niassa data
ndata <- read_excel(
  "Niassa province_EASF_Routine_data_cuamba_mandimba_AZM_09122024 _Year 1 & Year 2.xlsx",
  sheet = "Cuamba-Mandimba-EASF-Routine",
  col_types = "text", 
  na = c("", "N/A")
) %>%
  rename_all(tolower) %>%
  rename("collection date" = "date")

# read and preprocess Zambezia data
zdata <- read_excel(
  "Zambezia province_EASF-Routine-Gurue_Morrumbala-Year 1 & 2-12122024.xlsx",
  sheet = "EASF-Routine-Y1-Y2",
  col_types = "text", 
  na = c("", "N/A")
) %>%
  rename_all(tolower) %>%
  rename("street" = "village")

# combine the two data sets
adata <- bind_rows(
  ndata %>% select(intersect(names(ndata), names(zdata))),
  zdata %>% select(intersect(names(ndata), names(zdata)))
) %>%
  mutate(
    date = dmy(`collection date`),
    date2 = if_else(
      is.na(date),
      as.Date(as.numeric(`collection date`), origin = "1899-04-30"),
      date
    ),
    date3 = paste(year(date2), month(date2), sep = "-"),
    species = `species` %>%
      str_replace_all("\\s+", " ") %>%
      str_replace_all("Anopheles", "An.") %>%
      str_replace_all("An\\.(?! )", "An. ") %>%
      str_remove_all("\\.$") %>%
      str_to_title(),
    `trap type` = recode(`trap type`,
                         "AL-CDC" = "CDC",
                         "CDC-LT" = "CDC",
                         "Isca Humana" = "HLC",
                         "Isca humana" = "HLC",
                         "IH" = "HLC",
                         .default = `trap type`),
    `house id` = coalesce(
      `house id`,
      paste(district, `administrative post`, locality, street, `house no`, sep = "-")
    ) %>%
      str_replace_all("Franqueza", "Fraqueza")
  )

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")

aa <- cbind(adata$longitude, adata$latitude)
bb <- cbind(adata$`gps-garmin s`, adata$`gps-garmin e`)
idx <- apply(is.na(aa), 1, any)
aa[idx, ] <- convert_coords(bb[idx, 1], bb[idx, 2])
idx <- sapply(strsplit(aa[, 1], split="\\."), function(o) nchar(o[1]))
idx <- which(idx > 2)
aa[idx, ][1, 1] <- 35.65349
v <- aa[idx, ][-1, ]
v[, 1] <- paste0(substr(v[, 1], 1, 3), "°", substr(v[, 1], 4, nchar(v[, 1])))
v[, 2] <- paste0(substr(v[, 2], 1, 2), "°", substr(v[, 2], 3, nchar(v[, 2])))
aa[idx, ][-1, ] <- convert_coords(v[, 1], v[, 2])
adata$longitude <- as.numeric(aa[, 1])
adata$latitude <- -abs(as.numeric(aa[, 2]))

adata <- adata %>%
  select(-c(longitude, latitude)) %>%
  left_join(
    adata %>%
      group_by(`house id`) %>%
      summarise(longitude=mean(longitude, na.rm=TRUE),
                latitude=mean(latitude, na.rm=TRUE)),
    by=join_by(`house id`)
  )

adata <- adata %>%
  mutate(longitude2=case_when(longitude < 20 ~ abs(latitude),
                             .default=longitude),
         latitude2=case_when(latitude < -20 ~ -longitude,
                             .default=latitude),
         longitude=longitude2,
         latitude=latitude2) %>%
  select(longitude2, latitude2)

library("mapview")
library("sf")
adata %>%
  st_as_sf(coords=c("longitude2", "latitude2"),
           crs=4326) %>%
  mapview(., zcol="programme")


tt <- adata %>% group_by(`trap type`, programme, `house id`, date3) %>% 
  summarise(An=sum(str_detect(species, "An. ")),
            .groups="drop") 

tt %>% count(programme, `trap type`) %>%
  rename(effort=n) %>%
  left_join(
    tt %>% 
      group_by(programme, `trap type`) %>%
      summarise(An=sum(An, na.rm=TRUE)),
    by = join_by(programme, `trap type`)
  ) %>%
  mutate(mean=An / effort) %>%
  ggplot(aes(x=mean, y=`trap type`, fill=programme)) +
  geom_bar(stat="identity", position="dodge") +
  labs(y="Collection method", x="Average number of collected mosquitoes") +
  theme_light()


t.test(
  tt %>% filter(`trap type`=="CDC", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="CDC", programme=="easf") %>%
    pull(An)
)

t.test(
  tt %>% filter(`trap type`=="HLC", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="HLC", programme=="easf") %>%
    pull(An)
)

t.test(
  tt %>% filter(`trap type`=="Prokopack", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="Prokopack", programme=="easf") %>%
    pull(An)
)

dat <- adata %>% 
  filter(`trap type` %in% c("CDC", "HLC")) %>%
  group_by(programme, date3, `house id`, longitude, latitude) %>%
  summarise(y=sum(str_detect(species, "An. ")),
            .groups="drop") %>%
  mutate(y = replace_na(y, 0))


source("https://raw.githubusercontent.com/jalilian/CEASE/refs/heads/main/Ethiopia/codes/get_modis.R")

aa <- get_modis(what=dat %>% 
                  distinct(longitude, latitude) %>% 
                  as.matrix(),
                datetime="2023-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

