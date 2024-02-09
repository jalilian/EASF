
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
  mutate(nn=as.numeric(value),
         mm=myfun(nn))

gha_hlc <- gha_hlc %>% 
  filter(!is.na(mm)) %>%
  select(eventDate, value, mm) %>%
  arrange(value, eventDate) %>% 
  rename("House ID"="value", "Number of An."="mm") %>% 
  mutate(year=year(eventDate), month=month(eventDate)) %>%
  relocate(year, month, .after=eventDate)

gha_hlc %>%
  print(n=800)


gha_psc <- gha_psc %>%
  mutate(nn=as.numeric(value),
         mm=myfun(nn))

gha_psc <- gha_psc %>% 
  filter(!is.na(mm)) %>%
  select(eventDate, value, mm) %>%
  arrange(value, eventDate) %>% 
  rename("House ID"="value", "Number of An."="mm") %>% 
  mutate(year=year(eventDate), month=month(eventDate)) %>%
  relocate(year, month, .after=eventDate)

gha_psc %>% write_csv(file="~/Downloads/Ghana/gha_psc.csv")

# =========================================================
gha_coords <- read_csv("~/Downloads/Ghana/gha_coords.csv")

gha_coords <- gha_coords %>%
  mutate(`house ID`=str_remove_all(str_to_upper(`house ID`), " "))

gha_hlc <- gha_hlc %>%
  left_join(
    gha_coords %>% 
      select(`house ID`, long, lat) %>%
      rename(`House ID`=`house ID`),
    by=join_by(`House ID`)
  )

gha_hlc %>% write_csv(file="~/Downloads/Ghana/gha_hlc.csv")

library("mapview")
gha_hlc %>%
  distinct(`House ID`, long, lat) %>%
  st_as_sf(., coords=c("long", "lat"), crs="WGS84") %>%
  mapview()

gha_hlc %>% filter(str_detect(`House ID`, "OKU")) %>%
  summarise(summary(long), summary(lat))

gha_grid <- bind_rows(
  expand_grid(long=seq(-3.081485, -3.078780, l=10),
              lat=seq(6.081045, 6.082955, l=10)) %>%
    mutate(unit="Antokrom"),
  expand_grid(long=seq(-3.041285, -3.027480, l=10),
              lat=seq(6.108965, 6.126165, l=10)) %>%
    mutate(unit="Dadeiso"),
  expand_grid(long=seq(-0.3557335, -0.3547665, l=10),
              lat=seq(5.701030, 5.705005, l=10)) %>%
    mutate(unit="Doblo Gono"),
  expand_grid(long=seq(1.18324, 1.195920, l=10),
              lat=seq(6.146345, 6.159955, l=10)) %>%
    mutate(unit="Duta"),
  expand_grid(long=seq(1.019020, 1.023655, l=10),
              lat=seq(6.105100, 6.107910, l=10)) %>%
    mutate(unit="Glitame"),
  expand_grid(long=seq(-3.002740, -2.997475, l=10),
              lat=seq(6.132915, 6.137980, l=10)) %>%
    mutate(unit="Karlo"),
  expand_grid(long=seq(1.027475, 1.033130, l=10),
              lat=seq(6.076275, 6.079525, l=10)) %>%
    mutate(unit="Klikor"),
  expand_grid(long=seq(-3.107720, -3.104020, l=10),
              lat=seq(6.193100, 6.198035, l=10)) %>%
    mutate(unit="Kwasuo"),
  expand_grid(long=seq(-0.3328700, -0.3306500, l=10),
              lat=seq(5.644430, 5.650500, l=10)) %>%
    mutate(unit="Oduman"),
  expand_grid(long=seq(-0.3870000, -0.3848400, l=10),
              lat=seq(5.697255, 5.700525, l=10)) %>%
    mutate(unit="Okushibiade")
)


