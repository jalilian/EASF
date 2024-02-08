
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

tmp <- gha_data %>% 
  select(`Org unit name`, eventDate,
         `Datat element name`, value) %>%
  filter(
    `Datat element name` %in% 
      c("HH Number", 
        "Total Collected: Anopheles Female",
        "Total Collected: Anopheles Male",
        "Total Collected: Culicine Male")
  ) %>%
  mutate(value=str_remove_all(str_to_upper(value), " "),
         value=str_replace_all(value, "/", "-")) %>%
  mutate(value=case_match(value,
                          "KARHLC-03" ~ "KAR-HLC-03",
                          "KLI-HLC01" ~ "KLI-HLC-01",
                          "ODC-HLC-05" ~ "ODU-HLC-05",
                          "0KU-HLC-02" ~ "OKU-HLC-02",
                          "D0B-PSC-09" ~ "DOB-PSC-09",
                          "DOB=PSC-11" ~ "DOB-PSC-11",
                          "KAR-PSC05" ~ "KAR-PSC-05",
                          "KAR-PSC18" ~ "KAR-PSC-18",
                          "KARPSC-11" ~ "KAR-PSC-11",
                          "ODU-PSC-001" ~ "ODU-PSC-01",
                          "0KU-PSC-03" ~ "OKU-PSC-03",
                          "0KU-PSC-06" ~ "0KU-PSC-06",
                          .default=value))

  pivot_wider(names_from=`Datat element name`,
              values_from=value)
