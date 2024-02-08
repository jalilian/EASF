
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
  select(`Datat element name`, value,)
  count(value)
