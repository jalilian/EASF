
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

tmp_hlc <- gha_data %>%
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
                          "D0B-HCL-06" ~ "DOB-HCL-06",
                          .default=value)) %>%
  select(`Datat element name`, value) %>%
  ungroup()

tmp_psc <- gha_data %>%
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

tmp_hlc <- tmp_hlc %>%
  mutate(nn=as.numeric(value),
         mm=myfun(nn))

tmp_hlc <- tmp_hlc %>% 
  filter(!is.na(mm)) %>%
  select(eventDate, value, mm) %>%
  arrange(value, eventDate) %>% 
  rename("House ID"="value", "Number of An."="mm") %>% 
  mutate(year=year(eventDate), month=month(eventDate)) %>%
  relocate(year, month, .after=eventDate)

tmp_hlc %>%
  print(n=800)

tmp_hlc %>% write_csv(file="~/Downloads/Ghana/gha_hlc.csv")

tmp_psc <- tmp_psc %>%
  mutate(nn=as.numeric(value),
         mm=myfun(nn))

tmp_psc <- tmp_psc %>% 
  filter(!is.na(mm)) %>%
  select(eventDate, value, mm) %>%
  arrange(value, eventDate) %>% 
  rename("House ID"="value", "Number of An."="mm") %>% 
  mutate(year=year(eventDate), month=month(eventDate)) %>%
  relocate(year, month, .after=eventDate)

tmp_psc %>% write_csv(file="~/Downloads/Ghana/gha_psc.csv")

# =========================================================
