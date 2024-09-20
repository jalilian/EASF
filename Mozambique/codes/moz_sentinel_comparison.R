
library("readxl")
library("tidyverse")
library("sf")

setwd("~/Downloads/Mozambique/Niassa/")

easf <- list()
easf[[1]] <- read_excel(
  "EASF/EASF_Mosquito_Database_IH _laboratorio_Cuamba_Year2.xlsx"
  ) %>%
  rename("Method of collection"="Metodo de Colheita", 
         "Date of collection"="Data of collection")
easf[[2]] <- read_excel(
  "EASF/EASF_Mosquito_Database_IH _laboratorio_Mandimba_Year2.xlsx"
)

easf <- bind_rows(easf)

easf %>% count(District)
