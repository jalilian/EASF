
library("readxl")
library("tidyverse")
library("sf")

setwd("~/Downloads/Mozambique/Niassa/")

# =========================================================
# sampling efforts

efforts <- bind_rows(
  bind_rows(
    read_excel(
      "EASF/EASF_Principal _Database_ Comport Humano_IH _Cuamba_Year2 - Copy.xlsx"
    ),
    read_excel(
      "EASF/EASF_Principal _Database_ Comport Humano_IH _Mandimba_Year2 - Copy - Copy.xlsx"
    ) 
  ) %>%
    mutate(program="EASF"),
  bind_rows(
    read_excel(
      "Routine/Rotina _Principal _Database_ Comport Humano_IH _Cuamba_Year2.xlsx"
    ),
    read_excel(
      "Routine/Rotina _Principal _Database_ Comport Humano_IH _Mandimba_Year2.xlsx"
    ) %>%
      slice(-c(1009, 1010)) %>%
      rename("Province"="Província", "District"="Distrito", 
             "House ID"="Identificação da casa (ID)",
             "Method of collection"="Método de colheita",
             "Date of collection"="Data de colheita", 
             "Hour of collection"="Hora de colheita") 
  ) %>% 
    mutate(program="Routine")
  ) %>%
  select(program, Province, District, `House ID`, `Method of collection`,
         `Date of collection`, `Hour of collection`) %>%
  mutate(`Date of collection`=as.Date(`Date of collection`, 
                                      format="%d/%m/%Y"),
         year=as.integer(substr(`Date of collection`, 1, 4)),
         month=as.integer(substr(`Date of collection`, 6, 7))) %>%
  group_by(program, District, year, month) %>% count(`House ID`)

# =========================================================
# collected mosquitoes

easf <- list()
easf[[1]] <- read_excel(
  "EASF/EASF_Mosquito_Database_IH _laboratorio_Cuamba_Year2.xlsx"
  ) %>%
  rename("Method of collection"="Metodo de Colheita", 
         "Date of collection"="Data of collection")
easf[[2]] <- read_excel(
  "EASF/EASF_Mosquito_Database_IH _laboratorio_Mandimba_Year2.xlsx"
  ) %>% slice(-539)

easf <- bind_rows(easf)

easf %>% count(District)

routine <- list()
routine[[1]] <- read_excel(
  "Routine/Rotina_Mosquito_Database_IH _laboratorio_Cuamba.xlsx"
  )
routine[[2]] <- read_excel(
  "Routine/Rotina_Mosquito_Database_IH _laboratorio_Mandimba.xlsx"
  ) %>%
  rename("Method of collection"="Metodo de Colheita", 
         "Date of collection"="Data de Colheita",
         "Hour of collection"="Hora de Colheita")
routine <- bind_rows(routine)

niassa <- bind_rows(
  easf %>% mutate(program="EASF"),
  routine  %>% mutate(program="Routine")
  ) %>%
  mutate(`Date of collection`=as.Date(`Date of collection`, 
                                      format="%d/%m/%Y"),
         year=as.integer(substr(`Date of collection`, 1, 4)),
         month=as.integer(substr(`Date of collection`, 6, 7))) %>%
  mutate(`Place of collection`=case_match(`Place of collection`,
                                          "Dentro" ~ "inside",
                                          "Fora" ~ "outside",
                                          "fora" ~ "outside"))

niassa %>% 
  group_by(program, District) %>%
  count(`Date of collection`) %>% print(n=500)

niassa %>% group_by(program) %>%
  count(`Species name`)
niassa %>% 
  group_by(program) %>% 
  count(`House ID`)
