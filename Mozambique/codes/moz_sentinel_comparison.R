
library("readxl")
library("tidyverse")
library("sf")

setwd("~/Downloads/Mozambique/")

# =========================================================
# sampling efforts

efforts <- bind_rows(
  bind_rows(
    read_excel(
      "Niassa/EASF/EASF_Principal _Database_ Comport Humano_IH _Cuamba_Year2 - Copy.xlsx"
    ),
    read_excel(
      "Niassa/EASF/EASF_Principal _Database_ Comport Humano_IH _Mandimba_Year2 - Copy - Copy.xlsx"
    ) 
  ) %>%
    mutate(program="EASF"),
  bind_rows(
    read_excel(
      "Niassa/Routine/Rotina _Principal _Database_ Comport Humano_IH _Cuamba_Year2.xlsx"
    ),
    read_excel(
      "Niassa/Routine/Rotina _Principal _Database_ Comport Humano_IH _Mandimba_Year2.xlsx"
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
         month=as.integer(substr(`Date of collection`, 6, 7)))

efforts %>%
  filter(`Date of collection` >= as.Date("2023-12-1")) %>%
  group_by(program, District, year, month) %>% 
  count(`House ID`) %>%
  print(n=800)

# =========================================================
# collected mosquitoes


mosquitoes <- bind_rows(
  bind_rows(
    read_excel(
      "Niassa/EASF/EASF_Mosquito_Database_IH _laboratorio_Cuamba_Year2.xlsx"
    ) %>%
      rename("Method of collection"="Metodo de Colheita", 
             "Date of collection"="Data of collection"),
    read_excel(
      "Niassa/EASF/EASF_Mosquito_Database_IH _laboratorio_Mandimba_Year2.xlsx"
    ) %>% slice(-539)
  ) %>%
    mutate(program="EASF"),
  bind_rows(
    read_excel(
      "Niassa/Routine/Rotina_Mosquito_Database_IH _laboratorio_Cuamba.xlsx"
    ), read_excel(
      "Niassa/Routine/Rotina_Mosquito_Database_IH _laboratorio_Mandimba.xlsx"
    ) %>% 
      rename("Method of collection"="Metodo de Colheita",
             "Date of collection"="Data de Colheita",
             "Hour of collection"="Hora de Colheita")
  ) %>% 
    mutate(program="Routine")
) %>%
  rename("Collection method"="Method of collection") %>%
  select("program", "Province", "District", "House ID",     
         "Tablet-phone Latitude", "Tablet-phone Longitude", 
         "GPS-Garmin S", "GPS-Garmin E",
         "Collection method", "Date of collection",
         "Hour of collection", "Mosquito microscopy code",
         "Species name", "Place of collection") %>%
  mutate(`Date of collection`=as.Date(`Date of collection`, 
                                      format="%d/%m/%Y"),
         year=as.integer(substr(`Date of collection`, 1, 4)),
         month=as.integer(substr(`Date of collection`, 6, 7)))


zambezia <- bind_rows(
  read_excel(
    "Zambezia/EASF/EASF_Base dados_laboratorio_IH_Ano2 (1).xlsx"
  ) %>%
    rename("Mosquito microscopy code"="Mosquito code",
           "Latitude"="Latitude (telefone/tablet)",   
           "Longitude"="Longetude (telefone/tablet)") %>%
    select("Province", "District", #"House ID",     
           "Latitude", "Longitude", 
           "Collection method", "Date of collection",
           "Hour of collection", "Mosquito microscopy code",
           "Species name", "Place of collection") %>%
    mutate(program="EASF"),
  read_excel(
    "Zambezia/Routine/Second year HLC &CDC LT December 2023 to August 2024.xlsx"
  ) %>%
    rename("Mosquito microscopy code"="Mosquito cod",
           "Date of collection"="Collection date",
           "Hour of collection"="Time collection",
           "Place of collection"="House collection position") %>%
    select("Province", "District", #"House ID",     
           "Latitude", "Longitude", #"Elevation",
           "Collection method", "Date of collection",
           "Hour of collection", "Mosquito microscopy code",
           "Species name", "Place of collection") %>%
    mutate(program="Routine")
  )


mosquitoes %>% 
  group_by(program, District) %>% 
  count(`Species name`) %>% 
  summarise(n=sum(n))


zambezia %>% 
  group_by(program, District) %>% 
  count(`Species name`) %>%
  filter(`Species name` != "0") %>%
  summarise(n=sum(n))


v1 <- mosquitoes %>% 
  group_by(program, District, `House ID`, year, month) %>%
  count(`Species name`) %>% 
  summarise(n=sum(n))

v2 <- zambezia %>% 
  group_by(program, District, Longitude, Latitude, `Date of collection`) %>%
  count(`Species name`) %>% 
  filter(`Species name` != "0") %>%
  summarise(n=sum(n))

t.test(v1 %>%
         filter(District == "Cuamba", program == "EASF") %>%
         pull(n), 
       v1 %>%
         filter(District == "Cuamba", program == "Routine") %>%
         pull(n))

t.test(v1 %>%
         filter(District == "Mandimba", program == "EASF") %>%
         pull(n), 
       v1 %>%
         filter(District == "Mandimba", program == "Routine") %>%
         pull(n))


t.test(v2 %>% 
         filter(program == "EASF") %>%
         pull(n),
       v2 %>% 
         filter(program == "Routine") %>%
         pull(n) * 28 / 24)




# Simpson's diversity index
Simpson <- function(x, ci=0.95) 
{
  N <- sum(x)           # Total individuals
  P <- x / N                           # Proportions
  S <- sum(x > 0)                       # No Species
  ## CI calculations
  H <- 1 - sum(P^2)                         # Simpson's Index
  v1 <- 4 * N * (N - 1) * (N - 2) * sum(P^3)
  v2 <- 2 * N * (N - 1) * sum(P^2)
  v3 <- 2 * N * (N - 1) * (2 * N - 3) * sum(P^2)^2
  V <- (v1 + v2 - v3) / ((N * (N - 1))^2)       # Variance
  
  ## Final calculations
  Cv <- qt((1 - ci) / 2, df=S - 1, lower.tail=FALSE) # Critical value
  CI <- Cv * sqrt(V)                                         # Conf Int
  
  result <- c("Simpson"=H, "q0.025"=H - CI, "q0.975"=H + CI)
  return(result)
}

bind_rows(
  mosquitoes %>% 
    group_by(District, program) %>%
    count(`Species name`) %>%
    filter(!str_detect(`Species name`, "Culex")), 
  zambezia %>% 
    group_by(District, program) %>%
    count(`Species name`) %>%
    filter(`Species name` != "0")
 ) %>%
  mutate(`Species name`=case_match(`Species name`,
                                   "Anopheles gambiae s.l" ~ "An. gambiae s.l.",
                                   "An.  gambiae s.l." ~ "An. gambiae s.l.",
                                   "Anopheles funestus s.l" ~ "An. funestus s.l.",
                                   "An.rufipes" ~ "An. rufipes",
                                   "Anopheles rufipes" ~ "An. rufipes",
                                   "Anopheles tenebrosus" ~ "An. tenebrosus",
                                   "Anopheles outros" ~ "An. outros",
                                   .default=`Species name`)) %>%
  left_join(
    data.frame(District=rep(c("Cuamba", "Mandimba", "Morrumbala"), 2),
               program=rep(c("EASF", "Routine"), each=3), 
               effort=c(15 * 9 * 2, 12 * 9 * 2, 12 * 9 * 2,
                        6 * 8 * 2, 6 * 8 * 2, 6 * 4 * 28 / 12)),
    by=join_by(District, program)
  ) %>%
  mutate(mean=n/effort) %>%
  ggplot(aes(x=mean, y=`Species name`, fill=program)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Mean number of collected mosquitoes per house per 12 hours") +
  theme_classic() + 
  theme(legend.position = "bottom") + 
  facet_wrap(~ District)


    Simpson(
  mosquitoes %>% 
    filter(District == "Cuamba", program == "EASF") %>%
    count(`Species name`) %>%
    filter(!str_detect(`Species name`, "Culex")) %>%
    pull(n)
  )

Simpson(
  mosquitoes %>% 
    filter(District == "Cuamba", program == "Routine") %>%
    count(`Species name`) %>%
    filter(!str_detect(`Species name`, "Culex")) %>%
    pull(n)
  )

Simpson(
  mosquitoes %>% 
    filter(District == "Mandimba", program == "EASF") %>%
    count(`Species name`) %>%
    filter(!str_detect(`Species name`, "Culex")) %>%
    pull(n)
  )

Simpson(
  mosquitoes %>% 
    filter(District == "Mandimba", program == "Routine") %>%
    count(`Species name`) %>%
    filter(!str_detect(`Species name`, "Culex")) %>%
    pull(n)
)

Simpson(
  zambezia %>% 
    filter(program == "EASF") %>%
    count(`Species name`) %>%
    filter(`Species name` != "0") %>%
    pull(n)
)

Simpson(
  zambezia %>% 
    filter(program == "Routine") %>%
    count(`Species name`) %>%
    filter(`Species name` != "0") %>%
    pull(n)
)








easf <- list()

easf[[3]] <- read_excel(
  "Zambezia/EASF/EASF_Base dados_laboratorio_IH_Ano2 (1).xlsx"
  ) %>%
  rename("Mosquito microscopy code"="Mosquito code",
         "Latitude"="Latitude (telefone/tablet)",   
         "Longitude"="Longetude (telefone/tablet)") %>%
  select("Province", "District", #"House ID",     
         "Latitude", "Longitude", 
         "Collection method", "Date of collection",
         "Hour of collection", "Mosquito microscopy code",
         "Species name", "Place of collection") %>%
  mutate(`Date of collection`=as.Date(`Date of collection`, format="%d/%m/%Y"))


easf[[4]] <- read_excel(
  "Zambezia/EASF/EASF_Base_dados_AL_CDC_laboratorio_Ano2 (1).xlsx"
  ) %>%
  rename("Province"="Província", "District"="Distrito",
         "House ID"="Identificação da casa (ID)",
         "Collection method"="Método de colheita",
         "Date of collection"="Data de colheita",
         "Mosquito code"="Código do mosquito",
         "Species name"="Nome da espécie",
         "Place of collection"="Local de colheita (dentro ou fora)")

easf[[5]] <- read_excel(
  "Zambezia/Routine/Second year HLC &CDC LT December 2023 to August 2024.xlsx"
  ) %>%
  rename("Mosquito microscopy code"="Mosquito cod",
         "Date of collection"="Collection date",
         "Hour of collection"="Time collection",
         "Place of collection"="House collection position") %>%
  select("Province", "District", #"House ID",     
         "Latitude", "Longitude", "Elevation",
         "Collection method", "Date of collection",
         "Hour of collection", "Mosquito microscopy code",
         "Species name", "Place of collection") %>%
  mutate(`Date of collection`=as.Date(`Date of collection`, format="%d/%m/%Y"))



easf <- bind_rows(easf)

easf %>% count(District)

routine <- list()
routine[[1]] <- read_excel(
  "Niassa/Routine/Rotina_Mosquito_Database_IH _laboratorio_Cuamba.xlsx"
  )
routine[[2]] <- read_excel(
  "Niassa/Routine/Rotina_Mosquito_Database_IH _laboratorio_Mandimba.xlsx"
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
