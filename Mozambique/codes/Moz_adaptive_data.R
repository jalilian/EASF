
library("tidyverse")
# =========================================================

# path to the data directory
data_path <- "~/Downloads/Mozambique/"

# read lab data
lab_data <- readRDS(file=paste0(data_path, "lab_data.rds"))
# read field data
field_data <- readRDS(file=paste0(data_path, "field_data.rds"))
# read already merged data
merged_data <- readRDS(file=paste0(data_path, "merged_data.rds"))

# =========================================================
# creating the data table for adaptive sampling framework

adaptive_table <- list()

# Morrumbala Prokopack
adaptive_table[["Morrumbala-Prokopack"]] <- 
  merged_data$Morrumbala %>% 
  count(Province, District, `House ID`,
    `Collection date (dd/mm/yyyy)`, 
    `Species name`) %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>% 
  select(-`NA`)

# Moamba Flit
adaptive_table[["Moamba-Flit"]] <-
  merged_data$Moamba %>%
  select(Province, District, `House ID`,
         `Collection date (dd/mm/yyyy)`,
         `Nr. Total  An. gambiae sl`,
         `Nr. Total An. funestus sl`) %>%
  rename(`An. gambiae s.l`=`Nr. Total  An. gambiae sl`,
         `An. funestus s.l`=`Nr. Total An. funestus sl`)

# Gurue AL-CDC
adaptive_table[["Gurue-AL-CDC"]] <-
  field_data$Gurue %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection Hour`=n) %>%
  left_join(lab_data$Gurue %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0)

# Morrumbala HLC
adaptive_table[["Morrumbala-HLC"]] <-
  field_data$Morrumbala %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection Hour`=n) %>%
  left_join(lab_data$Morrumbala %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`)

# Moamba AL-CDC
adaptive_table[["Moamba-AL-CDC"]] <-
  field_data$Moamba %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection Hour`=n) %>%
  left_join(lab_data$Moamba %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`)

merged_data$Morrumbala %>% 
  count(`Species name`)

filed_data[["Gurue"]] %>% count(`Collection method`)
# path to the data directory
data_path <- "~/Downloads/Mozambique/"

all_data <- readRDS("~/Desktop/Moz_Year1_data.rds")

all_data %>% 
  count(Province, District, `House ID`,
        `Collection method`,
    `Collection date (dd/mm/yyyy)`) %>% print(n=500)

all_data %>% filter(is.na(`Collection date (dd/mm/yyyy)`)) %>% View()

# merge all lab data
lab_data <- 
  bind_rows(lab_data[["Gurue"]], 
            lab_data[["Morrumbala"]],
            lab_data[["Moamba"]])

# merge all field data
field_data <- 
  bind_rows(field_data[["Gurue"]],
            field_data[["Morrumbala"]]  %>%
              select(-c("CDC-LT worked all night", 
                        "Status of the lamp")),
            field_data[["Moamba"]] %>%
              select(-c("CDC-LT worked all night", 
                        "Status of the lamp", 
                        "Observations",
                        "Name of supervisor")))


# -----------------------------------------------
# merge lab and feild data
all_data <- 
  full_join(field_data, lab_data, 
            na_matches="never",
            relationship="many-to-many")

all_data <- all_data %>%
  mutate(`Place of collection`=case_match(`Place of collection`,
                                          "Dentro"~"Indoor",
                                          "For a" ~ "Outdoor",
                                          "Fora" ~ "Outdoor"))



# character to numeric conversion
all_data <- all_data %>%
  mutate(`Number of rooms sprayed`=as.numeric(`Number of rooms sprayed`)) %>%
  rename(`Number of people slept in the room of mosquito collection`=`Number of people slept in the room with the trap`,
         `Bednet present in the room of mosquito collecton`=`Bednet exist in the room where the trap is placed`)

# merging
all_data <- bind_rows(all_data, porko_data)


# merging
all_data <- bind_rows(all_data, flit_data)

# -----------------------------------------------
# tweaking the data

all_data <- all_data %>%
  mutate(`Temperature (oC) indoors`=ifelse(`Temperature (oC) indoors` > 35,
                                           NA, `Temperature (oC) indoors`),
         `Temperature (oC) outdoors`=ifelse(`Temperature (oC) outdoors` > 39,
                                            NA, `Temperature (oC) outdoors`),
         `% Relative Humidity  (RH) indoors`=ifelse(`% Relative Humidity  (RH) indoors` > 100,
                                                    `% Relative Humidity  (RH) indoors` / 10,
                                                    `% Relative Humidity  (RH) indoors`),
         `% Realtive Humidity  (RH) outdoors`=ifelse(`% Realtive Humidity  (RH) outdoors` > 100,
                                                     `% Realtive Humidity  (RH) outdoors` / 10,
                                                     `% Realtive Humidity  (RH) outdoors`))

all_data <- all_data %>%
  mutate(across(c(`Bednet present inside the house`,
                  `Bednet present in the room of mosquito collecton`,
                  `Did you sleep under bednet last night`,
                  `House sprayed`,
                  `Animals present inside the house`,
                  `Animal present outside the house`,
                  `Walls of the house modified`,
                  `Rain or No`),
                ~case_when(. == "Não" ~ "No",
                           . == "não" ~ "No",
                           . == "Nao" ~ "No",
                           . == "Sim" ~ "Yes",
                           . == "sim" ~ "Yes"))) %>%
  mutate(`Type of roof`=case_match(`Type of roof`, 
                                   "palha/capim" ~ "Palha/Capim",
                                   .default=`Type of roof`),
         `Name of the animal present inside the house`=
           case_match(`Name of the animal present inside the house`,
                      "cabrito" ~ "Cabrito",
                      "cobaias" ~ "Cobaias",
                      "galinhas" ~ "Galinhas",
                      "gato" ~ "Gato",
                      "gatos" ~ "Gato",
                      "galinhas/gato" ~ "Galinhas/gato",
                      "Cabrito/gato" ~ "Cabrito/Gato",
                      "Pato/Cobaias" ~ "Patos/Cobaias",
                      "Pato/galinhas" ~ "Patos/galinhas",
                      .default=`Name of the animal present inside the house`),
         `Name of the animal present outside the house`=
           case_match(`Name of the animal present outside the house`,
                      "gato" ~ "Gato",
                      "Cabrito/gato" ~ "Cabrito/Gato",
                      "Cão" ~ "Cao",
                      "Cabrito" ~ "Cabritos",
                      "Cabrito/gato, galinhas" ~ "Cabritos/Galinhas/Gato",
                      "pombos" ~ "Pombo",
                      "Pombos" ~ "Pombo",
                      "Patos/galinhas" ~ "Patos/Galinhas",
                      "Pato/galinha" ~ "Patos/Galinhas",
                      "3" ~ NA,
                      .default=`Name of the animal present outside the house`),
         `Type of bednet`=
           case_match(`Type of bednet`,
                      "Duranet" ~ "DuraNet",
                      "Duranet Plus" ~ "DuraNet Plus",
                      "Magnet" ~ "MagNet",
                      "Olyset-Net" ~ "Olyset Net",
                      "Olyset-net" ~ "Olyset Net",
                      "olyset-Net" ~ "Olyset Net",
                      "Olyset-plus" ~ "Olyset Plus",
                      "Duranet & Oliset Net" ~ "Duranet & Olyset Net",
                      "Duranet & Olyset" ~ "Duranet & Olyset Net",
                      "royal Sentry" ~ "Royal Sentry",
                      "royal guard" ~ "Royal guard",
                      .default=`Type of bednet`),
         `Name of compartment sprayed`=
           case_match(`Name of compartment sprayed`,
                      "Quartos" ~ "Quarto",
                      "Sala de estar e quarto" ~ "Sala de estar e Quarto",
                      "Sala de estar quarto e cozinha" ~ "Sala de estar ,Quarto e cozinha",
                      .default=`Name of compartment sprayed`),
         `Sex of volunteer inside the house`=
           case_match(`Sex of volunteer inside the house`,
                      "masculino" ~ "Masculino",
                      .default=`Sex of volunteer inside the house`),
         `Place of collection`=
           case_match(`Place of collection`,
                      "Dentro" ~ "Indoor",
                      .default=`Place of collection`),
         `Species name`=
           case_match(`Species name`,
                      "An. Desconhecido" ~ "An. desconhecido",
                      "0" ~ NA,
                      .default=`Species name`),
         `Technician name`=
           case_match(`Technician name`,
                      "Joao" ~ "João",
                      .default=`Technician name`))

saveRDS(all_data, file="~/Desktop/Moz_Year1_data.rds")
