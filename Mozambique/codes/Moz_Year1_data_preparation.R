
library("tidyverse")
library("readxl")
# =========================================================

# path to the data directory
data_path <- "~/Downloads/Mozambique/"

# names of the required data files
required_data_files <- c(
  # Zambezia province
  # field data for the Gurue district
  "Gurue_HLC_Field.xlsx", 
  # lab data for the Gurue district
  "Gurue_HLC_Lab.xlsx",   
  # field data for the Morrumbala district
  "Morrumbala_CDC_Field.xlsx", 
  # lab data for the Morrumbala district
  "Morrumbala_CDC_Lab.xlsx",   
  # merged (field and lab) data for Prokopack collection in the Morrumbala district
  "Morrumbala_Prokopack.xlsx", 
  # Maputo province
  # field data for CDC collection in the Moamba district
  "Moamba_CDC_Field.xlsx", 
  # lab data for CDC collection in the Moamba district
  "Moamba_CDC_Lab.xlsx",
  # merged (field and count) data for CDC collection in the Moamba district
  "Moamba_Flit.xlsx"
)

required_data_files <- 
  lapply(required_data_files, 
         function(o){ 
           any(grepl(o, list.files(data_path, recursive=TRUE))) 
           })

# check if data files are in the data directory
if (!all(unlist(required_data_files)))
{
  stop("The above data files are missing")
}

# =========================================================
# reading the data

# ---------------------------------------------
# lab data files: contains lab records on all 
#     morphologically identified Anopheles mosquitoes
# each row: record of a single mosquito
# ---------------------------------------------

lab_data <- list()

# Gurue lab data
lab_data[["Gurue"]] <- 
  read_excel(paste0(data_path, "Zambezia/Gurue_HLC_Lab.xlsx"), 
             sheet=" B.dados IH Gurue Anophles",
             range="A1:Q595", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Date of collection (dd/mm/yyyy)` = 
           as.Date(`Date of collection (dd/mm/yyyy)`,
                   format="%d/%m/%y"),
         `Hour of collection` = 
           factor(`Hour of collection`, 
                  levels=c("18-19", "19-20", "20-21", "21-22", 
                           "22-23", "23-24", "24-1", "1-2", 
                           "2-3", "3-4", "4-5", "5-6"))) %>%
  # rename columns to have the same variable names for all data
  rename(`Collection date (dd/mm/yyyy)`=`Date of collection (dd/mm/yyyy)`,
         `House number`=`House No.`,
         `Location number`=`Location No.`,
         `Place of collection`=`Position of collection`,
         `Collection method`=`Method of collection`,
         `Order number`=`Order No.`,
         `Species name`=Species,
         `Mosquito microscopy code`=`Mosquito Microscopy code`,
         `Technician name`=Technician) %>%
  select(-c(`Hour no.`, Months)) %>%
  # translate Isca humana to HLC
  mutate(`Collection method`=
           case_match(`Collection method`,
                      "Isca humana" ~ "HLC"))

# Morrumbala lab data
lab_data[["Morrumbala"]] <- 
  read_excel(paste0(data_path, "Zambezia/Morrumbala_CDC_Lab.xlsx"), 
             sheet="ID_ AL_CDC_Tenda Morrumbala",
             range="A1:O1688", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Collection date (dd/mm/yyyy)` = 
           as.Date(`Collection date (dd/mm/yyyy)`,
                   format="%d/%m/%y"),
         `Hour of collection` = 
           case_match(`Hour of collection`,
                      "24-01" ~ "24-1",
                      "01-02" ~ "1-2",
                      "02-03" ~ "2-3",
                      "03-04" ~ "3-4",
                      "04-05" ~ "4-5",
                      "05-06" ~ "5-6",
                      .default=`Hour of collection`),
         `Hour of collection` = 
           factor(`Hour of collection`, 
                  levels=c("18-19", "19-20", "20-21", "21-22", 
                           "22-23", "23-24", "24-1", "1-2", 
                           "2-3", "3-4", "4-5", "5-6"))) %>%
  # rename columns to have the same variable names for all data
  rename(`Comments/Observations`=`Comments/observation`)

# Moamba lab data
lab_data[["Moamba"]] <- 
  read_excel(paste0(data_path, "Maputo/Moamba_CDC_Lab.xlsx"), 
             sheet="AL-CDC Moamba",
             range="A1:O16", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Date of collection` = 
           as.Date(`Date of collection`,
                   format="%d/%m/%y")) %>%
  # rename columns to have the same variable names for all data
  rename(`Collection date (dd/mm/yyyy)`=`Date of collection`,
         `House number`=`House No.`,
         `Location number`=`location No.`,
         `Place of collection`=`Position of collection`,
         `Collection method`=`Method of collection`,
         `Order number`=`Order No.`,
         `Mosquito microscopy code`=`Mosquito code`,
         `Comments/Observations`=`Comments/Observation`,
         `Technician name`=Technician)

# check of all lab data frames have the same column names
all.equal(colnames(lab_data[["Gurue"]]),
          colnames(lab_data[["Morrumbala"]]),
          colnames(lab_data[["Moamba"]]))
  
# merge all lab data
lab_data <- 
  bind_rows(lab_data[["Gurue"]], 
            lab_data[["Morrumbala"]],
            lab_data[["Moamba"]])

# ---------------------------------------------
# filed data files: contains field records on 
#     collection date, time an locations, 
#     characteristics of houses, and human behavior 
# each row: record of a collection attempt
# ---------------------------------------------  

field_data <- list()
# Gurue field data
field_data[["Gurue"]] <- 
  read_excel(paste0(data_path, "Zambezia/Gurue_HLC_Field.xlsx"), 
             sheet="Base dados IH_Gurue",
             range="A1:BF3031", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Collection date (dd/mm/yyyy)` = 
           as.Date(`Collection date (dd/mm/yyyy)`,
                   format="%d/%m/%y"),
         `Hour of collection` = 
           factor(`Hour of collection`, 
                  levels=c("18-19", "19-20", "20-21", "21-22", 
                           "22-23", "23-24", "24-1", "1-2", 
                           "2-3", "3-4", "4-5", "5-6"))) %>%
  # rename columns to have the same variable names for all data
  rename(`House number`=`House No`,
         `Type of roof`=`Type of room`,
         `Number of compartment/rooms in the house`=`Number of rooms in the house`,
         `Number of people slept in the room with the trap`=`Number of people slept in the room where trap is placed`,
         `Animals present inside the house`=`Animal present inside the house`,
         `Name of the animal present outside the house`=`Name of the animal outside the house`,
         `Bednet exist in the room where the trap is placed`=`Bednet present in the room of mosquito collection`,
         `number of bednets present in the room where the trap is placed`=`Number of bednets present in the room of mosquito collection`,
         `Date of spray (dd/mm/yyyy)`=`Date of spraying`,
         `Insecticide used`=`Insectcide sprayed`,
         `Class of insecticide used`=`Class of insecticide sprayed`,
         `Name of compartment sprayed`=`Name of the room sprayed`,
         `Which rooms are modified`=`Which rooms modified`,
         `Methdo of modification`=`Type of modifcation done`,
         `Sex of volunteer outdoors`=`Sex of volunteer outside the house`,
         `Rain or No`=`Rain or no Rain`,
         `% Realtive Humidity  (RH) outdoors`=`% Relative Humidity relativa (RH) outdoors`,
         `% Relative Humidity  (RH) indoors`=`% Relative Humidity relative (RH) indoors`,
         `Code of volunteer outside the house`=`Code of volunteer outdoors`,
         `Name of tehnician`=`Name of technician`) %>%
  # removing rows where everything is missing
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  # make an adittional variable to match other data files
  mutate(`Bednet present inside the house` = 
           ifelse(`Bednet present inside the house` > 0,
                  "Sim", "Não")) %>%
  #fix Longitude and Longitude
  mutate(Longitude=
           as.numeric(Longitude),
         Latitude=
           as.numeric(Latitude)) %>%
  # fix collection method
  select(-`Collection method...37`) %>%
  rename(`Collection method`=`Collection method...9`)
  
# Morrumbala field data
field_data[["Morrumbala"]] <- 
  read_excel(paste0(data_path, "Zambezia/Morrumbala_CDC_Field.xlsx"), 
             sheet="B dados AL_CDC_Tenda_Morrumbala",
             range="A1:BH4320", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Date of collection (dd/mm/yyyy)` = 
           as.Date(`Date of collection (dd/mm/yyyy)`,
                   format="%d/%m/%y"),
         `Hour of collection` = 
           case_match(`Hour of collection`,
                      "24-01" ~ "24-1",
                      "01-02" ~ "1-2",
                      "02-03" ~ "2-3",
                      "03-04" ~ "3-4",
                      "04-05" ~ "4-5",
                      "05-06" ~ "5-6",
                      .default=`Hour of collection`),
         `Hour of collection` = 
           factor(`Hour of collection`, 
                  levels=c("18-19", "19-20", "20-21", "21-22", 
                           "22-23", "23-24", "24-1", "1-2", 
                           "2-3", "3-4", "4-5", "5-6"))) %>%
  # rename columns to have the same variable names for all data
  rename(`Collection date (dd/mm/yyyy)`=`Date of collection (dd/mm/yyyy)`) %>%
  # removing rows where everything is missing
  filter(if_any(everything(), ~ !is.na(.))) %>%
  # remove 3 rows with missing variables
  filter(!is.na(District))%>% 
  # fix collection start and stop times
  mutate(`Collection start time` = 
           as.character(`Collection start time`),
         `Collection stop time` = 
           as.character(`Collection stop time`)) %>%
  # fix temprature
  mutate(`Temperature (oC) outdoors`=
           as.numeric(`Temperature (oC) outdoors`)) %>%
  #fix Longitude and Longitude
  mutate(Longitude=
           as.numeric(Longitude),
         Latitude=
           as.numeric(Latitude)) %>%
  # fix collection method
  select(-`Collection method...37`) %>%
  rename(`Collection method`=`Collection method...9`)

# Moamba field data
field_data[["Moamba"]] <- 
  read_excel(paste0(data_path, "Maputo/Moamba_CDC_Field.xlsx"), 
             sheet="CDC Moamba",
             range="A1:BF519", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Date of collection` = 
           as.Date(`Date of collection`,
                   format="%d/%m/%y"),
         `Hour of collection` = "18-06") %>%
  # rename columns to have the same variable names for all data
  rename(`Collection date (dd/mm/yyyy)`=`Date of collection`,
         `Status of eaves in the house`=`Eaves status`,
         `Administrative post`=Administrative,
         `House number`=`House No.`,
         `Location number`=`Location No.`,
         `Collection method`=`Method of collection`,
         `Number of compartment/rooms in the house`=`Number of rooms in the house`,
         `Number of people slept in the house last night`=`No. of people slept in the house last night`,
         `Number of people slept in the room with the trap`=`No. of people slept in the room of mosquito collection`,
         `Animals present inside the house`=`Animals inside the house`,
         `Name of the animal present inside the house`= `Name of animal inside the house`,
         `Name of the animal present outside the house`=`Name of animal present outside the house`,
         `Bednet exist in the room where the trap is placed`=`Bednet present in the room of mosquito collection`,
         `Type of bednet`=`Type of bednest`,
         `number of bednets present in the room where the trap is placed`=`No. of bednets present in the room of mosquito collection`,
         `Did you sleep under bednet last night`=`Slept under bednight last night`,
         `Date of spray (dd/mm/yyyy)`=`Date of spraying`,
         `Number of rooms sprayed`=`No of rooms sprayed`,
         `Class of insecticide used`=`Insecticide class`,
         `Methdo of modification`=`Modifcation method`,
         `Walls of the house modified`=`Walls modified`,
         `Collection stop time`=`Collection end time`,
         `Name of compartment sprayed`=`Name of room sprayed`,
         `Which rooms are modified`=`Rooms with walls modified`,
         `Name of tehnician`=`Name of technician`,
         `CDC-LT worked all night`=`CDC-LTs worked all night`,
         `Status of the lamp`=`Statuis of the lamp`
  ) %>%
  # fix character to numeric conversion
  mutate(`number of bednets present in the room where the trap is placed`=
           as.numeric(`number of bednets present in the room where the trap is placed`))


# differences between column names of field data
setdiff(colnames(field_data[["Gurue"]]),
        colnames(field_data[["Morrumbala"]]))
setdiff(colnames(field_data[["Morrumbala"]]),
        colnames(field_data[["Gurue"]]))
setdiff(colnames(field_data[["Moamba"]]),
        colnames(field_data[["Gurue"]]))
setdiff(colnames(field_data[["Gurue"]]),
        colnames(field_data[["Moamba"]]))

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
  

# combine data with Morrumbala Prokopack data
porko_data <- 
  read_excel(paste0(data_path, 
                    "Zambezia/Morrumbala_Prokopack.xlsx"), 
             sheet="B.dados_procopac_Morrumbala",
             range="A1:AV130", 
             col_names = TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Collection date(dd/mm/yyyy)` = 
           as.Date(`Collection date(dd/mm/yyyy)`,
                   format="%d/%m/%y")) %>%
  # create collection start and stop times
  mutate(`Collection start time`=substr(as.character(`Collection start time`),
                                        12, 16),
         `Collection stop time`=substr(as.character(`Collection stop time`),
                                       12, 16)) %>%
  select(-`Method of collection`) %>%
  #fix Longitude and Longitude
  mutate(Longitude=
           as.numeric(Longitude),
         Latitude=
           as.numeric(Latitude)) %>%
  # rename columns to have the same variable names for all data
  rename(`Collection date (dd/mm/yyyy)`=`Collection date(dd/mm/yyyy)`,
         `House number`=`House No`,
         `Location number`=`Location No`,
         `Number of compartment/rooms in the house`=`Number of rooms in the house`,
         `Number of people slept in the house last night`=`No of people slept in the house last night`,
         `Number of people slept in the room of mosquito collection`=`No of people slept in the room of mosquito collection`,
         `Animals present inside the house`=`Animals inside the house`,
         `Name of the animal present inside the house`=`Name of animal inside the house`,
         `Animal present outside the house`=`Animal outside the house`,
         `Name of the animal present outside the house`=`Name of animal outside the house`,
         `Bednet present inside the house`=`Exist bednet in the house`,
         `number of bednets present in the room where the trap is placed`=`Number of bednets in the room of mosquito collection`,
         `Did you sleep under bednet last night`=`Slept under bednet last night`,
         `Name of compartment sprayed`=`Name of the room sprayed`,
         `Walls of the house modified`=`Walls modified`,
         `Which rooms are modified`=`Rooms modified`,
         `Methdo of modification`=`How the rooms are modified`,
         `Order number`=`Orden No`,
         `Species name`=`Species collected`,
         `Mosquito microscopy code`=`Mosquito code`,
         `Name of tehnician`=`Supervisor name`)  

# character to numeric conversion
all_data <- all_data %>%
  mutate(`Number of rooms sprayed`=as.numeric(`Number of rooms sprayed`)) %>%
  rename(`Number of people slept in the room of mosquito collection`=`Number of people slept in the room with the trap`,
         `Bednet present in the room of mosquito collecton`=`Bednet exist in the room where the trap is placed`)

# merging
all_data <- bind_rows(all_data, porko_data)
  
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
