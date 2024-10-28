
library("tidyverse")
library("readxl")
# =========================================================

# path to the data directory
data_path <- "~/Downloads/Mozambique/"

# names of the required data files
required_data_files <- 
  c(
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
    "Moamba_Flit.xlsx",
    # Niassa province
    # field data for CDC collection in the Cuamba district
    "Cuamba_HLC_Field.xlsx", 
    # lab data for CDC collection in the Cuamba district
    "Cuamba_HLC_Lab.xlsx",
    # field data for CDC collection in the Mandimba district
    "Mandimba_HLC_Field.xlsx", 
    # lab data for CDC collection in the Mandimba district
    "Mandimba_HLC_Lab.xlsx"
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
                      "Isca humana" ~ "HLC")) %>%
  # remove 3 rows with missing variables
  filter(!is.na(`Collection date (dd/mm/yyyy)`))

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
                   format="%d/%m/%Y")) %>%
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

# Cuamba lab data
lab_data[["Cuamba"]] <- 
  read_excel(paste0(data_path, "Niassa/Cuamba_HLC_Lab.xlsx"), 
             sheet="B__dados IH_laboratorio",
             range="A1:U722", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Date of collection` = 
           as.Date(`Date of collection`,
                   format="%d/%m/%Y"), 
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
  # make all letters in House ID upper case
  mutate(`House ID`=toupper(`House ID`)) %>%
  # make spelling of Niassa province consistent with Mandimba data file
  mutate(Province=case_match(Province,
                             "Níassa" ~ "Niassa",
                             .default=Province)) %>%
  # rename columns to have the same variable names for all data
  rename(`Collection date (dd/mm/yyyy)`=`Date of collection`,
         `House number`=`House No.`,
         `Location number`=`Location No.`,
         `Order number`=`Order No.`,
         `Mosquito microscopy code`=`Mosquito code`,
         `Comments/Observations`=`Comments`) %>%
  select(-c(`Municipality/villa`, `Administrative post`,
            Locality, Street, 
            Latitude, Longitude,
            `Complex/group name`)) %>%
  # translate Isca humana to HLC
  mutate(`Collection method`=
           case_match(`Collection method`,
                      "IH" ~ "HLC")) %>%
  # make mosquitoes species name consistent with other data files
  mutate(`Species name`=
           str_replace(`Species name`, 
                       "Anopheles", 
                       "An."))
if (FALSE)
{
  lab_data$Cuamba %>% 
    count(Province, District, `House ID`, Longitude, Latitude) %>%
    group_by(Province, District, `House ID`) %>%
    summarise(Longitude=mean(as.numeric(Longitude)),
              Latitude=mean(as.numeric(Latitude))) %>%
    write_csv(file="~/Desktop/Cuamba.csv")
}

# Mandimba lab data
lab_data[["Mandimba"]] <- 
  read_excel(paste0(data_path, "Niassa/Mandimba_HLC_Lab.xlsx"), 
             sheet="B__dados IH_laboratorio",
             range="A1:U1303", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  mutate(`Collection date` = 
           as.Date(`Collection date`,
                   format="%d/%m/%Y"), 
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
  rename(`Collection date (dd/mm/yyyy)`=`Collection date`,
         `House number`=`House  No.`,
         `Location number`=`Location No.`,
         `Collection method`=`Collection Method`,
         `Order number`=`Order No.`,
         `Place of collection`=`Position of collection`,
         `Mosquito microscopy code`=`Mosquito code`,
         `Technician name`=Technician,
         `Comments/Observations`=`Comments`) %>%
  select(-c(`Municiplaity/Village`, `Administrative post`,
            Locality, Street, 
            Latitude, Longitude,
            `Complex group`)) %>%
  # translate Isca humana to HLC
  mutate(`Collection method`=
           case_match(`Collection method`,
                      "IH" ~ "HLC")) %>%
  # make mosquitoes species name consistent with other data files
  mutate(`Species name`=
           str_replace(`Species name`, 
                       "Anopheles", 
                       "An."),
         `Species name`=
           case_match(`Species name`,
                      "An. Squamosus" ~ "An. squamosus",
                      "An. gambiae  s.l" ~ "An. gambiae s.l",
                      .default=`Species name`))

if (FALSE)
{
  lab_data$Mandimba %>% 
    count(Province, District, `House ID`, Longitude, Latitude) %>%
    group_by(Province, District, `House ID`) %>%
    summarise(Longitude=mean(as.numeric(Longitude)),
              Latitude=mean(as.numeric(Latitude))) %>%
    write_csv(file="~/Desktop/Mandimba.csv")
}

# check of all lab data frames have the same column names
all.equal(colnames(lab_data[["Gurue"]]),
          colnames(lab_data[["Morrumbala"]]))
all.equal(colnames(lab_data[["Gurue"]]),
          colnames(lab_data[["Cuamba"]]))
all.equal(colnames(lab_data[["Cuamba"]]),
          colnames(lab_data[["Mandimba"]]))
# save all lab data
saveRDS(lab_data, file=paste0(data_path, "lab_data.rds"))

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
         #`Type of roof`=`Type of room`,
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
  rename(`Collection method`=`Collection method...9`) %>%
  # translate Isca humana to HLC
  mutate(`Collection method`=
           case_match(`Collection method`,
                      "Isca humana" ~ "HLC"))
  
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


# Cuamba field data
field_data[["Cuamba"]] <- 
  read_excel(paste0(data_path, "Niassa/Cuamba_HLC_Field.xlsx"), 
             sheet="B. dados comportamen_IH_Cuamba",
             range="A1:Z1642", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  select(-`Hour of collection...20`) %>%
  rename(`Hour of collection`=
           `Hour of collection...11`) %>%
  mutate(`Date of collection` = 
           as.Date(`Date of collection`,
                   format="%d/%m/%Y"),
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
  rename(
    `Collection date (dd/mm/yyyy)`=`Date of collection`,
    `House number`=`House No.`,
    `Location number`=`Location no.`,
    `Number of people sleeping or awake but under the net (outside the house)`=`Number of people sleeping or awake but under the net (outside the home)`,
    `Number of people awake but NOT under the net (outside the house)`=`Number of people awake but NOT under the net (away from home)`,
    `Number of people sleeping or awake but under the net (indoors)`=`Number of people asleep or awake but under the net (indoors)`,
    `Rain or No`=`Rain or No rain`,
    `Number of mosquitoes collected indoors`=`Total mosquitoes collected indoors`,
    `Number of mosquitoes collected outdoors`=`Total mosquitoes collected outdoors`,
    `Code of volunteer outside the house`=`Volunteer code outdoors`,
    `Code of volunteer inside the house`=`Volunteer code indoors`,
    `Name of tehnician`=`Name of technician`,
    `Comments/Observations`=`Comments`
  ) %>%
  # translate Isca humana to HLC
  mutate(`Collection method`=
           case_match(`Collection method`,
                      "IH" ~ "HLC"))

# Mandimba field data
field_data[["Mandimba"]] <- 
  read_excel(paste0(data_path, "Niassa/Mandimba_HLC_Field.xlsx"), 
             sheet="B. dado comportamen_IH_Mandimba",
             range="A1:Z2212", 
             col_names=TRUE,
             na=c("", "N/A")) %>%
  # prepare date and time (hour) of collection
  select(-`Hour of collection...20`) %>%
  rename(`Hour of collection`=
           `Hour of collection...11`) %>%
  mutate(`Date of collection` = 
           as.Date(`Date of collection`,
                   format="%d/%m/%Y"),
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
  rename(
    `Collection date (dd/mm/yyyy)`=`Date of collection`,
    `House number`=`House No.`,
    `Location number`=`Location No.`,
#    `Number of people sleeping or awake but under the net (outside the house)`=`Number of people sleeping or awake but under the net (outside the home)`,
#    `Number of people awake but NOT under the net (outside the house)`=`Number of people awake but NOT under the net (away from home)`,
#    `Number of people sleeping or awake but under the net (indoors)`=`Number of people asleep or awake but under the net (indoors)`,
    `Rain or No`=`Rain or no Rain`,
    `Number of mosquitoes collected indoors`=`Total number of mosquitoes  inside`,
    `Number of mosquitoes collected outdoors`=`Total number of mosquitoes outside`,
    `Code of volunteer outside the house`=`Volunteer code outside`,
    `Code of volunteer inside the house`=`Volunteer code indoors`,
    `Name of tehnician`=`Technician name`,
    `Comments/Observations`=`Comments`
  ) %>%
  # translate Isca humana to HLC
  mutate(`Collection method`=
           case_match(`Collection method`,
                      "IH" ~ "HLC"))

# differences between column names of field data
setdiff(colnames(field_data[["Gurue"]]),
        colnames(field_data[["Morrumbala"]]))
setdiff(colnames(field_data[["Morrumbala"]]),
        colnames(field_data[["Gurue"]]))
setdiff(colnames(field_data[["Moamba"]]),
        colnames(field_data[["Gurue"]]))
setdiff(colnames(field_data[["Gurue"]]),
        colnames(field_data[["Moamba"]]))
setdiff(colnames(field_data[["Gurue"]]),
        colnames(field_data[["Cuamba"]]))
setdiff(colnames(field_data[["Cuamba"]]),
        colnames(field_data[["Mandimba"]]))

# save all field data
saveRDS(field_data, file=paste0(data_path, "field_data.rds"))

# ---------------------------------------------
# already merged data files: contains field records on 
#     collection date, time an locations, 
#     characteristics of houses, and human behavior
#     and collected mosquitoes information
# each row: record of a collection attempt or a collected mosquito
# ---------------------------------------------

merged_data <- list()

# Morrumbala Prokopack data
merged_data[["Morrumbala"]] <- 
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
         `Name of tehnician`=`Supervisor name`) %>%
  # change 0 for no msquitoes to NA
  mutate(`Species name`=
           case_match(`Species name`,
                      "0" ~ NA,
                      .default=`Species name`))

# Moamba Flit data

merged_data[["Moamba"]] <- 
  read_excel(paste0(data_path, 
                               "Maputo/Moamba_Flit.xlsx"), 
                        sheet="Base dados_Flit (PSCs)_Moamba",
                        range="A1:BE61", 
                        col_names = TRUE,
                        na=c("", "N/A")) %>%
  # rename columns to have the same variable names for all data
  rename(`House number`=`House no.`,
         `Location number`=`Location No.`,
         `Collection method`=`Collection Method`,
         `Collection date (dd/mm/yyyy)`=`Date of collection`,
         `Number of compartment/rooms in the house`=`Number of compartments in the room`,
         `Number of people slept in the house last night`=`No. of people slept in the house last night`,
         `Number of people slept in the room of mosquito collection`=`No. of people slepr in the room of mosquito collection`,
         `Name of the animal present inside the house`=`Name of animal present inside the house`,
         `Name of the animal present outside the house`=`Name of animal present outside the house`,
         `Bednet present in the room of mosquito collecton`=`Bednet present in the room where trap is placed`,
         `number of bednets present in the room where the trap is placed`=`No. f bednets present in the room bednet is placed`,
         `Did you sleep under bednet last night`=`Slept under bednet last night`,
         `Date of spray (dd/mm/yyyy)`=`Date of spraying`,
         `Number of rooms sprayed`=`No of rooms sprayed`,                                                 
         `Class of insecticide used`=`Class of insecticide`,                                               
         `Name of compartment sprayed`=`Nme of the compartment sprayed`,
         `Walls of the house modified`=`Modified walls`,                                             
         `Which rooms are modified`=`Rooms modified`,                                                
         `Methdo of modification`=`type of modification done`,                                                  
         `Eave status`=`Status of eave space`,
         `Nr. Total of  Culex`=`Nr. Total  Culex`,
         `Nr. An. gambiae sl fed`=`Nr. An. gambiae fed`,
         `Name of supervisor`=Supervisor) %>%
  # prepare date and time (hour) of collection
  mutate(`Collection date (dd/mm/yyyy)` = 
           as.Date(`Collection date (dd/mm/yyyy)`,
                   format="%d/%m/%y"),
         `Collection start time`=
           substr(`Collection start time`, 12, 16),
         `Collection stop time`=
           substr(`Collection stop time`, 12, 16)) %>%
  # character to numeric conversion
  mutate(`number of bednets present in the room where the trap is placed`=
           as.numeric(`number of bednets present in the room where the trap is placed`),
         `Number of rooms sprayed`=
           as.numeric(`Number of rooms sprayed`))

# save all merged data
saveRDS(merged_data, file=paste0(data_path, "merged_data.rds"))

