
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
  # compute collection time
  mutate(`Collection time`=
           difftime(as.POSIXlt(paste(`Collection date (dd/mm/yyyy)`, 
                                     `Collection stop time`)), 
                    as.POSIXlt(paste(`Collection date (dd/mm/yyyy)`, 
                                     `Collection start time`)), 
                    units="mins")) %>%
  # make sure times are positive
  mutate(`Collection time`=
           abs(`Collection time`)) %>%
  group_by(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  summarise(`Collection time`=
              sum(`Collection time`)) %>%
  left_join(merged_data$Morrumbala %>% 
              filter(!is.na(`Species name`)) %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Mosquito microscopy code`,
                    `Species name`) %>% 
              select(-c(`Mosquito microscopy code`, n))) %>%
  count(Province, District, `House ID`, 
        `Collection date (dd/mm/yyyy)`, 
        `Collection time`, 
        `Species name`) %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>% 
  select(-`NA`)

# Moamba Flit
adaptive_table[["Moamba-Flit"]] <-
  merged_data$Moamba %>%
  # compute collection time
  mutate(`Collection time`=
           difftime(as.POSIXlt(paste(`Collection date (dd/mm/yyyy)`, 
                                     `Collection stop time`)), 
                    as.POSIXlt(paste(`Collection date (dd/mm/yyyy)`, 
                                     `Collection start time`)), 
                    units="mins")) %>%
  # make sure times are positive
  mutate(`Collection time`=
           abs(`Collection time`)) %>%
  select(Province, District, `House ID`,
         `Collection date (dd/mm/yyyy)`,
         `Collection time`,
         `Nr. Total  An. gambiae sl`,
         `Nr. Total An. funestus sl`) %>%
  rename(`An. gambiae s.l`=`Nr. Total  An. gambiae sl`,
         `An. funestus s.l`=`Nr. Total An. funestus sl`)

# Gurue AL-CDC
all(unique(lab_data$Gurue %>% 
             pull(`House ID`)) %in% 
      unique(field_data$Gurue %>% 
               pull(`House ID`)))

adaptive_table[["Gurue-AL-CDC"]] <-
  field_data$Gurue %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection hour`=n) %>%
  left_join(lab_data$Gurue %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`)

# Morrumbala HLC
all(unique(lab_data$Morrumbala %>% 
             pull(`House ID`)) %in% 
      unique(field_data$Morrumbala %>% 
               pull(`House ID`)))

adaptive_table[["Morrumbala-HLC"]] <-
  field_data$Morrumbala %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection hour`=n) %>%
  left_join(lab_data$Morrumbala %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`)

# Moamba AL-CDC
all(unique(lab_data$Moamba %>% 
             pull(`House ID`)) %in% 
      unique(field_data$Moamba %>% 
               pull(`House ID`)))

adaptive_table[["Moamba-AL-CDC"]] <-
  field_data$Moamba %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection hour`=n) %>%
  left_join(lab_data$Moamba %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`) %>%
  rename(`An. gambiae s.l`=`Total of An. gambiae sl`,
         `An. funestus s.l`=`An. funestus sl`)

# Cuamba HLC
all(unique(lab_data$Cuamba %>% 
             pull(`House ID`)) %in% 
      unique(field_data$Cuamba %>% 
               pull(`House ID`)))

adaptive_table[["Cuamba-HLC"]] <-
  field_data$Cuamba %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection hour`=n) %>%
  left_join(lab_data$Cuamba %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`)

# Mandimba HLC
all(unique(lab_data$Mandimba %>% 
             pull(`House ID`)) %in% 
      unique(field_data$Mandimba %>% 
               pull(`House ID`)))

adaptive_table[["Mandimba-HLC"]] <-
  field_data$Mandimba %>% 
  count(Province, District, `House ID`,
        `Collection date (dd/mm/yyyy)`) %>%
  rename(`Collection hour`=n) %>%
  left_join(lab_data$Mandimba %>%
              count(Province, District, `House ID`,
                    `Collection date (dd/mm/yyyy)`,
                    `Species name`))  %>%
  pivot_wider(names_from=`Species name`, 
              values_from=n) %>% 
  replace(is.na(.), 0) %>%
  select(-`NA`)

# check column names
colnames(adaptive_table[["Morrumbala-Prokopack"]])
colnames(adaptive_table[["Moamba-Flit"]])
colnames(adaptive_table[["Gurue-AL-CDC"]])
colnames(adaptive_table[["Morrumbala-HLC"]])
colnames(adaptive_table[["Moamba-AL-CDC"]])
colnames(adaptive_table[["Cuamba-HLC"]])
colnames(adaptive_table[["Mandimba-HLC"]])

# marge
adaptive_table <- 
  adaptive_table %>% reduce(full_join) %>% as_tibble()

# =========================================================
# reading coordinates of the collection houses
house_coords <- 
  read_csv(paste0(data_path, "Moz_coordinates_updated.csv"))

# include coordinates in the adaptive table
adaptive_table <- 
  adaptive_table %>% 
  left_join(house_coords) %>%
  relocate(Longitude, Latitude, .after=`House ID`)

# monthly
adaptive_table <- 
  adaptive_table %>%
  mutate(Year=substr(`Collection date (dd/mm/yyyy)`, 1, 4),
         Month=substr(`Collection date (dd/mm/yyyy)`, 6, 7),
         Month=month.name[as.integer(Month)]) %>%
  relocate(Year, Month, .before=`Collection date (dd/mm/yyyy)`) %>%
  select(-`Collection date (dd/mm/yyyy)`) %>%
  group_by(Province, District, 
           `House ID`, `Collection method`,
           Longitude, Latitude,
           Year, Month) %>%
  summarise(across(c(`Collection hour`, 
                     `Collection time`,
                     starts_with("An. ")), 
                   ~sum(.x ,na.rm=TRUE))) %>% 
  as_tibble()

# save adaptive table
saveRDS(adaptive_table, 
        file=paste0(data_path, "adaptive_table.rds"))


# =========================================================
# download shapefile of Mozambique boundary
temp_dir <- "/tmp/Moz/"
dir.create(temp_dir)
download.file(
  url="https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_MOZ_shp.zip",
  destfile=paste0(temp_dir, "Moz_map.zip"))

# extract the shapefile
unzip(zipfile=paste0(temp_dir, "Moz_map.zip"), 
      exdir=temp_dir)

# read the shapefile
library("sf")
Moz_map <-
  read_sf(temp_dir, layer="gadm41_MOZ_3")

Moz_map %>% 
  ggplot() + geom_sf() +
  geom_sf(data=Moz_map %>%
            filter(NAME_1 %in% 
                     c("Maputo", "Nassa", "Zambezia")
                   ), fill="grey50") +
  geom_sf(data=Moz_map %>%
            filter(NAME_2 %in% 
                     c("Moamba", "Gurue", "Morrumbala", "Cuamba")
            ), fill="blue") +
  geom_sf(data=Moz_map %>%
            filter(NAME_3 %in% 
                     c("Mandimba-Sede")
                   ), fill="blue") +
  geom_point(data=adaptive_table %>%
               distinct(Longitude, Latitude),
             aes(x=Longitude, y=Latitude),
             col="red", shape=2, size=0.5)

# check coordinates of sampling sites
adaptive_table %>%
  group_by(Province, District) %>%
  summarise(lat_median=median(Latitude),
            lon_median=median(Longitude),
            lat_min=min(Latitude),
            lon_min=min(Longitude),
            lat_max=max(Latitude),
            lon_max=max(Longitude))

# =========================================================
# construct a set of regularly spaced coordinate points, 
# forming a regular grid, as reference locations for prediction 
# =========================================================

# generate coordinates of predictive grid points
adaptive_grid <- 
  bind_rows(
    # grid points for the Moamba district
    expand_grid(Longitude=seq(32, 32.35, length=20),
                Latitude=seq(-25.75, -25.4, length=20)) %>%
      mutate(Province="Maputo", District="Moamba")
    ,
    # grid points for the Gurue district
    expand_grid(Longitude=seq(36.55, 36.95, length=20),
                Latitude=seq(-15.5, -15.1, length=20)) %>%
      mutate(Province="Zambezia", District="Gurue")
    ,
    # grid points for the Morrumbala district
    expand_grid(Longitude=seq(35.4, 35.7, length=20),
                Latitude=seq(-17.5, -17.2, length=20)) %>%
      mutate(Province="Zambezia", District="Morrumbala"),
    # grid points for the Cuamba district
    expand_grid(Longitude=seq(36.4, 36.7, length=20),
                Latitude=seq(-14.9, -14.6, length=20)) %>%
      mutate(Province="Niassa", District="Cuamba"),
    # grid points for the Mandimba district
    expand_grid(Longitude=seq(35.6, 35.9, length=20),
                Latitude=seq(-14.4, -14.1, length=20)) %>%
      mutate(Province="Niassa", District="Mandimba")
  ) %>%
  relocate(Longitude, Latitude, .after=District)

Moz_map %>% ggplot() +
  geom_sf() +
  geom_point(data=adaptive_grid, 
             aes(x=Longitude, y=Latitude), 
             size=0.01, alpha=0.5, shape=20, color="red") +
  geom_point(data=adaptive_table,
             aes(x=Longitude, y=Latitude), 
             size=0.01, color="blue")

dev.copy2pdf(file=paste0(temp_dir, "grid.pdf"), 
             width=7, height=11)

# add time (Year, Month) to predictive grid data
# by sampling sampling times

adaptive_grid <- 
  adaptive_grid %>%
  expand_grid(adaptive_table %>% 
                as_tibble() %>% 
                distinct(Year, Month))

if (FALSE)
{
  # option 2: sample time (Year, Month)
  adaptive_grid <-
    bind_rows(
      adaptive_grid %>% 
        filter(District == "Moamba") %>%
        bind_cols(
          adaptive_table %>%
            as_tibble() %>%
            filter(District == "Moamba") %>%
            select(Year, Month) %>%
            slice_sample(n=400, replace=TRUE) 
        )
      ,
      adaptive_grid %>% 
        filter(District == "Gurue") %>%
        bind_cols(
          adaptive_table %>%
            as_tibble() %>%
            filter(District == "Gurue") %>%
            select(Year, Month) %>%
            slice_sample(n=400, replace=TRUE) 
        )
      ,
      adaptive_grid %>% 
        filter(District == "Morrumbala") %>%
        bind_cols(
          adaptive_table %>%
            as_tibble() %>%
            filter(District == "Morrumbala") %>%
            select(Year, Month) %>%
            slice_sample(n=400, replace=TRUE) 
        )
    )
}


# save adaptive grid
saveRDS(adaptive_grid, 
        file=paste0(data_path, "adaptive_grid.rds"))

# =========================================================

















# =========================================================
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
