
library("readxl")
library("tidyverse")
library("sf")
library("mapview")

sdata <- list()
sdata[[1]] <- read_excel("~/Downloads/Zambezia_Gurue _Morrumbala 2022Vw.xlsx",
                    sheet="Morrumbala Anopheles CDC")
sdata[[2]] <- read_excel("~/Downloads/Zambezia_Gurue _Morrumbala 2022Vw.xlsx",
                     sheet="Morrumbala Anopheles")
  

sdata <- bind_rows(
  sdata[[1]] %>%
    select(`CASA #`, Longetud, Latitud, DATA, `ID MORFOLOGICA`) %>%
    rename(houseID=`CASA #`, longitude=Longetud, latitude=Latitud, 
           date=DATA, species=`ID MORFOLOGICA`) %>%
    mutate(method="CDC"),
  sdata[[2]] %>%
    select(`CASA Nᵒ`, Longetude, Latitude, DATA, `IDENT. MORFOLOGICA`) %>%
    rename(houseID=`CASA Nᵒ`, longitude=Longetude, latitude=Latitude, 
           date=DATA, species=`IDENT. MORFOLOGICA`)%>%
    mutate(method="PK")
)

a <- sdata %>% 
  group_by(houseID, longitude, latitude, date, method) %>% 
  count(species) %>%
  mutate(species=str_replace_all(species, " ", "")) %>%
  pivot_wider(names_from=species, values_from=n) %>%
  select(-`0`) %>% replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(date=strptime(date, "%d/%m/%y"),
         year=substr(date, 1, 4), 
         month=month.name[as.numeric(substr(date, 6, 7))])


coordtrans <- function(v, sep="◦")
{
  unlist(lapply(strsplit(v, sep), 
                function(x){ 
                  y <- as.numeric(x)
                  y[1] + y[2] / 60
                  }))
}

a <- a %>%
  mutate(longitude=coordtrans(longitude),
         latitude=coordtrans(latitude))


a %>% group_by(method) %>% summarise(sum(An.gambiaes.l.))
a %>% group_by(method) %>% summarise(sum(An.funestuss.l.))
a %>% group_by(method) %>% summarise(sum(An.maculipalpis))
a %>% group_by(method) %>% count(houseID)
a %>% group_by(method) %>% count(year, month)

a <- a %>% 
  group_by(houseID, longitude, latitude, year, month) %>%
  summarise(across(An.gambiaes.l.:An.tenebrosus, sum, .names="{.col}")) %>%
  ungroup()

source("https://github.com/jalilian/CEASE/raw/main/Ethiopia/codes/get_Copernicus_climate_data.R")
# user credentials for ECMWF data access
user <- "****************"
cds.key <- "********************************"

v <- list()
v[[1]] <- get_cds(user, cds.key, year=2022, month=7:12, time="12:00",
                  what=a %>% select(longitude, latitude) %>% 
                    distinct(longitude, latitude) %>% as.matrix())
v[[2]] <- get_cds(user, cds.key, year=2023, month=1:12, time="12:00",
                  what=a %>% select(longitude, latitude) %>% 
                    distinct(longitude, latitude) %>% as.matrix())

v <- bind_rows(v)
v <- v %>% 
  mutate(year=substr(time, 1, 4), 
         month=month.name[as.numeric(substr(time, 6, 7))]) %>%
  group_by(longitude, latitude, year, month) %>%
  summarise(across(u10:ssr, list(mean=mean, sd=sd, min=min, max=max), 
                   .names = "{.col}_{.fn}")) %>%
  ungroup()

vnames <- colnames(v)[-(1:4)]

for (j in 0:3)
{
  a <- a %>% mutate(m=match(month, month.name)) %>%
    mutate(ii=month.name[ifelse(m - j <= 0, 12 + m - j, m - j)]) %>%
    left_join(v %>% mutate(ii=month), 
              by=join_by(longitude, latitude, year, month)) %>%
    select(-c(ii.x, ii.y))
  colnames(a)[seq.int(to=ncol(a), length.out=length(vnames))] <- 
    paste(vnames, j, sep="_")
}

colnames(a)

source("https://github.com/jalilian/CEASE/raw/main/Ethiopia/codes/get_land_covars_africa.R")

v <- get_covars(a %>% select(longitude, latitude) %>% as.matrix(), 
                path="~/Downloads/Africa_covars/")

b <- readRDS("~/Downloads/adaptive_table_coavrs.rds") %>% 
  filter(District == "Morrumbala")


b %>% group_by(`Collection method`) %>% summarise(sum(`An. gambiae s.l`))
b %>% group_by(`Collection method`) %>% summarise(sum(`An. funestus s.l`))
b %>% group_by(`Collection method`) %>% summarise(sum(`An. maculipalpis`))
b %>% group_by(`Collection method`) %>% count(`House ID`) %>% print(n=50)
b %>% group_by(`Collection method`) %>% count(Year, Month)

# map
bind_rows(
  b %>%
    rename(longitude=Longitude, latitude=Latitude) %>%
    mutate(site="ESAF", latitude=-latitude) %>%
    select(longitude, latitude, site),
  a %>%
    mutate(site="sentinel") %>%
    select(longitude, latitude, site)
  ) %>% #write_csv(., file="~/Downloads/MurSites.csv")
  st_as_sf(., coords=c("longitude", "latitude")) %>%
  ggplot(aes(colour=site)) + 
  geom_sf() 


library("INLA")

fit1 <- inla(y ~  lai_hv_mean_0, 
             data=b %>% mutate(y=`An. gambiae s.l`),
             family="nbinomial")
fit1$summary.fixed
fit1$summary.hyperpar
