
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
  group_by(houseID, longitude, latitude, year, month, method) %>%
  summarise(across(An.gambiaes.l.:An.tenebrosus, sum, .names="{.col}")) %>%
  left_join(a %>% 
              group_by(houseID, longitude, latitude, year, month) %>% 
              count(year, month)) %>%
  rename(efforts=n) %>%
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
colnames(v) <- c("land_cover", "pop_density", "elevation")
a <- a %>% bind_cols(v) %>%
  mutate(land_cover=as.factor(land_cover))


b <- readRDS("~/Downloads/adaptive_table_coavrs.rds") %>% 
  filter(District == "Morrumbala") %>% 
  mutate(land_cover=unlist(land_cover),
         elevation=unlist(elevation))

b %>% group_by(`Collection method`) %>% summarise(sum(`An. gambiae s.l`))
b %>% group_by(`Collection method`) %>% summarise(sum(`An. funestus s.l`))
b %>% group_by(`Collection method`) %>% summarise(sum(`An. maculipalpis`))
b %>% group_by(`Collection method`) %>% count(`House ID`) %>% print(n=50)
b %>% group_by(`Collection method`) %>% count(Year, Month)


bind_rows(
  b %>% 
    filter(`Collection method` == "CDC") %>%
    summarise(across(`An. gambiae s.l`:`An. ziemanni`, mean, 
                     .names="{.col}")) %>%
    pivot_longer(`An. gambiae s.l`:`An. ziemanni`, 
                 names_to="species", values_to="counts") %>%
    mutate(sampling="ESAF"),
  a %>% 
    filter(method == "CDC") %>%
    summarise(across(An.gambiaes.l.:An.tenebrosus, 
                     \(x) weighted.mean(x, w=efforts),
                     .names="{.col}")) %>%
    pivot_longer(An.gambiaes.l.:An.tenebrosus,
                 names_to="species", values_to="counts") %>%
    mutate(sampling="routine")
  ) %>%
  mutate(species=str_replace_all(species, " ", ""),
         species=case_match(species,
                            "An.Desconhecido" ~ "An.desconhecido",
                            "An.funestuss.l" ~ "An.funestuss.l.",
                            "An.gambiaes.l" ~ "An.gambiaes.l.",
                            .default=species)) %>%
  group_by(species, sampling) %>% summarise(counts=mean(counts)) %>%
  ungroup() %>%
  #pivot_wider(names_from=sampling, values_from=counts) %>% 
  #arrange(desc(ESAF), desc(routine))
  #mutate(species=as.factor(species), counts=as.integer(counts)) %>%
  arrange(sampling, desc(counts)) %>%
  ggplot(aes(x=species, y=counts, fill=sampling)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  coord_flip() + 
  labs(y="avergae number of collected mosquitoes")
  theme_light()


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
mesh <- inla.mesh.2d(cbind(a$longitude, a$latitude))
mesh <- inla.mesh.2d(cbind(b$Longitude, b$Latitude))

fm <- y ~  elevation + 
  f(month) + 
  f(mesh$idx$loc, model=inla.spde2.matern(mesh=mesh, alpha=2)) +
  lai_hv_mean_0 + lai_hv_min_0 + lai_hv_max_0 + lai_hv_sd_0 + 
  lai_lv_mean_0 + lai_lv_min_0 + lai_lv_max_0 + lai_lv_sd_0 + 
  skt_mean_0 + skt_min_0 + skt_max_0 + skt_sd_0 + 
  tp_mean_0 + tp_min_0 + tp_max_0 + tp_sd_0 + 
  swvl1_mean_0 + swvl1_min_0 + swvl1_max_0 + swvl1_sd_0 +
  lai_hv_mean_1 + lai_hv_min_1 + lai_hv_max_1 + lai_hv_sd_1 + 
  lai_lv_mean_1 + lai_lv_min_1 + lai_lv_max_1 + lai_lv_sd_1 + 
  skt_mean_1 + skt_min_1 + skt_max_1 + skt_sd_1 +
  tp_mean_1 + tp_min_1 + tp_max_1 + tp_sd_1 + 
  swvl1_mean_1 + swvl1_min_1 + swvl1_max_1 + swvl1_sd_1 + 
  lai_hv_mean_2 + lai_hv_min_2 + lai_hv_max_2 + lai_hv_sd_2 + 
  lai_lv_mean_2 + lai_lv_min_2 + lai_lv_max_2 + lai_lv_sd_2 +
  skt_mean_2 + skt_min_2 + skt_max_2 + skt_sd_2 +
  tp_mean_2 + tp_min_2 + tp_max_2 + tp_sd_2 +
  swvl1_mean_2 + swvl1_min_2 + swvl1_max_2 + swvl1_sd_2

fit1 <- inla(fm,  
             data=a %>% mutate(y=An.gambiaes.l.),
             family="zeroinflatednbinomial2")
fit2 <- inla(fm,  
             data=b %>% rename(y=`An. gambiae s.l`,
                               month=Month),
             family="zeroinflatednbinomial2")

fit1$summary.fixed
fit1$summary.hyperpar
fit2$summary.fixed
fit2$summary.hyperpar

cvfun <- function(dat)
{
  out <- matrix(NA, ncol=2, nrow=nrow(dat))
  dat <- dat %>% 
    mutate(tidx=as.numeric(factor(paste(year, month))))
  for (i in 1:nrow(dat))
  {
    mesh <- inla.mesh.2d(cbind(dat$longitude, dat$latitude))
    fm <- y ~  elevation + #f(idx) + 
      f(tidx) + 
      f(mesh$idx$loc, model=inla.spde2.matern(mesh=mesh, alpha=2)) +
      lai_hv_mean_0 + lai_hv_min_0 + lai_hv_max_0 + lai_hv_sd_0 + 
      lai_lv_mean_0 + lai_lv_min_0 + lai_lv_max_0 + lai_lv_sd_0 + 
      skt_mean_0 + skt_min_0 + skt_max_0 + skt_sd_0 + 
      tp_mean_0 + tp_min_0 + tp_max_0 + tp_sd_0 + 
      swvl1_mean_0 + swvl1_min_0 + swvl1_max_0 + swvl1_sd_0 +
      lai_hv_mean_1 + lai_hv_min_1 + lai_hv_max_1 + lai_hv_sd_1 + 
      lai_lv_mean_1 + lai_lv_min_1 + lai_lv_max_1 + lai_lv_sd_1 + 
      skt_mean_1 + skt_min_1 + skt_max_1 + skt_sd_1 +
      tp_mean_1 + tp_min_1 + tp_max_1 + tp_sd_1 + 
      swvl1_mean_1 + swvl1_min_1 + swvl1_max_1 + swvl1_sd_1 + 
      lai_hv_mean_2 + lai_hv_min_2 + lai_hv_max_2 + lai_hv_sd_2 + 
      lai_lv_mean_2 + lai_lv_min_2 + lai_lv_max_2 + lai_lv_sd_2 +
      skt_mean_2 + skt_min_2 + skt_max_2 + skt_sd_2 +
      tp_mean_2 + tp_min_2 + tp_max_2 + tp_sd_2 +
      swvl1_mean_2 + swvl1_min_2 + swvl1_max_2 + swvl1_sd_2
    dat$y[i] <- NA
    fiti <- inla(fm, data=dat, family="gaussian",
                 #control.predictor=list(link=1),
                 #control.compute=list(config=TRUE, dic=TRUE, waic=TRUE)
                 )
    #c(dic=fiti$dic$dic, waic=fiti$waic$waic)
    out[i, ] <- unlist(fiti$summary.fitted.values[i, c(1, 2)])
  }
  return(out)
}

dat1 <- a %>% 
  mutate(y=An.gambiaes.l. + An.outros + An.funestuss.l. + 
           An.pretoriensis + An.maculipalpis + An.rufipes + 
           An.coustani + An.tenebrosus)
cv1 <- cvfun(dat1)

mean(((cv1[, 1] - dat1$y) / (dat1$y + 1))^2)
mean(cv1[, 2])

(mean(((cv1[, 1] - dat1$y) / cv1[, 2])^2))

dat2 <- b %>%
  rename(longitude=Longitude, latitude=Latitude,
         year=Year, month=Month) %>%
  mutate(y=`An. gambiae s.l` + `An. funestus s.l` + 
           `An. tenebrosus` + `An. maculipalpis` +  
           `An. coustani` + `An. desconhecido` +
           `An. rufipes` + `An. pretoriensis` +  
           `An. Desconhecido` + `An. squamosus` +
           `An. ziemanni`)
cv2 <- cvfun(dat2)

mean(((cv2[, 1] - dat2$y) / (dat2$y + 1))^2)
mean(cv2[, 2])
(mean(((cv2[, 1] - dat2$y) / cv2[, 2])^2))



tibble(sampling=c("ESAF", "routine"),
       `Mean square standardized prediction error`=c(58.4, 3470.1)) %>%
  ggplot(aes(x= `Mean square standardized prediction error`, y=sampling)) +
  geom_bar(stat="identity")

