
library("readxl")
library("tidyverse")
library("sf")

# ===============================================
# GPS coordinates of sampling locations in both programs

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")

gps <- read_excel(
  "~/Downloads/Ghana/GHA_GPS EB review 12_6_24.xlsx",
  sheet="gha_gps"
  ) %>%
  mutate(method=case_match(method,
                           "HLS" ~ "HLC",
                           .default=method),
         `house ID`=str_replace(`house ID`, 
                                 "_(.*?)_", 
                                 paste0("_", method, "_")))

aa <- convert_coords(gps$LONGITUDE, gps$LATITUDE)

gps <- gps %>%
  mutate(LONGITUDE=aa[, 1],
         LATITUDE=aa[, 2]) %>%
  distinct()

write_csv(gps, file="~/Downloads/Ghana/gha_gps.csv")

egps <- gps %>% filter(program == "EASF")
sgps <- gps %>% filter(program == "Routine")

st_as_sf(gps, coords=c( "LONGITUDE", "LATITUDE"),
         crs=4326) %>%
  ggplot() +
  geom_sf(aes(colour=program, shape=method)) +
  theme_light() +
  theme(legend.position="bottom")

library("mapview")
mp <- st_as_sf(gps, coords=c( "LONGITUDE", "LATITUDE"),
               crs=4326) %>%
  mutate(met=paste(program, method, sep=":")) %>%
  mapview(zcol="met", label="house ID")
mapshot2(mp, url="~/Downloads/Ghana/map.html")

# =========================================================
# EASF data
edata <- list()
edata[[1]] <- 
  read_excel("~/Downloads/Ghana/Ghana EASF_HLC data (Jul-Dec23).xlsx",
             sheet="Sheet 1 - events1",
             col_types="text")
edata[[2]] <- 
  read_excel("~/Downloads/Ghana/Ghana EASF_PSC data (Jul-Dec23).xlsx",
             sheet="Sheet 1 - events", skip=1,
             col_types="text") %>%
  rename("Program stage name"="enrollment")
edata[[3]] <- 
  read_excel("~/Downloads/Ghana/HLC_data_EASF_Jan_Jul.xlsx",
             sheet="Sheet 1 - events_hlc_csv",
             col_types="text") %>%
  rename("Program stage name" = "programstagename",
         "Org unit name" = "orgunitname") %>%
  select(-attributeOptionCombo)
edata[[4]] <- 
  read_excel("~/Downloads/Ghana/CID_data_EASF_Jan_Jul.xlsx",
             sheet="Sheet 1 - events_cid_csv",
             col_types="text") %>%
  select(-attributeOptionCombo)

edata <- bind_rows(edata)

prepfun <- function(dat)
{
  dat %>%
    # convert eventDate to Date type and extract year and month
    mutate(eventDate = as.Date(eventDate),
           year=substr(eventDate, 1, 4), 
           month=month.name[as.numeric(substr(eventDate, 6, 7))]) %>%
    group_by(event) %>%
    # extract 'houseID'
    mutate(houseID=
             ifelse(`Datat element name` == "HH Number", 
                    value, 
                    NA)) %>%
    mutate(houseID=toupper(houseID),
           houseID=ifelse(sum(!is.na(houseID)) > 0,
                          houseID[!is.na(houseID)],
                          NA),
           houseID=str_remove_all(houseID, " "),
           houseID=str_replace_all(houseID, "-|=|/", "_"),
           houseID=gsub("_+", "_", houseID)) %>%
    mutate(houseID2=paste(
      toupper(substr(`Org unit name`, 1, 3)),
      `Program stage name`,
      str_pad(as.numeric(gsub("[^0-9]", "", 
                              sub("^(([^_]*_){2}[^_]*)_.*", 
                                  "\\1", houseID))), 
              width=2, pad="0"), 
      sep="_")) %>%
    # Collection place
    mutate(
      `Collection place` = case_when(
        `Program name` == "CID" ~ "Indoor",
        `Program name` == "HLC" & 
          str_detect(`Datat element name`, "- In| Indoor") ~ "Indoor",
        `Program name` == "HLC" & 
          str_detect(`Datat element name`, "- Out| Outdoor") ~ "Outdoor",
        .default=NA
        )
    ) %>%
    # Add 'An. gambiae' variable with conditions
    mutate(
      `An. gambiae` = case_when(
        `Program name` == "CID" & 
          `Datat element name` %in% 
          c("Total Collected: Anopheles Female", 
            "Total Collected: Anopheles Male",
            "An. gambiae s.l.: Collected") ~ value,
        `Program name` == "HLC" & 
          str_detect(`Datat element name`, 
                     "An. gambiae s.l.- In|An. gambiae s.l.- Out") ~ value,
      #    `Datat element name` %in%
      #    c("An. gambiae s.l. caught") ~ value,
        .default=NA
      )
    )
}

edata <- prepfun(edata)

edata <- edata %>%
  mutate(houseID2=str_replace_all(houseID2, "CID", "PSC"),
         houseID2=case_match(houseID2, 
                             "ANT_PSC_00" ~ "ANT_PSC_15",
                             "DOB_PSC_NA" ~ "DOB_PSC_05",
                             "DUT_HLC_NA" ~ "DUT_HLC_02",
                             .default=houseID2))

names(table(edata$houseID2[!(edata$houseID2 %in% egps$`house ID`)]))
egps$`house ID`[!(egps$`house ID` %in% edata$houseID2)]


edata <- edata %>% 
  rename(`house ID`=houseID2) %>%
  left_join(
    egps %>% select(`house ID`, LONGITUDE, LATITUDE),
    by=join_by(`house ID`)
  )

edata %>% 
  count(`Program name`, eventDate, houseID, 
        longitude, latitude, geometry) %>%
  arrange(eventDate, houseID) %>%
  select(-n) %>%
  write_csv(file="~/Desktop/gha_easf_sites.csv")

# sentinel site data
sdata <- list()
sdata[[1]] <- 
  read_excel("~/Downloads/Ghana/Control sites HLC.xlsx",
             sheet="Sheet 1 - events 2",
             col_types="text") %>%
  rename("Org unit name" = "orgunitname",
         "Program stage name" = "programstagename")
sdata[[2]] <- 
  read_excel("~/Downloads/Ghana/Control sites CID.xlsx",
             sheet="Sheet 1 - events", skip=1,
             col_types="text") %>%
  rename("Program name" = "programname")
sdata[[3]] <- 
  read_excel("~/Downloads/Ghana/Control sites HLC_Jan-Jul.xlsx",
             sheet="Sheet 1 - events_hlc_csv",
             col_types="text") %>%
  rename("Org unit name" = "orgunitname",
         "Program stage name" = "programstagename")
sdata[[4]] <- 
  read_excel("~/Downloads/Ghana/Control sites CID_Jan-Jul.xlsx",
             sheet="Sheet 1 - events_cid_csv",
             col_types="text")


sdata <- bind_rows(sdata) 

sdata <- prepfun(sdata)

sdata <- sdata %>%
  mutate(houseID2=str_replace_all(houseID2, "CID", "PSC"),
         houseID2=str_replace_all(houseID2, "FOD", "HEL"),
         houseID2=str_replace_all(houseID2, "OLD", "OTET"),
         houseID2=str_replace_all(houseID2, "WIA", "SEFW"))

names(table(sdata$houseID2[!(sdata$houseID2 %in% sgps$`house ID`)]))
sgps$`house ID`[!(sgps$`house ID` %in% sdata$houseID2)]

sdata <- sdata %>% 
  rename(`house ID`=houseID2) %>%
  left_join(
    sgps %>% select(`house ID`, LONGITUDE, LATITUDE),
    by=join_by(`house ID`)
  )

sdata %>% 
  count(`Program name`, eventDate, houseID, 
        longitude, latitude, geometry) %>%
  arrange(eventDate, houseID) %>%
  select(-n) %>%
  write_csv(file="~/Desktop/gha_control_sites.csv")

# =========================================================
# combine all data

adata <- bind_rows(
  edata %>%
    mutate(`An. gambiae`=as.numeric(replace_na(`An. gambiae`, "0"))) %>%
    group_by(event, eventDate, `Program name`, `Org unit name`, `house ID`) %>%
    summarise(`An. gambiae`=max(`An. gambiae`)) %>%
    mutate(program="EASF"),
  sdata %>%
    mutate(`An. gambiae`=as.numeric(replace_na(`An. gambiae`, "0"))) %>%
    group_by(event, eventDate, `Program name`, `Org unit name`, `house ID`) %>%
    summarise(`An. gambiae`=max(`An. gambiae`)) %>%
    mutate(program="Routine")
  )  %>%
  rename(method=`Program name`) %>%
  ungroup() %>%
  mutate(method=case_match(method,
                           "CID" ~ "PSC",
                           .default=method)) %>%
  left_join(gps,
            by=join_by(`house ID`, method, program)) %>%
  na.omit()

table(sub("_.*", "", gps$`house ID`))
table(sub("_.*", "", adata$`house ID`))

adata <- adata  %>% 
  select(-event) %>% 
  group_by(program, method, eventDate, 
           `Org unit name`, `house ID`, LONGITUDE, LATITUDE) %>%
  summarise(`An. gambiae`=sum(`An. gambiae`), .groups="drop") %>%
  mutate(year=year(eventDate), month=month(eventDate))

adata %>% 
  write_csv(file="~/Desktop/gha_all_sites.csv")

adata <- adata %>% 
  group_by(program, method, year, month, 
           `Org unit name`, `house ID`, LONGITUDE, LATITUDE) %>%
  summarise(`An. gambiae`=sum(`An. gambiae`), .groups="drop") %>%
  left_join(adata %>% 
              count(program, method, year, month, 
                    `Org unit name`, `house ID`, 
                    LONGITUDE, LATITUDE) %>%
              rename("effort"="n"),
            by = join_by(program, method, year, month, 
                         `Org unit name`, `house ID`, LONGITUDE, LATITUDE)
  )

saveRDS(adata, file="adata.rds")
# =========================================================
adata <- readRDS("adata.rds")

# land cover, population density and elevation data
source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_land_covars_africa.R")
covars <- get_covars(adata %>% 
                       select(LONGITUDE, LATITUDE) %>% 
                       as.matrix(),
                     path="~/Downloads/Africa_covars/")
names(covars) <- c("land_cover", "pop_density", "elevation")
covars <- covars %>%
  mutate(land_cover=c("Tree cover areas",
                      "Shrubs cover areas", 
                      "Grassland", 
                      "Cropland",
                      "Vegetation aquatic or regularly flooded",
                      "Lichens Mosses / Sparse vegetation",
                      "Bare areas",
                      "Built-up areas",
                      "Snow and/or Ice",
                      "Open Water")[land_cover])

adata <- bind_cols(adata, covars)

source("https://raw.githubusercontent.com/jalilian/CEASE/refs/heads/main/Ethiopia/codes/get_modis.R")

aa <- get_modis(what=adata %>% 
                  distinct(LONGITUDE, LATITUDE) %>% 
                  as.matrix(),
                datetime="2023-07-01/2024-07-31", 
                aggregate=TRUE, fill_in=TRUE)

saveRDS(aa, "aa.rds")
aa <- readRDS("aa.rds")

aa$`modis-16A3GF-061` <- aa$`modis-16A3GF-061` %>%
  select(-date) %>%
  slice(rep(1:n(), 14)) %>%
  mutate(date=aa$`modis-11A2-061`$date) %>%
  relocate(date, .before=long)

lapply(aa, function(a) range(a$date))

aa <- reduce(aa, left_join, by=join_by("date", "long", "lat")) %>% 
  mutate(year=as.numeric(sub("-.*", "", date)),
         month=as.numeric(sub(".*?-", "", date))) %>%
  select(-date) %>%
  relocate(year, month, .before=long)

adata <- adata %>%
  rename("long"="LONGITUDE", "lat"="LATITUDE") %>%
  left_join(aa, 
            by=join_by(year, month, long, lat)) %>%
  rename_with(~ gsub("__focal", "", .))

colnames(adata)

saveRDS(adata, file="adata.rds")
# =========================================================
adata <- readRDS("adata.rds")
# =========================================================
# field data analysis

adata %>% 
  count(method, program) %>%
  left_join(adata %>% 
              group_by(method, program) %>% 
              summarise(total=sum(`An. gambiae`))) %>%
  mutate(mean=round(total/n, 2)) %>%
  ggplot(aes(x=mean, y=method, fill=program)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Average number of collected mosquitoes",
       y="Collection method")+
  theme_light()

t.test(adata %>% 
         filter(method == "HLC", program == "EASF") %>%
         pull(`An. gambiae`),
       adata %>% 
         filter(method == "HLC", program == "Routine") %>%
         pull(`An. gambiae`))

t.test(adata %>% 
         filter(method == "PSC", program == "EASF") %>%
         pull(`An. gambiae`),
       adata %>% 
         filter(method == "PSC", program == "Routine") %>%
         pull(`An. gambiae`))


library("INLA")
library("fmesher")
library("sf")


dat <- adata %>%
  rename("y" = "An. gambiae",
         "EVI_16_days"="250m_16_days_EVI",
         "NDVI_16_days"="250m_16_days_NDVI",
         "District"="Org unit name") %>% 
  filter(method == "HLC") %>%
  mutate(tidx=as.numeric(factor(ymd(paste(year, month, 1, sep="-")))),
         sidx=as.numeric(factor(paste(long, lat, sep="-")))) %>%
  mutate(region=case_when(long > 0 ~ "A",
                          long > -1 ~ "B",
                          long < -2 ~ "C"))

dat %>% count(tidx, year, month)
dat %>% count(sidx, long, lat) %>% print(n=800)

# standardise covariates
dat <- dat %>%
  mutate(across(pop_density:FireMask, ~ scale(.x))) 
# mutate(across(contains("_"), ~ scales::rescale(.x, to=c(-1, 1)))) %>%
#  mutate(elevation=scales::rescale(elevation, to=c(-1, 1)))

fitfun <- function(dat0, max.edge=0.25)
{
  mesh <- fm_mesh_2d_inla(loc=dat0[, c("long", "lat")], 
                          max.edge=max.edge)
  #spde <- inla.spde2.matern(mesh, alpha=2)
  spde <- inla.spde2.pcmatern(
    mesh, alpha=2,
    # lower tail quantile and probability for the range 
    prior.range = c(0.01, 0.025),
    # upper tail quantile and probability for the standard deviation
    prior.sigma = c(10, 0.025)
  )
  
  fm <- y ~  elevation + pop_density +
    f(tidx, model="rw1", constr=TRUE) + 
    f(mesh$idx$loc, model=spde) +
    #f(sidx, model="iid", constr=TRUE) +
    land_cover + pop_density + elevation +
    LST_Day_1km + LST_Night_1km + Gpp_500m +
    PsnNet_500m + sur_refl_b07 + 
    ET_500m + LE_500m + PET_500m + Lai_500m +
    Fpar_500m + 
    EVI_16_days + NDVI_16_days +
    FireMask
  inla(fm, data=dat0, family="nbinomial",
       #control.family(control.link=list(model="log")),
       #control.compute=list(return.marginals.predictor=TRUE),
       control.predictor=list(compute=TRUE, link=1), 
       #control.inla=list(strategy="laplace", npoints=21),
       #control.inla=list(fast=FALSE, strategy="laplace", dz=0.25, h=1e-5)
       control.compute=list(config=TRUE, dic=TRUE, waic=TRUE),
       silent=1L, num.threads=1)
}

fit1 <- fitfun(dat %>% filter(program == "Routine"))
round(fit1$summary.fixed[, c(1, 3, 5)], 2)
fit1$summary.hyperpar

fit2 <- fitfun(dat %>% filter(program == "EASF"))
round(fit2$summary.fixed[, c(1, 3, 5)], 2)
fit2$summary.hyperpar

cvfun <- function(dat, newdat=NULL, max.edge=0.25, mc.cores=4)
{
  if (is.null(newdat))
  {
    idx <- 1:nrow(dat)
  } else{
    idx <- nrow(dat) + 1:nrow(newdat)
    dat <- bind_rows(dat, newdat)
  }
  fitifun <- function(i)
  {
    dati <- dat
    dati$y[i] <- NA
    fiti <- fitfun(dati, max.edge=max.edge)
    unlist(fiti$summary.fitted.values[i, c(1, 2)])
  }
  out <- parallel::mclapply(idx, fitifun, mc.cores=mc.cores)
  out <- matrix(unlist(out), ncol=2, byrow=TRUE)
  out <- cbind(out, dat$y[idx])
  return(out)
}

cv1 <- cvfun(dat %>% filter(program == "Routine"))
mean(((cv1[, 1] - dat %>% filter(program == "Routine") %>% pull(y)) / 
        (dat %>% filter(program == "Routine") %>% pull(y) + 1))^2)
sqrt(mean(((cv1[, 1] - dat %>% filter(program == "Routine") %>% pull(y)) / 
             cv1[, 2])^2))


nsim <- 499 
mc.cores <- 6
cv2s <- cv21s <- cv12s <- matrix(NA, nrow=nsim, ncol=2)
for (j in 1:nsim)
{
  routine_dat <- dat %>%
    filter(program == "Routine")
  
  nested_dat <- dat %>%
    filter(program == "EASF")%>% 
    group_by(year, month, region) %>%  
    nest() %>%            
    ungroup() %>% 
    left_join(
      routine_dat %>% 
        group_by(year, month, region) %>% 
        count(year, month, region),
      by = join_by(year, month, region)
    ) %>%
    na.omit()
  
  samp_dat <- nested_dat %>%
    mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>%
    select(-data) %>%
    unnest(samp)
  
  cv2 <- cvfun(samp_dat, newdat=NULL, max.edge=NULL, mc.cores=mc.cores)
  cv21 <- cvfun(samp_dat, newdat=routine_dat, max.edge=NULL, mc.cores=mc.cores)
  cv12 <- cvfun(routine_dat, newdat=samp_dat, max.edge=NULL, mc.cores=mc.cores)
  
  if (is.numeric(cv2) & is.numeric(cv21) & is.numeric(cv12))
  {
    cv2s[j, 1] <- mean(((cv2[, 3] - cv2[, 1]) / (1 + cv2[, 3]))^2)
    cv2s[j, 2] <- mean(((cv2[, 3] - cv2[, 1]) / cv2[, 2])^2)
    cv21s[j, 1] <- mean(((cv21[, 3] - cv21[, 1]) / (1 + cv21[, 3]))^2)
    cv21s[j, 2] <- mean(((cv21[, 3] - cv21[, 1]) / cv21[, 2])^2)
    cv12s[j, 1] <- mean(((cv12[, 3] - cv12[, 1]) / (1 + cv12[, 3]))^2)
    cv12s[j, 2] <- mean(((cv12[, 3] - cv12[, 1]) / cv12[, 2])^2)
  }
  cat("\nj: ", j, "\t", cv2s[j, ], "\t", cv21s[j, ], "\t", cv12s[j, ], "\n")
}

mean(sqrt(cv2s[, 2]), na.rm=TRUE)
quantile(sqrt(cv2s[, 2]), c(0.025, 0.975), na.rm=TRUE)


mean(sqrt(cv21s[, 2]), na.rm=TRUE)
quantile(sqrt(cv21s[, 2]), c(0.025, 0.975), na.rm=TRUE)

mean(sqrt(cv12s[, 2]), na.rm=TRUE)
quantile(sqrt(cv12s[, 2]), c(0.025, 0.975), na.rm=TRUE)


mean(sqrt(cv21s[, 2]) <= mean(sqrt(cv12s[, 2])))
mean(sqrt(cv12s[, 2]) <= mean(sqrt(cv21s[, 2])))

# =========================================================
# molecular analysis of EASF mosquito samples 
# from July 2023 – July 2024
mdata <- read_excel(
  #"~/Downloads/Ghana/EASF Molecular Data July 2023 - July 2024.xlsx",
  "~/Downloads/Ghana/EASF Molecular Data July 2023 - July 2024_updated.xlsx",
  sheet="All collections"
) %>%
  slice(-1) %>%
  mutate(`FIELD ID`=gsub("-|/", "_", `FIELD ID`)) %>%
  mutate(`house ID`=paste(toupper(substr(COMMUNITY, 1, 3)),
                          `TYPE OF COLLECTION`, 
                          str_pad(as.numeric(gsub("[^0-9]", "", `FIELD ID`)), 
                                  width=2, pad=0),
                          sep="_"),
         `house ID`=str_replace_all(`house ID`, "TOG", "TOR")) %>%
  relocate(`house ID`, .before=`FIELD ID`)

mdata %>% count(`FIELD ID`, `house ID`) %>% print(n=800)

mdata %>% 
  filter(str_detect(`house ID`, "NA")) %>%
  count(`FIELD ID`, `house ID`) %>% print(n=800)

mdata %>% 
  count(COMMUNITY, `TYPE OF COLLECTION`, `FIELD ID`) %>%
  print(n=800)
mdata %>% count(COMMUNITY, sub("_.*", "", `FIELD ID`)) %>% print(n=500)

mdata %>%
  mutate(`house ID`=gsub("-|/", "_", `FIELD ID`)) %>%
  pull(`house ID`) %>% sub("_.*", "", .) %>% table(.)

mdata %>% 
  filter(`CSP DETECTION` == "Positive") %>% 
  left_join(egps, by = join_by(`house ID`)) %>%
  filter(!is.na(LONGITUDE)) %>%
  st_as_sf(., coords=c( "LONGITUDE", "LATITUDE"),
           crs=4326) %>%
  mapview()









fitfun2 <- function(dat0, max.edge=0.25)
{
  mesh <- fm_mesh_2d_inla(loc=dat0[, c("long", "lat")], 
                          max.edge=max.edge)
  #spde <- inla.spde2.matern(mesh, alpha=2)
  spde <- inla.spde2.pcmatern(
    mesh, alpha=2,
    # lower tail quantile and probability for the range 
    prior.range = c(0.01, 0.025),
    # upper tail quantile and probability for the standard deviation
    prior.sigma = c(10, 0.025)
  )
  
  fm <- y ~  elevation + pop_density +
   # f(tidx, model="iid", constr=TRUE) + 
    f(mesh$idx$loc, model=spde) +
    #f(sidx, model="iid", constr=TRUE) +
    land_cover + pop_density + elevation +
    LST_Day_1km + LST_Night_1km + Gpp_500m +
    PsnNet_500m + sur_refl_b07 + 
    ET_500m + LE_500m + PET_500m + Lai_500m +
    Fpar_500m + 
    EVI_16_days + NDVI_16_days +
    FireMask
  fit0 <- inla(fm, data=dat0, family="nbinomial",
       #control.family(control.link=list(model="log")),
       #control.compute=list(return.marginals.predictor=TRUE),
       control.predictor=list(compute=TRUE, link=1), 
       #control.inla=list(strategy="laplace", npoints=21),
       #control.inla=list(fast=FALSE, strategy="laplace", dz=0.25, h=1e-5)
       control.compute=list(config=TRUE, dic=TRUE, waic=TRUE),
       silent=1L, num.threads=1)
  v <- bind_cols(fit0$summary.fitted.values, dat0)
  mean(abs((v$mean - v$y2) / v$sd)[is.na(v$y)], na.rm=TRUE)
}

cvfun2 <- function(dat)
{
  selected_data <- dat %>% 
    filter(program == "EASF") %>%
    mutate(date=factor(as.Date(paste(year, month, "01", sep="-"))))
  
  hh <- NULL
  for (dd in levels(selected_data$date))
  {
    dat2 <- selected_data %>%
      filter(as.Date(date) <= as.Date(dd), 
             !(`house ID` %in% hh)) %>%
      group_by(`house ID`) %>%
      nest() %>%
      mutate(
        train_data = map(`house ID`, ~ selected_data %>% 
                            filter(`house ID` != .x, 
                                   as.Date(date) <= as.Date(dd), 
                                   !(`house ID` %in% hh))),
        data = map2(data, train_data, ~ bind_rows(mutate(.x, y2=y, y = NA), .y))
        )
    
    dat2 <- dat2 %>%
      mutate(v = map(data, fitfun2)) %>%
      summarise(v = map_dbl(v, mean, na.rm=TRUE)) 
    
    hh0 <- dat2 %>%
      filter(v < 0.01) %>%
      pull(`house ID`)
    hh <- c(hh, hh0)
    print(dd)
    print(hh)
  }
  
  dat2 <- routine_data %>%
    group_by(date, `house ID`) %>%
    nest() %>%
    mutate(
      train_data = map2(`house ID`, date, ~ routine_data %>% 
                         filter(`house ID` != .x, date <= .y)),
      data = map2(data, train_data, ~ bind_rows(mutate(.x, y2=y, y = NA), .y)))
  
  dat2 <- dat2 %>%
    mutate(fit = map(data, fitfun2))
  
  dat2 %>%
    mutate(
      v = map(data, ~ 
                  abs((.x$mean - .x$y2) / .x$sd)[is.na(.x$y)]
      ),
      n = map(data, ~ rep(1, sum(is.na(.x$y))))
    ) %>%
    summarise(v = map_dbl(v, mean, na.rm=TRUE),
              n = map_dbl(n, sum)) %>%
    unnest(cols = c(v, n)) %>%
    mutate(s=v < 0.1) %>%
    filter(s) 
  
  dat3 <- dat %>% 
    filter(program == "Routine") %>%
    mutate(y2=y)
  
  dat3$y[sample(nrow(dat3), 14)]  <- NA
  fit3 <- fitfun(dat3)
  fit3$summary.fitted.values[is.na(dat2$y), ] %>%
    mutate(cis=(`0.975quant` - `0.025quant`) / mean) %>%
    summarise(cis=sum(cis))
}





fitfun2 <- function(dat0, max.edge=0.25)
{
  fm <- y ~  elevation + pop_density +
    land_cover + pop_density + elevation +
    LST_Day_1km + LST_Night_1km + Gpp_500m +
    PsnNet_500m + sur_refl_b07 + 
    ET_500m + LE_500m + PET_500m + Lai_500m +
    Fpar_500m + 
    EVI_16_days + NDVI_16_days +
    FireMask
  glm(fm, data=dat0, family="poisson")
}


aa <- unlist(lapply(1:nrow(dat), function(i){ logLik(fitfun2(dat[-i, ])) }))

quantile(aa, 0.025)

bb <- matrix(
  unlist(lapply(1:nrow(dat), function(i){ coef(fitfun2(dat[-i, ])) })),
  nrow=nrow(dat), byrow=TRUE)

apply(bb, 2, mean)
apply(bb, 2, sd)

dat %>% filter(program == "Routine")
