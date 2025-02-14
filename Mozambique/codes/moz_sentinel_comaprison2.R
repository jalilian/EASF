
library("tidyverse")
library("readxl")

setwd("~/Downloads/Mozambique/Merged/")
# read and preprocess Niassa data
ndata <- read_excel(
  "Niassa province_EASF_Routine_data_cuamba_mandimba_AZM_09122024 _Year 1 & Year 2.xlsx",
  sheet = "Cuamba-Mandimba-EASF-Routine",
  col_types = "text", 
  na = c("", "N/A")
) %>%
  rename_all(tolower) %>%
  rename("collection date" = "date")

# read and preprocess Zambezia data
zdata <- read_excel(
  "Zambezia province_EASF-Routine-Gurue_Morrumbala-Year 1 & 2-12122024.xlsx",
  sheet = "EASF-Routine-Y1-Y2",
  col_types = "text", 
  na = c("", "N/A")
) %>%
  rename_all(tolower) %>%
  rename("street" = "village")

# combine the two data sets
adata <- bind_rows(
  ndata %>% select(intersect(names(ndata), names(zdata))),
  zdata %>% select(intersect(names(ndata), names(zdata)))
) %>%
  mutate(
    date = dmy(`collection date`),
    date2 = if_else(
      is.na(date),
      as.Date(as.numeric(`collection date`), origin = "1899-04-30"),
      date
    ),
    date3 = paste(year(date2), month(date2), sep = "-"),
    species = `species` %>%
      str_replace_all("\\s+", " ") %>%
      str_replace_all("Anopheles", "An.") %>%
      str_replace_all("An\\.(?! )", "An. ") %>%
      str_remove_all("\\.$") %>%
      str_to_title(),
    `trap type` = recode(`trap type`,
                         "AL-CDC" = "CDC",
                         "CDC-LT" = "CDC",
                         "Isca Humana" = "HLC",
                         "Isca humana" = "HLC",
                         "IH" = "HLC",
                         .default = `trap type`),
    `house id` = coalesce(
      `house id`,
      paste(district, `administrative post`, locality, street, `house no`, sep = "-")
    ) %>%
      str_replace_all("Franqueza", "Fraqueza")
  )

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")

aa <- cbind(adata$longitude, adata$latitude)
bb <- cbind(adata$`gps-garmin s`, adata$`gps-garmin e`)
idx <- apply(is.na(aa), 1, any)
aa[idx, ] <- convert_coords(bb[idx, 1], bb[idx, 2])
idx <- sapply(strsplit(aa[, 1], split="\\."), function(o) nchar(o[1]))
idx <- which(idx > 2)
aa[idx, ][1, 1] <- 35.65349
v <- aa[idx, ][-1, ]
v[, 1] <- paste0(substr(v[, 1], 1, 3), "°", substr(v[, 1], 4, nchar(v[, 1])))
v[, 2] <- paste0(substr(v[, 2], 1, 2), "°", substr(v[, 2], 3, nchar(v[, 2])))
aa[idx, ][-1, ] <- convert_coords(v[, 1], v[, 2])
adata$longitude <- as.numeric(aa[, 1])
adata$latitude <- -abs(as.numeric(aa[, 2]))

adata <- adata %>%
  select(-c(longitude, latitude)) %>%
  left_join(
    adata %>%
      group_by(`house id`) %>%
      summarise(longitude=mean(longitude, na.rm=TRUE),
                latitude=mean(latitude, na.rm=TRUE)),
    by=join_by(`house id`)
  )

adata <- adata %>%
  mutate(longitude2=case_when(longitude < 20 ~ abs(latitude),
                             .default=longitude),
         latitude2=case_when(latitude < -20 ~ -longitude,
                             .default=latitude),
         longitude=longitude2,
         latitude=latitude2) %>%
  select(-c(longitude2, latitude2))

library("mapview")
library("sf")
adata %>%
  st_as_sf(coords=c("longitude2", "latitude2"),
           crs=4326) %>%
  mapview(., zcol="programme")


tt <- adata %>% group_by(`trap type`, programme, `house id`, date3) %>% 
  summarise(An=sum(str_detect(species, "An. ")),
            .groups="drop") 

tt %>% count(programme, `trap type`) %>%
  rename(effort=n) %>%
  left_join(
    tt %>% 
      group_by(programme, `trap type`) %>%
      summarise(An=sum(An, na.rm=TRUE)),
    by = join_by(programme, `trap type`)
  ) %>%
  mutate(mean=An / effort) %>%
  ggplot(aes(x=mean, y=`trap type`, fill=programme)) +
  geom_bar(stat="identity", position="dodge") +
  labs(y="Collection method", x="Average number of collected mosquitoes") +
  theme_light()


t.test(
  tt %>% filter(`trap type`=="CDC", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="CDC", programme=="easf") %>%
    pull(An)
)

t.test(
  tt %>% filter(`trap type`=="HLC", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="HLC", programme=="easf") %>%
    pull(An)
)

t.test(
  tt %>% filter(`trap type`=="Prokopack", programme=="Routine") %>%
    pull(An),
  tt %>% filter(`trap type`=="Prokopack", programme=="easf") %>%
    pull(An)
)

dat <- adata %>% 
  filter(`trap type` %in% c("CDC", "HLC")) %>%
  group_by(programme, date3, `house id`, longitude, latitude) %>%
  summarise(y=sum(str_detect(species, "An. ")),
            .groups="drop") %>%
  mutate(y = replace_na(y, 0))


# land cover, population density and elevation data
source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_land_covars_africa.R")
covars <- get_covars(dat %>% 
                       select(longitude, latitude) %>% 
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

dat <- bind_cols(dat, covars)

saveRDS(dat, file="dat.rds")

dat <- readRDS("dat.rds")

source("https://raw.githubusercontent.com/jalilian/CEASE/refs/heads/main/Ethiopia/codes/get_modis.R")

aa <- get_modis("modis-11A2-061",
                c("LST_Day_1km", "LST_Night_1km"),
                what=dat %>% 
                  distinct(longitude, latitude) %>% 
                  as.matrix(),
                datetime="2022-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

dat <- dat %>%
  rename("long"="longitude", "lat"="latitude", "date"="date3") %>%
  left_join(aa,
            by=join_by(date, long, lat))

aa <- get_modis("modis-17A2H-061",
                c("Gpp_500m", "PsnNet_500m"),
                what=dat %>% 
                  distinct(long, lat) %>% 
                  as.matrix(),
                datetime="2022-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

dat <- dat %>%
  left_join(aa,
            by=join_by(date, long, lat))

aa <- get_modis("modis-09A1-061",
                c("sur_refl_b07"),
                what=dat %>% 
                  distinct(long, lat) %>% 
                  as.matrix(),
                datetime="2022-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

dat <- dat %>%
  left_join(aa,
            by=join_by(date, long, lat))

aa <- get_modis("modis-15A2H-061",
                c("Lai_500m", "Fpar_500m"),
                what=dat %>% 
                  distinct(long, lat) %>% 
                  as.matrix(),
                datetime="2022-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

dat <- dat %>%
  left_join(aa,
            by=join_by(date, long, lat))

aa <- get_modis("modis-13Q1-061",
                c("250m_16_days_EVI", "250m_16_days_NDVI"),
                what=dat %>% 
                  distinct(long, lat) %>% 
                  as.matrix(),
                datetime="2022-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

dat <- dat %>%
  left_join(aa,
            by=join_by(date, long, lat))

aa <- get_modis( "modis-14A2-061",
                c("FireMask"),
                what=dat %>% 
                  distinct(long, lat) %>% 
                  as.matrix(),
                datetime="2022-10-01/2024-08-30", 
                aggregate=TRUE, fill_in=TRUE)

dat <- dat %>%
  left_join(aa,
            by=join_by(date, long, lat))



dat <- dat %>%
  rename_with(~ gsub("__focal", "", .))

colnames(dat)

dat <- dat %>%
  rename("EVI_16_days"="250m_16_days_EVI",
         "NDVI_16_days"="250m_16_days_NDVI")

saveRDS(dat, file="dat.rds")
# =========================================================
dat <- readRDS("dat.rds")

dat <- dat %>%  
  mutate(tidx=as.numeric(factor(as.Date(paste(date, "01", sep="-")))),
             sidx=as.numeric(factor(paste(long, lat, sep="-"))))
  
dat %>% count(date, tidx)

dat <- dat %>%
  mutate(across(pop_density:FireMask, ~ scale(.x))) 

library("INLA")
library("fmesher")
library("sf")


fitfun <- function(dat0, max.edge=0.2)
{
  mesh <- fm_mesh_2d_inla(loc=dat0[, c("long", "lat")], 
                          max.edge=max.edge)
  #spde <- inla.spde2.matern(mesh, alpha=2)
  spde <- inla.spde2.pcmatern(
    mesh, alpha=2,
    # lower tail quantile and probability for the range 
    prior.range = c(0.1, 0.025),
    # upper tail quantile and probability for the standard deviation
    prior.sigma = c(10, 0.025)
  )
  
  fm <- y ~  elevation + pop_density +
    f(tidx, model="iid", constr=TRUE) + 
    f(mesh$idx$loc, model=spde) +
    #f(sidx, model="iid", constr=TRUE) +
    land_cover + pop_density + elevation +
    LST_Day_1km + LST_Night_1km + Gpp_500m +
    PsnNet_500m + sur_refl_b07 + Lai_500m +
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

fit1 <- fitfun(dat %>% filter(programme == "Routine"))
fit1$summary.fixed


fit2 <- fitfun(dat %>% filter(programme == "easf"))
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

cv1 <- cvfun(dat %>% filter(programme == "Routine"))
sqrt(mean(((cv1[, 1] - dat %>% filter(programme == "Routine") %>% pull(y)) / 
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
