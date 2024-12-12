
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
    mutate(Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude)) %>%
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
    mutate(Latitude=as.numeric(substr(Latitude, 1, 2)) + 
             as.numeric(substr(Latitude, 4, 9)) / 60,
           Latitude=-Latitude,
           Longitude=as.numeric(substr(Longitude, 1, 3)) + 
             as.numeric(substr(Longitude, 5, 10)) / 60) %>%
    mutate(program="Routine")
  ) %>%
  mutate(`Date of collection`=as.Date(`Date of collection`, 
                                      format="%d/%m/%y"),
         year=as.integer(substr(`Date of collection`, 1, 4)),
         month=as.integer(substr(`Date of collection`, 6, 7)))



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

##
# coordinates

bind_rows(
  mosquitoes %>% 
    distinct(program, District, 
             `Tablet-phone Latitude`, `Tablet-phone Longitude`) %>%
    rename("Latitude"="Tablet-phone Latitude",
           "Longitude"="Tablet-phone Longitude"),
  zambezia %>%
    distinct(program, District, Latitude, Longitude)
  ) %>%
  write_csv(file="~/Desktop/Mozyear2EASFvsRoutine.csv")

  


dat <- bind_rows(
  mosquitoes %>% 
    rename("Latitude"="Tablet-phone Latitude",
           "Longitude"="Tablet-phone Longitude") %>%
    group_by(District, program, year, month, Longitude, Latitude) %>%
    count(`Species name`) %>%
    filter(!str_detect(`Species name`, "Culex")) %>%
    summarise(n=sum(n)), 
  zambezia %>% 
    group_by(District, program, year, month, Longitude, Latitude) %>%
    count(`Species name`) %>%
    filter(`Species name` != "0") %>%
    summarise(n=sum(n))
  ) %>% ungroup()



# =========================================================
# land cover, population density and elevation data

source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_land_covars_africa.R")

covars <- get_covars(dat %>% select(Longitude, Latitude) %>% as.matrix(),
                     path="~/Downloads/Africa_covars/")
names(covars) <- c("land_cover", "pop_density", "elevation")
dat <- bind_cols(dat, covars)

# =========================================================
# Copernicus climate data

source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_Copernicus_climate_data.R")

key <- "********************************"

covars <- bind_rows(
  get_cds(key, user="ecmwfr",
          year=2023, month=sprintf("%02d", 9:12), 
          what=cbind(dat$Longitude, dat$Latitude)),
  get_cds(key, user="ecmwfr",
          year=2024, month=sprintf("%02d", 1:8), 
          what=cbind(dat$Longitude, dat$Latitude))
)

covars <- covars %>%
  mutate(skt = skt -273.15)


collapfun <- function(dat)
{
  dat %>%
    mutate(time=as.Date(as.POSIXct(time)),
           year=year(time), month=month(time)) %>% 
    group_by(longitude, latitude, year, month) %>%
    summarise(across(u10:ssr, list(mean=\(x) mean(x, na.rm=TRUE),
                                   sd=\(x) sd(x, na.rm=TRUE),
                                   min=\(x) min(x, na.rm=TRUE),
                                   max=\(x) max(x, na.rm=TRUE)),
                     .names="{.col}_{.fn}")) %>%
    ungroup()
}

covars <- collapfun(covars) 

covars %>%
  group_by(year, month) %>% 
  summarise_all(~sum(is.na(.)))

lagfun <- function(dat0)
{
  out <- list()
  m <- c(12, 1:8)
  for (i in 1:length(m))
  {
    out[[i]] <- bind_cols(
      dat0 %>% 
        filter(month == m[i]) %>%
        rename_at(vars(-(longitude:month)),function(x) paste0(x, "_0")),
      dat0 %>% 
        filter(month == (m[i] - 1 - 1) %% 12 + 1) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_1")),
      dat0 %>% 
        filter(month == (m[i] - 2 - 1) %% 12 + 1) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_2")),
      dat0 %>% 
        filter(month == (m[i] - 3 - 1) %% 12 + 1) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_3"))
    )
  }
  return(bind_rows(out))
}

covars <- lagfun(covars)

dat <- dat %>%
  rename(longitude=Longitude, latitude=Latitude) %>%
  left_join(covars, 
            by=join_by(year, month, longitude, latitude)) 

saveRDS(dat, file="~/Desktop/mozdat.rds")

# =========================================================

dat <- readRDS("~/Desktop/mozdat.rds")


library("INLA")
library("fmesher")

dat <- dat %>%
  rename("y"="n") %>%
  mutate(tidx=as.numeric(factor(paste0(year, month))),
         sidx=as.numeric(factor(paste(longitude, latitude, sep=""))))

dat %>% count(year, month, tidx)

dat %>% group_by(District) %>% count(longitude, latitude, sidx) %>% print(n=800)

# standardise covariates
dat <- dat %>%
  mutate(across(contains("_"), ~ scale(.x))) %>%
  mutate(elevation=scale(elevation))
 # mutate(across(contains("_"), ~ scales::rescale(.x, to=c(-1, 1)))) %>%
#  mutate(elevation=scales::rescale(elevation, to=c(-1, 1)))

fitfun <- function(dat0, max.edge=0.025)
{
  fm <- y ~  elevation + pop_density +
    f(tidx, model="rw1", constr=TRUE) + 
    f(sidx, model="iid", constr=TRUE) +
    u10_mean_0 + v10_mean_0 + lai_hv_mean_0 + lai_lv_mean_0 + 
    skt_mean_0 + sp_mean_0 + tp_mean_0 + swvl1_mean_0 + 
    pev_mean_0 + ssr_mean_0 +
    #u10_sd_0 + v10_sd_0 + lai_hv_sd_0 + lai_lv_sd_0 + 
    #skt_sd_0 + sp_sd_0 + tp_sd_0 + swvl1_sd_0 + 
    #pev_sd_0 + ssr_sd_0 +
    u10_mean_1 + v10_mean_1 + lai_hv_mean_1 + lai_lv_mean_1 + 
    skt_mean_1 + sp_mean_1 + tp_mean_1 + swvl1_mean_1 + 
    pev_mean_1 + ssr_mean_1 
  inla(fm, data=dat0, family="poisson",
       #control.family(control.link=list(model="log")),
       #control.compute=list(return.marginals.predictor=TRUE),
       control.predictor=list(compute=TRUE, link=1), 
       #control.inla=list(strategy="laplace", npoints=21),
       #control.inla=list(fast=FALSE, strategy="laplace", dz=0.25, h=1e-5)
       #control.compute=list(config=TRUE, dic=TRUE, waic=TRUE)
       silent=1L, num.threads=1)
}

fit1 <- fitfun(dat %>% filter(program == "Routine"))
round(fit1$summary.fixed[, c(1, 3, 5)], 2)
fit1$summary.hyperpar

fit2 <- fitfun(dat %>% filter(program == "EASF"))
round(fit2$summary.fixed[, c(1, 3, 5)], 2)
fit2$summary.hyperpar

cvfun <- function(dat, newdat=NULL, max.edge=0.025, mc.cores=4)
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
    group_by(District, year, month) %>%  
    nest() %>%            
    ungroup() %>% 
    left_join(
      routine_dat %>% 
        group_by(District, year, month) %>% 
        count(year, month),
      by = join_by(District, year, month)
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
################################### NEW #############################
# =========================================================

source("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/codes/convert_coords.R")

ndata <- read_excel(
  "~/Downloads/Mozambique/Niassa province_EASF_Routine_data_cuamba_mandimba_AZM_09122024 _Year 1 & Year 2.xlsx",
  sheet="Cuamba-Mandimba-EASF-Routine")

ndata <- ndata %>%
  mutate(latitude=case_match(latitude,
                             "1-14.351183" ~ "-14.351182",  # NMA2IHC001
                             "-14.34.66583" ~ "-14.3466583", # NMA8IHC001
                             .default=latitude),
         date=as.Date(date, format="%d/%m/%Y"),
         date=if_else(is.na(date), as.Date("2024-07-11"), date))

aa <- convert_coords(ndata$`GPS-Garmin E`, 
                     paste("-", ndata$`GPS-Garmin S`))

ndata %>%
  group_by(programme) %>%
  count(date) %>% print(n=500)
ndata %>% count(province, district)
ndata %>% count(`house id`) %>% print(n=500)

library("mapview")
ndata %>% 
  st_as_sf(., coords=c("longitude", "latitude"),
           crs=4326) %>%
  mapview()

ndata %>%
  select(`house id`, longitude, latitude, date, programme) %>%
  write_csv("~/Desktop/niassa_all.csv")
