
library("readxl")
library("tidyverse")
library("sf")

# ===============================================


sdata %>% 
  group_by(event) %>% 
  count(longitude, latitude) %>%
  print(n=700)









sdata <- sdata %>% 
  mutate(vv=str_replace_all(value, " ", ""),
         vv=str_replace_all(vv, "-|=", "_"),
         vv=str_replace(vv, "_LC_", "_HLC_"),
         vv=str_replace(vv, "_SC_", "_PSC_"),
         vv=str_replace(vv, "^HL_|^HLC_", "HEL_"),
         vv=str_replace(vv, "0TET_", "OTET_"), 
         vv=str_replace(vv, "_00", "_0"),
         #vv=str_replace(vv, "_14$|_0014$|_014$", "_14"),
         #vv=str_replace(vv, "_0011", "_011"),
         vv=case_match(vv, 
                      # "HEL_PSC_08" ~ "HEL_PSC_008",
                       "HEC_HLC_02" ~ "HEL_HLC_02", 
                       "HEL_HC_03" ~ "HEL_HLC_03",
                       "HEL_HCL_04" ~ "HEL_HLC_04",
                       "HEL_PSC005" ~ "HEL_PSC_05", .default=vv),
         vv=ifelse(nchar(vv) == 14, substr(vv, 1, 11), vv),
         vv=ifelse(str_detect(vv, "^SW"), 
                   paste(programname, 
                         str_replace(vv, "(?=[:digit:])", "_"), 
                         sep="_"), vv)) %>%
  group_by(programname, `Org unit name`, year, month)

sdata <- sdata %>%
  mutate(hh=ifelse(`Datat element name` == "HH Number", 
                           vv, NA))
idx <- which(!is.na(sdata$hh))
sdata$`house ID` <- NA
for (i in 1:nrow(sdata))
{
  ll <- sum(idx < i) + 1
  sdata$`house ID`[i] <- sdata$hh[idx][ll]
}

sdata %>% 
  filter(`Datat element name` == "HH Number") %>% 
  ungroup() %>%
  count(value, vv) %>% print(n=700)

sdata %>% 
  filter(`Datat element name` == "HH Number") %>% 
  count(vv, geometry) %>% print(n=700)

sdata <- sdata %>% 
  group_by(programname, `Org unit name`, `house ID`, month)



























sent_coords <- as_tibble(matrix(c(
  "HEL_HLC_01", 0.51125, 7.0709, 
  "HEL_HLC_02", 0.51072, 7.07006,
  "HEL_HLC_03", 0.51072, 7.06965,
  "HEL_HLC_04", 0.51089, 7.07195,
  "HEL_HLC_05", 0.5097, 7.06965,
  "HEL_HLC_06", 0.51005, 7.07195,
  "HEL_HLC_07", 0.51014, 7.07195,
  "HEL_HLC_08", 0.51005, 7.07175,
  "OTET_HLC_01", -0.30888, 5.54196,
  "OTET_HLC_02", -0.30853, 5.54149,
  "OTET_HLC_03", -0.31041, 5.54086,
  "OTET_HLC_04", -0.31086, 5.54083,
  "OTET_HLC_05", -0.3134, 5.5396,
  "OTET_HLC_06", -0.31336, 5.53995,
  "OTET_HLC_07", -0.31129, 5.53178,
  "OTET_HLC_08", -0.31155, 5.53143,
  "HEL_PSC_001", 0.510948, 7.070647,
  "HEL_PSC_002", 0.510978, 7.070618,
  "HEL_PSC_003", 0.51092, 7.070617,
  "HEL_PSC_004", 0.510908, 7.070909,
  "HEL_PSC_005", 0.510996, 7.070658,
  "HEL_PSC_006", 0.510634, 7.069969,
  "HEL_PSC_007", 0.510716, 7.069776,
  "HEL_PSC_008", 0.510634, 7.069745,
  "HEL_PSC_009", 0.510772, 7.069799,
  "HEL_PSC_010", 0.510762, 7.069788,
  "HEL_PSC_011", 0.510215, 7.070134,
  "HEL_PSC_012", 0.511656, 7.070245,
  "HEL_PSC_013", 0.510301, 7.070071,
  "HEL_PSC_014", 0.510067, 7.070102,
  "HEL_PSC_015", 0.509972, 7.071589,
  "HEL_PSC_016", 0.510246, 7.069983,
  "HEL_PSC_017", 0.510254, 7.069981,
  "HEL_PSC_018", 0.51021, 7.069988,
  "HEL_PSC_019", 0.510258, 7.069982,
  "HEL_PSC_020", 0.510781, 7.070421,
  "OTET_PSC_01", -0.309438, 5.542005,
  "OTET_PSC_02", -0.309393, 5.542103,
  "OTET_PSC_03", -0.309625, 5.542676,
  "OTET_PSC_04", -0.311746, 5.541052,
  "OTET_PSC_05", -0.309319, 5.543349,
  "OTET_PSC_06", -0.310141, 5.540407,
  "OTET_PSC_07", -0.310156, 5.540017,
  "OTET_PSC_08", -0.311229, 5.541541,
  "OTET_PSC_09", -0.310141, 5.540407,
  "OTET_PSC_10", -0.310141, 5.540407,
  "OTET_PSC_11", -0.318238, 5.551906,
  "OTET_PSC_12", -0.311595, 5.540842,
  "OTET_PSC_13", -0.311229, 5.541541,
  "OTET_PSC_14", -0.318238, 5.55196,
  "OTET_PSC_15", -0.309075, 5.542215,
  "OTET_PSC_16", -0.309461, 5.543356
), ncol=3, byrow=TRUE)) %>%
  mutate(V2=as.numeric(V2), V3=as.numeric(V3)) %>%
  rename("HH"="V1", "longitude"="V2", "latitude"="V3")

easf_coords <- read_csv("~/Downloads/Ghana/gha_coords.csv") %>% 
  mutate(`house ID`=str_remove_all(`house ID`, " ")) %>%
  distinct(`house ID`, long, lat)

if (FALSE)
{
  library("mapview")
  sent_coords %>% 
    st_as_sf(., coords=c("longitude", "latitude"), crs="WGS84") %>%
    mapview()
  
  easf_coords %>%
    st_as_sf(., coords=c("long", "lat"), crs="WGS84") %>%
    mapview()
}

bind_rows(
  sent_coords %>% 
    rename("house ID" = "HH") %>%
    mutate(program="sentinel"),
  easf_coords %>%
    rename("longitude"="long", "latitude"="lat") %>%
    mutate(program="EASF")
  ) %>% 
  mutate(method="HLC") %>%
  mutate(method=ifelse(str_detect(`house ID`, "PSC"), "PSC", method)) %>%
  write_csv(file="~/Downloads/Ghana/gha_coords_all.csv")

# ===============================================



sent_hlc <- sdata %>% 
  filter(programname == "HLC", 
         str_detect(`Datat element name`, ": An. gambiae")) %>%
  summarise(gambiae=sum(as.numeric(value), na.rm=TRUE)) %>%
  ungroup()

sent_hlc <- sent_hlc %>% 
  left_join(
    sdata %>% 
      filter(programname == "HLC", 
             str_detect(`Datat element name`, "An. funestus")) %>%
      summarise(funestus=sum(as.numeric(value), na.rm=TRUE)) %>% 
      ungroup()
    ) %>%
  mutate(funestus=ifelse(is.na(funestus), 0, funestus))

sdata %>% 
  filter(programname == "HLC", 
         str_detect(`Datat element name`, "An. funestus")) %>%
  summarise(funestus=sum(as.numeric(value), na.rm=TRUE)) %>%
  ungroup()

sent_psc <- sdata %>% 
  filter(programname == "CID",
         str_detect(`Datat element name`, "An. gambiae s.l.: Collected")) %>%
  summarise(gambiae=sum(as.numeric(value), na.rm=TRUE)) %>%
  ungroup()

## Collection intensity 

gha_hlc <- read_csv("~/Downloads/Ghana/gha_hlc.csv")

# t-tests
t.test(
  gha_hlc %>% pull(`Number of An.`),
  sent_hlc %>% filter(!str_detect(`house ID`, "SW|PSC")) %>% 
    pull(gambiae)
  )

t.test(
  gha_psc %>% filter(!str_detect(`House ID`, "PSC-PSC-13")) %>% 
    pull(`Number of An.`),
  sent_psc %>% pull(gambiae)
  )

data.frame(program=rep(c("EASF", "Routine"), 2), 
           method=rep(c("HLC", "PSC"), each=2),
           mean=c(mean(gha_hlc %>% pull(`Number of An.`)),
                  mean(sent_hlc %>% filter(!str_detect(`house ID`, "SW|PSC")) %>% 
                         pull(gambiae)),
                  mean(gha_psc %>% filter(!str_detect(`House ID`, "PSC-PSC-13")) %>% 
                         pull(`Number of An.`)),
                  mean(sent_psc %>% pull(gambiae)))) %>%
  ggplot(aes(x=mean, y=program, fill=method)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  labs(x="Average number of collected An. gambiae",
      y="Sampling program") +
  theme_classic()

## Predictive power 

sent_hlc <- sent_hlc %>% 
  left_join(sent_coords %>%
              rename("house ID"="HH"),
            by=join_by(`house ID`)) %>%
  na.omit() %>%
  filter(!str_detect(`house ID`, "PSC"))


# =========================================================
# land cover, population density and elevation data

source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_land_covars_africa.R")

covars <- get_covars(gha_hlc %>% select(long, lat) %>% as.matrix(),
                     path="~/Downloads/Africa_covars/")
names(covars) <- c("land_cover", "pop_density", "elevation")
gha_hlc <- bind_cols(gha_hlc, covars)

covars <- get_covars(sent_hlc %>% select(longitude, latitude) %>% as.matrix(),
                     path="~/Downloads/Africa_covars/")
names(covars) <- c("land_cover", "pop_density", "elevation")
sent_hlc <- bind_cols(sent_hlc, covars)

# =========================================================
# Copernicus climate data

source("https://raw.githubusercontent.com/jalilian/CEASE/main/Ethiopia/codes/get_Copernicus_climate_data.R")

key <- "********************************"

covars <- get_cds(key, user="ecmwfr",
                  year=2023, month=sprintf("%02d", 4:12), 
                  what=cbind(gha_hlc$long, gha_hlc$lat))

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

lagfun <- function(dat)
{
  out <- list()
  m <- 7:12
  for (i in 1:length(m))
  {
    out[[i]] <- bind_cols(
      dat %>% 
        filter(month == m[i]) %>%
        rename_at(vars(-(longitude:month)),function(x) paste0(x, "_0")),
      dat %>% 
        filter(month == m[i] - 1) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_1")),
      dat %>% 
        filter(month == m[i] - 2) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_2")),
      dat %>% 
        filter(month == m[i] - 3) %>%
        select(-(longitude:month)) %>%
        rename_all(function(x) paste0(x, "_3"))
    )
  }
  return(bind_rows(out))
}

covars <- lagfun(covars)

gha_hlc <- gha_hlc %>%
  rename(longitude=long, latitude=lat) %>%
  left_join(covars, 
            by=join_by(year, month, longitude, latitude)) 


covars <- get_cds(key, user="ecmwfr",
                  year=2023, month=sprintf("%02d", 4:12), 
                  what=cbind(sent_hlc$longitude, sent_hlc$latitude))

covars <- covars %>%
  mutate(skt = skt -273.15)


covars <- collapfun(covars) 

covars %>%
  group_by(year, month) %>% 
  summarise_all(~sum(is.na(.)))

covars <- lagfun(covars)

sent_hlc <- sent_hlc %>%
  mutate(year=2023, month=match(month, month.name)) %>%
  relocate(year, .before=month) %>%
  left_join(covars, 
            by=join_by(year, month, longitude, latitude)) 


a <- sent_hlc %>%
  select(year, month, longitude, latitude, gambiae, 
         land_cover:ssr_max_3) %>%
  rename(y=gambiae)

b <- gha_hlc %>%
  select(year, month, longitude, latitude, `Number of An.`, 
         land_cover:ssr_max_3) %>%
  rename(y=`Number of An.`)

saveRDS(a, file="~/Downloads/Ghana/a.rds")
saveRDS(b, file="~/Downloads/Ghana/b.rds")

###

a <- readRDS("~/Downloads/Ghana/a.rds")
b <- readRDS("~/Downloads/Ghana/b.rds")

library("INLA")
library("fmesher")

a <- a %>%
  mutate(tidx=month,
         sidx=as.numeric(factor(paste(longitude, latitude, sep=""))))
b <- b %>%
  mutate(tidx=month,
         sidx=as.numeric(factor(paste(longitude, latitude, sep=""))))

fitfun <- function(dat, max.edge=0.025)
{
  dat <- dat %>% 
    mutate(across(contains("_"), ~ scales::rescale(.x, to=c(-1, 1)))) %>%
    mutate(elevation=scales::rescale(elevation, to=c(-1, 1))) %>%
    mutate(tidx=as.integer(factor(letters[tidx])),
           sidx=as.integer(factor(letters[sidx])))
  fm <- y ~  elevation + pop_density +
    f(tidx, model="rw1", constr=TRUE) + 
    f(sidx, model="iid", constr=TRUE) +
    u10_mean_0 + v10_mean_0 + lai_hv_mean_0 + lai_lv_mean_0 + 
    skt_mean_0 + sp_mean_0 + tp_mean_0 + swvl1_mean_0 + 
    pev_mean_0 + ssr_mean_0 #+
    #u10_sd_0 + v10_sd_0 + lai_hv_sd_0 + lai_lv_sd_0 + 
    #skt_sd_0 + sp_sd_0 + tp_sd_0 + swvl1_sd_0 + 
    #pev_sd_0 + ssr_sd_0 +
    #u10_mean_1 + v10_mean_1 + lai_hv_mean_1 + lai_lv_mean_1 + 
    #skt_mean_1 + sp_mean_1 + tp_mean_1 + swvl1_mean_1 + 
    #pev_mean_1 + ssr_mean_1 
  inla(fm, data=dat, family="nbinomial",
       #control.family(control.link=list(model="log")),
       #control.compute=list(return.marginals.predictor=TRUE),
       control.predictor=list(compute=TRUE, link=1), 
       #control.inla=list(strategy="laplace", npoints=21),
       #control.inla=list(fast=FALSE, strategy="laplace", dz=0.25, h=1e-5)
       #control.compute=list(config=TRUE, dic=TRUE, waic=TRUE)
       silent=1L, num.threads=1)
}

fit1 <- fitfun(a)
round(fit1$summary.fixed[, c(1, 3, 5)], 2)
fit1$summary.hyperpar

fit2 <- fitfun(b)
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

cv1 <- cvfun(a)
mean(((cv1[, 1] - a$y) / (a$y + 1))^2)
(mean(((cv1[, 1] - a$y) / cv1[, 2])^2))

cv2 <- cvfun(b)
mean(((cv1[, 1] - b$y) / (b$y + 1))^2)
(mean(((cv1[, 1] - b$y) / cv1[, 2])^2))


nsim <- 499 
mc.cores <- 6
cv2s <- cv21s <- cv12s <- matrix(NA, nrow=nsim, ncol=2)
for (j in 1:nsim)
{
  b2 <- b %>%
    filter(sidx %in% sample(unique(b$sidx), size=16))
  a1 <- NULL
  for (i in 1:16)
  {
    a1 <- bind_rows(
      a1,
      a[-48, ] %>% 
        filter(sidx == i, 
               month %in% sample(7:12, 
                                 size=(b2 %>% count(sidx) %>% pull(n))[i]))
      )
  }
  a1 <- a1  %>%
    distinct(sidx, month, .keep_all=TRUE)
  bind_rows(a1, a %>% 
              filter(sidx==1, 
                     month %in% sample(7:12, size=(b2 %>% count(sidx) %>% pull(n))[1])))
  cv2 <- cvfun(b2, newdat=NULL, max.edge=NULL, mc.cores=mc.cores)
  cv21 <- cvfun(b2, newdat=a, max.edge=NULL, mc.cores=mc.cores)
  cv12 <- cvfun(a, newdat=b2, max.edge=NULL, mc.cores=mc.cores)
  
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



hist(log(cv2s[, 2]))
abline(v=log(mean(((cv1[, 3] - cv1[, 1]) / cv1[, 2])^2)), col="blue")
mean(cv2s[, 2], na.rm=TRUE)
quantile(cv2s[, 2], c(0.025, 0.975), na.rm=TRUE)

mean(cv2s[, 2] <= mean(((cv1[, 3] - cv1[, 1]) / cv1[, 2])^2), na.rm=TRUE)

mean(cv12s[, 2] <= mean(cv21s[, 2])) 
mean(cv21s[, 2] <= mean(cv12s[, 2]))

mean(cv21s[, 2], na.rm=TRUE)
quantile(cv21s[, 2], c(0.025, 0.975), na.rm=TRUE)
mean(cv12s[, 2], na.rm=TRUE)
quantile(cv12s[, 2], c(0.025, 0.975), na.rm=TRUE)

