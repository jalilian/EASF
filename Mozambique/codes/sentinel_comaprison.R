
library("readxl")
library("tidyverse")
library("sf")

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
  mutate(date=as.Date(date),
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

a %>% group_by(method) %>% 
  summarise(across(starts_with("An."), ~ sum(.x, na.rm = TRUE)))
a %>% group_by(method) %>% 
  summarise(across(starts_with("An."), ~ sum(.x, na.rm = TRUE))) %>%
  select(-method) / c(48, 60) %>% round(., 2)

a %>% group_by(method) %>% count(houseID)
a %>% group_by(method) %>% count(year, month)

b <- readRDS("~/Downloads/adaptive_table_coavrs.rds") %>% 
  filter(District == "Morrumbala") %>% 
  mutate(land_cover=unlist(land_cover),
         elevation=unlist(elevation))

b %>% group_by(`Collection method`) %>%
  summarise(across(starts_with("An. "), ~ sum(.x, na.rm = TRUE)))
            
b %>% group_by(`Collection method`) %>% count(`House ID`) %>% print(n=50)
b %>% group_by(`Collection method`) %>% count(Year, Month)

# t-tests
t.test(a %>% filter(method == "CDC") %>% pull(An.gambiaes.l.),
       b %>% filter(`Collection method` == "CDC") %>% 
         pull(`An. gambiae s.l`))
t.test(a %>% filter(method == "CDC") %>% pull(An.funestuss.l.),
       b %>% filter(`Collection method` == "CDC") %>% 
         pull(`An. funestus s.l`))
t.test(a %>% filter(method == "CDC") %>% pull(An.maculipalpis),
       b %>% filter(`Collection method` == "CDC") %>% 
         pull(`An. maculipalpis`))
t.test(a %>% filter(method == "CDC") %>% pull(An.coustani),
       b %>% filter(`Collection method` == "CDC") %>% 
         pull(`An. coustani`))
t.test(a %>% filter(method == "CDC") %>% pull(An.pretoriensis),
       b %>% filter(`Collection method` == "CDC") %>% 
         pull(`An. pretoriensis`))
t.test(a %>% filter(method == "CDC") %>% pull(An.rufipes),
       b %>% filter(`Collection method` == "CDC") %>% 
         pull(`An. rufipes`))
t.test(a %>% filter(method == "PK") %>% pull(An.funestuss.l.),
       b %>% filter(`Collection method` == "PK") %>% 
         pull(`An. funestus s.l`))


ab <- bind_rows(
  b %>% 
    group_by(`Collection method`) %>%
    summarise(across(starts_with("An."), sum,
                     .names="{.col}")) %>%
    pivot_longer(starts_with("An."), 
                 names_to="species", values_to="counts") %>%
    ungroup() %>%
    left_join(
      b %>% 
        group_by(`Collection method`) %>%
        summarise(across(starts_with("An."), mean,
                         .names="{.col}")) %>%
        pivot_longer(starts_with("An."), 
                     names_to="species", values_to="mean") %>%
        ungroup()) %>%
    rename(method=`Collection method`) %>%
    mutate(sampling="ESAF"),
  a %>% 
    group_by(method) %>% 
    summarise(across(starts_with("An."), sum,
                     .names="{.col}")) %>%
    pivot_longer(starts_with("An."),
                 names_to="species", values_to="counts") %>%
    ungroup() %>%
    left_join(
      a %>% 
        group_by(method) %>% 
        summarise(across(starts_with("An."), mean,
                         .names="{.col}")) %>%
        pivot_longer(starts_with("An."),
                     names_to="species", values_to="mean") %>%
        ungroup()
    ) %>%
    mutate(sampling="routine")
  ) %>%
  mutate(species=str_replace_all(species, " ", ""),
         species=case_match(species,
                            "An.Desconhecido" ~ "An.desconhecido",
                            "An.funestuss.l" ~ "An.funestuss.l.",
                            "An.gambiaes.l" ~ "An.gambiaes.l.",
                            .default=species))
ab <- expand_grid(method=unique(ab %>% pull(method)),
            species=unique(ab %>% pull(species)), 
            sampling=unique(ab %>% pull(sampling))) %>%
  left_join(ab, by=join_by(method, species, sampling)) %>%
  replace(is.na(.), 0) %>%
  arrange(method, sampling, desc(counts)) 

library("ggpubr")
ggarrange(
  ab %>%
    filter(method == "CDC") %>%
    ggplot(aes(y=species, x=mean, fill=sampling)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    labs(x="avergae number of collected mosquitoes with CDC", y="") + 
    scale_y_discrete(position = "right") + 
    scale_x_reverse() + 
    theme_classic2(),
  ab %>%
    filter(method == "PK") %>%
    ggplot(aes(y=species, x=mean, fill=sampling)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    labs(x="avergae number of collected mosquitoes with Prokopack") + 
    theme_classic2() + 
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank()),
  common.legend =TRUE, align="h", widths=c(1.25, 1), nrow=1)


# Simpson's diversity index
Simoson <- function(x, ci=0.95) 
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

ab %>% filter(method == "CDC", sampling == "ESAF") %>% 
  summarise(Simoson(counts))

ab %>% group_by(method, sampling) %>% 
  summarise(s=Simoson(counts)[1], q1=Simoson(counts)[2], q2=Simoson(counts)[3]) %>%
  ungroup() %>%
  rename(program=sampling) %>%
  ggplot(aes(x=s, y=method, colour=program)) + 
  geom_errorbar(aes(xmin=q1, xmax=q2)) + 
  geom_point() + 
  labs(y="collection method", x="Simpson's diversity index") + 
  theme_classic2()


# ===============================================
### Modelling part
# ===============================================

a <- a %>% 
    group_by(houseID, longitude, latitude, year, month, method) %>%
    summarise(across(starts_with("An."), sum, .names="{.col}")) %>%
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


# map
bind_rows(
  b %>%
    filter(`Collection method` == "CDC") %>%
    rename(longitude=Longitude, latitude=Latitude) %>%
    mutate(site="ESAF", latitude=-latitude) %>%
    select(longitude, latitude, site),
  a %>%
    filter(method == "CDC") %>%
    mutate(site="sentinel") %>%
    select(longitude, latitude, site)
  ) %>% #write_csv(., file="~/Downloads/MurSites.csv")
  st_as_sf(., coords=c("longitude", "latitude")) %>%
  ggplot(aes(colour=site)) + 
  geom_sf() 

saveRDS(a, file="~/Downloads/a.rds")
saveRDS(b, file="~/Downloads/b.rds")

a <- readRDS("~/Downloads/a.rds")
b <- readRDS("~/Downloads/b.rds")

library("INLA")
library("fmesher")

fitfun <- function(dat, max.edge=0.025)
{
  #mesh <- fm_mesh_2d_inla(loc=cbind(dat$longitude, dat$latitude), 
  #                        max.edge=max.edge)
  dat <- dat %>% 
    mutate(across(contains("_"), ~ scales::rescale(.x, to=c(-1, 1)))) %>%
    mutate(elevation=scales::rescale(elevation, to=c(-1, 1))) %>%
    mutate(tidx=as.integer(factor(letters[tidx])),
           sidx=as.integer(factor(letters[sidx])))
  fm <- y ~  elevation + 
    f(tidx, model="iid", constr=TRUE) + 
    f(sidx, model="iid", constr=TRUE) +
  #  f(mesh$idx$loc, 
  #    model=inla.spde2.matern(mesh=mesh, alpha=2, constr=TRUE)) +
  #  lai_hv_mean_0 + #lai_hv_min_0 + lai_hv_max_0 + lai_hv_sd_0 + 
    lai_lv_mean_0 + #lai_lv_min_0 + lai_lv_max_0 + lai_lv_sd_0 + 
    skt_mean_0 + #skt_min_0 + skt_max_0 + skt_sd_0 + 
    #tp_mean_0 + #tp_min_0 + tp_max_0 + tp_sd_0 + 
    swvl1_mean_0 #+ #swvl1_min_0 + swvl1_max_0 + swvl1_sd_0 +
  #  lai_hv_mean_1 + #lai_hv_min_1 + lai_hv_max_1 + lai_hv_sd_1 + 
  #  lai_lv_mean_1 + #lai_lv_min_1 + lai_lv_max_1 + lai_lv_sd_1 + 
  #  skt_mean_1 + #skt_min_1 + skt_max_1 + skt_sd_1 +
  #  tp_mean_1 + #tp_min_1 + tp_max_1 + tp_sd_1 + 
  #  swvl1_mean_1 #+ #swvl1_min_1 + swvl1_max_1 + swvl1_sd_1 #+ 
    #lai_hv_mean_2 + #lai_hv_min_2 + lai_hv_max_2 + lai_hv_sd_2 + 
    #lai_lv_mean_2 + #lai_lv_min_2 + lai_lv_max_2 + lai_lv_sd_2 +
    #skt_mean_2 + #skt_min_2 + skt_max_2 + skt_sd_2 +
    #tp_mean_2 + #tp_min_2 + tp_max_2 + tp_sd_2 +
    #swvl1_mean_2 #+ swvl1_min_2 + swvl1_max_2 + swvl1_sd_2
  inla(fm, data=dat, family="zeroinflatednbinomial2",
       #control.family(control.link=list(model="log")),
       #control.compute=list(return.marginals.predictor=TRUE),
       control.predictor=list(compute=TRUE, link=1), 
       control.inla=list(strategy="laplace", npoints=21),
       #control.inla=list(fast=FALSE, strategy="laplace", dz=0.25, h=1e-5)
       #control.compute=list(config=TRUE, dic=TRUE, waic=TRUE)
       silent=1L, num.threads=1)
}


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

dat1 <- a %>% 
  mutate(y=An.gambiaes.l. + An.outros + An.funestuss.l. + 
           An.pretoriensis + An.maculipalpis + An.rufipes + 
           An.coustani + An.tenebrosus) %>% 
  mutate(tidx=as.numeric(factor(paste(year, month)))) %>%
  mutate(sidx=as.numeric(factor(houseID))) %>%
  select(-land_cover)

fit1 <- fitfun(dat1)
fit1$summary.fixed
fit1$summary.hyperpar

cv1 <- cvfun(dat1)

mean(((cv1[, 1] - dat1$y) / (dat1$y + 1))^2)
(mean(((cv1[, 1] - dat1$y) / cv1[, 2])^2))

dat2 <- b %>%
  rename(longitude=Longitude, latitude=Latitude,
         year=Year, month=Month) %>%
  mutate(y=`An. gambiae s.l` + `An. funestus s.l` + 
           `An. tenebrosus` + `An. maculipalpis` +  
           `An. coustani` + `An. desconhecido` +
           `An. rufipes` + `An. pretoriensis` +  
           `An. Desconhecido` + `An. squamosus` +
           `An. ziemanni`) %>%
  mutate(tidx=as.numeric(factor(paste(year, month)))) %>%
  mutate(sidx=as.numeric(factor(`House ID`))) %>%
  select(-land_cover)

fit2 <- fitfun(dat2)
fit2$summary.fixed
fit2$summary.hyperpar

cv2 <- cvfun(dat2)

mean(((cv2[, 1] - dat2$y) / (dat2$y + 1))^2)
(mean(((cv2[, 1] - dat2$y) / cv2[, 2])^2))


dat3 <- bind_rows(
  dat1 %>% select(which(colnames(dat1) %in% colnames(dat2))) %>%
    mutate(latitude=-latitude,
           program="routine"),
  dat2 %>% select(which(colnames(dat2) %in% colnames(dat1))) %>%
    mutate(program="ESAF")
  ) %>%
  #filter(latitude > -17.5) %>%
  mutate(tidx=as.numeric(factor(paste(year, month))))

cv3 <- cvfun(dat3)

(mean(((cv3[, 1] - dat3$y) / cv3[, 2])^2))

mspe1 <- c(
  mean(((cv1[, 1] - dat1$y) / cv1[, 2])^2),
  mean(((cv2[, 1] - dat2$y) / cv2[, 2])^2)
)

dat31 <- dat32 <- dat3
dat31$y[dat3$program == "ESAF"] <- NA 
dat32$y[dat3$program == "routine"] <- NA
fit31 <- fitfun(dat31)
fit32 <- fitfun(dat32)

mspe2 <- 1000 * c(
  mean(((dat3$y[is.na(dat31$y)] - fit31$summary.fitted.values[is.na(dat31$y), 1]) / 
          fit31$summary.fitted.values[is.na(dat31$y), 2])^2),
  mean(((dat3$y[is.na(dat32$y)] - fit32$summary.fitted.values[is.na(dat32$y), 1]) / 
          fit32$summary.fitted.values[is.na(dat32$y), 2])^2)
  )


ggarrange(
  tibble(program=c("routine", "ESAF"),
         `In-sample mean square standardized prediction error`=mspe1) %>%
    ggplot(aes(x= `In-sample mean square standardized prediction error`, 
               y=program)) +
    geom_bar(stat="identity") + 
    labs(y="") + 
    scale_y_discrete(position = "right") + 
    scale_x_reverse() +
    theme_classic2(),
  tibble(program=c("routine", "ESAF"),
         `Out-of-sample mean square standardized prediction error`=mspe2) %>%
    ggplot(aes(x= `Out-of-sample mean square standardized prediction error`, 
               y=program)) +
    labs(y="") +
    geom_bar(stat="identity") + 
    theme_classic2()+ 
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank()),
  nrow=1, widths=c(1.25, 1))



# new

dat11 <- dat1 %>% filter(method == "CDC")
dat22 <- dat2 %>% 
  filter(`Collection method` == "CDC",
         month %in% c("April", "March", "October", "January"))

cv1 <- cvfun(dat11, max.edge=NULL)
mean(((cv1[, 3] - cv1[, 1]) / cv1[, 2])^2)
mean(((cv1[, 3] - cv1[, 1]) / (1 + cv1[, 3]))^2)
quantile(((cv1[, 3] - cv1[, 1]) / cv1[, 2])^2, c(0.025, 0.975))

matrix(unlist(lapply(lapply(fit1$marginals.fitted.values, 
                            function(o){ inla.tmarginal(exp, o) }),
                     inla.zmarginal)), ncol=7, byrow=TRUE)

nsim <- 499 
mc.cores <- 6
cv2s <- cv21s <- cv12s <- matrix(NA, nrow=nsim, ncol=2)
for (j in 1:nsim)
{
  dat222 <- dat22 %>%
    filter(`House ID` %in% sample(unique(dat22$`House ID`), size=4))
  cv2 <- cvfun(dat222, newdat=NULL, max.edge=NULL, mc.cores=mc.cores)
  cv21 <- cvfun(dat222, newdat=dat11, max.edge=NULL, mc.cores=mc.cores)
  cv12 <- cvfun(dat11, newdat=dat222, max.edge=NULL, mc.cores=mc.cores)

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


mean(cv21s[, 2], na.rm=TRUE)
quantile(cv21s[, 2], c(0.025, 0.975), na.rm=TRUE)
mean(cv12s[, 2], na.rm=TRUE)
quantile(cv12s[, 2], c(0.025, 0.975), na.rm=TRUE)
