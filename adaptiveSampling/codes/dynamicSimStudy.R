
library("tidyverse")
library("sf")
library("spatstat")
library("fmesher")
library("INLA")
# =========================================================

if (FALSE)
{
  #  WorldClim version 2.1 climate data for 1970-2000
  # 10 minutes
  urls <- c(
    # average temperature (°C)
    "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_10m_tavg.zip",
    # precipitation (mm) 
    "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_10m_prec.zip",
    # wind speed (m/s)
    "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_10m_wind.zip"
  )
  
  for (ur in urls)
  {
    download.file(
      url=ur,
      destfile=paste0(tempdir(), "/aa.zip")
    )
    unzip(paste0(tempdir(), "/aa.zip"), exdir=paste0(tempdir(), "/aa/"))
  }
  
  library("terra")
  # average temperature (°C)
  z1 <- terra::rast(list.files(path=paste0(tempdir(), "/aa/"), 
                                pattern="tavg", full.names=TRUE))     
  # precipitation (mm) 
  z2 <- terra::rast(list.files(path=paste0(tempdir(), "/aa/"), 
                               pattern="prec", full.names=TRUE))   
  # wind speed (m/s)
  z3 <- terra::rast(list.files(path=paste0(tempdir(), "/aa/"), 
                               pattern="wind", full.names=TRUE))   

  # country iso code
  country <- "moz"
  
  # map of the country
  cmap <- read_sf(
    paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/kmz/gadm41_",
           toupper(country), "_0.kmz")
    )
  
  if (FALSE)
  {
    library("rnaturalearth")
    cmap <- ne_download()
    cmap <- cmap %>% 
      filter(CONTINENT == "Africa") %>%
      filter(!(NAME %in% 
                 c("Algeria", "Egypt", "Libya", 
                   "Mauritania", "Morocco", "Sudan", 
                   "Tunisia", "Western Sahara", "W. Sahara")))
  }
  
  # get Copernicus data
  source(
    "https://github.com/jalilian/CEASE/raw/refs/heads/main/Ethiopia/codes/get_Copernicus_climate_data.R"
  )
  key <- "******************************"
  
  envars <- bind_rows(
    get_cds(key, year=2022, month=sprintf("%02d", 1:12), day=1, what=cmap),
    get_cds(key, year=2023, month=sprintf("%02d", 1:12), day=1, what=cmap),
    get_cds(key, year=2024, month=sprintf("%02d", 1:12), day=1, what=cmap),
  )

  xy <- unique(st_coordinates(envars))
  xy <- data.frame(longitude=xy[, "X"], latitude=xy[, "Y"])
  tnfun <- function(z, xy, name)
  {
    zv <- terra::extract(z, as.matrix(xy))
    colnames(zv) <- 1:12
    zv %>%
      mutate(longitude=xy$longitude, latitude=xy$latitude) %>%
      pivot_longer(-c(longitude, latitude), 
                   names_to="month", values_to=name) %>%
      mutate(month=as.numeric(month))
  }
  
  envars <- envars %>%
    mutate(year=year(date), month=month(date)) %>%
    mutate(longitude=st_coordinates(envars)[, "X"], 
           latitude=st_coordinates(envars)[, "Y"]) %>%
    relocate("year", "month", "longitude", "latitude", .after=date) %>%
    left_join(
      tnfun(z1, xy, "tavg") %>%
        left_join(tnfun(z3, xy, "wind"), 
                  by=join_by(longitude, latitude, month))  %>%
        left_join(tnfun(z2, xy, "perc"), 
                  by=join_by(longitude, latitude, month)),
              by=join_by(longitude, latitude, month)
      )
  envars <- st_filter(envars, cmap)
  saveRDS(envars, 
          paste0("~/Documents/GitHub/EASF/adaptiveSampling/envars_",
                 country, ".rds"))
}

# =========================================================
# environmental variables 

tfun <- function(x)
{
  # x / sd(x, na.rm=TRUE)
  scales::rescale(x, to=c(-1, 1))
}
envars <- readRDS(
  #url("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/envars_gha.rds")
  url("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/envars_moz.rds")
  ) %>%
  na.omit() %>%
  # compute wind speed and Temp in centigrade
  mutate(wind_speed=sqrt(`10m_u_component_of_wind`^2 + 
                           `10m_v_component_of_wind`^2),
         skin_temperature=skin_temperature - 273.15,
         total_precipitation=1000 * total_precipitation,
         log_perc=log(.Machine$double.eps + perc),
         log_total_precipitation=
           log(.Machine$double.eps + total_precipitation)) %>%
  filter(date >= as.Date("2022-10-1")) %>% 
  # rescale the selected covariates to [-1, 1] interval
  mutate(
    skin_temperature=tfun(skin_temperature), 
    total_precipitation=tfun(total_precipitation),
    tavg=tfun(tavg),
    perc=tfun(perc),
    log_total_precipitation=tfun(log_total_precipitation),
    log_perc=tfun(log_perc),
    wind_speed=tfun(wind_speed), 
    wind=tfun(wind)
    )

longlat <- as.data.frame(st_coordinates(envars)) %>%
  rename(longitude=X, latitude=Y) %>%
  distinct() %>%
  mutate(sid=1:nrow(.))

envars <- envars %>%
  mutate(tid=as.numeric(factor(date))) %>%
  left_join(
    longlat, 
            by=join_by(longitude, latitude))

W <- as.im(data.frame(x=longlat$longitude, 
                      y=longlat$latitude,
                      z=TRUE))

par(mar=c(1, 0, 1, 0))
plot(W, main="")

# =========================================================
# sampling locations

samplocs <- function(n, grid_percent=0.6, domain=as.owin(W), dimyx=dim(W),
                     plotit=FALSE)
{
  # number of points to place on a grid
  n_grid <- n * grid_percent * area(boundingbox(domain)) / area(domain)
  # aspect ratio of the domain (height/width)
  apr <- dimyx[1] / dimyx[2]
  #  number of grid cells in the x and y directions
  nx <- sqrt(n_grid / apr)
  ny <- nx * apr
  # regular grid of points on the domain
  Q <- gridcentres(domain, nx=round(nx), ny=round(ny))
  # points falling inside the domain's boundary
  ok <- inside.owin(Q$x, Q$y, domain)
  # grid points that are inside the domain 
  S1 <- ppp(Q$x[ok], Q$y[ok], window=domain)
  # randomly generated points to make up the rest of locations
  S2 <- rpoint(n=n - sum(ok), win=domain)
  if (plotit)
  {
    as.polygonal(domain)$bdry[[1]] %>% 
      as.data.frame() %>% 
      ggplot(aes(x=x, y=y)) + 
      geom_polygon(fill="grey75") +
      geom_point(data=as.data.frame(S1), shape=3, col="blue") +
      geom_point(data=as.data.frame(S2), shape=4, col="red") +
      labs(x="longitude", y="latitude") +
      coord_fixed() +
      theme_light()
  }
  superimpose(S1, S2)
}

sampdata <- function(dt, S)
{
  tidx <- sample(c(TRUE, TRUE), size=nrow(dt), 
                 replace=TRUE, prob=c(3, 1))
  sidx <- nncross(X=S, Y=ppp(longlat$longitude, longlat$latitude, 
                            window=as.owin(W)), what="which")
  dt %>% 
    mutate(y=if_else((sid %in% sidx) & tidx, y, NA)) %>%
    na.omit() %>%
    mutate(s1idx=as.numeric(factor(sid)),
           t1idx=as.numeric(factor(tid)),
           s2idx=as.numeric(factor(sid)),
           t2idx=as.numeric(factor(tid)),
           iidx=1:nrow(.))
}

fitfun <- function(dt, dat, domain=as.owin(W), verbose=FALSE)
{
  dt1 <- dt
  D <- as.polygonal(domain)$bdry[[1]]
  mh <- fm_mesh_2d_inla(
    loc=cbind(dt1$longitude, dt1$latitude),
    loc.domain=cbind(D$x, D$y), 
    max.edge=min(diff(domain$xrange), diff(domain$yrange)) / 10#,
    #cutoff= 0.1
  )
  spde <- inla.spde2.matern(mesh=mh, alpha=2, constr=TRUE)
  fit <- inla(y ~ 1 + tavg + perc + 
                f(s1idx, skin_temperature, model=spde) + 
                f(t1idx, skin_temperature, model="ar1", constr=TRUE) +
                f(s2idx, total_precipitation, model=spde) +
                f(t2idx, total_precipitation, model="ar1", constr=TRUE),# +
                #f(iidx),
              data=dt1,
              family="poisson",
              control.predictor=list(link=1),
              verbose=verbose, num.threads=6)
  mj <- inla.mesh.projector(mh, loc=cbind(dat$longitude, dat$latitude))
  b1shat <- inla.mesh.project(mj, field=fit$summary.random$s1idx$mean)
  b1that <- fit$summary.random$t1idx$mean[dat$tid]
  b2shat <- inla.mesh.project(mj, field=fit$summary.random$s2idx$mean)
  b2that <- fit$summary.random$t2idx$mean[dat$tid]
  yhat <- fit$summary.fitted.values$mean
  return(list(b1shat=b1shat, b1that=b1that,
              b2shat=b2shat, b2that=b2that,
              gammahat=fit$summary.fixed$mean,
              thetahat=fit$summary.hyperpar$mean,
              y=dt1$y, yhat=yhat, 
              cputime=fit$cpu.used["Total"]))
}

simfun <- function(beta0=3, 
                   beta1pars=c(mu=0.5, var=0.05, scale=0.8, nu=1, 
                               arsig=0.05, arphi=0.5),
                   beta2pars=c(mu=0.5, var=0.05, scale=1, nu=1, 
                               arsig=0.05, arphi=0.5),
                   xipars=c(var=0.05, scale=0.1, nu=1),
                   n=200, cvp=0.2)
{
  dat <- envars
  b1s <- rGRFmatern(W=owin(xrange=W$xrange, yrange=W$yrange), 
                    mu=0, 
                    var=beta1pars["var"], 
                    scale=beta1pars["scale"], 
                    nu=beta1pars["nu"], dimyx=dim(W))
  b2s <- rGRFmatern(W=owin(xrange=W$xrange, yrange=W$yrange), 
                    mu=0, 
                    var=beta2pars["var"], 
                    scale=beta2pars["scale"], 
                    nu=beta2pars["nu"], dimyx=dim(W))
  b1t <- arima.sim(model=list(ar=beta1pars["arphi"]), 
                   n=max(envars$tid), 
                   sd=beta1pars["arsig"])
  b2t <- arima.sim(model=list(ar=beta2pars["arphi"]), 
                   n=max(envars$tid), 
                   sd=beta2pars["arsig"])
  dat <- dat %>% 
    mutate(b1s=interp.im(b1s, st_coordinates(envars)),
           b1s=b1s - mean(b1s),
           b2s=interp.im(b2s, st_coordinates(envars)),
           b2s=b2s - mean(b2s),
           b1t=b1t[tid],
           b1t=b1t - mean(b1t),
           b2t=b2t[tid],
           b2t=b2t - mean(b2t), 
           xi=rnorm(nrow(.), mean=0, 
                    sd=sqrt(xipars["var"]))) %>%
    mutate(eta=beta0 + 
             beta1pars["mu"] * tavg + 
             (b1s + b1t) * skin_temperature + 
             beta2pars["mu"] * perc + 
             (b2s + b2t) * total_precipitation,# + xi,
           y=rpois(n=nrow(.), lambda=exp(eta)))

  S <- samplocs(n=n, grid_percent=0.6)
  dt <- sampdata(dat, S=S)
  fit <- fitfun(dt, dat)
  e1s <- (fit$b1shat - dat$b1s)
  e1t <- (fit$b1that - dat$b1t)
  e2s <- (fit$b2shat - dat$b2s)
  e2t <- (fit$b2that - dat$b2t)
  mse1s <- mean((e1s)^2)
  mse1t <- mean((e1t)^2)
  mse1 <- mean((e1s + e1t)^2)
  mse2s <- mean((e2s)^2)
  mse2t <- mean((e2t)^2)
  mse2 <- mean((e2s + e2t)^2)
  ep <- (fit$y - fit$yhat) / (fit$y + 0.5 * (fit$y == 0))
  mpe <- mean(ep^2)
  idx <- sample(unique(dt$sid), size=round(n * cvp))
  dt0 <- dt %>% 
    mutate(y=if_else(sid %in% idx, NA, y))
  fit0 <- fitfun(dt0, dat)
  e1s0 <- (fit0$b1shat - dat$b1s)
  e1t0 <- (fit0$b1that - dat$b1t)
  e2s0 <- (fit0$b2shat - dat$b2s)
  e2t0 <- (fit0$b2that - dat$b2t)
  cvmse1s <- mean((e1s0)^2)
  cvmse1t <- mean((e1t0)^2)
  cvmse1 <- mean((e1s0 + e1t0)^2)
  cvmse2s <- mean((e2s0)^2)
  cvmse2t <- mean((e2t0)^2)
  cvmse2 <- mean((e2s0 + e2t0)^2)
  ep0 <- (dt$y[dt$sid %in% idx] - fit0$yhat[dt$sid %in% idx]) / 
    (dt$y[dt$sid %in% idx] +  0.5 * (dt$y[dt$sid %in% idx] == 0))
  cvmpe <- mean(ep0^2)
  return(list(b1s=b1s, b1t=b1t, b2s=b2s, b2t=b2t, 
              e1s=e1s, e1t=e1t, e2s=e2s, e2t=e2t, 
              theta=fit$thetahat, 
              mse1s=mse1s, mse1t=mse1t, mse1=mse1,
              mse2s=mse2s, mse2t=mse2t, mse2=mse2,
              cvmse1s=cvmse1s, cvmse1t=cvmse1t, cvmse1=cvmse1,
              cvmse2s=cvmse2s, cvmse2t=cvmse2t, cvmse2=cvmse2,
              mpe=mpe, cvmpe=cvmpe, cputime=fit$cputime))
}
# =========================================================
inla.setOption(inla.timeout=60, safe=TRUE, silent=FALSE)

nsim <- 500
#nn <- c(50, 100, 150, 200, 250, 300)
nn <- c(50, 100, 150, 200, 250)
vv <- c(0.025, 0.05 , 0.1)
mse1s <- mse1t <- mse1 <- mse2s <-  mse2t <- mse2 <-
  mpe <- cputime <- array(dim=c(length(nn), length(vv), 2))
dimnames(mse1s) <- dimnames(mse1t) <- dimnames(mse1) <- 
  dimnames(mse2s) <- dimnames(mse2t) <- dimnames(mse2) <- 
  dimnames(mpe) <- dimnames(cputime) <- list("n"=nn, "var"=vv, "type"=c("all", "cv"))
for (i in 1:length(nn))
{
  for (j in 1:length(vv))
  {
    cat("i: ", i, "j: ", j, "\tn =" , nn[i], "\tv =" , vv[j], "\n")
    simres <- vector("list", length=nsim)
    for (k in 1:nsim)
    {
      simres[[k]] <- 
        simfun(beta0=3, 
               beta1pars=c(mu=0.5, var=vv[j], scale=1.5, nu=1, 
                           arsig=0.1, arphi=0.75),
               beta2pars=c(mu=0.5, var=0.05, scale=1.5, nu=1, 
                           arsig=0.1, arphi=0.75),
               xipars=c(var=0.0125, scale=0.15, nu=1),
               n=nn[i])
      
      progressreport(k, nsim)
    }
    mse1s[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse1s })))
    mse1t[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse1t })))
    mse1[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse1 })))
    mse2s[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse2s })))
    mse2t[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse2t })))
    mse2[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse2 })))
    
    mse1s[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse1s })))
    mse1t[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse1t })))
    mse1[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse1 })))
    mse2s[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse2s })))
    mse2t[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse2t })))
    mse2[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse2 })))
    
    mpe[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mpe })))
    mpe[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmpe })))
    
    cputime[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$cputime })))

    print(c(mse1[i, j, 1], mse2[i, j, 1], mpe[i, j, 2]))
    print(cputime[i, j, 1])
  }
}

saveRDS(mse1, "~/mse1.rds")
saveRDS(mse2, "~/mse2.rds")
saveRDS(mpe, "~/mpe.rds")


library("ggpubr")
g1 <- 
  as.data.frame.table(mse1) %>%
  filter(type == "all") %>%
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=MSE)) +
  geom_line(aes(col=var, linetype=var)) +
  theme_classic2()

g2 <- 
  as.data.frame.table(mse2) %>%
  filter(type == "all") %>%
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=MSE)) +
  geom_line(aes(col=var, linetype=var)) +
  theme_classic2()

g3 <-
  as.data.frame.table(mpe) %>%
  filter(type == "cv") %>%
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MPE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=MPE)) +
  geom_line(aes(col=var, linetype=var)) +
  theme_classic2()

ggarrange(g1, g2, g3, nrow=1, labels = c("a", "b", "c"),
          common.legend = TRUE, legend = "bottom")
ggsave("~/simmoz.pdf", width=10, height=3.25)

as.data.frame.table(mse1s) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=(MSE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mse1t) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=(MSE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mse2s) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=(MSE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mse2t) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=MSE)) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mpe) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MPE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=(MPE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

