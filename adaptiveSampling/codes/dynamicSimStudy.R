
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

envars <- readRDS(
  #url("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/envars_moz.rds")
  url("https://github.com/jalilian/EASF/raw/refs/heads/main/adaptiveSampling/envars_moz.rds")
  ) %>%
  na.omit() %>%
  # compute wind speed and Temp in centigrade
  mutate(wind_speed=sqrt(`10m_u_component_of_wind`^2 + 
                           `10m_v_component_of_wind`^2),
         skin_temperature=skin_temperature - 273.15) %>%
  # rescale the selected covariates to [-1, 1] interval
  mutate(skin_temperature=base::scale(skin_temperature, center=FALSE), 
         tavg=base::scale(tavg, center=FALSE),
         log_total_precipitation=
           base::scale(log(.Machine$double.eps + total_precipitation), 
                       center=TRUE),
         log_perc=
           base::scale(log(.Machine$double.eps + perc), center=TRUE),
         wind_speed=base::scale(wind_speed, center=FALSE), 
         wind=base::scale(wind, center=FALSE))

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

samplocs <- function(n, grid_percent=0.6, domain=as.owin(W), dimyx=dim(W))
{
  # number of points to place on a grid
  n_grid <- n * grid_percent
  # aspect ratio of the domain (height/width)
  apr <- dimyx[1] / dimyx[2]
  #  number of grid cells in the x and y directions
  nx <- sqrt(n_grid / apr)
  ny <- nx * apr
  # regular grid of points on the domain
  Q <- gridcentres(domain, nx=round(nx), ny=round(ny))
  # points falling inside the domain's boundary
  ok <- inside.owin(Q$x, Q$y, domain)
  superimpose(
    # grid points that are inside the domain 
    ppp(Q$x[ok], Q$y[ok], window=domain), 
    # randomly generated points to make up the rest of locations
    rpoint(n=n - sum(ok), win=domain)
  )
}

sampdata <- function(dt, S)
{
  tidx <- sample(c(TRUE, FALSE), size=nrow(dt), replace=TRUE, prob=c(1, 1))
  sidx <- nncross(X=S, Y=ppp(longlat$longitude, longlat$latitude, 
                            window=as.owin(W)), what="which")
  dt %>% 
    mutate(y=if_else((sid %in% sidx) & tidx, y, NA)) %>%
    na.omit() %>%
    mutate(s1idx=as.numeric(factor(sid)),
           t1idx=as.numeric(factor(tid)),
           s2idx=as.numeric(factor(sid)),
           t2idx=as.numeric(factor(tid)))
}

fitfun <- function(dt, domain=as.owin(W), verbose=FALSE)
{
  dt1 <- dt
  D <- as.polygonal(domain)$bdry[[1]]
  mh <- fm_mesh_2d_inla(
    loc=cbind(dt1$longitude, dt1$latitude),
    #boundary=cbind(D$x, D$y), 
    #max.edge = c(0.5, 1.2),
    cutoff= 0.1
  )
  spde <- inla.spde2.matern(mesh=mh, alpha=2, constr=TRUE)
  fit <- inla(y ~ 1 + tavg + log_perc + 
                f(s1idx, skin_temperature, model=spde) + 
                f(t1idx, skin_temperature, model="rw1") +
                f(s2idx, log_total_precipitation, model=spde) +
                f(t2idx, log_total_precipitation, model="rw1"), #+
                #f(iidx),
              data=dt1,
              family="poisson",
              control.predictor=list(link=1),
              verbose=verbose, num.threads=2)
  mj <- inla.mesh.projector(mh, loc=cbind(dt$longitude, dt$latitude))
  b1shat <- inla.mesh.project(mj, field=fit$summary.random$s1idx$mean)
  b1that <- fit$summary.random$t1idx$mean[dt$tid]
  b2shat <- inla.mesh.project(mj, field=fit$summary.random$s2idx$mean)
  b2that <- fit$summary.random$t2idx$mean[dt$tid]
  yhat <- fit$summary.fitted.values$mean
  return(list(b1shat=b1shat, b1that=b1that,
              b2shat=b2shat, b2that=b2that,
              gammahat=fit$summary.fixed$mean,
              thetahat=fit$summary.hyperpar$mean,
              y=dt1$y, yhat=yhat))
}

simfun <- function(beta0=1, 
                   beta1pars=c(mu=0.5, var=0.025, scale=0.8, nu=1, sig=0.05),
                   beta2pars=c(mu=0.5, var=0.025, scale=1, nu=1, sig=0.05),
                   xipars=c(var=0.05, scale=0.1, nu=1),
                   n=200, cvp=0.2)
{
  b1s <- rGRFmatern(W=owin(xrange=W$xrange, yrange=W$yrange), 
                    mu=0, 
                    var=beta1pars["var"], 
                    scale=beta1pars["scale"], 
                    nu=beta1pars["nu"], dimyx=dim(W))
  b1s <- interp.im(b1s, st_coordinates(envars))
  b1t <- cumsum(rnorm(max(envars$tid), mean=0, 
                      sd=beta1pars["sig"]))[envars$tid]
  b2s <- rGRFmatern(W=owin(xrange=W$xrange, yrange=W$yrange), 
                    mu=0, 
                    var=beta2pars["var"], 
                    scale=beta2pars["scale"], 
                    nu=beta2pars["nu"], dimyx=dim(W))
  b2s <- interp.im(b2s, st_coordinates(envars))
  b2t <- cumsum(rnorm(max(envars$tid), mean=0, 
                      sd=beta2pars["sig"]))[envars$tid]

  xi <- rnorm(nrow(envars), 0, sd=xipars["var"])
  eta <- beta0 + 
    beta1pars["mu"] * envars$tavg + 
    (b1s + b1t) * envars$skin_temperature + 
    beta2pars["mu"] * envars$log_perc + 
    (b2s + b2t) * envars$log_total_precipitation #+ xi
  
  y <- rpois(n=length(eta), lambda=exp(eta))
  S <- samplocs(n=n, grid_percent=0.6)
  dt <- sampdata(envars %>% mutate(y=y), S=S)
  fit <- fitfun(dt)
  e1s <- (fit$b1s - b1s)
  e1t <- (fit$b1that - b1t)
  e2s <- (fit$b2s - b2s)
  e2t <- (fit$b2that - b2t)
  mse1s <- mean((e1s)^2, na.rm=TRUE)
  mse1t <- mean((e1t)^2, na.rm=TRUE)
  mse2s <- mean((e2s)^2, na.rm=TRUE)
  mse2t <- mean((e2t)^2, na.rm=TRUE)
  ep <- (fit$y - fit$yhat) / (fit$y + 0.5 * (fit$y == 0))
  mpe <- mean(ep^2, na.rm=TRUE)
  idx <- sample(nrow(dt), size=round(n * cvp))
  dt0 <- dt
  dt0$y[idx] <- NA
  fit0 <- fitfun(dt0)
  e1s0 <- (fit0$b1shat - b1s)
  e1t0 <- (fit0$b1that - b1t)
  e2s0 <- (fit0$b2shat - b2s)
  e2t0 <- (fit0$b2that - b2t)
  cvmse1s <- mean((e1s0)^2, na.rm=TRUE)
  cvmse1t <- mean((e1t0)^2, na.rm=TRUE)
  cvmse2s <- mean((e2s0)^2, na.rm=TRUE)
  cvmse2t <- mean((e2t0)^2, na.rm=TRUE)
  ep0 <- (fit$y - fit0$yhat) / (fit0$y +  0.5 * (fit0$y == 0))
  cvmpe <- mean(ep0^2, na.rm=TRUE)
  return(list(b1s=b1s, b1t=b1t, b2s=b2s, b2t=b2t, 
              e1s=e1s, e1t=e1t, e2s=e2s, e2t=e2t, 
              theta=fit$thetahat, 
              mse1s=mse1s, mse1t=mse1t, mse2s=mse2s, mse2t=mse2t, 
              cvmse1s=cvmse1s, cvmse1t=cvmse1t,
              cvmse2s=cvmse2s, cvmse2t=cvmse2t,
              mpe=mpe, cvmpe=cvmpe))
}
# =========================================================
inla.setOption(inla.timeout=60, safe=TRUE, silent=FALSE)

nsim <- 500
#nn <- c(50, 100, 150, 200, 250, 300)
nn <- c(25, 50, 75, 100, 150, 200)
vv <- c(0.025, 0.05 , 0.1)
mse1s <- mse1t <- mse2s <-  mse2t <- 
  mpe <- array(dim=c(length(nn), length(vv), 2))
dimnames(mse1s) <- dimnames(mse1t) <- 
  dimnames(mse2s) <- dimnames(mse2t) <- 
  dimnames(mpe) <- list("n"=nn, "var"=vv, "type"=c("all", "cv"))
for (i in 1:length(nn))
{
  for (j in 1:length(vv))
  {
    cat("i: ", i, "j: ", j, "\tn =" , nn[i], "\tv =" , vv[j], "\n")
    simres <- vector("list", length=nsim)
    for (k in 1:nsim)
    {
      simres[[k]] <- 
        simfun(beta0=2, 
               beta1pars=c(mu=0.5, var=vv[j], scale=1.75, nu=1, sig=0.05),
               beta2pars=c(mu=0.5, var=0.05, scale=1.75, nu=1, sig=0.05),
               xipars=c(var=0.05, scale=0.15, nu=1),
               n=nn[i])
      
      progressreport(k, nsim)
    }
    mse1s[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse1s })))
    mse1t[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse1t })))
    mse2s[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse2s })))
    mse2t[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse2t })))
    
    mse1s[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse1s })))
    mse1t[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse1t })))
    mse2s[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse2s })))
    mse2t[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse2t })))

    mpe[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mpe })))
    mpe[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmpe })))
  }
}

saveRDS(mse1, "~/mse1.rds")
saveRDS(mse2, "~/mse2.rds")
saveRDS(mpe, "~/mpe.rds")


as.data.frame.table(mse1) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=log(MSE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mse2) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=log(MSE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mse3) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=MSE)) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mpe) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MPE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=log(MPE))) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

