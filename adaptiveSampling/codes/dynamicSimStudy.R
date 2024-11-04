
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
  # compute wind speed and Temp in centigrade
  mutate(wind_speed=sqrt(`10m_u_component_of_wind`^2 + 
                           `10m_v_component_of_wind`^2),
         skin_temperature=skin_temperature - 273.15) %>%
  # rescale the selected covariates to [-1, 1] interval
  mutate(skin_temperature=base::scale(skin_temperature, center=FALSE), 
         tavg=base::scale(tavg, center=FALSE),
         total_precipitation=base::scale(total_precipitation, center=FALSE),
         perc=base::scale(perc, center=FALSE),
         wind_speed=base::scale(wind_speed, center=FALSE), 
         wind=base::scale(wind, center=FALSE))

longlat <- as.data.frame(st_coordinates(envars)) %>%
  rename(longitude=X, latitude=Y) %>%
  distinct() %>%
  mutate(sid=1:nrow(.))

envars <- envars %>% 
  st_join(st_as_sf(longlat, 
                     coords=c("longitude", "latitude"), 
                     crs=st_crs(envars)))

envars <- envars %>%
  mutate(tid=as.numeric(factor(date)))

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

sampdata <- function(envars, S)
{
  sidx <- nncross(X=S, Y=ppp(longlat$longitude, longlat$latitude, 
                            window=as.owin(W)), what="which")
  envars %>% 
    filter(sid %in% sidx)
}
fitfun <- function(dt, z1, z2, z3, x1, x2, x3, domain=as.owin(W), verbose=FALSE)
{
  D <- as.polygonal(domain)$bdry[[1]]
  mh <- fm_mesh_2d_inla(
    loc=cbind(dt$longitude, dt$latitude),
    boundary=cbind(D$x, D$y), 
    max.edge = c(0.5, 1.2),
    cutoff= 0.1, 
  )
  dt <- data.frame(longitude=mh$loc[, 1], 
                   latitude=mh$loc[, 2]) %>%
    mutate(z1=interp.im(z1, list(x=longitude, y=latitude)), 
           z2=interp.im(z2, list(x=longitude, y=latitude)),
           z3=interp.im(z3, list(x=longitude, y=latitude)),
           x1=interp.im(x1, list(x=longitude, y=latitude)), 
           x2=interp.im(x2, list(x=longitude, y=latitude)),
           x3=interp.im(x3, list(x=longitude, y=latitude)),
           s1idx=1:mh$n, s2idx=1:mh$n, s3idx=1:mh$n) %>%
    left_join(dt, by=c("longitude", "latitude", 
                       "z1", "z2", "z3", "x1", "x2", "x3"))
  spde <- inla.spde2.matern(mesh=mh, alpha=2, constr=TRUE)
  fit <- inla(y ~ 1 + z1 + z2 + #z3 + 
                f(s1idx, x1, model=spde) + 
                f(s2idx, x2, model=spde),# + 
                #f(s3idx, z3, model=spde), 
              data=dt, family="poisson",
              control.predictor=list(link=1),
              verbose=verbose, num.threads=2)
             #silent=TRUE, num.threads=1)
  mj <- inla.mesh.projector(mh,  dims=rev(z1$dim))
  b1m <- inla.mesh.project(mj, fit$summary.random$s1idx$mean)
  b2m <- inla.mesh.project(mj, fit$summary.random$s2idx$mean)
  #b3m <- inla.mesh.project(mj, fit$summary.random$s3idx$mean)
  ym <- inla.mesh.project(mj, fit$summary.fitted.values$mean)
  W <- as.owin(z1)
  beta1hat <- as.im(list(x=mj$x, y=mj$y, z=b1m), W=W) 
  beta2hat <- as.im(list(x=mj$x, y=mj$y, z=b2m), W=W) 
  #beta3hat <- as.im(list(x=mj$x, y=mj$y, z=b3m), W=W)
  yhat <- as.im(list(x=mj$x, y=mj$y, z=ym), W=W)
  return(list(beta1hat=beta1hat, 
              beta2hat=beta2hat, 
              #beta3hat=beta3hat, 
              gammahat=fit$summary.fixed$mean,
              thetahat=fit$summary.hyperpar$mean,
              y=dt$y[!is.na(dt$y)], yhat=yhat))
}

simfun <- function(beta0=3, 
                   beta1pars=c(mu=1, var=0.1, scale=0.8, nu=1),
                   beta2pars=c(mu=1, var=0.1, scale=1, nu=1),
                   #beta3pars=c(mu=1, var=0.1, scale=1.5, nu=1),
                   xipars=c(var=0.1, scale=0.1, nu=1),
                   n=200, cvp=0.2)
{
  beta1s <- rGRFmatern(W=owin(xrange=W$xrange, yrange=W$yrange), 
                      mu=0, 
                      var=beta1pars["var"], 
                      scale=beta1pars["scale"], 
                      nu=beta1pars["nu"], dimyx=dim(W))
  r1xy <- diff(W$xrange) / diff(W$yrange)
  if (r1xy < 1)
  {
    vcov <- c(r1xy, 1)
  } else{
    vcov <- c(1, r1xy)
  }
  beta1s <- Smooth(beta1s, normalise=TRUE, vcov=beta1pars["var"] * vcov)
  beta1t <- cumsum(rnorm(nt, mean=0, sd=sig1))
  beta2 <- rGRFmatern(W=owin(xrange=z2$xrange, yrange=z2$yrange), 
                      mu=0, 
                      var=beta2pars["var"], 
                      scale=beta2pars["scale"], 
                      nu=beta2pars["nu"], dimyx=z2$dim)
  r2xy <- diff(z2$xrange) / diff(z2$yrange)
  if (r2xy < 1)
  {
    vcov <- c(r2xy, 1)
  } else{
    vcov <- c(1, r2xy)
  }
  beta2 <- Smooth(beta2, normalise=TRUE, vcov=beta2pars["var"] * vcov)
  #beta3 <- rGRFmatern(W=owin(xrange=z3$xrange, yrange=z3$yrange), 
  #                    mu=beta3pars["mu"], 
  #                    var=beta3pars["var"], 
  #                    scale=beta3pars["scale"], 
  #                    nu=beta3pars["nu"], dimyx=z3$dim)
  xi <- rGRFmatern(W=owin(xrange=z3$xrange, yrange=z3$yrange), 
                      mu=0, var=xipars["var"], 
                      scale=xipars["scale"], 
                      nu=xipars["nu"], dimyx=z3$dim)
  eta <- beta0 + 
    beta1pars["mu"] * z1 + (beta1s + beta1t)* x1 + 
    beta2pars["mu"] * z2 + beta2 * x2 + #beta3 * z3 +
    xi
  S <- samplocs(n=n, grid_percent=0.6)
  dt <- data.frame(longitude=S$x, latitude=S$y, 
                   z1=interp.im(z1, S), 
                   z2=interp.im(z2, S), 
                   z3=interp.im(z3, S), 
                   x1=interp.im(x1, S), 
                   x2=interp.im(x2, S), 
                   x3=interp.im(x3, S), 
                   eta=interp.im(eta, S))
  dt <- na.omit(dt) %>% 
    mutate(y=rpois(n=length(eta), lambda=exp(eta)))
  fit <- fitfun(dt, z1, z2, z3, x1, x2, x3)
  e1 <- (fit$beta1hat - beta1)
  e2 <- (fit$beta2hat - beta2)
  #e3 <- (fit$beta3hat - beta3)
  xy <- list(x=dt$longitude, y=dt$latitude)
  mse1 <- mean((interp.im(e1, xy))^2)
  mse2 <- mean((interp.im(e2, xy))^2)
  #mse3 <- mean((interp.im(e3, xy))^2)
  ep <- (dt$y - interp.im(fit$yhat, xy)) / (dt$y + 0.5 * (dt$y == 0))
  mpe <- mean(ep^2, na.rm=TRUE)
  idx <- sample(nrow(dt), size=round(nrow(dt) * cvp))
  fit0 <- fitfun(dt[-idx, ], z1, z2, z3, x1, x2, x3)
  xyidx <- list(x=dt$longitude[idx], y=dt$latitude[idx])
  e10 <- (fit0$beta1hat - beta1)
  e20 <- (fit0$beta2hat - beta2)
  #e30 <- (fit0$beta3hat - beta3)
  cvmse1 <- mean((interp.im(e10, xyidx))^2)
  cvmse2 <- mean((interp.im(e20, xyidx))^2)
  #cvmse3 <- mean((interp.im(e30, xyidx))^2)
  ep0 <- (dt$y[idx] - interp.im(fit0$yhat, xyidx)) / 
    (dt$y[idx] +  0.5 * (dt$y[idx] == 0))
  cvmpe <- mean(ep0^2, na.rm=TRUE)
  return(list(beta1=beta1, beta2=beta2, #beta3=beta3, 
              e1=e1, e2=e2, #e3=e3, 
              theta=fit$thetahat, 
              mse1=mse1, mse2=mse2, #mse3=mse3, 
              cvmse1=cvmse1, cvmse2=cvmse2, #cvmse3=cvmse3, 
              mpe=mpe, cvmpe=cvmpe))
}
# =========================================================
inla.setOption(inla.timeout=60, safe=TRUE, silent=FALSE)

nsim <- 500
#nn <- c(50, 100, 150, 200, 250, 300)
nn <- c(50, 75, 100, 150, 200, 250)
vv <- c(0.05, 0.1 , 0.2)
mse1 <- mse2 <-  #mse3 <- 
  mpe <- array(dim=c(length(nn), length(vv), 2))
dimnames(mse1) <- dimnames(mse2) <- #dimnames(mse3) <- 
  dimnames(mpe) <- list("n"=nn, "var"=vv, "type"=c("all", "cv"))
for (i in 1:length(nn))
{
  for (j in 1:length(vv))
  {
    cat("i: ", i, "j: ", j, "\tn =" , nn[i], "\tv =" , vv[j], "\n")
    simres <- vector("list", length=nsim)
    for (k in 1:nsim)
    {
      simres[[k]] <- simfun(beta0=2, 
                            beta1pars=c(mu=1, var=vv[j], scale=1.75, nu=1),
                            beta2pars=c(mu=1, var=0.1, scale=1.75, nu=1),
                            #beta3pars=c(mu=1, var=0.1, scale=1.5, nu=1),
                            xipars=c(var=0.1, scale=0.15, nu=1),
                            n=nn[i])
      
      progressreport(k, nsim)
    }
    #simres <- parallel::mclapply(1:nsim, function(k){ 
    #  simfun(beta0=2, 
    #         beta1pars=c(mu=1, var=vv[j], scale=1.5),
    #         beta2pars=c(mu=1, var=0.1, scale=1.5),
    #         #beta3pars=c(mu=1, var=0.1, scale=1.5),
    #         n=nn[i]) 
    #}, mc.cores=6)
    mse1[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse1 })))
    mse1[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse1 })))
    mse2[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse2 })))
    mse2[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse2 })))
    #mse3[i, j, 1] <- mean(unlist(lapply(simres, function(o){ o$mse3 })))
    #mse3[i, j, 2] <- mean(unlist(lapply(simres, function(o){ o$cvmse3 })))
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

