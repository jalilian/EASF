
library("tidyverse")
library("sf")
library("spatstat")
library("fmesher")
library("INLA")
# =========================================================

if (FALSE)
{
  source(
    "https://github.com/jalilian/CEASE/raw/refs/heads/main/Ethiopia/codes/get_Copernicus_climate_data.R"
    )
  key <- "******************************"
  envars_gha <- get_cds(key, year=2024, month=7, day=1, what=cmap)
  saveRDS(envars_gha, "~/Documents/GitHub/EASF/adaptiveSampling/envars_gha.rds")

  # North, West, South, East
  area <- c(38, -20, -38, 51)
  a1 <- get_cds(key, year=2024, month=7, day=1, what=area)
}

# read environmental variables for Mozambique 
dat <- readRDS(
  url("https://github.com/jalilian/EASF/raw/main/Mozambique/cds_land_data.rds")
  ) %>% 
  # fix a time slot: June 2023 
  filter(year == 2023, month == "June") %>%
  # rescale the selected covariates to [-1, 1] interval
  mutate(skt_mean=scales::rescale(skt_mean, to=c(-1, 1)), 
         tp_mean=scales::rescale(tp_mean, to=c(-1, 1)), 
         lai_lv_mean=scales::rescale(lai_lv_mean, to=c(-1, 1)), 
         swvl1_mean=scales::rescale(swvl1_mean, to=c(-1, 1)))
# convert selected environmental variables to spatstat pixel images
z1 <- as.im(list(x=unique(dat$longitude), y=unique(dat$latitude), 
                z=matrix(dat$skt_mean, ncol=length(unique(dat$latitude)), 
                         byrow=TRUE)))
z2 <- as.im(list(x=unique(dat$longitude), y=unique(dat$latitude), 
                 z=matrix(dat$lai_lv_mean, ncol=length(unique(dat$latitude)), 
                          byrow=TRUE)))
z3 <- as.im(list(x=unique(dat$longitude), y=unique(dat$latitude), 
                 z=matrix(dat$swvl1_mean, ncol=length(unique(dat$latitude)), 
                          byrow=TRUE)))

par(mfrow=c(1, 3), mar=c(1, 0, 1, 0))
plot(z1, main="temprature", ribside="bottom", ribsep=0.02)
plot(z2, main="vegetation", ribside="bottom", ribsep=0.02)
plot(z3, main="surface wetness", ribside="bottom", ribsep=0.02)
# =========================================================

samplocs <- function(n, grid_percent=0.6, domain=as.owin(z1), dimyx=dim(z1))
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

fitfun <- function(dt, z1, z2, z3, domain=as.owin(z1))
{
  D <- as.polygonal(domain)$bdry[[1]]
  mh <- fm_mesh_2d_inla(loc=cbind(dt$longitude, dt$latitude),
                        loc.domain=cbind(D$x, D$y),
                        max.edge=0.8, offset=NULL)
  dt <- data.frame(longitude=mh$loc[, 1], 
                   latitude=mh$loc[, 2]) %>%
    mutate(z1=interp.im(z1, list(x=longitude, y=latitude)), 
           z2=interp.im(z2, list(x=longitude, y=latitude)),
           z3=interp.im(z3, list(x=longitude, y=latitude)),
           s1idx=1:mh$n, s2idx=1:mh$n, s3idx=1:mh$n) %>%
    left_join(dt, by=c("longitude", "latitude", "z1", "z2", "z3"))
  spde <- inla.spde2.matern(mesh=mh, alpha=2, constr=TRUE)
  fit <- inla(y ~ 1 + z1 + z2 + #z3 + 
                f(s1idx, z1, model=spde) + 
                f(s2idx, z2, model=spde),# + 
                #f(s3idx, z3, model=spde), 
              data=dt, family="poisson",
              control.predictor=list(link=1),
              verbose=TRUE)
             #silent=TRUE, num.threads=1)
  mj <- inla.mesh.projector(mh,  dims=rev(z1$dim))
  b1m <- inla.mesh.project(mj, fit$summary.random$s1idx$mean)
  b2m <- inla.mesh.project(mj, fit$summary.random$s2idx$mean)
  #b3m <- inla.mesh.project(mj, fit$summary.random$s3idx$mean)
  ym <- inla.mesh.project(mj, fit$summary.fitted.values$mean)
  W <- as.owin(z1)
  beta1hat <- as.im(list(x=mj$x, y=mj$y, z=b1m), W=W) +
    fit$summary.fixed$mean[1 + 1]
  beta2hat <- as.im(list(x=mj$x, y=mj$y, z=b2m), W=W) +
    fit$summary.fixed$mean[2 + 1]
  #beta3hat <- as.im(list(x=mj$x, y=mj$y, z=b3m), W=W) +
  #  fit$summary.fixed$mean[3 + 1]
  yhat <- as.im(list(x=mj$x, y=mj$y, z=ym), W=W)
  return(list(beta1hat=beta1hat, 
              beta2hat=beta2hat, 
              #beta3hat=beta3hat, 
              thetahat=fit$summary.hyperpar$mean,
              y=dt$y[!is.na(dt$y)], yhat=yhat))
}

simfun <- function(beta0=2, 
                   beta1pars=c(mu=1, var=0.1, scale=1.75, nu=1),
                   beta2pars=c(mu=1, var=0.1, scale=1.75, nu=1),
                   #beta3pars=c(mu=1, var=0.1, scale=1.5, nu=1),
                   n=200, cvp=0.2)
{
  beta1 <- rGRFmatern(W=owin(xrange=z1$xrange, yrange=z1$yrange), 
                      mu=beta1pars["mu"], 
                      var=beta1pars["var"], 
                      scale=beta1pars["scale"], 
                      nu=beta1pars["nu"], dimyx=z1$dim)
  beta2 <- rGRFmatern(W=owin(xrange=z2$xrange, yrange=z2$yrange), 
                      mu=beta2pars["mu"], 
                      var=beta2pars["var"], 
                      scale=beta2pars["scale"], 
                      nu=beta2pars["nu"], dimyx=z2$dim)
  #beta3 <- rGRFmatern(W=owin(xrange=z3$xrange, yrange=z3$yrange), 
  #                    mu=beta3pars["mu"], 
  #                    var=beta3pars["var"], 
  #                    scale=beta3pars["scale"], 
  #                    nu=beta3pars["nu"], dimyx=z3$dim)
  eta <- beta0 + beta1 * z1 + beta2 * z2 #+ beta3 * z3
  S <- samplocs(n=n, grid_percent=0.6)
  dt <- data.frame(longitude=S$x, latitude=S$y, 
                   z1=interp.im(z1, S), 
                   z2=interp.im(z2, S), 
                   z3=interp.im(z3, S), 
                   eta=interp.im(eta, S))
  dt <- na.omit(dt) %>% 
    mutate(y=rpois(n=length(eta), lambda=exp(eta)))
  fit <- fitfun(dt, z1, z2, z3)
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
  fit0 <- fitfun(dt[-idx, ], z1, z2, z3)
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

nsim <- 300
nn <- c(50, 100, 150, 200, 250, 300)
vv <- c(0.05, 0.1 , 0.2)
mse1 <- mse2 <-  #mse3 <- 
  mpe <- array(dim=c(length(nn), length(vv), 2))
dimnames(mse1) <- dimnames(mse2) <- #dimnames(mse3) <- 
  dimnames(mpe) <- list("n"=nn, "var"=vv, "type"=c("all", "cv"))
for (i in 1:length(nn))
{
  cat("i: ", i, "\tn[i]: " , nn[i], "\n")
  for (j in 1:length(vv))
  {
    cat("i: ", i, "\tn[i]: " , nn[i], "j: ", j, "\tv[j]: " , vv[j], "\n")
    simres <- vector("list", length=nsim)
    for (k in 1:nsim)
    {
      simres[[k]] <- simfun(beta0=2, 
                            beta1pars=c(mu=1, var=vv[j], scale=1.75, nu=1),
                            beta2pars=c(mu=1, var=0.1, scale=1.75, nu=1),
                            #beta3pars=c(mu=1, var=0.1, scale=1.5, nu=1),
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
  ggplot(aes(x=`sample size`, y=MSE)) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

as.data.frame.table(mse2) %>% 
  mutate(n=as.numeric(as.character(n))) %>%
  rename(MSE=Freq, "sample size"=n) %>%
  ggplot(aes(x=`sample size`, y=MSE)) +
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
  ggplot(aes(x=`sample size`, y=MPE)) +
  geom_line(aes(col=var, linetype=type)) +
  theme_light()

