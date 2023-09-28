
library("tidyverse")
# =========================================================

# read the satellite land data
land_data <- 
  readRDS(url("https://github.com/jalilian/EASF/raw/main/Mozambique/cds_land_data.rds"))

# takes time (year, month) and location (longitude, latitude)
# returns environmental covariates from the satellite land data
get_covars <- function(data_table, land_data)
{
  if (!all(c("Longitude", "Latitude", "Year", "Month") %in%
           colnames(data_table)))
  {
    stop("data table must conatin columns with the names: Longitude, Latitude, Year, and Month")
  }
  # coordinates of the land data
  land_grid <- expand_grid(x0=unique(land_data %>% 
                                       pull(longitude)),
                           y0=unique(land_data %>% 
                                       pull(latitude)))
  
  # coordinates of the sampling site
  x1 <- data_table %>% pull(Longitude)
  y1 <- data_table %>% pull(Latitude)
  
  # squared Eucleadian distance
  dd <- outer(x1, land_grid$x0, "-")^2 +
    outer(y1, land_grid$y0, "-")^2
  
  # for each sampling site find the closest land data location
  idx <- apply(dd, 1, which.min)
  land_grid <- land_grid %>%
    slice(idx)
  
  covars <- NULL
  for (i in 1:nrow(data_table))
  {
    dt <- data_table %>% 
      as_tibble() %>%
      slice(i) %>%
      select(Year, Month)
    dt <- ym(paste(dt, collapse = " ")) - months(0:3)
    dt <- str_split(format(dt, "%Y %B"), " ")
    
    a <- land_data %>% 
      filter(longitude == land_grid$x0[i],
             latitude == land_grid$y0[i])
    covars_i <- NULL
    for (j in 1:length(dt))
    {
      covars_i <- covars_i %>% 
        bind_cols( 
          a %>% 
            filter(year == dt[[j]][1],
                   month == dt[[j]][2]) %>%
            select(-(1:4)) %>%
            rename_with(~paste0(.x, "_", j - 1))
        )
      
    }
    covars <- covars %>%
      bind_rows(covars_i)
  }
  
  data_table <- data_table %>% 
    bind_cols(covars)
  return(data_table)
}



# read adaptive_table
data_path <- "~/Downloads/Mozambique/"
adaptive_table <- 
  readRDS(paste0(data_path, "adaptive_table.rds"))

# extract land data for the adaptive table
adaptive_table_covars <- 
  get_covars(adaptive_table, land_data)

saveRDS(adaptive_table_covars, 
        file=paste0(data_path, "adaptive_table_coavrs.rds"))
