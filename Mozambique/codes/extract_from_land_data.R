
library("tidyverse")
# =========================================================

# read the satellite land data
land_data <- 
  readRDS(url("https://github.com/jalilian/EASF/raw/main/Mozambique/cds_land_data.rds"))

# takes time (year, month) and location (longitude, latitude)
# returns environmental covariates from the satellite land data
get_covars <- function(data_table, land_data)
{
  # check if required columns exist
  required_cols <- c("Longitude", "Latitude", "Year", "Month")
  if (!all(required_cols %in% colnames(data_table))) 
  {
    stop("data table must contain columns with names: ", 
         paste(required_cols, collapse = ", "))
  }

  # grid of unique land data coordinates
  land_grid <- expand_grid(x0=unique(land_data %>% 
                                       pull(longitude)),
                           y0=unique(land_data %>% 
                                       pull(latitude)))
  
  # coordinates of the data site
  x1 <- data_table %>% pull(Longitude)
  y1 <- data_table %>% pull(Latitude)
  
  # squared Euclidean distance for all data locations to land data coordinates
  dd <- outer(x1, land_grid$x0, "-")^2 +
    outer(y1, land_grid$y0, "-")^2
  
  # index of the closest land data location for each data location
  idx <- apply(dd, 1, which.min)
  land_grid <- land_grid %>%
    slice(idx)
  
  # extract land data for each data location and time
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
  
  # Combine data_table and covariate data
  data_table <- data_table %>% 
    bind_cols(covars)
  return(data_table)
}

# read adaptive table
data_path <- "~/Downloads/Mozambique/"
adaptive_table <- 
  readRDS(paste0(data_path, "adaptive_table.rds"))
# read adaptive grid data
adaptive_grid <- 
  readRDS(paste0(data_path, "adaptive_grid.rds"))


# extract land data for the adaptive table
adaptive_table_covars <- 
  get_covars(adaptive_table, land_data)

# extract land data for the adaptive grid
adaptive_grid_covars <- 
  get_covars(adaptive_grid, land_data)

# save data table and grid with covariates
saveRDS(adaptive_table_covars, 
        file=paste0(data_path, "adaptive_table_coavrs.rds"))

saveRDS(adaptive_grid_covars, 
        file=paste0(data_path, "adaptive_grid_coavrs.rds"))
