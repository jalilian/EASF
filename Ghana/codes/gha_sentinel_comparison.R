

library("readxl")
library("tidyverse")
library("sf")

sdata <- list()
sdata[[1]] <- read_excel("~/Downloads/Ghana/Control sites HLC.xlsx",
                         sheet="Sheet 1 - events 2")
sdata[[2]] <- read_excel("~/Downloads/Ghana/Control sites CID.xlsx",
                         sheet="Sheet 1 - events")
