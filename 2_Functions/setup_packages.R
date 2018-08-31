check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  print(sapply(pkg, require, character.only = TRUE))
}

packages<-c("furrr",
            "future.batchtools", 
            "NLMR", 
            "maptools",
            "mobsim", 
            "SHAR",
            "tidyverse", 
            "UtilityFunctions",
            "raster",
            "RColorBrewer",
            "sf",
            "sp",
            "spatstat",
            "spex"
)

check.packages(packages)
rm(packages)

library(furrr)
library(future.batchtools)
library(NLMR)
library(maptools)
library(mobsim)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)
library(raster)
library(RColorBrewer)
library(sf)
library(sp)
library(spatstat)
library(spex)
