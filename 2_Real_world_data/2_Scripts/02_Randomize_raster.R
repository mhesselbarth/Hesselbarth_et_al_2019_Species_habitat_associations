###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Classify habitats using MRT ####

#### Load packages ####
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt")
library(raster)
library(shar)
library(tidyverse)

# import data
classification_raster_list <- readr::read_rds("2_Real_world_data/3_Results/classification_raster_list.rds")

# subset only raster based on species
classification_raster_species <- classification_raster_list$species

random_habitats_species <- shar::randomize_raster(raster = classification_raster_species, 
                                                  n_random = 199)

shar::plot_randomized_raster(random_habitats_species, col = viridis::viridis(n = 4))

suppoRt::save_rds(object = random_habitats_species, 
                  filename = "random_habitats_species.rds", 
                  path = "2_Real_world_data/3_Results/")
