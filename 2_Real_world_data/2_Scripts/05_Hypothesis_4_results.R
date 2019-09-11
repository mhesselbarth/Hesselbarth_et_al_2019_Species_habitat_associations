###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 4 ####

# Load packages #

library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

source("2_Real_world_data/2_Scripts/00_helper_functions.R")

#### Import point pattern data ####

# import point pattern data
pattern_2007 <- readr::read_rds("2_Real_world_data/1_Data/02_pattern_2007_ppp.rds")

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007, species == "beech")

# split into living and dead
beech_living <- spatstat::unmark(spatstat::subset.ppp(beech, type != "dead"))

beech_dead <- spatstat::unmark(spatstat::subset.ppp(beech, type == "dead"))

#### Import environmental data ####
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

random_habitats_species <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_species.rds"))

#### Import randomized point pattern data ####

# import reconstructed patterns
reconstructed_beech_living <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_living.rds"))
reconstructed_beech_living <- create_random_pat(reconstructed_beech_living, method = "reconstruct_pattern_cluster()")

reconstructed_beech_dead <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_dead.rds"))
reconstructed_beech_dead <- create_random_pat(reconstructed_beech_dead, method = "reconstruct_pattern_cluster()")

# import fitted patterns
fitted_beech_living <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_living.rds"))

fitted_beech_dead <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_dead.rds"))

#### Habitat associations ####

# Living #
associations_beech_living_fitted <- shar::results_habitat_association(pattern = fitted_beech_living, 
                                                                      raster = classification_raster_list$species)

associations_beech_living_walk <- shar::results_habitat_association(pattern = beech_living,
                                                                    raster = random_habitats_species)

associations_beech_living_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_living, 
                                                                            raster = classification_raster_list$species)

# Dead #

# associations between MRT map and pattern
associations_beech_dead_fitted <- shar::results_habitat_association(pattern = fitted_beech_dead, 
                                                                    raster = classification_raster_list$species)

associations_beech_dead_walk <- shar::results_habitat_association(pattern = beech_dead,
                                                                  raster = random_habitats_species)

associations_beech_dead_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_dead, 
                                                                          raster = classification_raster_list$species)

#### Save results ####
# 
# overwrite <- FALSE
# 
# # reconstructed data
# suppoRt::save_rds(object = associations_beech_living_reconstruced, 
#                  filename = "associations_beech_living_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_dead_reconstruced, 
#                  filename = "associations_beech_dead_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# # fitted data 
# suppoRt::save_rds(object = associations_beech_living_fitted, 
#                  filename = "associations_beech_living_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_dead_fitted, 
#                  filename = "associations_beech_dead_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# # walk data 
# suppoRt::save_rds(object = associations_beech_living_walk, 
#                  filename = "associations_beech_living_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_dead_walk, 
#                  filename = "associations_beech_dead_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
