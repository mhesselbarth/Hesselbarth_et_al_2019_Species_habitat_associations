###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 3 ####

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

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, type != "dead")

pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, type == "dead")

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007_living, species == "beech")

# split into DBH groups
beech_small <- spatstat::unmark(spatstat::subset.ppp(beech, dbh_group_07 == "small"))

beech_medium <- spatstat::unmark(spatstat::subset.ppp(beech, dbh_group_07 == "medium"))

beech_large <- spatstat::unmark(spatstat::subset.ppp(beech, dbh_group_07 == "large"))


#### import environmental data ####
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

# random_habitats_size <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_size.rds"))
random_habitats_species <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_species.rds"))

#### import randomized point pattern data ####

# import reconstructed pattern
reconstructed_beech_small <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_small.rds"))
reconstructed_beech_small <- create_random_pat(reconstructed_beech_small, method = "reconstruct_pattern_cluster()")

reconstructed_beech_medium <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_medium.rds"))
reconstructed_beech_medium <- create_random_pat(reconstructed_beech_medium, method = "reconstruct_pattern_cluster()")

reconstructed_beech_large <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_large.rds"))
reconstructed_beech_large <- create_random_pat(reconstructed_beech_large, method = "reconstruct_pattern_cluster()")

# import fitted pattern
fitted_beech_small <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_small.rds"))

fitted_beech_medium <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_medium.rds"))

fitted_beech_large <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_large.rds"))

#### Habitat associations ####

# Small #
associations_beech_small_fitted <- shar::results_habitat_association(pattern = fitted_beech_small, 
                                                                     raster = classification_raster_list$species)

associations_beech_small_walk <- shar::results_habitat_association(pattern = beech_small,
                                                                   raster = random_habitats_species)

associations_beech_small_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_small, 
                                                                           raster = classification_raster_list$species)

# Medium #
associations_beech_medium_fitted <- shar::results_habitat_association(pattern = fitted_beech_medium, 
                                                                      raster = classification_raster_list$species)

associations_beech_medium_walk <- shar::results_habitat_association(pattern = beech_medium,
                                                                    raster = random_habitats_species)

associations_beech_medium_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_medium, 
                                                                            raster = classification_raster_list$species)

# Large #
associations_beech_large_fitted <- shar::results_habitat_association(pattern = fitted_beech_large, 
                                                                     raster = classification_raster_list$species)

associations_beech_large_walk <- shar::results_habitat_association(pattern = beech_large,
                                                                   raster = random_habitats_species)

associations_beech_large_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_large, 
                                                                           raster = classification_raster_list$species)

#### Save results ####
# 
# overwrite <- FALSE
# 
# # reconstructed data
# suppoRt::save_rds(object = associations_beech_small_reconstruced, 
#                  filename = "associations_beech_small_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_medium_reconstruced, 
#                  filename = "associations_beech_medium_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_large_reconstruced, 
#                  filename = "associations_beech_large_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# # fitted data
# suppoRt::save_rds(object = associations_beech_small_fitted, 
#                  filename = "associations_beech_small_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_medium_fitted, 
#                  filename = "associations_beech_medium_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_large_fitted, 
#                  filename = "associations_beech_large_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# # random walk data
# suppoRt::save_rds(object = associations_beech_small_walk, 
#                  filename = "associations_beech_small_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_medium_walk, 
#                  filename = "associations_beech_medium_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_beech_large_walk, 
#                  filename = "associations_beech_large_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
