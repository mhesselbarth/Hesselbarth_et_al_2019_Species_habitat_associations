###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 1 & 2 ####

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

# split into species
beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, species == "beech"))
ash <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "ash"))
hornbeam <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "hornbeam"))
sycamore <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "sycamore"))
others <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "others"))

#### Import environmental data ####
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

random_habitats_species <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_species.rds"))

#### Import randomized point pattern data ####

# import reconstructed pattern
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
# reconstructed_beech <- create_random_pat(x = reconstructed_beech, method = "reconstruct_pattern_cluster()")

reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))
# reconstructed_ash <- create_random_pat(x = reconstructed_ash, method = "reconstruct_pattern_cluster()")

reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))
# reconstructed_hornbeam <- create_random_pat(x = reconstructed_hornbeam, method = "reconstruct_pattern_cluster()")

reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))
# reconstructed_sycamore <- create_random_pat(x = reconstructed_sycamore, method = "reconstruct_pattern_cluster()")

reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))
# reconstructed_others <- create_random_pat(x = reconstructed_others, method = "reconstruct_pattern_cluster()")

# import fitted pattern
fitted_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech.rds"))

fitted_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_ash.rds"))

fitted_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_hornbeam.rds"))

fitted_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_sycamore.rds"))

fitted_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_others.rds"))

#### Habitat associations

# Beech
associations_beech_fitted <- shar::results_habitat_association(pattern = fitted_beech, 
                                                               raster = classification_raster_list$species)

associations_beech_walk <- shar::results_habitat_association(pattern = beech, 
                                                             raster = random_habitats_species)

associations_beech_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech, 
                                                                     raster = classification_raster_list$species)

# Ash
associations_ash_fitted <- shar::results_habitat_association(pattern = fitted_ash, 
                                                             raster = classification_raster_list$species)

associations_ash_walk <- shar::results_habitat_association(pattern = ash, 
                                                           raster = random_habitats_species)

associations_ash_reconstruced <- shar::results_habitat_association(pattern = reconstructed_ash, 
                                                                   raster = classification_raster_list$species)

# Hornbeam
associations_hornbeam_fitted <- shar::results_habitat_association(pattern = fitted_hornbeam, 
                                                                  raster = classification_raster_list$species)

associations_hornbeam_walk <- shar::results_habitat_association(pattern = hornbeam, 
                                                                raster = random_habitats_species)

associations_hornbeam_reconstruced <- shar::results_habitat_association(pattern = reconstructed_hornbeam, 
                                                                        raster = classification_raster_list$species)

# Sycamore
associations_sycamore_fitted <- shar::results_habitat_association(pattern = fitted_sycamore,
                                                                  raster = classification_raster_list$species)

associations_sycamore_walk <- shar::results_habitat_association(pattern = sycamore, 
                                                                raster = random_habitats_species)

associations_sycamore_reconstruced <- shar::results_habitat_association(pattern = reconstructed_sycamore, 
                                                                        raster = classification_raster_list$species)

# others
associations_others_fitted <- shar::results_habitat_association(pattern = fitted_others, 
                                                                raster = classification_raster_list$species)

associations_others_walk <- shar::results_habitat_association(pattern = others, 
                                                              raster = random_habitats_species)

associations_others_reconstruced <- shar::results_habitat_association(pattern = reconstructed_others, 
                                                                      raster = classification_raster_list$species)

#### Save results ####
# 
# overwrite <- FALSE
# 
# # reconstructed data
# suppoRt::save_rds(object = associations_beech_reconstruced, 
#                  filename = "associations_beech_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_ash_reconstruced, 
#                  filename = "associations_ash_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_hornbeam_reconstruced, 
#                  filename = "associations_hornbeam_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_sycamore_reconstruced, 
#                  filename = "associations_sycamore_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_others_reconstruced, 
#                  filename = "associations_others_reconstruced.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# # fitted data 
# suppoRt::save_rds(object = associations_beech_fitted, 
#                  filename = "associations_beech_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_ash_fitted, 
#                  filename = "associations_ash_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_hornbeam_fitted, 
#                  filename = "associations_hornbeam_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_sycamore_fitted, 
#                  filename = "associations_sycamore_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_others_fitted, 
#                  filename = "associations_others_fitted.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# # random walk data
# suppoRt::save_rds(object = associations_beech_walk, 
#                  filename = "associations_beech_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_ash_walk, 
#                  filename = "associations_ash_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_hornbeam_walk, 
#                  filename = "associations_hornbeam_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_sycamore_walk, 
#                  filename = "associations_sycamore_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
# 
# suppoRt::save_rds(object = associations_others_walk, 
#                  filename = "associations_others_walk.rds", 
#                  path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                  overwrite = overwrite)
