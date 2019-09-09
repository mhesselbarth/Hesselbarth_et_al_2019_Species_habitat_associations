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

#### Environmental data ####

# environmental_data <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_', full.names = TRUE) %>%
#   purrr::map(function(x) {
#     
#     data <- readr::read_rds(x)
#     
#     environment_raster <- raster::rasterFromXYZ(data)
#     
#     shar::classify_habitats(environment_raster, classes = 5)
#   })
# 
# names_environment <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

# import MRT classified map
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

# random_habitats_status <- shar::randomize_raster(classification_raster_list$status, n_random = n_random_large)
# 
# suppoRt::save_rds(object = random_habitats_status, 
#                            filename = "random_habitats_status.rds", 
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                            overwrite = FALSE)

# random_habitats_status <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_status.rds"))

random_habitats_species <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_species.rds"))


#### Habitat associations ####

# import reconstructed patterns
reconstructed_beech_living <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_living.rds"))

reconstructed_beech_dead <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_dead.rds"))

# import fitted patterns
fitted_beech_living <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_living.rds"))

fitted_beech_dead <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_dead.rds"))

# living
# associations_beech_living <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech_living,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_living) <- names_environment

# associations between MRT map and pattern
associations_beech_living_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_living, 
                                                                            raster = classification_raster_list$species)

associations_beech_living_fitted <- shar::results_habitat_association(pattern = fitted_beech_living, 
                                                                      raster = classification_raster_list$species)

associations_beech_living_walk <- shar::results_habitat_association(pattern = beech_living,
                                                                    raster = random_habitats_species)

# dead
# associations_beech_dead <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech_dead,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_dead) <- names_environment

# associations between MRT map and pattern
associations_beech_dead_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_dead, 
                                                                          raster = classification_raster_list$species)

associations_beech_dead_fitted <- shar::results_habitat_association(pattern = fitted_beech_dead, 
                                                                    raster = classification_raster_list$species)

associations_beech_dead_walk <- shar::results_habitat_association(pattern = beech_dead,
                                                                  raster = random_habitats_species)

#### Save results

overwrite <- FALSE

# reconstructed data
suppoRt::save_rds(object = associations_beech_living_reconstruced, 
                 filename = "associations_beech_living_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_dead_reconstruced, 
                 filename = "associations_beech_dead_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# fitted data 
suppoRt::save_rds(object = associations_beech_living_fitted, 
                 filename = "associations_beech_living_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_dead_fitted, 
                 filename = "associations_beech_dead_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# walk data 
suppoRt::save_rds(object = associations_beech_living_walk, 
                 filename = "associations_beech_living_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_dead_walk, 
                 filename = "associations_beech_dead_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)
