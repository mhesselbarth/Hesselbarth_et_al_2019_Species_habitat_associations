###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 3 ####

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

# import MRT classification
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

# random_habitats_size <- shar::randomize_raster(classification_raster_list$size, n_random = n_random_large)
# 
# suppoRt::save_rds(object = random_habitats_size, 
#                            filename = "random_habitats_size.rds", 
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                            overwrite = FALSE)

# random_habitats_size <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_size.rds"))
random_habitats_species <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_species.rds"))


#### Habitat associations ####

# import reconstructed pattern
reconstructed_beech_small <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_small.rds"))

reconstructed_beech_medium <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_medium.rds"))

reconstructed_beech_large <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_large.rds"))

# import fitted pattern
fitted_beech_small <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_small.rds"))

fitted_beech_medium <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_medium.rds"))

fitted_beech_large <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech_large.rds"))

# small
# associations_beech_small <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech_small,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_small) <- names_environment

# get habitat associations to MRT map
associations_beech_small_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_small, 
                                                                           raster = classification_raster_list$species)

associations_beech_small_fitted <- shar::results_habitat_association(pattern = fitted_beech_small, 
                                                                     raster = classification_raster_list$species)

associations_beech_small_walk <- shar::results_habitat_association(pattern = beech_small,
                                                                   raster = random_habitats_species)

# medium
# associations_beech_medium <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech_medium,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_medium) <- names_environment

# get habitat associations to MRT map
associations_beech_medium_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_medium, 
                                                                            raster = classification_raster_list$species)

associations_beech_medium_fitted <- shar::results_habitat_association(pattern = fitted_beech_medium, 
                                                                      raster = classification_raster_list$species)

associations_beech_medium_walk <- shar::results_habitat_association(pattern = beech_medium,
                                                                    raster = random_habitats_species)

# large
# associations_beech_large <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech_large,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_large) <- names_environment

# get habitat associations to MRT map
associations_beech_large_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech_large, 
                                                                           raster = classification_raster_list$species)

associations_beech_large_fitted <- shar::results_habitat_association(pattern = fitted_beech_large, 
                                                                     raster = classification_raster_list$species)

associations_beech_large_walk <- shar::results_habitat_association(pattern = beech_large,
                                                                   raster = random_habitats_species)

#### Save results

overwrite <- FALSE

# reconstructed data
suppoRt::save_rds(object = associations_beech_small_reconstruced, 
                 filename = "associations_beech_small_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_medium_reconstruced, 
                 filename = "associations_beech_medium_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_large_reconstruced, 
                 filename = "associations_beech_large_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# fitted data
suppoRt::save_rds(object = associations_beech_small_fitted, 
                 filename = "associations_beech_small_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_medium_fitted, 
                 filename = "associations_beech_medium_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_large_fitted, 
                 filename = "associations_beech_large_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# random walk data
suppoRt::save_rds(object = associations_beech_small_walk, 
                 filename = "associations_beech_small_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_medium_walk, 
                 filename = "associations_beech_medium_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_beech_large_walk, 
                 filename = "associations_beech_large_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

