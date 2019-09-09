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

#### Environmental data ####
# 
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

# imporrt MRT classification
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

# random_habitats_species <- shar::randomize_raster(classification_raster_list$species, n_random = n_random_large)
# 
# suppoRt::save_rds(object = random_habitats_species, 
#                            filename = "random_habitats_species.rds", 
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                            overwrite = FALSE)

random_habitats_species <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_species.rds"))

#### Habitat associations ####

# import reconstructed pattern
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))

reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))

reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))

reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))

reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))

# import fitted pattern
fitted_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech.rds"))

fitted_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_ash.rds"))

fitted_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_hornbeam.rds"))

fitted_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_sycamore.rds"))

fitted_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_others.rds"))

# Beech
# associations_beech <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech) <- names_environment

# calculate associations with MRT map
associations_beech_reconstruced <- shar::results_habitat_association(pattern = reconstructed_beech, 
                                                                     raster = classification_raster_list$species)

associations_beech_fitted <- shar::results_habitat_association(pattern = fitted_beech, 
                                                               raster = classification_raster_list$species)

associations_beech_walk <- shar::results_habitat_association(pattern = beech, 
                                                             raster = random_habitats_species)


# Ash
# associations_ash <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_ash,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_ash) <- names_environment

# calculate associations with MRT map
associations_ash_reconstruced <- shar::results_habitat_association(pattern = reconstructed_ash, 
                                                                   raster = classification_raster_list$species)

associations_ash_fitted <- shar::results_habitat_association(pattern = fitted_ash, 
                                                             raster = classification_raster_list$species)

associations_ash_walk <- shar::results_habitat_association(pattern = ash, 
                                                           raster = random_habitats_species)

# Hornbeam
# associations_hornbeam <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_hornbeam,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_hornbeam) <- names_environment

# calculate associations with MRT map
associations_hornbeam_reconstruced <- shar::results_habitat_association(pattern = reconstructed_hornbeam, 
                                                                        raster = classification_raster_list$species)

associations_hornbeam_fitted <- shar::results_habitat_association(pattern = fitted_hornbeam, 
                                                                  raster = classification_raster_list$species)

associations_hornbeam_walk <- shar::results_habitat_association(pattern = hornbeam, 
                                                                raster = random_habitats_species)

# Sycamore
# associations_sycamore <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_sycamore,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_sycamore) <- names_environment

# calculate associations with MRT map
associations_sycamore_reconstruced <- shar::results_habitat_association(pattern = reconstructed_sycamore, 
                                                                        raster = classification_raster_list$species)

associations_sycamore_fitted <- shar::results_habitat_association(pattern = fitted_sycamore,
                                                                  raster = classification_raster_list$species)

associations_sycamore_walk <- shar::results_habitat_association(pattern = sycamore, 
                                                                raster = random_habitats_species)

# others
# associations_others <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_others,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_others) <- names_environment

# calculate associations with MRT map
associations_others_reconstruced <- shar::results_habitat_association(pattern = reconstructed_others, 
                                                                      raster = classification_raster_list$species)

associations_others_fitted <- shar::results_habitat_association(pattern = fitted_others, 
                                                                raster = classification_raster_list$species)

associations_others_walk <- shar::results_habitat_association(pattern = others, 
                                                              raster = random_habitats_species)

#### Save results

overwrite <- FALSE

# reconstructed data
suppoRt::save_rds(object = associations_beech_reconstruced, 
                 filename = "associations_beech_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_ash_reconstruced, 
                 filename = "associations_ash_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_hornbeam_reconstruced, 
                 filename = "associations_hornbeam_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_sycamore_reconstruced, 
                 filename = "associations_sycamore_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_others_reconstruced, 
                 filename = "associations_others_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# fitted data 
suppoRt::save_rds(object = associations_beech_fitted, 
                 filename = "associations_beech_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_ash_fitted, 
                 filename = "associations_ash_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_hornbeam_fitted, 
                 filename = "associations_hornbeam_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_sycamore_fitted, 
                 filename = "associations_sycamore_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_others_fitted, 
                 filename = "associations_others_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# random walk data
suppoRt::save_rds(object = associations_beech_walk, 
                 filename = "associations_beech_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_ash_walk, 
                 filename = "associations_ash_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_hornbeam_walk, 
                 filename = "associations_hornbeam_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_sycamore_walk, 
                 filename = "associations_sycamore_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

suppoRt::save_rds(object = associations_others_walk, 
                 filename = "associations_others_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)
