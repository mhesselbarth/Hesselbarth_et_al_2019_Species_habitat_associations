###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 4 ####

# Load packages #
# library(clustermq)

library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

#### Import data ####

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007, Species == "Beech")

# split into living and dead
beech_living <- spatstat::unmark(spatstat::subset.ppp(beech, Type != "dead"))

beech_dead <- spatstat::unmark(spatstat::subset.ppp(beech, Type == "dead"))

#### Pattern reconstruction ####
# 
# # set parameters
# # n_random <- 199 # 199
# n_random_hpc <- rep(1, 199) # if HPC is used
# n_random_large <- 4999
# 
# max_runs <- 20000 # 20000
# 
# fitting <- TRUE # TRUE
# 
# comp_fast <- 0
# 
# # reconstruct pattern
# 
# # living
# # reconstructed_beech_living <- shar::reconstruct_pattern(pattern = beech_living,
# #                                                         n_random = n_random,
# #                                                         max_runs = max_runs,
# #                                                         fitting = fitting,
# #                                                         comp_fast = TRUE)
# 
# reconstructed_beech_living <- clustermq::Q(fun = reconstruct_pattern,
#                                            n_random = n_random_hpc,
#                                            const = list(pattern = beech_living,
#                                                         max_runs = max_runs,
#                                                         fitting = fitting,
#                                                         comp_fast = comp_fast,
#                                                         return_input = FALSE,
#                                                         simplify = TRUE,
#                                                         verbose = FALSE),
#                                            seed = 42,
#                                            n_jobs = length(n_random),
#                                            template = list(queue = "mpi",
#                                                            walltime = "48:00",
#                                                            processes = 1))
# 
# # add observed pattern
# reconstructed_beech_living[[length(n_random) + 1]] <- spatstat::unmark(beech_living)
# 
# # add names to list
# names(reconstructed_beech_living) <- c(paste0("randomized_", seq_along(n_random)),
#                                        "observed")
# 
# fitted_beech_living <- shar::fit_point_process(beech_living, n_random = n_random_large, 
#                                                process = "cluster")
# 
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_beech_living,
#                            filename = "reconstructed_beech_living.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = fitted_beech_living,
#                            filename = "fitted_beech_living.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # dead
# # reconstructed_beech_dead <- shar::reconstruct_pattern(pattern = beech_dead,
# #                                                       n_random = n_random,
# #                                                       max_runs = max_runs,
# #                                                       fitting = fitting)
# 
# reconstructed_beech_dead <- clustermq::Q(fun = reconstruct_pattern,
#                                          n_random = n_random_hpc,
#                                          const = list(pattern = beech_dead,
#                                                       max_runs = max_runs,
#                                                       fitting = fitting,
#                                                       comp_fast = comp_fast,
#                                                       return_input = FALSE,
#                                                       simplify = TRUE,
#                                                       verbose = FALSE),
#                                          seed = 42,
#                                          n_jobs = length(n_random),
#                                          template = list(queue = "mpi",
#                                                          walltime = "48:00",
#                                                          processes = 1))
# 
# # add observed pattern
# reconstructed_beech_dead[[length(n_random) + 1]] <- spatstat::unmark(beech_dead)
# 
# # add names to list
# names(reconstructed_beech_dead) <- c(paste0("randomized_", seq_along(n_random)),
#                                      "observed")
# 
# fitted_beech_dead <- shar::fit_point_process(beech_dead, n_random = n_random_large,
#                                              process = "cluster")
# 
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_beech_dead,
#                            filename = "reconstructed_beech_dead.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = fitted_beech_dead,
#                            filename = "fitted_beech_dead.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))

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
# UtilityFunctions::save_rds(object = random_habitats_status, 
#                            filename = "random_habitats_status.rds", 
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
#                            overwrite = FALSE)

random_habitats_status <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/random_habitats_status.rds"))

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
associations_beech_living <- shar::results_habitat_association(pattern = reconstructed_beech_living, 
                                                               raster = classification_raster_list$status)

associations_beech_living_fitted <- shar::results_habitat_association(pattern = fitted_beech_living, 
                                                                      raster = classification_raster_list$status)

associations_beech_living_walk <- shar::results_habitat_association(pattern = beech_living,
                                                                    raster = random_habitats_status)

# dead
# associations_beech_dead <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech_dead,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_dead) <- names_environment

# associations between MRT map and pattern
associations_beech_dead <- shar::results_habitat_association(pattern = reconstructed_beech_dead, 
                                                             raster = classification_raster_list$status)

associations_beech_dead_fitted <- shar::results_habitat_association(pattern = fitted_beech_dead, 
                                                                    raster = classification_raster_list$status)

associations_beech_dead_walk <- shar::results_habitat_association(pattern = beech_dead,
                                                                  raster = random_habitats_status)

#### Save results

overwrite <- FALSE

# reconstructed data
UtilityFunctions::save_rds(object = associations_beech_living, 
                           filename = "associations_beech_living.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_beech_dead, 
                           filename = "associations_beech_dead.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

# fitted data 
UtilityFunctions::save_rds(object = associations_beech_living_fitted, 
                           filename = "associations_beech_living_fitted.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_beech_dead_fitted, 
                           filename = "associations_beech_dead_fitted.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

# walk data 
UtilityFunctions::save_rds(object = associations_beech_living_walk, 
                           filename = "associations_beech_living_walk.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_beech_dead_walk, 
                           filename = "associations_beech_dead_walk.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)
