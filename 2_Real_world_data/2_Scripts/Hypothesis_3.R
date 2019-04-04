###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 3 ####

# Load packages #
# library(clustermq)

library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)


#### Import data #### 

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")

pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007_living, Species == "Beech")

# split into DBH groups
beech_small <- spatstat::unmark(spatstat::subset.ppp(beech, DBH_group == "small"))

beech_medium <- spatstat::unmark(spatstat::subset.ppp(beech, DBH_group == "medium"))

beech_large <- spatstat::unmark(spatstat::subset.ppp(beech, DBH_group == "large"))

#### Pattern randomization ####
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
# # small
# # reconstructed_beech_small <- shar::reconstruct_pattern(pattern = beech_small,
# #                                                        n_random = n_random,
# #                                                        max_runs = max_runs,
# #                                                        fitting = fitting)
# 
# reconstructed_beech_small <- clustermq::Q(fun = reconstruct_pattern,
#                                           n_random = n_random_hpc,
#                                           const = list(pattern = beech_small,
#                                                        max_runs = max_runs,
#                                                        fitting = fitting,
#                                                        comp_fast = comp_fast,
#                                                        return_input = FALSE,
#                                                        simplify = TRUE,
#                                                        verbose = FALSE),
#                                           seed = 42,
#                                           n_jobs = length(n_random),
#                                           template = list(queue = "mpi",
#                                                           walltime = "48:00",
#                                                           processes = 1))
# 
# # add observed pattern
# reconstructed_beech_small[[length(n_random) + 1]] <- beech_small
# 
# # add names to list
# names(reconstructed_beech_small) <- c(paste0("randomized_", seq_along(n_random)),
#                                       "observed")
# 
# fitted_beech_small <- shar::fit_point_process(beech_small, n_random = n_random_large, 
#                                               process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_beech_small,
#                            filename = "reconstructed_beech_small.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_beech_small,
#                            filename = "fitted_beech_small.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # medium
# # reconstructed_beech_medium <- shar::reconstruct_pattern(pattern = beech_medium,
# #                                                         n_random = n_random,
# #                                                         max_runs = max_runs,
# #                                                         fitting = fitting)
# 
# reconstructed_beech_medium <- clustermq::Q(fun = reconstruct_pattern,
#                                            n_random = n_random_hpc,
#                                            const = list(pattern = beech_medium,
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
# reconstructed_beech_medium[[length(n_random) + 1]] <- beech_medium
# 
# # add names to list
# names(reconstructed_beech_medium) <- c(paste0("randomized_", seq_along(n_random)),
#                                        "observed")
# 
# fitted_beech_medium <- shar::fit_point_process(beech_medium, n_random = n_random_large, 
#                                                process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_beech_medium,
#                            filename = "reconstructed_beech_medium.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_beech_medium,
#                            filename = "fitted_beech_medium.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # large
# # reconstructed_beech_large <- shar::reconstruct_pattern(pattern = beech_large,
# #                                                         n_random = n_random,
# #                                                         max_runs = max_runs,
# #                                                         fitting = fitting)
# 
# reconstructed_beech_large <- clustermq::Q(fun = reconstruct_pattern,
#                                           n_random = n_random_hpc,
#                                           const = list(pattern = beech_large,
#                                                        max_runs = max_runs,
#                                                        fitting = fitting,
#                                                        comp_fast = comp_fast,
#                                                        return_input = FALSE,
#                                                        simplify = TRUE,
#                                                        verbose = FALSE),
#                                           seed = 42,
#                                           n_jobs = length(n_random),
#                                           template = list(queue = "mpi",
#                                                           walltime = "48:00",
#                                                           processes = 1))
# 
# # add observed pattern
# reconstructed_beech_large[[length(n_random) + 1]] <- beech_large
# 
# # add names to list
# names(reconstructed_beech_large) <- c(paste0("randomized_", seq_along(n_random)),
#                                       "observed")
# 
# fitted_beech_large <- shar::fit_point_process(beech_large, n_random = n_random_large, 
#                                               process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_beech_large,
#                            filename = "reconstructed_beech_large.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_beech_large,
#                            filename = "fitted_beech_large.rds",
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

# import MRT classification
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

# random_habitats_size <- shar::randomize_raster(classification_raster_list$size, n_random = n_random_large)
# 
# helpeR::save_rds(object = random_habitats_size, 
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
helpeR::save_rds(object = associations_beech_small_reconstruced, 
                 filename = "associations_beech_small_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_beech_medium_reconstruced, 
                 filename = "associations_beech_medium_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_beech_large_reconstruced, 
                 filename = "associations_beech_large_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# fitted data
helpeR::save_rds(object = associations_beech_small_fitted, 
                 filename = "associations_beech_small_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_beech_medium_fitted, 
                 filename = "associations_beech_medium_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_beech_large_fitted, 
                 filename = "associations_beech_large_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# random walk data
helpeR::save_rds(object = associations_beech_small_walk, 
                 filename = "associations_beech_small_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_beech_medium_walk, 
                 filename = "associations_beech_medium_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_beech_large_walk, 
                 filename = "associations_beech_large_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

