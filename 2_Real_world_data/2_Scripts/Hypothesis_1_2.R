###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 1 & 2 ####

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

# split into species
beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Beech"))
ash <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Ash"))
hornbeam <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Hornbeam"))
sycamore <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Sycamore"))
others <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "others"))

#### Pattern randomization ####

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
# # Beech
# 
# # reconstruct pattern
# reconstructed_beech <- shar::reconstruct_pattern(pattern = beech,
#                                                  n_random = n_random,
#                                                  max_runs = max_runs,
#                                                  fitting = fitting,
#                                                  comp_fast = TRUE)
# 
# reconstructed_beech <- clustermq::Q(fun = reconstruct_pattern,
#                                     n_random = n_random_hpc,
#                                     const = list(pattern = beech,
#                                                  max_runs = max_runs,
#                                                  fitting = fitting,
#                                                  comp_fast = comp_fast,
#                                                  verbose = FALSE,
#                                                  return_input = FALSE,
#                                                  simplify = TRUE),
#                                     seed = 42,
#                                     n_jobs = length(n_random),
#                                     template = list(queue = "mpi",
#                                                     walltime = "48:00",
#                                                     processes = 1))
# 
# # add observed pattern
# reconstructed_beech[[length(n_random) + 1]] <- beech
# 
# # add names to list
# names(reconstructed_beech) <- c(paste0("randomized_", seq_along(n_random)),
#                                 "observed")
# 
# # fit point process
# fitted_beech <- shar::fit_point_process(beech, n_random = n_random_large,
#                                         process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_beech,
#                            filename = "reconstructed_beech.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_beech,
#                            filename = "fitted_beech.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # Ash
# 
# # reconstruct pattern
# # reconstructed_ash <- shar::reconstruct_pattern(pattern = ash,
# #                                                n_random = n_random,
# #                                                max_runs = max_runs,
# #                                                fitting = fitting)
# 
# reconstructed_ash <- clustermq::Q(fun = reconstruct_pattern,
#                                   n_random = n_random_hpc,
#                                   const = list(pattern = ash,
#                                                max_runs = max_runs,
#                                                fitting = fitting,
#                                                comp_fast = comp_fast,
#                                                return_input = FALSE,
#                                                simplify = TRUE,
#                                                verbose = FALSE),
#                                   seed = 42,
#                                   n_jobs = length(n_random),
#                                   template = list(queue = "mpi",
#                                                   walltime = "48:00",
#                                                   processes = 1))
# 
# # add observed pattern
# reconstructed_ash[[length(n_random) + 1]] <- ash
# 
# # add names to list
# names(reconstructed_ash) <- c(paste0("randomized_", seq_along(n_random)),
#                               "observed")
# 
# # fit point process
# fitted_ash <- shar::fit_point_process(ash, n_random = n_random_large, 
#                                       process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_ash,
#                            filename = "reconstructed_ash.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_ash,
#                            filename = "fitted_ash.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # Hornbeam
# 
# # reconstruct pattern
# # reconstructed_hornbeam <- shar::reconstruct_pattern(pattern = hornbeam,
# #                                                     n_random = n_random,
# #                                                     max_runs = max_runs,
# #                                                     fitting = fitting)
# 
# reconstructed_hornbeam <- clustermq::Q(fun = reconstruct_pattern,
#                                        n_random = n_random_hpc,
#                                        const = list(pattern = hornbeam,
#                                                     max_runs = max_runs,
#                                                     fitting = fitting,
#                                                     comp_fast = comp_fast,
#                                                     return_input = FALSE,
#                                                     simplify = TRUE,
#                                                     verbose = FALSE),
#                                        seed = 42,
#                                        n_jobs = length(n_random),
#                                        template = list(queue = "mpi",
#                                                        walltime = "48:00",
#                                                        processes = 1))
# 
# # add observed pattern
# reconstructed_hornbeam[[length(n_random) + 1]] <- hornbeam
# 
# # add names to list
# names(reconstructed_hornbeam) <- c(paste0("randomized_", seq_along(n_random)),
#                                    "observed")
# 
# # fit point process
# fitted_hornbeam <- shar::fit_point_process(hornbeam, n_random = n_random_large, 
#                                            process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_hornbeam,
#                            filename = "reconstructed_hornbeam.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_hornbeam,
#                            filename = "fitted_hornbeam.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # Sycamore
# 
# # reconstruct pattern
# # reconstructed_sycamore <- shar::reconstruct_pattern(pattern = sycamore,
# #                                                     n_random = n_random,
# #                                                     max_runs = max_runs,
# #                                                     fitting = fitting)
# 
# reconstructed_sycamore <- clustermq::Q(fun = reconstruct_pattern,
#                                        n_random = n_random_hpc,
#                                        const = list(pattern = sycamore,
#                                                     max_runs = max_runs,
#                                                     fitting = fitting,
#                                                     comp_fast = comp_fast,
#                                                     return_input = FALSE,
#                                                     simplify = TRUE,
#                                                     verbose = FALSE),
#                                        seed = 42,
#                                        n_jobs = length(n_random),
#                                        template = list(queue = "mpi",
#                                                        walltime = "48:00",
#                                                        processes = 1))
# 
# # add observed pattern
# reconstructed_sycamore[[length(n_random) + 1]] <- sycamore
# 
# # add names to list
# names(reconstructed_sycamore) <- c(paste0("randomized_", seq_along(n_random)),
#                                    "observed")
# 
# # fit point process
# fitted_sycamore <- shar::fit_point_process(sycamore, n_random = n_random_large, 
#                                            process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_sycamore,
#                            filename = "reconstructed_sycamore.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_sycamore,
#                            filename = "fitted_sycamore.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # others
# 
# # reconstruct pattern
# # reconstructed_others <- shar::reconstruct_pattern(pattern = others,
# #                                                   n_random = n_random,
# #                                                   max_runs = max_runs,
# #                                                   fitting = fitting)
# 
# reconstructed_others <- clustermq::Q(fun = reconstruct_pattern,
#                                      n_random = n_random_hpc,
#                                      const = list(pattern = others,
#                                                   max_runs = max_runs,
#                                                   fitting = fitting,
#                                                   comp_fast = comp_fast,
#                                                   return_input = FALSE,
#                                                   simplify = TRUE,
#                                                   verbose = FALSE),
#                                      seed = 42,
#                                      n_jobs = length(n_random),
#                                      template = list(queue = "mpi",
#                                                      walltime = "48:00",
#                                                      processes = 1))
# 
# # add observed pattern
# reconstructed_others[[length(n_random) + 1]] <- others
# 
# # add names to list
# names(reconstructed_others) <- c(paste0("randomized_", seq_along(n_random)),
#                                  "observed")
# 
# # fit point process
# fitted_others <- shar::fit_point_process(others, n_random = n_random_large, 
#                                          process = "cluster")
# 
# # save reconstructed pattern
# helpeR::save_rds(object = reconstructed_others,
#                            filename = "reconstructed_others.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # save reconstructed pattern
# helpeR::save_rds(object = fitted_others,
#                            filename = "fitted_others.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
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

# imporrt MRT classification
classification_raster_list <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/classification_raster_list.rds"))

# random_habitats_species <- shar::randomize_raster(classification_raster_list$species, n_random = n_random_large)
# 
# helpeR::save_rds(object = random_habitats_species, 
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
helpeR::save_rds(object = associations_beech_reconstruced, 
                 filename = "associations_beech_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_ash_reconstruced, 
                 filename = "associations_ash_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_hornbeam_reconstruced, 
                 filename = "associations_hornbeam_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_sycamore_reconstruced, 
                 filename = "associations_sycamore_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_others_reconstruced, 
                 filename = "associations_others_reconstruced.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# fitted data 
helpeR::save_rds(object = associations_beech_fitted, 
                 filename = "associations_beech_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_ash_fitted, 
                 filename = "associations_ash_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_hornbeam_fitted, 
                 filename = "associations_hornbeam_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_sycamore_fitted, 
                 filename = "associations_sycamore_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_others_fitted, 
                 filename = "associations_others_fitted.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

# random walk data
helpeR::save_rds(object = associations_beech_walk, 
                 filename = "associations_beech_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_ash_walk, 
                 filename = "associations_ash_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_hornbeam_walk, 
                 filename = "associations_hornbeam_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_sycamore_walk, 
                 filename = "associations_sycamore_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)

helpeR::save_rds(object = associations_others_walk, 
                 filename = "associations_others_walk.rds", 
                 path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                 overwrite = overwrite)
