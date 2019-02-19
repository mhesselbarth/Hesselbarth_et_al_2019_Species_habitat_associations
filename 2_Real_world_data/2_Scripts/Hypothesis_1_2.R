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
# library(clustermq)

library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

#### Pattern reconstruction ####
# 
# # import point pattern data
# pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))
# 
# # split into living and dead
# pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")
# 
# pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")
# 
# # set parameters
# # n_random <- 199 # 199
# 
# n_random <- rep(1, 199) # if HPC is used
# 
# max_runs <- 20000 # 20000
# 
# fitting <- TRUE # TRUE
# 
# comp_fast <- 0
# 
# # Beech
# beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Beech"))
# 
# reconstruct pattern
# reconstructed_beech <- shar::reconstruct_pattern(pattern = beech,
#                                                  n_random = n_random,
#                                                  max_runs = max_runs,
#                                                  fitting = fitting,
#                                                  comp_fast = TRUE)
# 
# reconstructed_beech <- clustermq::Q(fun = reconstruct_pattern,
#                                     n_random = n_random,
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
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_beech,
#                            filename = "reconstructed_beech.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # Ash
# ash <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Ash"))
# 
# # reconstruct pattern
# # reconstructed_ash <- shar::reconstruct_pattern(pattern = ash,
# #                                                n_random = n_random,
# #                                                max_runs = max_runs,
# #                                                fitting = fitting)
# 
# reconstructed_ash <- clustermq::Q(fun = reconstruct_pattern,
#                                   n_random = n_random,
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
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_ash,
#                            filename = "reconstructed_ash.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # Hornbeam
# hornbeam <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Hornbeam"))
# 
# # reconstruct pattern
# # reconstructed_hornbeam <- shar::reconstruct_pattern(pattern = hornbeam,
# #                                                     n_random = n_random,
# #                                                     max_runs = max_runs,
# #                                                     fitting = fitting)
# 
# reconstructed_hornbeam <- clustermq::Q(fun = reconstruct_pattern,
#                                        n_random = n_random,
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
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_hornbeam,
#                            filename = "reconstructed_hornbeam.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # Sycamore
# sycamore <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Sycamore"))
# 
# # reconstruct pattern
# # reconstructed_sycamore <- shar::reconstruct_pattern(pattern = sycamore,
# #                                                     n_random = n_random,
# #                                                     max_runs = max_runs,
# #                                                     fitting = fitting)
# 
# reconstructed_sycamore <- clustermq::Q(fun = reconstruct_pattern,
#                                        n_random = n_random,
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
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_sycamore,
#                            filename = "reconstructed_sycamore.rds",
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"))
# 
# # others
# others <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "others"))
# 
# # reconstruct pattern
# # reconstructed_others <- shar::reconstruct_pattern(pattern = others,
# #                                                   n_random = n_random,
# #                                                   max_runs = max_runs,
# #                                                   fitting = fitting)
# 
# reconstructed_others <- clustermq::Q(fun = reconstruct_pattern,
#                                      n_random = n_random,
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
# # save reconstructed pattern
# UtilityFunctions::save_rds(object = reconstructed_others,
#                            filename = "reconstructed_others.rds",
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
soil_mrt_classified <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/soil_mrt_classified.rds"))

#### Habitat associations ####

# import reconstructed pattern
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))

reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))

reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))

reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))

reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))

# Beech
# associations_beech <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_beech,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech) <- names_environment

# calculate associations with MRT map
associations_beech <- shar::results_habitat_association(pattern = reconstructed_beech, 
                                                        raster = soil_mrt_classified)

# Ash
# associations_ash <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_ash,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_ash) <- names_environment

# calculate associations with MRT map
associations_ash <- shar::results_habitat_association(pattern = reconstructed_ash, 
                                                      raster = soil_mrt_classified)

# Hornbeam
# associations_hornbeam <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_hornbeam,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_hornbeam) <- names_environment

# calculate associations with MRT map
associations_hornbeam <- shar::results_habitat_association(pattern = reconstructed_hornbeam, 
                                                           raster = soil_mrt_classified)

# Sycamore
# associations_sycamore <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_sycamore,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_sycamore) <- names_environment

# calculate associations with MRT map
associations_sycamore <- shar::results_habitat_association(pattern = reconstructed_sycamore, 
                                                           raster = soil_mrt_classified)

# others
# associations_others <- purrr::map(environmental_data, function(x) {
#   shar::results_habitat_association(pattern = reconstructed_others,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_others) <- names_environment

# calculate associations with MRT map
associations_others <- shar::results_habitat_association(pattern = reconstructed_others, 
                                                         raster = soil_mrt_classified)

#### Save results

overwrite <- FALSE

UtilityFunctions::save_rds(object = associations_beech, 
                           filename = "associations_beech.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_ash, 
                           filename = "associations_ash.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_hornbeam, 
                           filename = "associations_hornbeam.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_sycamore, 
                           filename = "associations_sycamore.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = associations_others, 
                           filename = "associations_others.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"), 
                           overwrite = overwrite)
