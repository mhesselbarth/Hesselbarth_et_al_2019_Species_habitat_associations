###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 3 ####

# Load packages #
library(clustermq)

library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(raster)
library(SHAR) # devtools::install_github("r-spatialecology/SHAR")
library(spatstat)
library(tidyverse)

#### Pattern reconstruction ####

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")
pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007_living, Species == "Beech")

# split into DBH groups
beech_small <- spatstat::subset.ppp(beech, DBH_group == "small")

beech_medium <- spatstat::subset.ppp(beech, DBH_group == "medium")

beech_large <- spatstat::subset.ppp(beech, DBH_group == "large")

# set parameters
# n_random <- 199 # 199
n_random <- rep(1, 199) # if HPC is used
max_runs <- 10000 # 10000
fitting <- TRUE # TRUE
com_fast <- TRUE

# reconstruct pattern

# small
# reconstructed_beech_small <- SHAR::reconstruct_pattern(pattern = beech_small, 
#                                                        n_random = n_random, 
#                                                        max_runs = max_runs, 
#                                                        fitting = fitting)

reconstructed_beech_small <- clustermq::Q(fun = reconstruct_pattern,
                                          n_random = n_random,
                                          const = list(pattern = beech_small,
                                                       max_runs = max_runs,
                                                       fitting = fitting,
                                                       com_fast = comp_fast,
                                                       return_input = FALSE,
                                                       simplify = TRUE,
                                                       verbose = FALSE),
                                          seed = 42,
                                          n_jobs = length(n_random),
                                          template = list(queue = "mpi",
                                                          walltime = "48:00",
                                                          processes = 1))

reconstructed_beech_small[[length(n_random) + 1]] <- spatstat::unmark(beech_small)
names(reconstructed_beech_small) <- c(paste0("randomized_", seq_along(n_random)),
                                      "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_small, 
                           filename = "reconstructed_beech_small.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# medium
# reconstructed_beech_medium <- SHAR::reconstruct_pattern(pattern = beech_medium, 
#                                                         n_random = n_random, 
#                                                         max_runs = max_runs, 
#                                                         fitting = fitting)

reconstructed_beech_medium <- clustermq::Q(fun = reconstruct_pattern,
                                           n_random = n_random,
                                           const = list(pattern = beech_medium,
                                                        max_runs = max_runs,
                                                        fitting = fitting,
                                                        com_fast = comp_fast,
                                                        return_input = FALSE,
                                                        simplify = TRUE,
                                                        verbose = FALSE),
                                           seed = 42,
                                           n_jobs = length(n_random),
                                           template = list(queue = "mpi",
                                                           walltime = "48:00",
                                                           processes = 1))

reconstructed_beech_medium[[length(n_random) + 1]] <- spatstat::unmark(beech_medium)
names(reconstructed_beech_medium) <- c(paste0("randomized_", seq_along(n_random)),
                                       "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_medium, 
                           filename = "reconstructed_beech_medium.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# large
# reconstructed_beech_large <- SHAR::reconstruct_pattern(pattern = beech_large, 
#                                                         n_random = n_random, 
#                                                         max_runs = max_runs, 
#                                                         fitting = fitting)

reconstructed_beech_large <- clustermq::Q(fun = reconstruct_pattern,
                                          n_random = n_random,
                                          const = list(pattern = beech_large,
                                                       max_runs = max_runs,
                                                       fitting = fitting, 
                                                       com_fast = comp_fast,
                                                       return_input = FALSE,
                                                       simplify = TRUE, 
                                                       verbose = FALSE),
                                          seed = 42,
                                          n_jobs = length(n_random),
                                          template = list(queue = "mpi",
                                                          walltime = "48:00",
                                                          processes = 1))

reconstructed_beech_large[[length(n_random) + 1]] <- spatstat::unmark(beech_large)
names(reconstructed_beech_large) <- c(paste0("randomized_", seq_along(n_random)),
                                      "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_large, 
                           filename = "reconstructed_beech_large.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))


#### Environmental data ####

# environmental_data <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_', full.names = TRUE) %>%
#   purrr::map(function(x) {
#     
#     data <- readr::read_rds(x)
#     
#     environment_raster <- raster::rasterFromXYZ(data)
#     
#     SHAR::classify_habitats(environment_raster, classes = 5)
#   })
# 
# names_environment <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

soil_mrt_classified <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/soil_mrt_classified.rds"))

#### Habitat associations ####

# reconstructed_beech_small <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_small.rds"))
# reconstructed_beech_medium <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_medium.rds"))
# reconstructed_beech_large <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_large.rds"))

# small
# associations_beech_small <- purrr::map(environmental_data, function(x) {
#   SHAR::results_habitat_association(pattern = reconstructed_beech_small,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_small) <- names_environment

associations_beech_small <- SHAR::results_habitat_association(pattern = reconstructed_beech_small, 
                                                              raster = soil_mrt_classified)

# medium
# associations_beech_medium <- purrr::map(environmental_data, function(x) {
#   SHAR::results_habitat_association(pattern = reconstructed_beech_medium,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_medium) <- names_environment

associations_beech_medium <- SHAR::results_habitat_association(pattern = reconstructed_beech_medium, 
                                                               raster = soil_mrt_classified)

# large
# associations_beech_large <- purrr::map(environmental_data, function(x) {
#   SHAR::results_habitat_association(pattern = reconstructed_beech_large,
#                                     raster = x, 
#                                     verbose = FALSE)
# })
# 
# names(associations_beech_large) <- names_environment

associations_beech_large <- SHAR::results_habitat_association(pattern = reconstructed_beech_large, 
                                                              raster = soil_mrt_classified)

#### Save results

UtilityFunctions::save_rds(object = associations_beech_small, 
                           filename = "associations_beech_small.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))


UtilityFunctions::save_rds(object = associations_beech_medium, 
                           filename = "associations_beech_medium.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))


UtilityFunctions::save_rds(object = associations_beech_large, 
                           filename = "associations_beech_large.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

