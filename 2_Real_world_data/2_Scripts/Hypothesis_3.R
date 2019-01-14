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
n_random <- 199
max_runs <- 10000
fitting <- TRUE

# reconstruct pattern

# small
reconstructed_beech_small <- SHAR::reconstruct_pattern(pattern = beech_small, 
                                                       n_random = n_random, 
                                                       max_runs = max_runs, 
                                                       fitting = fitting)

# beech_small <- rep(beech_small, n_random)
# reconstructed_beech_small <- clustermq::Q(fun = reconstruct_pattern, 
#                                           pattern = beech_small,
#                                           const = list(n_random = 1, 
#                                                        max_runs = max_runs, 
#                                                        fitting = fitting,
#                                                        verbose = FALSE),
#                                           seed = 42, 
#                                           n_jobs = n_random, 
#                                           template = list(queue = "mpi", 
#                                                           walltime = "48:00", 
#                                                           processes = 1))

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_small, 
                           filename = "reconstructed_beech_small.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# medium
reconstructed_beech_medium <- SHAR::reconstruct_pattern(pattern = beech_medium, 
                                                        n_random = n_random, 
                                                        max_runs = max_runs, 
                                                        fitting = fitting)

# beech_medium <- rep(beech_medium, n_random)
# reconstructed_beech_medium <- clustermq::Q(fun = reconstruct_pattern, 
#                                            pattern = beech_medium,
#                                            const = list(n_random = 1, 
#                                                         max_runs = max_runs, 
#                                                         fitting = fitting, 
#                                                         verbose = FALSE),
#                                            seed = 42, 
#                                            n_jobs = n_random, 
#                                            template = list(queue = "mpi", 
#                                                            walltime = "48:00", 
#                                                            processes = 1))

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_medium, 
                           filename = "reconstructed_beech_medium.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# large
reconstructed_beech_large <- SHAR::reconstruct_pattern(pattern = beech_large, 
                                                        n_random = n_random, 
                                                        max_runs = max_runs, 
                                                        fitting = fitting)

# beech_large <- rep(beech_large, n_random)
# reconstructed_beech_large <- clustermq::Q(fun = reconstruct_pattern, 
#                                           pattern = beech_large,
#                                           const = list(n_random = 1, 
#                                                        max_runs = max_runs, 
#                                                        fitting = fitting),
#                                           seed = 42, 
#                                           n_jobs = n_random, 
#                                           template = list(queue = "mpi", 
#                                                           walltime = "48:00", 
#                                                           processes = 1))

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_large, 
                           filename = "reconstructed_beech_large.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))


#### Environmental data ####

environmental_data <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    
    data <- readr::read_rds(x)
    
    environment_raster <- raster::rasterFromXYZ(data)
    
    SHAR::classify_habitats(environment_raster, classes = 5)
  })

#### Habitat associations ####

# reconstructed_beech_small <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
# reconstructed_beech_medium <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_medium.rds"))
# reconstructed_beech_large <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_large.rds"))

names_environment <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

# small
associations_beech_small <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_beech_small,
                                    raster = x, 
                                    verbose = FALSE)
})

# medium
associations_beech_medium <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_beech_medium,
                                    raster = x, 
                                    verbose = FALSE)
})

# large
associations_beech_large <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_beech_large,
                                    raster = x, 
                                    verbose = FALSE)
})

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

