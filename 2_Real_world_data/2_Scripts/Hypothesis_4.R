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
library(SHAR) # devtools::install_github("r-spatialecology/SHAR")
library(spatstat)
library(tidyverse)

#### Pattern reconstruction ####

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007, Species == "Beech")

# split into living and dead
beech_living <- spatstat::subset.ppp(beech, Type != "dead")

beech_dead <- spatstat::subset.ppp(beech, Type == "dead")

# set parameters
n_random <- 199
max_runs <- 10000
fitting <- TRUE

# reconstruct pattern

# living
reconstructed_beech_living <- SHAR::reconstruct_pattern(pattern = beech_living, 
                                                        n_random = n_random, 
                                                        max_runs = max_runs, 
                                                        fitting = fitting, 
                                                        comp_fast = TRUE)

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_living, 
                           filename = "reconstructed_beech_living.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# dead
reconstructed_beech_dead <- SHAR::reconstruct_pattern(pattern = beech_dead, 
                                                      n_random = n_random, 
                                                      max_runs = max_runs, 
                                                      fitting = fitting)

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech_dead, 
                           filename = "reconstructed_beech_dead.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

#### Environmental data ####

environmental_data <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    
    data <- readr::read_rds(x)
    
    environment_raster <- raster::rasterFromXYZ(data)
    
    SHAR::classify_habitats(environment_raster, classes = 5)
  })

#### Habitat associations ####

# reconstructed_beech_living <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_living.rds"))
# reconstructed_beech_dead <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech_dead.rds"))

names_environment <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

# living
associations_beech_living <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_beech_living,
                                    raster = x, 
                                    verbose = FALSE)
})

# dead
associations_beech_dead <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_beech_dead,
                                    raster = x, 
                                    verbose = FALSE)
})

#### Save results

UtilityFunctions::save_rds(object = associations_beech_living, 
                           filename = "associations_beech_living.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))


UtilityFunctions::save_rds(object = associations_beech_dead, 
                           filename = "associations_beech_dead.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))


