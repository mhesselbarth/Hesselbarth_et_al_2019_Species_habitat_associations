###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 1 ####

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

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")
pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")

# set parameters
n_random <- 199
max_runs <- 10000
fitting <- TRUE

# Beech
beech <- spatstat::subset.ppp(pattern_2007_living, Species == "Beech")

# reconstruct pattern
beech_reconstructed <- SHAR::reconstruct_pattern(pattern = beech, 
                                                 n_random = n_random, 
                                                 max_runs = max_runs, 
                                                 fitting = fitting, 
                                                 comp_fast = TRUE)

# save reconstructed pattern
UtilityFunctions::save_rds(object = beech_reconstructed, 
                           filename = "beech_reconstructed.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Ash
ash <- subset.ppp(pattern_2007_living, Species == "Ash")

# reconstruct pattern
ash_reconstructed <- SHAR::reconstruct_pattern(pattern = ash, 
                                               n_random = n_random, 
                                               max_runs = max_runs, 
                                               fitting = fitting, 
                                               comp_fast = TRUE)

# save reconstructed pattern
UtilityFunctions::save_rds(object = ash_reconstructed, 
                           filename = "ash_reconstructed.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Hornbeam
hornbeam <- subset.ppp(pattern_2007_living, Species == "Hornbeam")

# reconstruct pattern
hornbeam_reconstructed <- SHAR::reconstruct_pattern(pattern = hornbeam, 
                                                    n_random = n_random, 
                                                    max_runs = max_runs, 
                                                    fitting = fitting, 
                                                    comp_fast = TRUE)

# save reconstructed pattern
UtilityFunctions::save_rds(object = hornbeam_reconstructed, 
                           filename = "hornbeam_reconstructed.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Sycamore
sycamore <- subset.ppp(pattern_2007_living, Species == "Sycamore")

# reconstruct pattern
sycamore_reconstructed <- SHAR::reconstruct_pattern(pattern = sycamore, 
                                                    n_random = n_random, 
                                                    max_runs = max_runs, 
                                                    fitting = fitting, 
                                                    comp_fast = TRUE)

# save reconstructed pattern
UtilityFunctions::save_rds(object = sycamore_reconstructed, 
                           filename = "sycamore_reconstructed.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# others
others <- subset.ppp(pattern_2007_living, Species == "others")

# reconstruct pattern
others_reconstructed <- SHAR::reconstruct_pattern(pattern = others, 
                                                  n_random = n_random, 
                                                  max_runs = max_runs, 
                                                  fitting = fitting, 
                                                  comp_fast = TRUE)

# save reconstructed pattern
UtilityFunctions::save_rds(object = others_reconstructed, 
                           filename = "others_reconstructed.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

#### Environmental data ####

environmental_data <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    
    data <- readr::read_rds(x)
    
    environment_raster <- raster::rasterFromXYZ(data)
    
    SHAR::classify_habitats(environment_raster, classes = 5)
  })

#### Habitat associations ####

# beech_reconstructed <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/beech_reconstructed.rds"))
# ash_reconstructed <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/ash_reconstructed.rds"))
# hornbeam_reconstructed <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/hornbeam_reconstructed.rds"))
# sycamore_reconstructed <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/sycamore_reconstructed.rds"))
# others_reconstructed <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/others_reconstructed.rds"))

names_environment <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

# Beech
associations_beech <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = beech_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_beech) <- names_environment

# Ash
associations_ash <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = ash_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_ash) <- names_environment

# Hornbeam
associations_hornbeam <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = hornbeam_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_hornbeam) <- names_environment

# Sycamore
associations_sycamore <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = sycamore_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_sycamore) <- names_environment

# others
associations_others <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = others_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_others) <- names_environment

#### Save results

UtilityFunctions::save_rds(object = associations_beech, 
                           filename = "associations_beech.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_ash, 
                           filename = "associations_ash.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_hornbeam, 
                           filename = "associations_hornbeam.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_sycamore, 
                           filename = "associations_sycamore.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_others, 
                           filename = "associations_others.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))
