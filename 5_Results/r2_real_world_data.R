#### Results real world data #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
# Packages #
# library(clustermq)
library(UtilityFunctions)
library(SHAR)
library(spatstat)
library(raster)
library(tidyverse)

# Data #
reconstructions <- list.files(paste0(getwd(), '/4_Output/real_world'), pattern = 'o2_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

environmental_data <- list.files(paste0(getwd(), '/1_Data'), pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

DEM <- readr::read_rds(paste0(getwd(), '/1_Data/3b_DEM.rds'))

#### 2. Preprocessing data ####
# Point pattern
names_result <- list.files(paste0(getwd(), '/4_Output/real_world'), pattern = 'o2_', full.names = FALSE)
names_short <- stringr::str_sub(names_result, start = 1, end = -5)
names_split <- stringr::str_split(names_short, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3])

names(reconstructions) <- names_combined

# Environmetal data
names_result <- list.files(paste0(getwd(), '/1_Data'), pattern = '3_', full.names = FALSE)
names_short <- stringr::str_sub(names_result, start = 1, end = -5)
names_split <- stringr::str_split(names_short, pattern = "_", simplify = TRUE)

names(environmental_data) <- names_split[,2]

environmental_data <- purrr::map(environmental_data, function(x) {
  environment_raster <- raster::rasterFromXYZ(x)
  SHAR::classify_habitats(environment_raster, classes = 5)
  })

environmental_data$slope <- DEM$Slope.Habitat
environmental_data$elevation<- DEM$Elevation.Habitat

#### 3. ####

# Beech
# SHAR::calculate_energy(reconstructions$beech_reconstructed, comp_fast = TRUE, return_mean = TRUE)
# SHAR::plot_randomized_pattern(reconstructions$beech_reconstructed, comp_fast = TRUE)

associations_beech <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructions$beech_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

# Ash
# SHAR::calculate_energy(reconstructions$ash_reconstructed, comp_fast = TRUE, return_mean = TRUE)
# SHAR::plot_randomized_pattern(reconstructions$ash_reconstructed, comp_fast = TRUE, return_mean = TRUE)

associations_ash <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructions$ash_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

# Sycamore
# SHAR::calculate_energy(reconstructions$sycamore_reconstructed, comp_fast = TRUE, return_mean = TRUE)
# SHAR::plot_randomized_pattern(reconstructions$sycamore_reconstructed, comp_fast = TRUE, return_mean = TRUE)

associations_sycamore <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructions$sycamore_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

# Hornbeam
# SHAR::calculate_energy(reconstructions$hornbeam_reconstructed, comp_fast = TRUE, return_mean = TRUE)
# SHAR::plot_randomized_pattern(reconstructions$hornbeam_reconstructed, comp_fast = TRUE)

associations_hornbeam <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructions$hornbeam_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})

# others
# SHAR::calculate_energy(reconstructions$others_reconstructed, comp_fast = TRUE, return_mean = TRUE)
# SHAR::plot_randomized_pattern(reconstructions$others_reconstructed, comp_fast = TRUE)

associations_others <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructions$others_reconstructed,
                                    raster = x, 
                                    verbose = FALSE)
})
