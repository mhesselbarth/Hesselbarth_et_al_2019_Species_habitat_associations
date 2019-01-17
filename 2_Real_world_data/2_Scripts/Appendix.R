###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data -Appendix ####

# Load packages #
library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(SHAR) # devtools::install_github("r-spatialecology/SHAR")
library(spatstat)
library(tidyverse)

# Import data 
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))
reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))
reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))
reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))


full_patterns_list <- list(Beech = reconstructed_beech, 
                           Ash = reconstructed_ash, 
                           Hornbeam = reconstructed_hornbeam, 
                           Sycamore =reconstructed_sycamore,
                           others = reconstructed_others)

mean_energy_full_patterns <- purrr::map_dfr(full_patterns_list, function(x){
  data.frame(mean_energy = SHAR::calculate_energy(pattern = x, 
                                                  return_mean = TRUE, 
                                                  comp_fast = TRUE, 
                                                  verbose = FALSE))}, .id = "species")

