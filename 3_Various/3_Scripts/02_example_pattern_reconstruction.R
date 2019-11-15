###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### 1. Import packages & functions ####

# Packages
library(mobsim)
library(NLMR)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(shar)
library(spatstat)
library(spex)
library(tidyverse)

# source all functions in R_functions folder
list.files(path = "1_Simulation_study/1_Functions/", full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

#### 2. Create example data ####

# create landscape
simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE, 
                                      cPrintlevel = 0) %>%
  shar::classify_habitats(classes = 5)

# create pattern with 4 species
simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 100, 
                                                association_strength = 0.35)

# pick species 2 as example
example_species <- spatstat::subset.ppp(simulation_pattern, species_code == 2)

#### Randomize data ####

# fit clustered pattern to data
gamma_test <- shar::fit_point_process(spatstat::unmark(example_species), 
                                      n_random = 199,
                                      process = "cluster")

pattern_reconstruction <- shar::reconstruct_pattern_cluster(spatstat::unmark(example_species), 
                                                            n_random = 199, 
                                                            max_runs = 20000, 
                                                            verbose = FALSE)

#### 3. Save results ####
overwrite <- FALSE

suppoRt::save_rds(object = gamma_test,
                 filename = "02_gamma_test.rds",
                 path = "3_Various/1_Results",
                 overwrite = overwrite)

suppoRt::save_rds(object = pattern_reconstruction,
                 filename = "02_pattern_reconstruction.rds",
                 path = "3_Various/1_Results",
                 overwrite = overwrite)

