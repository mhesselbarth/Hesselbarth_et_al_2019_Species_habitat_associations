#### Simulation study - Number habitats ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & sunctions ####

# Packages #
library(furrr)
library(future)
library(future.batchtools)
library(NLMR)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '.R', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Define parameters ####

# Set seed
seed <- set.seed(42)

# Number of coloumns and rows for neutral landscape
number_coloumns <- 30 # 30
number_rows <- 30 # 30
# Resolution of neutral landscape
resolution <- 20 # 20
# Roughness of neutral landscape
roughness <- 0.3 # 0.3
# Approxmitated number of points for each species
number_points <- 50 # 50 
# Number of runs
simulation_runs <- 3 # 50
# Number of randomized habitat maps / point patterns
number_maps <- 9 # 199
number_pattern <- 9 # 199
# Number of itertations pattern reconstruction
max_runs <- 5 # 2500
# Association strengths
alpha <- 0.350 # seq(0.25, 0.75, 0.025)
# Number of habitats
number_habitats <- c(2,5,10)

UtilityFunctions::save_rds(object = number_habitats,
                           filename = "number_habitats.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = TRUE)

#### 3. Specify future topology ####
# 
# future_map for 1) alpha (x) 2) simulation runs (y) 3) within null model function
# login node -> { cluster nodes } -> { multiple cores }
# 
# login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
# bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl',
#                       resources = list(job.name = 'pattern_reconstruction',
#                                        log.file = 'pattern_reconstruction.log',
#                                        queue = 'mpi-long',
#                                        walltime = '120:00',
#                                        processes = 24))
# 
# future::plan(list(login, bsub, future::multiprocess))
# 
# future::plan(list(future::multiprocess, future::multiprocess))
future::plan(future::multiprocess)
# 
#### 4. Simulation study of different methods to analyze species habitat assocations ####

# Habitat randomization (Harms et al. 2001) #
habitat_randomization %<-% {simulate_habitat_random_number_habitats(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  resolution = resolution,
  roughness = roughness,
  number_maps = number_maps,
  number_points = number_points,
  alpha = alpha,
  number_habitats = number_habitats,
  simulation_runs = simulation_runs)
}

# Torus translation (Harms et al. 2001) #
torus_translation %<-% {simulate_torus_trans_number_habitats(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  resolution = resolution,
  roughness = roughness,
  number_points = number_points,
  alpha = alpha,
  number_habitats = number_habitats,
  simulation_runs = simulation_runs)
}

# Fitting point process (Plotkin et al. 2000) #
point_process %<-% {simulate_point_process_number_habitats(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  resolution = resolution,
  roughness = roughness,
  number_pattern = number_pattern,
  number_points = number_points,
  alpha = alpha,
  number_habitats = number_habitats,
  simulation_runs = simulation_runs)
}

# Pattern reconstruction #
pattern_reconstruction %<-% {simulate_pattern_recon_number_habitats(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  resolution = resolution,
  roughness = roughness,
  number_pattern = number_pattern,
  number_points = number_points,
  alpha = alpha,
  number_habitats = number_habitats,
  simulation_runs = simulation_runs)
}

#### 5. Save data ####

UtilityFunctions::save_rds(object = habitat_randomization,
                           filename = paste0("2_habitat_randomization_", simulation_runs, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = torus_translation,
                           filename = paste0("2_torus_translation_", simulation_runs, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = point_process,
                           filename = paste0("2_point_process_", simulation_runs, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)

UtilityFunctions::save_rds(object=pattern_reconstruction,
                           filename = paste0("2_pattern_reconstruction_", simulation_runs, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)
