#### Simulation study - Strength associations ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### Import packages ####
# Packages #
library(furrr)
library(future)
library(future.batchtools)
library(NLMR)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/R_functions'), pattern = '.R', full.names = TRUE) %>%
  purrr::map(function(x) source(x))

# Set seed
set.seed(42)

#### Define parameters ####
# Number of coloumns and rows for neutral landscape
number_coloumns <- 30
number_rows <- 30
# Resolution of neutral landscape
resolution <- 20
# Roughness of neutral landscape
roughness <- 0.3
# Approxmitated number of points for each species
number_points <- 100 
# Number of runs
simulation_runs <- 50 # 50
# Number of randomized habitat maps / point patterns
number_maps <- 199 # 199
number_pattern <- 199 # 199
# Number of itertations pattern reconstruction
max_runs <- 2500 # 2500
# Different association strengths
alpha_sequence <- seq(0.25, 0.75, 0.025) # seq(0.25, 0.75, 0.025)

# Specify future topology
# login node -> { cluster nodes } -> { multiple cores }
login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl', 
                      resources = list(job.name = 'pattern_reconstruction',
                                       log.file = 'pattern_reconstruction.log',
                                       queue = 'mpi',
                                       walltime = '48:00',
                                       processes = 24))

future::plan(list(login, bsub, future::multiprocess))

# future::plan(future::multiprocess)

#### Simulation study of different methods to analyze species habitat assocations ####
# Habitat randomization (Harms et al. 2001) #
habitat_randomization %<-% {Simulation.Habitat.Randomization.Association.Strength(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  roughness = roughness,
  resolution = resolution,
  number_maps = number_maps,
  number_points = number_points,
  alpha_sequence = alpha_sequence,
  simulation_runs = simulation_runs)
}
print(habitat_randomization)

# Save.Function.rds(object=habitat_randomization,
#                   file=paste0(results,"/strength_association_habitat_randomization.rds"))


# Torus translation (Harms et al. 2001) #
torus_translation %<-% {Simulation.Torus.Translation.Association.Strength(
  number_coloumns=number_coloumns,
  number_rows=number_rows,
  roughness=roughness,
  resolution=resolution,
  number_points=number_points,
  alpha_sequence=alpha_sequence,
  simulation_runs=simulation_runs)
  }
print(torus_translation)
# Save.Function.rds(object=torus_translation,
#                   file=paste0(results,"/strength_association_torus_translation.rds"))


# Fitting point process (Plotkin et al. 2000) #
point_process %<-% {Simulation.Point.Process.Association.Strength(
  number_coloumns=number_coloumns,
  number_rows=number_rows,
  roughness=roughness,
  resolution=resolution,
  number_pattern=number_pattern,
  number_points=number_points,
  alpha_sequence=alpha_sequence,
  simulation_runs=simulation_runs)
  }
print(point_process)

# Save.Function.rds(object=point_process,
#                   file=paste0(results,"/strength_association_point_process.rds"))

# Pattern reconstruction #
pattern_reconstruction %<-% {Simulation.Pattern.Reconstruction.Association.Strength(
  number_coloumns=number_coloumns,
  number_rows=number_rows,
  roughness=roughness,
  resolution=resolution,
  number_pattern=number_pattern,
  number_points=number_points,
  simulation_runs=simulation_runs,
  max_runs=max_runs,
  alpha_sequence=alpha_sequence)
  }
print(pattern_reconstruction)

# Save.Function.rds(object=pattern_reconstruction,
#                   file=paste0(results,"/strength_association_pattern_reconstruction.rds"))
