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
library(NLMR)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)

source(paste0(getwd(), '/R_functions/Simulation_habitat_randomization.R'))
source(paste0(getwd(), '/R_functions/Simulation_torus_translation.R'))
source(paste0(getwd(), '/R_functions/Simulation_point_process.R'))
source(paste0(getwd(), '/R_functions/Simulation_pattern_reconstruction.R'))
source(paste0(getwd(), '/R_functions/Fit_point_process.R'))

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
simulation_runs <- 10 
# Number of randomized habitat maps / point patterns
number_maps <- 19 
number_pattern <- 19
# Number of itertations pattern reconstruction
max_runs <- 100
# Different association strengths
alpha_sequence <- seq(0.25, 0.75, 0.25) # seq(0.25, 0.75, 0.025)

workers <- c(4, 4, 4)


#### Simulation study of different methods to analyze species habitat assocations ####
# Habitat randomization (Harms et al. 2001) #
system.time(habitat_randomization <- Simulation.Habitat.Randomization(number_coloumns = number_coloumns,
                                                          number_rows = number_rows,
                                                          roughness = roughness,
                                                          resolution = resolution,
                                                          number_maps = number_maps,
                                                          number_points = number_points,
                                                          alpha_sequence = alpha_sequence,
                                                          simulation_runs = simulation_runs, 
                                                          workers = workers))

# Save.Function.rds(object=habitat_randomization,
#                   file=paste0(results,"/strength_association_habitat_randomization.rds"))


# Torus translation (Harms et al. 2001) #
system.time(torus_translation<- Simulation.Torus.Translation(number_coloumns=number_coloumns,
                                                 number_rows=number_rows,
                                                 roughness=roughness,
                                                 resolution=resolution,
                                                 number_points=number_points,
                                                 alpha_sequence=alpha_sequence,
                                                 simulation_runs=simulation_runs,
                                                 workers = workers))

# Save.Function.rds(object=torus_translation,
#                   file=paste0(results,"/strength_association_torus_translation.rds"))


# Fitting point process (Plotkin et al. 2000) #
system.time(point_process <- Simulation.Point.Process(number_coloumns=number_coloumns,
                                          number_rows=number_rows,
                                          roughness=roughness,
                                          resolution=resolution,
                                          number_pattern=number_pattern,
                                          number_points=number_points,
                                          alpha_sequence=alpha_sequence,
                                          simulation_runs=simulation_runs,
                                          workers = workers))

# Save.Function.rds(object=point_process,
#                   file=paste0(results,"/strength_association_point_process.rds"))

# Pattern reconstruction #
system.time(pattern_reconstruction <- Simulation.Pattern.Reconstruction(number_coloumns=number_coloumns,
                                                            number_rows=number_rows,
                                                            roughness=roughness,
                                                            resolution=resolution,
                                                            number_pattern=number_pattern,
                                                            number_points=number_points,
                                                            simulation_runs=simulation_runs,
                                                            max_runs=max_runs,
                                                            alpha_sequence=alpha_sequence,
                                                            workers = workers))

# Save.Function.rds(object=pattern_reconstruction,
#                   file=paste0(results,"/strength_association_pattern_reconstruction.rds"))
