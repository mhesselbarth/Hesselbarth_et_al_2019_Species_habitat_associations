###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Simulation study ####

#### 1. Import packages & functions ####

# Packages
library(clustermq)

library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(maptools)
library(mobsim)
library(NLMR)
library(raster)
library(sf)
library(spatstat)
library(spex)
library(shar)
library(tidyverse)

# Source all functions in R_functions folder
list.files(paste0(getwd(), "/1_Simulation_study/1_Functions"), full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Define parameters ####

overwrite <- FALSE

# Set seed
# set.seed(42, kind = "L'Ecuyer-CMRG")

# Number of coloumns and rows for neutral landscape
number_coloumns <- 50 # 30
number_rows <- 50 # 30

# Resolution of neutral landscape
resolution <- 20 # 20

# fract_dim of neutral landscape
fract_dim <- 1.5 # 1.5

# Approxmitated number of points for each species
number_points <- 100 # 250 - 250 - 500???

# Number of randomized habitat maps / point patterns
n_random <- 199 # 199

# Number of itertations pattern reconstruction
max_runs <- 10000 # 20000

# Threshold for fast computation of summary functions
comp_fast <- 0

# Threshold to stop reconstruction if no change occured
no_change <- 5000

# Number of simulation runs
simulation_runs <- 50 # 100

# Different association strengths / repeat each strength simulation_runs times
# association_strength <- rep(seq(0, 1, 0.05), each = simulation_runs) # rep(seq(0, 1, 0.025), each = simulation_runs)
association_strength <- seq(0, 1, 0.05) # rep(seq(0, 1, 0.025), each = simulation_runs)


#### Create input data ####

# input_data <- create_input_data(number_coloumns = number_coloumns,
#                                 number_rows = number_rows,
#                                 resolution = resolution,
#                                 fract_dim = fract_dim,
#                                 number_points = number_points, 
#                                 association_strength = association_strength,
#                                 simulation_runs = simulation_runs, 
#                                 threshold = 0.75)
# 
# helpeR::save_rds(object = input_data, 
#                  filename = "input_data.rds", 
#                  path = "1_Simulation_study/3_Results", 
#                  overwrite = overwrite)

input_data <- readr::read_rds("1_Simulation_study/3_Results/input_data.rds")


#### 3. Run simulations using HPC (clustermq)

# parallelize each association strength and repetition

# Randomization algorithm
# habitat_randomization <- clustermq::Q(fun = simulate_habitat_random_association_strength,
#                                       association_strength = association_strength,
#                                       const = list(number_coloumns = number_coloumns,
#                                                    number_rows = number_rows,
#                                                    resolution = resolution,
#                                                    fract_dim = fract_dim,
#                                                    number_points = number_points,
#                                                    n_random = n_random),
#                                       export = list(create_simulation_species = create_simulation_species,
#                                                     create_simulation_pattern = create_simulation_pattern,
#                                                     detect_habitat_associations = detect_habitat_associations),
#                                       seed = 42,
#                                       n_jobs = length(association_strength),
#                                       job_size = 1,
#                                       template = list(queue = "mpi",
#                                                       walltime = "48:00",
#                                                       processes = 1))

habitat_randomization <- clustermq::Q(fun = simulate_habitat_random_association_strength,
                                      simulation_habitat = input_data$habitats,
                                      simulation_pattern = input_data$patterns,
                                      association_strength = rep(association_strength, each = simulation_runs),
                                      const = list(n_random = n_random),
                                      export = list(detect_habitat_associations = detect_habitat_associations),
                                      seed = 42,
                                      n_jobs = length(input_data$habitats),
                                      job_size = 1,
                                      template = list(queue = "mpi",
                                                      walltime = "12:00",
                                                      processes = 1))

# combine results to one data frame
habitat_randomization <- dplyr::bind_rows(habitat_randomization)

helpeR::save_rds(object = habitat_randomization,
                 filename = paste0("habitat_randomization_", simulation_runs, "_runs.rds"),
                 path = paste0(getwd(), "/1_Simulation_study/3_Results"),
                 overwrite = overwrite)

# Torus translation
# torus_translation <- clustermq::Q(fun = simulate_torus_trans_association_strength,
#                                   association_strength = association_strength,
#                                   const = list(number_coloumns = number_coloumns,
#                                                number_rows = number_rows,
#                                                resolution = resolution,
#                                                fract_dim = fract_dim,
#                                                number_points = number_points),
#                                   export = list(create_simulation_species = create_simulation_species,
#                                                 create_simulation_pattern = create_simulation_pattern,
#                                                 detect_habitat_associations = detect_habitat_associations),
#                                   seed = 42,
#                                   n_jobs = length(association_strength),
#                                   job_size = 1,
#                                   template = list(queue = "mpi",
#                                                   walltime = "48:00",
#                                                   processes = 1))

torus_translation <- clustermq::Q(fun = simulate_torus_trans_association_strength,
                                  simulation_habitat = input_data$habitats,
                                  simulation_pattern = input_data$patterns,
                                  association_strength = rep(association_strength, each = simulation_runs),
                                  export = list(detect_habitat_associations = detect_habitat_associations),
                                  seed = 42,
                                  n_jobs = length(input_data$habitats),
                                  job_size = 1,
                                  template = list(queue = "mpi",
                                                  walltime = "12:00",
                                                  processes = 1))

# combine results to one data frame
torus_translation <- dplyr::bind_rows(torus_translation)

helpeR::save_rds(object = torus_translation,
                 filename = paste0("torus_translation_", simulation_runs, "_runs.rds"),
                 path = paste0(getwd(), "/1_Simulation_study/3_Results"),
                 overwrite = overwrite)

# Gamma test
# gamma_test <- clustermq::Q(fun = simulate_point_process_association_strength,
#                            association_strength = association_strength,
#                            const = list(number_coloumns = number_coloumns,
#                                         number_rows = number_rows,
#                                         resolution = resolution,
#                                         fract_dim = fract_dim,
#                                         number_points = number_points,
#                                         n_random = n_random),
#                            export = list(create_simulation_species = create_simulation_species,
#                                          create_simulation_pattern = create_simulation_pattern,
#                                          detect_habitat_associations = detect_habitat_associations),
#                            seed = 42,
#                            n_jobs = length(association_strength),
#                            job_size = 1,
#                            template = list(queue = "mpi",
#                                            walltime = "48:00",
#                                            processes = 1))

# Gamma test
gamma_test <- clustermq::Q(fun = simulate_point_process_association_strength,
                           simulation_habitat = input_data$habitats,
                           simulation_pattern = input_data$patterns,
                           association_strength = rep(association_strength, each = simulation_runs),
                           const = list(n_random = n_random),
                           export = list(detect_habitat_associations = detect_habitat_associations),
                           seed = 42,
                           n_jobs = length(input_data$habitats),
                           job_size = 1,
                           template = list(queue = "mpi",
                                           walltime = "12:00",
                                           processes = 1))

# combine results to one data frame
gamma_test <- dplyr::bind_rows(gamma_test)

helpeR::save_rds(object = gamma_test,
                 filename = paste0("gamma_test_", simulation_runs, "_runs.rds"),
                 path = paste0(getwd(), "/1_Simulation_study/3_Results"),
                 overwrite = overwrite)

# Pattern reconstruction
# pattern_reconstruction <- clustermq::Q(fun = simulate_pattern_recon_association_strength, 
#                                        association_strength = association_strength, 
#                                        const = list(number_coloumns = number_coloumns, 
#                                                     number_rows = number_rows, 
#                                                     resolution = resolution,
#                                                     fract_dim = fract_dim, 
#                                                     number_points = number_points, 
#                                                     n_random = n_random, 
#                                                     max_runs = max_runs, 
#                                                     comp_fast = comp_fast,
#                                                     no_change = no_change),
#                                        export = list(create_simulation_species = create_simulation_species, 
#                                                      create_simulation_pattern = create_simulation_pattern,
#                                                      detect_habitat_associations = detect_habitat_associations), 
#                                        seed = 42, 
#                                        n_jobs = length(association_strength), 
#                                        job_size = 1,
#                                        template = list(queue = "mpi", 
#                                                        walltime = "48:00", 
#                                                        processes = 1))

pattern_reconstruction <- clustermq::Q(fun = simulate_pattern_recon_association_strength, 
                                       simulation_habitat = input_data$habitats,
                                       simulation_pattern = input_data$patterns,
                                       association_strength = rep(association_strength, each = simulation_runs),
                                       const = list(n_random = n_random, 
                                                    max_runs = max_runs, 
                                                    comp_fast = comp_fast,
                                                    no_change = no_change),
                                       export = list(detect_habitat_associations = detect_habitat_associations),
                                       seed = 42,
                                       n_jobs = length(input_data$habitats),
                                       job_size = 1,
                                       template = list(queue = "mpi", 
                                                       walltime = "48:00", 
                                                       processes = 1))

# combine results to one data frame
pattern_reconstruction <- dplyr::bind_rows(pattern_reconstruction)

helpeR::save_rds(object = pattern_reconstruction,
                 filename = paste0("pattern_reconstruction_", simulation_runs, "_runs.rds"),
                 path = paste0(getwd(), "/1_Simulation_study/3_Results"),
                 overwrite = overwrite)

#### 4. Specify future topology ####
# 
# future_map for 1) alpha (x) 2) simulation runs (y) 3) within null model function
# login node -> { cluster nodes } -> { multiple cores }
# 
# login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
# bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl',
#                       resources = list(job.name = 'association_strength',
#                                        log.file = 'association_strength.log',
#                                        queue = 'mpi-short',
#                                        walltime = '02:00',
#                                        processes = 1))
# 
# future::plan(list(login, bsub, future::sequential))
# 
# future::plan(future::multiprocess)
# 
#### 5. Simulation study of different methods to analyze species habitat assocations ####
# 
# # Habitat randomization (Harms et al. 2001) #
# habitat_randomization %<-% {simulate_habitat_random_association_strength(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_null_model = number_null_model,
#   number_points = number_points,
#   alpha_sequence = alpha_sequence)}
# 
# future::resolved(future::futureOf(habitat_randomization))
# while (TRUE) {
# 
#   Sys.sleep(300)
#   
#   if(future::resolved(future::futureOf(habitat_randomization))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(), 
#                       "\n\nFile '", deparse(substitute(habitat_randomization)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }
# 
# # Torus translation (Harms et al. 2001) #
# torus_translation %<-% {simulate_torus_trans_association_strength(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_points = number_points,
#   alpha_sequence = alpha_sequence)
# }
# 
# future::resolved(future::futureOf(torus_translation))
# while (TRUE) {
#   
#   Sys.sleep(300)
# 
#   if(future::resolved(future::futureOf(torus_translation))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(), 
#                       "\n\nFile '", deparse(substitute(torus_translation)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }
# 
# # Fitting point process (Plotkin et al. 2000) #
# point_process %<-% {simulate_point_process_association_strength(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_null_model = number_null_model,
#   number_points = number_points,
#   alpha_sequence = alpha_sequence)
# }
# 
# future::resolved(future::futureOf(point_process))
# while (TRUE) {
# 
#   Sys.sleep(300)
#   
#   if(future::resolved(future::futureOf(point_process))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(), 
#                       "\n\nFile '", deparse(substitute(point_process)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }
#
# # Pattern reconstruction #
# pattern_reconstruction %<-% {simulate_pattern_recon_association_strength(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_null_model = number_null_model,
#   number_points = number_points,
#   max_runs = max_runs,
#   alpha_sequence = alpha_sequence)
# }
#
# future::resolved(future::futureOf(pattern_reconstruction))
# while (TRUE) {
# 
#   Sys.sleep(300)
#  
#   if(future::resolved(future::futureOf(pattern_reconstruction))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(), 
#                       "\n\nFile '", deparse(substitute(pattern_reconstruction)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }
