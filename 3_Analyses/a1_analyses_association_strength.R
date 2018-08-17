#### Simulation study - Strength associations ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & sunctions ####

# Packages #
library(furrr)
library(future.batchtools)
library(listenv)
library(NLMR)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '^[a0_ a1_]', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Define parameters ####

# Set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

# Number of coloumns and rows for neutral landscape
number_coloumns <- 50 # 30
number_rows <- 50 # 30

# Resolution of neutral landscape
resolution <- 20 # 20

# fract_dim of neutral landscape
fract_dim <- 1.5 # 1.5

# Approxmitated number of points for each species
number_points <- 250 # 100 - 250 - 500???

# Number of runs
simulation_runs <- 50 # 50 - 100?

# Number of randomized habitat maps / point patterns
number_maps <- 199 # 199
number_pattern <- 199 # 199

# Number of itertations pattern reconstruction
max_runs <- 5000 # 5000

# Different association strengths
alpha_sequence <- seq(0, 1, 0.025) # seq(0, 1, 0.025)

# Set seed for landscapes


# UtilityFunctions::save_rds(object = alpha_sequence,
#                            filename = "alpha_sequence.rds",
#                            path = paste0(getwd(), "/4_Output"), 
#                            overwrite = FALSE)

#### 3. Specify future topology ####
# 
# future_map for 1) alpha (x) 2) simulation runs (y) 3) within null model function
# login node -> { cluster nodes } -> { multiple cores }
# 
# login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
# bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl',
#                       resources = list(job.name = 'association_strength',
#                                        log.file = 'association_strength.log',
#                                        queue = 'mpi',
#                                        walltime = '48:00',
#                                        processes = 24))
# 
# future::plan(list(login, bsub, future::multiprocess))
# 
# future::plan(list(future::multiprocess, future::multiprocess))
# future::plan(future::multiprocess)
# 
#### 4. Simulation study of different methods to analyze species habitat assocations ####

# Habitat randomization (Harms et al. 2001) #
habitat_randomization %<-% {simulate_habitat_random_association_strength(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  fract_dim = fract_dim,
  resolution = resolution,
  number_maps = number_maps,
  number_points = number_points,
  alpha_sequence = alpha_sequence,
  simulation_runs = simulation_runs)
}

# while (TRUE) {
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

future::resolved(future::futureOf(habitat_randomization))

# Torus translation (Harms et al. 2001) #
torus_translation %<-% {simulate_torus_trans_association_strength(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  fract_dim = fract_dim,
  resolution = resolution,
  number_points = number_points,
  alpha_sequence = alpha_sequence,
  simulation_runs = simulation_runs)
}

# while (TRUE) {
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

future::resolved(future::futureOf(torus_translation))

# Fitting point process (Plotkin et al. 2000) #
point_process %<-% {simulate_point_process_association_strength(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  fract_dim = fract_dim,
  resolution = resolution,
  number_pattern = number_pattern,
  number_points = number_points,
  alpha_sequence = alpha_sequence,
  simulation_runs = simulation_runs)}

# while (TRUE) {
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

future::resolved(future::futureOf(point_process))

# Pattern reconstruction #
pattern_reconstruction %<-% {simulate_pattern_recon_association_strength(
  number_coloumns = number_coloumns,
  number_rows = number_rows,
  fract_dim = fract_dim,
  resolution = resolution,
  number_pattern = number_pattern,
  number_points = number_points,
  simulation_runs = simulation_runs,
  max_runs = max_runs,
  alpha_sequence = alpha_sequence)
}

# while (TRUE) {
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

future::resolved(future::futureOf(pattern_reconstruction))

#### 5. Save data ####

UtilityFunctions::save_rds(object = habitat_randomization,
                           filename = paste0("a1_habitat_randomization_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = torus_translation,
                           filename = paste0("a1_torus_translation_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = point_process,
                           filename = paste0("a1_point_process_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)

UtilityFunctions::save_rds(object=pattern_reconstruction,
                           filename = paste0("a1_pattern_reconstruction_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = FALSE)


