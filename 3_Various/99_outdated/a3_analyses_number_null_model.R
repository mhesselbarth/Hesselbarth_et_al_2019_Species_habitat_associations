# #### Simulation study - Number null model ####
# 
# ###################################################
# ##    Author: Maximilian Hesselbarth             ##
# ##    Department of Ecosystem Modelling          ##
# ##    University of Goettingen                   ##
# ##    maximilian.hesselbarth@uni-goettingen.de   ##
# ###################################################
# 
# #### 1. Import packages & functions ####
# 
# # Packages
# source(paste0(getwd(), '/2_Functions/setup_packages.R'))
# 
# # Source all functions in R_functions folder
# list.files(paste0(getwd(), '/2_Functions'), pattern = '^[f0_ f3_]', full.names = TRUE) %>%
#   purrr::walk(function(x) source(x))
# 
# #### 2. Define parameters ####
# 
# # Set seed
# set.seed(42, kind = "L'Ecuyer-CMRG")
# 
# # Number of coloumns and rows for neutral landscape
# number_coloumns <- 50 # 30
# number_rows <- 50 # 30
# 
# # Resolution of neutral landscape
# resolution <- 20 # 20
# 
# # Roughness of neutral landscape
# fract_dim <- 0.3 # 0.3
# 
# # Approxmitated number of points for each species
# number_points <- 100 # 100 
# 
# # Number of runs
# simulation_runs <- 100 # 50
# 
# # Number of randomized habitat maps / point patterns
# number_null_model <- rep(c(19, 39, 99, 199, 499), each = simulation_runs) # c(19, 39, 99, 199, 499)
# 
# # Number of itertations pattern reconstruction
# max_runs <- 1000 # 2500
# 
# # Different association strengths
# alpha <- 0.35 # seq(0.25, 0.75, 0.025)
# 
# #### 3. Specify future topology ####
# # 
# # future_map for 1) alpha (x) 2) simulation runs (y) 3) within null model function
# # login node -> { cluster nodes } -> { multiple cores }
# # 
# # login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
# # bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl',
# #                       resources = list(job.name = 'number_null_model',
# #                                        log.file = 'number_null_model.log',
# #                                        queue = 'mpi',
# #                                        walltime = '48:00',
# #                                        processes = 24))
# # 
# # future::plan(list(login, bsub, future::multiprocess))
# # 
# # future::plan(future::multiprocess)
# # 
# #### 4. Simulation study of different methods to analyze species habitat assocations ####
# 
# # Habitat randomization (Harms et al. 2001) #
# habitat_randomization %<-% {simulate_habitat_random_number_null_model(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_null_model = number_null_model,
#   number_points = number_points,
#   alpha = alpha)}
# 
# # Torus translation (Harms et al. 2001) #
# # Doesn't make sense here
# 
# # Fitting point process (Plotkin et al. 2000) #
# point_process %<-% {simulate_point_process_number_null_model(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_null_model = number_null_model,
#   number_points = number_points,
#   alpha = alpha)
# }
# 
# # Pattern reconstruction #
# pattern_reconstruction %<-% {simulate_pattern_recon_number_null_model(
#   number_coloumns = number_coloumns,
#   number_rows = number_rows,
#   fract_dim = fract_dim,
#   resolution = resolution,
#   number_null_model = number_null_model,
#   number_points = number_points,
#   simulation_runs = simulation_runs,
#   max_runs = max_runs,
#   alpha = alpha)
# }
# 
# #### 5. Save data ####
# 
# helpeR::save_rds(object = habitat_randomization,
#                            filename = paste0("a3_habitat_randomization_", simulation_runs, "_", number_pattern, ".rds"),
#                            path = paste0(getwd(), "/4_Output"), 
#                            overwrite = FALSE)
# 
# helpeR::save_rds(object = point_process,
#                            filename = paste0("a3_point_process_", simulation_runs, "_", number_pattern, ".rds"),
#                            path = paste0(getwd(), "/4_Output"),
#                            overwrite = FALSE)
# 
# helpeR::save_rds(object=pattern_reconstruction,
#                            filename = paste0("a3_pattern_reconstruction_", simulation_runs, "_", number_pattern, ".rds"),
#                            path = paste0(getwd(), "/4_Output"),
#                            overwrite = FALSE)
# 
# 
