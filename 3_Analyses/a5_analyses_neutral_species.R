#### Simulation study - Neutral species ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages
source(paste0(getwd(), '/2_Functions/setup_packages.R'))

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '^[f0_ f5_]', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Define parameters ####

# Set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

# Number of coloumns and rows for neutral landscape
number_coloumns <- 30 # 30
number_rows <- 30 # 30

# Resolution of neutral landscape
resolution <- 20 # 20

# Roughness of neutral landscape
fract_dim <- 1.5 # 0.3

# Approxmitated number of points for each species
number_points <- 100 # 100 

# Number of randomized habitat maps / point patterns
number_null_model <- 199 # 199

# Number of itertations pattern reconstruction
max_runs <- 1000 # 2500

#### 3. Specify future topology ####
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
#### 4. Simulation study of different methods to analyze species habitat assocations ####

# Habitat randomization (Harms et al. 2001) #
habitat_randomization %<-% simulate_habitat_random_neutral(number_coloumns = number_coloumns,
                                                           number_rows = number_rows,
                                                           fract_dim = fract_dim,
                                                           resolution = resolution,
                                                           number_null_model = number_null_model,
                                                           number_points = number_points)

# Torus translation (Harms et al. 2001) #
torus_translation %<-% simulate_torus_trans_neutral(number_coloumns = number_coloumns,
                                                    number_rows = number_rows,
                                                    fract_dim = fract_dim,
                                                    resolution = resolution,
                                                    number_points = number_points)

# Gamma test (Plotkin et al. 2000) #
point_process %<-% simulate_point_process_neutral(number_coloumns = number_coloumns, 
                                                  number_rows = number_rows,
                                                  fract_dim =fract_dim,
                                                  resolution = resolution,
                                                  number_null_model = number_null_model,
                                                  number_points = number_points)

# Pattern reconstruction #
pattern_reconstruction %<-% simulate_pattern_recon_neutral(number_coloumns = number_coloumns, 
                                                           number_rows = number_rows,
                                                           roughness = roughness,
                                                           resolution = resolution,
                                                           number_null_model = number_null_model,
                                                           number_points = number_points)

#### 5. Save data ####

UtilityFunctions::save_rds(object = habitat_randomization,
                           filename = paste0("a5_habitat_randomization_",simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = torus_translation,
                           filename = paste0("a5_torus_translation_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = point_process,
                           filename = paste0("a5_point_process_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object=pattern_reconstruction,
                           filename = paste0("a5_pattern_reconstruction_", simulation_runs, "_", number_pattern, ".rds"),
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)
