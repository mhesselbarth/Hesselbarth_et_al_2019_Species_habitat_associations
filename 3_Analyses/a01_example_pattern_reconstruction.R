#### Simulation study ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages #
source(paste0(getwd(), '/2_Functions/setup_packages.R'))

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '^[a0_ a1_]', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Specify future topology ####
# 
# future_map for 1) alpha (x) 2) simulation runs (y) 3) within null model function
# login node -> { cluster nodes } -> { multiple cores }
# 
# login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
# bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl',
#                       resources = list(job.name = 'pattern_recon_example',
#                                        log.file = 'pattern_recon_example.log',
#                                        queue = 'mpi-short',
#                                        walltime = '02:00',
#                                        processes = 1))
# 
# future::plan(list(login, bsub, future::sequential))
# 
# future::plan(list(future::multiprocess, future::multiprocess))
# future::plan(future::multiprocess)
# 
# 
#### 3. Create data ####

set.seed(42, kind = "L'Ecuyer-CMRG")

simulation_habitats <- NLMR::nlm_fbm(ncol = 30, nrow = 30,
                                     resolution = 20, fract_dim = 1.5, 
                                     verbose = FALSE) %>%
  SHAR::classify_habitats(classes = 5)

simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                number_points = 100, 
                                                association_strength = 0.35)
    
names_species <- simulation_pattern$marks$Species %>%
  unique() %>%
  as.character()  

#### 4. Pattern reconstruction ####

number_reconstructions <- 199
max_runs <- 1000

# Species 1
species_1 <- spatstat::subset.ppp(simulation_pattern, Species_code == 1)

reconstruction_species_1 <- SHAR::reconstruct_pattern(pattern = species_1, 
                                                      number_reconstructions = number_reconstructions, 
                                                      max_runs = max_runs, 
                                                      fitting = FALSE, 
                                                      verbose = TRUE)

# SHAR::plot_randomized_pattern(reconstruction_species_1)

# Species 2
species_2 <- spatstat::subset.ppp(simulation_pattern, Species_code == 2)

reconstruction_species_2 <- SHAR::reconstruct_pattern(pattern = species_2, 
                                                      number_reconstructions = number_reconstructions, 
                                                      max_runs = max_runs, 
                                                      fitting = TRUE, 
                                                      verbose = TRUE)

# SHAR::plot_randomized_pattern(reconstruction_species_2)


# Species 3
species_3 <- spatstat::subset.ppp(simulation_pattern, Species_code == 3)

reconstruction_species_3 <- SHAR::reconstruct_pattern(pattern = species_3, 
                                                      number_reconstructions = number_reconstructions, 
                                                      max_runs = max_runs, 
                                                      fitting = FALSE, 
                                                      verbose = TRUE)

# SHAR::plot_randomized_pattern(reconstruction_species_3)


# Species 4
species_4 <- spatstat::subset.ppp(simulation_pattern, Species_code == 4)

reconstruction_species_4 <- SHAR::reconstruct_pattern(pattern = species_4, 
                                                      number_reconstructions = number_reconstructions, 
                                                      max_runs = max_runs, 
                                                      fitting = TRUE, 
                                                      verbose = TRUE)

# SHAR::plot_randomized_pattern(reconstruction_species_4)

#### 5. Point process ####
fitted_species_1 <- fit_point_process(species_1, 
                                      process = "poisson", 
                                      number_pattern = number_reconstructions)

fitted_species_2 <- fit_point_process(species_2, 
                                      process = "cluster", 
                                      number_pattern = number_reconstructions)

fitted_species_3 <- fit_point_process(species_3, 
                                      process = "poisson", 
                                      number_pattern = number_reconstructions)

fitted_species_4 <- fit_point_process(species_4, 
                                      process = "cluster", 
                                      number_pattern = number_reconstructions)

#### 6. Save results ####

overwrite <- TRUE

# Species 1
UtilityFunctions::save_rds(object = reconstruction_species_1,
                           filename = "a01_reconstruction_species_1.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = fitted_species_1,
                           filename = "a01_fitted_species_1.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 2
UtilityFunctions::save_rds(object = reconstruction_species_2,
                           filename = "a01_reconstruction_species_2.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = fitted_species_2,
                           filename = "a01_fitted_species_2.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 3
UtilityFunctions::save_rds(object = reconstruction_species_3,
                           filename = "a01_reconstruction_species_3.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = fitted_species_3,
                           filename = "a01_fitted_species_3.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

# Species 4
UtilityFunctions::save_rds(object = reconstruction_species_4,
                           filename = "a01_reconstruction_species_4.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = fitted_species_4,
                           filename = "a01_fitted_species_4.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)


