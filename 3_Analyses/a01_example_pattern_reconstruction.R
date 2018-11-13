#### Simulation study ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages #
library(clustermq)
library(NLMR)
library(UtilityFunctions)
library(SHAR)
library(spatstat)
library(tidyverse)

overwrite <- TRUE

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '^[f0_ f1_]', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Create data ####

simulation_habitats <- NLMR::nlm_fbm(ncol = 50, nrow = 50,
                                     resolution = 20, fract_dim = 1.5, 
                                     user_seed = 42,
                                     verbose = FALSE) %>%
  SHAR::classify_habitats(classes = 5)

set.seed(42, kind = "L'Ecuyer-CMRG")
simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                number_points = 100, 
                                                association_strength = 0.35)
    
names_species <- simulation_pattern$marks$Species %>%
  unique() %>%
  as.character()  

#### 4. Pattern reconstruction ####

# n_random <- 199
n_random <- rep(1, 199) # rep(1, 199)
max_runs <- 10000  # 2500

# Species 1
species_1 <- spatstat::subset.ppp(simulation_pattern, species_code == 1)

# reconstruction_species_1 <- SHAR::reconstruct_pattern(pattern = species_1, 
#                                                       n_random = n_random, 
#                                                       max_runs = max_runs, 
#                                                       fitting = FALSE,
#                                                       verbose = TRUE)

reconstruction_species_1 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_1,
                                                      e_threshold = 0.01,
                                                      max_runs = max_runs,
                                                      fitting = FALSE, 
                                                      comp_fast = FALSE,
                                                      return_input = FALSE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "48:00",
                                                         processes = 1))

reconstruction_species_1 <- purrr::flatten(reconstruction_species_1)

reconstruction_species_1[[length(n_random) + 1]] <- spatstat::unmark(species_1)
names(reconstruction_species_1) <- c(rep(paste0("randomized_", 1:length(n_random))), 
                   "observed")

UtilityFunctions::save_rds(object = reconstruction_species_1,
                           filename = "o01_reconstruction_species_1.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 2
species_2 <- spatstat::subset.ppp(simulation_pattern, species_code == 2)

# reconstruction_species_2 <- SHAR::reconstruct_pattern(pattern = species_2, 
#                                                       n_random = n_random, 
#                                                       max_runs = max_runs, 
#                                                       fitting = TRUE,
#                                                       verbose = TRUE)

reconstruction_species_2 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_2,
                                                      e_threshold = 0.01,
                                                      max_runs = max_runs,
                                                      fitting = TRUE, 
                                                      comp_fast = FALSE,
                                                      return_input = FALSE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "48:00",
                                                         processes = 1))

reconstruction_species_2 <- purrr::flatten(reconstruction_species_2)

reconstruction_species_2[[length(n_random) + 1]] <- spatstat::unmark(species_2)
names(reconstruction_species_2) <- c(rep(paste0("randomized_", 1:length(n_random))), 
                                     "observed")

UtilityFunctions::save_rds(object = reconstruction_species_2,
                           filename = "o01_reconstruction_species_2.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 3
species_3 <- spatstat::subset.ppp(simulation_pattern, species_code == 3)

# reconstruction_species_3 <- SHAR::reconstruct_pattern(pattern = species_3, 
#                                                       n_random = n_random, 
#                                                       max_runs = max_runs, 
#                                                       fitting = FALSE,
#                                                       verbose = TRUE)

reconstruction_species_3 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_3,
                                                      e_threshold = 0.01,
                                                      max_runs = max_runs,
                                                      fitting = FALSE, 
                                                      comp_fast = FALSE,
                                                      return_input = FALSE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "48:00",
                                                         processes = 1))

reconstruction_species_3 <- purrr::flatten(reconstruction_species_3)

reconstruction_species_3[[length(n_random) + 1]] <- spatstat::unmark(species_3)
names(reconstruction_species_3) <- c(rep(paste0("randomized_", 1:length(n_random))), 
                                     "observed")

UtilityFunctions::save_rds(object = reconstruction_species_3,
                           filename = "o01_reconstruction_species_3.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 4
species_4 <- spatstat::subset.ppp(simulation_pattern, species_code == 4)

# reconstruction_species_4 <- SHAR::reconstruct_pattern(pattern = species_4, 
#                                                       n_random = n_random, 
#                                                       max_runs = max_runs, 
#                                                       fitting = TRUE,
#                                                       verbose = TRUE)

reconstruction_species_4 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_4,
                                                      e_threshold = 0.01,
                                                      max_runs = max_runs,
                                                      fitting = TRUE, 
                                                      comp_fast = FALSE,
                                                      return_input = FALSE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "48:00",
                                                         processes = 1))

reconstruction_species_4 <- purrr::flatten(reconstruction_species_4)

reconstruction_species_4[[length(n_random) + 1]] <- spatstat::unmark(species_4)
names(reconstruction_species_4) <- c(rep(paste0("randomized_", 1:length(n_random))), 
                                     "observed")

UtilityFunctions::save_rds(object = reconstruction_species_4,
                           filename = "o01_reconstruction_species_4.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

#### 5. Point process ####
n_random <- 199

set.seed(42, kind = "L'Ecuyer-CMRG")

# Species 1
fitted_species_1 <- SHAR::fit_point_process(species_1, 
                                            n_random = n_random,
                                            process = "poisson")

UtilityFunctions::save_rds(object = fitted_species_1,
                           filename = "o01_fitted_species_1.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 2
fitted_species_2 <- SHAR::fit_point_process(species_2, 
                                            n_random = n_random,
                                            process = "cluster")

UtilityFunctions::save_rds(object = fitted_species_2,
                           filename = "o01_fitted_species_2.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 3
fitted_species_3 <- SHAR::fit_point_process(species_3, 
                                            n_random = n_random,
                                            process = "poisson")

UtilityFunctions::save_rds(object = fitted_species_3,
                           filename = "o01_fitted_species_3.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

# Species 4
fitted_species_4 <- SHAR::fit_point_process(species_4, 
                                            n_random = n_random,
                                            process = "cluster")

UtilityFunctions::save_rds(object = fitted_species_4,
                           filename = "o01_fitted_species_4.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)




