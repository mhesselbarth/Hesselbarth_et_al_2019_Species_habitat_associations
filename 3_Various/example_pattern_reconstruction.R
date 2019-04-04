###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### 1. Import packages & functions ####

# Packages #
library(clustermq)

library(NLMR)
library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(shar)
library(spatstat)
library(tidyverse)

# set overwrite save results parameter
overwrite <- TRUE

# source all functions in R_functions folder
list.files(path = "1_Simulation_study/1_Functions/", full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Create data ####

# set seed 
set.seed(42, kind = "L'Ecuyer-CMRG")

# create simulation habitats
simulation_habitats <- NLMR::nlm_fbm(ncol = 50, nrow = 50,
                                     resolution = 20, fract_dim = 1.5, 
                                     user_seed = 42,
                                     verbose = FALSE) %>%
  shar::classify_habitats(classes = 5)

# create simulation pattern
simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                number_points = 100, 
                                                association_strength = 0.35)
    
# names of species include associatione
names_species <- simulation_pattern$marks$Species %>%
  unique() %>%
  as.character()  

#### 4. Pattern reconstruction ####

# set parameters
# n_random <- 199

n_random <- rep(1, 199) # rep(1, 199)

max_runs <- 20000 # 2500

# Species 1
species_1 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, species_code == 1))

# reconstruction_species_1 <- shar::reconstruct_pattern(pattern = species_1,
#                                                       n_random = n_random,
#                                                       max_runs = max_runs,
#                                                       fitting = FALSE)

reconstruction_species_1 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_1,
                                                      max_runs = max_runs,
                                                      fitting = FALSE,
                                                      return_input = FALSE,
                                                      simplify = TRUE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "01:00",
                                                         processes = 1))

# add observed pattern
reconstruction_species_1[[length(n_random) + 1]] <- species_1

# add names to list
names(reconstruction_species_1) <- c(rep(paste0("randomized_", 1:length(n_random))),
                   "observed")

# Species 2
species_2 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, species_code == 2))

# reconstruction_species_2 <- shar::reconstruct_pattern(pattern = species_2,
#                                                       n_random = n_random,
#                                                       max_runs = max_runs,
#                                                       fitting = TRUE,
#                                                       verbose = TRUE)

reconstruction_species_2 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_2,
                                                      max_runs = max_runs,
                                                      fitting = FALSE,
                                                      return_input = FALSE,
                                                      simplify = TRUE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "01:00",
                                                         processes = 1))

# add observed pattern
reconstruction_species_2[[length(n_random) + 1]] <- spatstat::unmark(species_2)

# add names to list
names(reconstruction_species_2) <- c(rep(paste0("randomized_", 1:length(n_random))),
                                     "observed")

# Species 3
species_3 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, species_code == 3))

# reconstruction_species_3 <- shar::reconstruct_pattern(pattern = species_3,
#                                                       n_random = n_random,
#                                                       max_runs = max_runs,
#                                                       fitting = FALSE,
#                                                       verbose = TRUE)

reconstruction_species_3 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_3,
                                                      max_runs = max_runs,
                                                      fitting = FALSE,
                                                      return_input = FALSE,
                                                      simplify = TRUE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "01:00",
                                                         processes = 1))

# add observed pattern
reconstruction_species_3[[length(n_random) + 1]] <- spatstat::unmark(species_3)

# add names to list
names(reconstruction_species_3) <- c(rep(paste0("randomized_", 1:length(n_random))),
                                     "observed")

# Species 4
species_4 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, species_code == 4))

# reconstruction_species_4 <- shar::reconstruct_pattern(pattern = species_4,
#                                                       n_random = n_random,
#                                                       max_runs = max_runs,
#                                                       fitting = TRUE,
#                                                       verbose = TRUE)

reconstruction_species_4 <- clustermq::Q(fun = reconstruct_pattern,
                                         n_random = n_random,
                                         const = list(pattern = species_4,
                                                      max_runs = max_runs,
                                                      fitting = FALSE,
                                                      return_input = FALSE,
                                                      simplify = TRUE,
                                                      verbose = FALSE),
                                         seed = 42,
                                         n_jobs = length(n_random),
                                         template = list(queue = "mpi",
                                                         walltime = "01:00",
                                                         processes = 1))

# add observed pattern
reconstruction_species_4[[length(n_random) + 1]] <- spatstat::unmark(species_4)

# add names to list
names(reconstruction_species_4) <- c(rep(paste0("randomized_", 1:length(n_random))),
                                     "observed")

# combine to one list
example_reconstructed_pattern <- list(species_1 = reconstruction_species_1, 
                                      species_2 = reconstruction_species_2, 
                                      species_3 = reconstruction_species_3, 
                                      species_4 = reconstruction_species_4)

# save results
helpeR::save_rds(object = example_reconstructed_pattern,
                 filename = "example_reconstructed_pattern.rds",
                 path = "3_Various/1_Output/", 
                 overwrite = overwrite)

#### 5. Point process ####

# set parameters
n_random <- 199

set.seed(42, kind = "L'Ecuyer-CMRG")

# Species 1
fitted_species_1 <- shar::fit_point_process(species_1, 
                                            n_random = n_random,
                                            process = "poisson")

# Species 2
fitted_species_2 <- shar::fit_point_process(species_2, 
                                            n_random = n_random,
                                            process = "cluster")

# Species 3
fitted_species_3 <- shar::fit_point_process(species_3, 
                                            n_random = n_random,
                                            process = "poisson")

# Species 4
fitted_species_4 <- shar::fit_point_process(species_4, 
                                            n_random = n_random,
                                            process = "cluster")

# combine to one list
example_fitted_pattern <- list(species_1 = fitted_species_1, 
                               species_2 = fitted_species_2, 
                               species_3 = fitted_species_3,
                               species_4 = fitted_species_4)

# save results
helpeR::save_rds(object = example_fitted_pattern,
                 filename = "example_fitted_pattern.rds",
                 path = "3_Various/1_Output/", 
                 overwrite = overwrite)
