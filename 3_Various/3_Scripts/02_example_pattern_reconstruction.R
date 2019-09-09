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
library(mobsim)
library(NLMR)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(shar)
library(spatstat)
library(spex)
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
names_species <- simulation_pattern$marks$species %>%
  unique() %>%
  as.character()  

#### 3. Subset species ####

# Species 1
species_1 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, 
                                                   species_code == 1))

# Species 2
species_2 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, 
                                                   species_code == 2))

# Species 3
species_3 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, 
                                                   species_code == 3))

# Species 4
species_4 <- spatstat::unmark(spatstat::subset.ppp(simulation_pattern, 
                                                   species_code == 4))

#### 4. Pattern reconstruction ####

# set parameters
n_random <- 199
n_random_hpc <- rep(x = 1, times = n_random) # rep(1, 199)

max_runs <- 20000 # 2500

# spatstat::envelope(species_1, fun = pcf, correction = "Ripley", 
#                    funargs = list(divisor = "d")) %>% 
#   onpoint::plot_quantums()

# reconstruction_species_1 <- shar::reconstruct_pattern_homo(pattern = species_1,
#                                                            n_random = n_random,
#                                                            max_runs = max_runs)

reconstruction_species_1 <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_homo,
                                                       n_random = n_random_hpc,
                                                       const = list(pattern = species_1,
                                                                    max_runs = max_runs,
                                                                    return_input = FALSE,
                                                                    simplify = TRUE,
                                                                    verbose = FALSE),
                                                       seed = 42,
                                                       n_jobs = length(n_random_hpc),
                                                       template = list(job_name = "exmpl_recon_spec_1",
                                                                       queue = "medium",
                                                                       walltime = "01:00:00",
                                                                       mem_cpu = "2048",
                                                                       processes = 1, 
                                                                       log_file = "exmpl_recon_spec_1.log"))

# add observed pattern
reconstruction_species_1[[length(n_random) + 1]] <- species_1

# add names to list
names(reconstruction_species_1) <- c(rep(paste0("randomized_", 1:length(n_random))),
                   "observed")

# spatstat::envelope(species_2, fun = pcf, correction = "Ripley",
#                    funargs = list(divisor = "d")) %>%
#   onpoint::plot_quantums()

# reconstruction_species_2 <- shar::reconstruct_pattern_cluster(pattern = species_2,
#                                                               n_random = n_random,
#                                                               max_runs = max_runs,
#                                                               verbose = TRUE)

reconstruction_species_2 <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                       n_random = n_random_hpc,
                                                       const = list(pattern = species_2,
                                                                    max_runs = max_runs,
                                                                    return_input = FALSE,
                                                                    simplify = TRUE,
                                                                    verbose = FALSE),
                                                       seed = 42,
                                                       n_jobs = length(n_random_hpc),
                                                       template = list(job_name = "exmpl_recon_spec_2",
                                                                       queue = "medium",
                                                                       walltime = "01:00:00",
                                                                       mem_cpu = "2048",
                                                                       processes = 1, 
                                                                       log_file = "exmpl_recon_spec_2.log"))

# add observed pattern
reconstruction_species_2[[length(n_random) + 1]] <- spatstat::unmark(species_2)

# add names to list
names(reconstruction_species_2) <- c(rep(paste0("randomized_", 1:length(n_random))),
                                     "observed")

# spatstat::envelope(species_3, fun = pcf, correction = "Ripley",
#                    funargs = list(divisor = "d")) %>%
#   onpoint::plot_quantums()

# reconstruction_species_3 <- shar::reconstruct_pattern_homo(pattern = species_3,
#                                                            n_random = n_random,
#                                                            max_runs = max_runs,
#                                                            verbose = TRUE)

reconstruction_species_3 <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_homo,
                                                       n_random = n_random_hpc,
                                                       const = list(pattern = species_3,
                                                                    max_runs = max_runs,
                                                                    return_input = FALSE,
                                                                    simplify = TRUE,
                                                                    verbose = FALSE),
                                                       seed = 42,
                                                       n_jobs = length(n_random_hpc),
                                                       template = list(job_name = "exmpl_recon_spec_3",
                                                                       queue = "medium",
                                                                       walltime = "01:00:00",
                                                                       mem_cpu = "2048",
                                                                       processes = 1, 
                                                                       log_file = "exmpl_recon_spec_3.log"))

# add observed pattern
reconstruction_species_3[[length(n_random) + 1]] <- spatstat::unmark(species_3)

# add names to list
names(reconstruction_species_3) <- c(rep(paste0("randomized_", 1:length(n_random))),
                                     "observed")

# spatstat::envelope(species_4, fun = pcf, correction = "Ripley",
#                    funargs = list(divisor = "d")) %>%
#   onpoint::plot_quantums()

# reconstruction_species_4 <- shar::reconstruct_pattern_cluster(pattern = species_4,
#                                                               n_random = n_random,
#                                                               max_runs = max_runs,
#                                                               verbose = TRUE)

reconstruction_species_4 <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                       n_random = n_random_hpc,
                                                       const = list(pattern = species_4,
                                                                    max_runs = max_runs,
                                                                    return_input = FALSE,
                                                                    simplify = TRUE,
                                                                    verbose = FALSE),
                                                       seed = 42,
                                                       n_jobs = length(n_random_hpc),
                                                       template = list(job_name = "exmpl_recon_spec_4",
                                                                       queue = "medium",
                                                                       walltime = "01:00:00",
                                                                       mem_cpu = "2048",
                                                                       processes = 1, 
                                                                       log_file = "exmpl_recon_spec_4.log"))

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
suppoRt::save_rds(object = example_reconstructed_pattern,
                  filename = "02_example_reconstructed_pattern.rds",
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
suppoRt::save_rds(object = example_fitted_pattern,
                  filename = "02_example_fitted_pattern.rds",
                  path = "3_Various/1_Results/", 
                  overwrite = overwrite)
