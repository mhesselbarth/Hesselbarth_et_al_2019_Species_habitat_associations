###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 4 ####

# Load packages #

library(clustermq)
library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

#### Import data ####

# import point pattern data
pattern_2007 <- readr::read_rds("2_Real_world_data/1_Data/02_pattern_2007_ppp.rds")

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007, species == "beech")

# split into living and dead
beech_living <- spatstat::unmark(spatstat::subset.ppp(beech, type != "dead"))

beech_dead <- spatstat::unmark(spatstat::subset.ppp(beech, type == "dead"))

#### Pattern reconstruction ####

# set parameters
n_random <- 199 # 199
n_random_hpc <- rep(x = 1, times = n_random) # if HPC is used
n_random_large <- 4999

max_runs <- 20000 # 20000

comp_fast <- 0

# living
# reconstruct pattern
# reconstructed_beech_living <- shar::reconstruct_pattern_cluster(pattern = beech_living,
#                                                                 n_random = n_random,
#                                                                 max_runs = max_runs,
#                                                                 comp_fast = comp_fast)

reconstructed_beech_living <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                         n_random = n_random_hpc,
                                                         const = list(pattern = beech_living,
                                                                      max_runs = max_runs,
                                                                      comp_fast = comp_fast,
                                                                      return_input = FALSE,
                                                                      simplify = TRUE,
                                                                      verbose = FALSE),
                                                         seed = 42,
                                                         n_jobs = length(n_random_hpc),
                                                         template = list(job_name = "beech_living_recon",
                                                                         queue = "medium",
                                                                         walltime = "48:00",
                                                                         processes = 1,
                                                                         log_file = "beech_living_recon.log"))

# add observed pattern
reconstructed_beech_living[[length(n_random) + 1]] <- spatstat::unmark(beech_living)

# add names to list
names(reconstructed_beech_living) <- c(paste0("randomized_", seq_along(n_random)),
                                       "observed")

fitted_beech_living <- shar::fit_point_process(beech_living, n_random = n_random_large,
                                               process = "cluster")

# save reconstructed pattern
helpeR::save_rds(object = reconstructed_beech_living,
                           filename = "reconstructed_beech_living.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
helpeR::save_rds(object = fitted_beech_living,
                           filename = "fitted_beech_living.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# dead
# reconstruct pattern
# reconstructed_beech_dead <- shar::reconstruct_pattern_cluster(pattern = beech_dead,
#                                                               n_random = n_random,
#                                                               max_runs = max_runs,
#                                                               comp_fast = comp_fast)

reconstructed_beech_dead <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                       n_random = n_random_hpc,
                                                       const = list(pattern = beech_dead,
                                                                    max_runs = max_runs,
                                                                    comp_fast = comp_fast,
                                                                    return_input = FALSE,
                                                                    simplify = TRUE,
                                                                    verbose = FALSE),
                                                       seed = 42,
                                                       n_jobs = length(n_random_hpc),
                                                       template = list(job_name = "beech_dead_recon",
                                                                       queue = "medium",
                                                                       walltime = "48:00",
                                                                       processes = 1,
                                                                       log_file = "beech_dead_recon.log"))

# add observed pattern
reconstructed_beech_dead[[length(n_random) + 1]] <- spatstat::unmark(beech_dead)

# add names to list
names(reconstructed_beech_dead) <- c(paste0("randomized_", seq_along(n_random)),
                                     "observed")

fitted_beech_dead <- shar::fit_point_process(beech_dead, n_random = n_random_large,
                                             process = "cluster")

# save reconstructed pattern
helpeR::save_rds(object = reconstructed_beech_dead,
                           filename = "reconstructed_beech_dead.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
helpeR::save_rds(object = fitted_beech_dead,
                           filename = "fitted_beech_dead.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))
