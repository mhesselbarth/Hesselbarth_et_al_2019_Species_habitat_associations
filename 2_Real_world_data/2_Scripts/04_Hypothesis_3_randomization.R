###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 3 ####

# Load packages #

library(clustermq)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

#### Import data #### 

# import point pattern data
pattern_2007 <- readr::read_rds("2_Real_world_data/1_Data/02_pattern_2007_ppp.rds")

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, type != "dead")

pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, type == "dead")

# only enough individuals in all groups for beech
beech <- spatstat::subset.ppp(pattern_2007_living, species == "beech")

# split into DBH groups
beech_small <- spatstat::unmark(spatstat::subset.ppp(beech, dbh_group_07 == "small"))

beech_medium <- spatstat::unmark(spatstat::subset.ppp(beech, dbh_group_07 == "medium"))

beech_large <- spatstat::unmark(spatstat::subset.ppp(beech, dbh_group_07 == "large"))

#### Pattern randomization ####

# set parameters
n_random <- 199 # 199
n_random_hpc <- rep(x = 1, times = n_random) # if HPC is used
n_random_large <- 4999

max_runs <- 20000 # 20000

comp_fast <- 0

# small
# reconstruct pattern
# reconstructed_beech_small <- shar::reconstruct_pattern_cluster(pattern = beech_small,
#                                                                n_random = n_random,
#                                                                max_runs = max_runs,
#                                                                comp_fast = comp_fast)

reconstructed_beech_small <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                        n_random = n_random_hpc,
                                                        const = list(pattern = beech_small,
                                                                     max_runs = max_runs,
                                                                     comp_fast = comp_fast,
                                                                     return_input = FALSE,
                                                                     simplify = TRUE,
                                                                     verbose = FALSE),
                                                        seed = 42,
                                                        n_jobs = length(n_random_hpc),
                                                        template = list(job_name = "beech_small_recon",
                                                                        queue = "medium",
                                                                        walltime = "48:00",
                                                                        processes = 1,
                                                                        log_file = "beech_small_recon.log"))
# add observed pattern
reconstructed_beech_small[[length(n_random) + 1]] <- beech_small

# add names to list
names(reconstructed_beech_small) <- c(paste0("randomized_", seq_along(n_random)),
                                      "observed")

fitted_beech_small <- shar::fit_point_process(beech_small, n_random = n_random_large,
                                              process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_beech_small,
                           filename = "reconstructed_beech_small.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_beech_small,
                           filename = "fitted_beech_small.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# medium
# reconstruct pattern
# reconstructed_beech_medium <- shar::reconstruct_pattern_cluster(pattern = beech_medium,
#                                                                 n_random = n_random,
#                                                                 max_runs = max_runs,
#                                                                 comp_fast = comp_fast)

reconstructed_beech_medium <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                         n_random = n_random_hpc,
                                                         const = list(pattern = beech_medium,
                                                                      max_runs = max_runs,
                                                                      comp_fast = comp_fast,
                                                                      return_input = FALSE,
                                                                      simplify = TRUE,
                                                                      verbose = FALSE),
                                                         seed = 42,
                                                         n_jobs = length(n_random_hpc),
                                                         template = list(job_name = "beech_medium_recon",
                                                                         queue = "medium",
                                                                         walltime = "48:00",
                                                                         processes = 1,
                                                                         log_file = "beech_medium_recon.log"))

# add observed pattern
reconstructed_beech_medium[[length(n_random) + 1]] <- beech_medium

# add names to list
names(reconstructed_beech_medium) <- c(paste0("randomized_", seq_along(n_random)),
                                       "observed")

fitted_beech_medium <- shar::fit_point_process(beech_medium, n_random = n_random_large,
                                               process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_beech_medium,
                           filename = "reconstructed_beech_medium.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_beech_medium,
                           filename = "fitted_beech_medium.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# large
# reconstruct pattern
# reconstructed_beech_large <- shar::reconstruct_pattern_cluster(pattern = beech_large,
#                                                                n_random = n_random,
#                                                                max_runs = max_runs,
#                                                                comp_fast = comp_fast)

reconstructed_beech_large <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                        n_random = n_random_hpc,
                                                        const = list(pattern = beech_large,
                                                                     max_runs = max_runs,
                                                                     comp_fast = comp_fast,
                                                                     return_input = FALSE,
                                                                     simplify = TRUE,
                                                                     verbose = FALSE),
                                                        seed = 42,
                                                        n_jobs = length(n_random_hpc),
                                                        template = list(job_name = "beech_large_recon",
                                                                        queue = "medium",
                                                                        walltime = "48:00",
                                                                        processes = 1,
                                                                        log_file = "beech_large_recon.log"))

# add observed pattern
reconstructed_beech_large[[length(n_random) + 1]] <- beech_large

# add names to list
names(reconstructed_beech_large) <- c(paste0("randomized_", seq_along(n_random)),
                                      "observed")

fitted_beech_large <- shar::fit_point_process(beech_large, n_random = n_random_large,
                                              process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_beech_large,
                           filename = "reconstructed_beech_large.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_beech_large,
                           filename = "fitted_beech_large.rds",
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))