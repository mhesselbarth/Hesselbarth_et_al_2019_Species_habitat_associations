###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 1 & 2 ####

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

# split into species
beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, species == "beech"))
ash <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "ash"))
hornbeam <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "hornbeam"))
sycamore <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "sycamore"))
others <- spatstat::unmark(subset.ppp(pattern_2007_living, species == "others"))

#### Pattern randomization ####

# set parameters
n_random <- 199 # 199
n_random_hpc <- rep(x = 1, times = n_random) # if HPC is used
n_random_large <- 4999

max_runs <- 20000 # 20000

# use fast computation
comp_fast <- 0

# Beech #
# reconstruct pattern
# reconstructed_beech <- shar::reconstruct_pattern_cluster(pattern = beech,
#                                                          n_random = n_random,
#                                                          max_runs = max_runs,
#                                                          comp_fast = comp_fast)

reconstructed_beech <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                  n_random = n_random_hpc,
                                                  const = list(pattern = beech,
                                                               max_runs = max_runs,
                                                               comp_fast = comp_fast,
                                                               verbose = FALSE,
                                                               return_input = FALSE,
                                                               simplify = TRUE),
                                                  seed = 42,
                                                  n_jobs = n_random,
                                                  template = list(job_name = "beech_rec",
                                                                  queue = "medium",
                                                                  walltime = "01:00:00",
                                                                  mem_cpu = "3072",
                                                                  processes = 1,
                                                                  log_file = "log_beech_rec.log"))

# add observed pattern
reconstructed_beech[[n_random + 1]] <- beech

# add names to list
names(reconstructed_beech) <- c(paste0("randomized_", 1:n_random),
                                "observed")

# fit point process
fitted_beech <- shar::fit_point_process(beech, n_random = n_random_large,
                                        process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_beech,
                  filename = "reconstructed_beech.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_beech,
                  filename = "fitted_beech.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Ash
# reconstruct pattern
# reconstructed_ash <- shar::reconstruct_pattern_cluster(pattern = ash,
#                                                        n_random = n_random,
#                                                        max_runs = max_runs,
#                                                        comp_fast = comp_fast)

reconstructed_ash <- suppoRt::submit_to_cluster(fun = reconstruct_pattern_cluster,
                                                n_random = n_random_hpc,
                                                const = list(pattern = ash,
                                                             max_runs = max_runs,
                                                             comp_fast = comp_fast,
                                                             return_input = FALSE,
                                                             simplify = TRUE,
                                                             verbose = FALSE),
                                                seed = 42,
                                                n_jobs = n_random,
                                                template = list(job_name = "ash_rec",
                                                                queue = "medium",
                                                                walltime = "01:00:00",
                                                                mem_cpu = "3072",
                                                                processes = 1,
                                                                log_file = "log_ash_rec.log"))

# add observed pattern
reconstructed_ash[[n_random + 1]] <- ash

# add names to list
names(reconstructed_ash) <- c(paste0("randomized_", 1:n_random),
                              "observed")

# fit point process
fitted_ash <- shar::fit_point_process(ash, n_random = n_random_large,
                                      process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_ash,
                  filename = "reconstructed_ash.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_ash,
                  filename = "fitted_ash.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Hornbeam
# reconstruct pattern
# reconstructed_hornbeam <- shar::reconstruct_pattern_cluster(pattern = hornbeam,
#                                                             n_random = n_random,
#                                                             max_runs = max_runs,
#                                                             comp_fast = comp_fast)

reconstructed_hornbeam <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                     n_random = n_random_hpc,
                                                     const = list(pattern = hornbeam,
                                                                  max_runs = max_runs,
                                                                  comp_fast = comp_fast,
                                                                  return_input = FALSE,
                                                                  simplify = TRUE,
                                                                  verbose = FALSE),
                                                     seed = 42,
                                                     n_jobs = n_random,
                                                     template = list(job_name = "hornbeam_rec",
                                                                     queue = "medium",
                                                                     walltime = "01:00:00",
                                                                     mem_cpu = "3072",
                                                                     processes = 1,
                                                                     log_file = "log_hornbeam_rec.log"))

# add observed pattern
reconstructed_hornbeam[[n_random + 1]] <- hornbeam

# add names to list
names(reconstructed_hornbeam) <- c(paste0("randomized_", 1:n_random),
                                   "observed")

# fit point process
fitted_hornbeam <- shar::fit_point_process(hornbeam, n_random = n_random_large,
                                           process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_hornbeam,
                  filename = "reconstructed_hornbeam.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_hornbeam,
                  filename = "fitted_hornbeam.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Sycamore
# reconstruct pattern
# reconstructed_sycamore <- shar::reconstruct_pattern_cluster(pattern = sycamore,
#                                                             n_random = n_random,
#                                                             max_runs = max_runs,
#                                                             comp_fast = comp_fast)

reconstructed_sycamore <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                     n_random = n_random_hpc,
                                                     const = list(pattern = sycamore,
                                                                  max_runs = max_runs,
                                                                  comp_fast = comp_fast,
                                                                  return_input = FALSE,
                                                                  simplify = TRUE,
                                                                  verbose = FALSE),
                                                     seed = 42,
                                                     n_jobs = n_random,
                                                     template = list(job_name = "sycamore_rec",
                                                                     queue = "medium",
                                                                     walltime = "01:00:00",
                                                                     mem_cpu = "3072",
                                                                     processes = 1,
                                                                     log_file = "log_sycamore_rec.log"))

# add observed pattern
reconstructed_sycamore[[n_random + 1]] <- sycamore

# add names to list
names(reconstructed_sycamore) <- c(paste0("randomized_", 1:n_random),
                                   "observed")

# fit point process
fitted_sycamore <- shar::fit_point_process(sycamore, n_random = n_random_large,
                                           process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_sycamore,
                  filename = "reconstructed_sycamore.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_sycamore,
                  filename = "fitted_sycamore.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# others
# reconstruct pattern
# reconstructed_others <- shar::reconstruct_pattern_cluster(pattern = others,
#                                                           n_random = n_random,
#                                                           max_runs = max_runs,
#                                                           comp_fast = comp_fast)

reconstructed_others <- suppoRt::submit_to_cluster(fun = shar::reconstruct_pattern_cluster,
                                                   n_random = n_random_hpc,
                                                   const = list(pattern = others,
                                                                max_runs = max_runs,
                                                                comp_fast = comp_fast,
                                                                return_input = FALSE,
                                                                simplify = TRUE,
                                                                verbose = FALSE),
                                                   seed = 42,
                                                   n_jobs = n_random,
                                                   template = list(job_name = "others_rec",
                                                                   queue = "medium",
                                                                   walltime = "01:00:00",
                                                                   mem_cpu = "3072",
                                                                   processes = 1,
                                                                   log_file = "log_others_rec.log"))

# add observed pattern
reconstructed_others[[n_random + 1]] <- others

# add names to list
names(reconstructed_others) <- c(paste0("randomized_", 1:n_random),
                                 "observed")

# fit point process
fitted_others <- shar::fit_point_process(others, n_random = n_random_large,
                                         process = "cluster")

# save reconstructed pattern
suppoRt::save_rds(object = reconstructed_others,
                  filename = "reconstructed_others.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# save reconstructed pattern
suppoRt::save_rds(object = fitted_others,
                  filename = "fitted_others.rds",
                  path = paste0(getwd(), "/2_Real_world_data/3_Results"))
