###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 1 & 2 ####

# Load packages #

# library(clustermq)
library(clustermq)

library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(raster)
library(SHAR) # devtools::install_github("r-spatialecology/SHAR")
library(spatstat)
library(tidyverse)

#### Pattern reconstruction ####

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")
pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")

# set parameters
# n_random <- 19 # 199
n_random <- rep(1, 199) # if HPC is used
max_runs <- 10 # 10000
fitting <- FALSE # TRUE

# Beech
beech <- spatstat::subset.ppp(pattern_2007_living, Species == "Beech")

# reconstruct pattern
# reconstructed_beech <- SHAR::reconstruct_pattern(pattern = beech, 
#                                                  n_random = n_random, 
#                                                  max_runs = max_runs, 
#                                                  fitting = fitting, 
#                                                  comp_fast = TRUE)

reconstructed_beech <- clustermq::Q(fun = reconstruct_pattern,
                                    n_random = n_random,
                                    const = list(pattern = beech,
                                                 max_runs = max_runs,
                                                 fitting = fitting,
                                                 comp_fast = TRUE,
                                                 verbose = FALSE,
                                                 return_input = FALSE,
                                                 simplify = TRUE),
                                    seed = 42,
                                    n_jobs = length(n_random),
                                    template = list(queue = "mpi",
                                                    walltime = "48:00",
                                                    processes = 1))

reconstructed_beech[[length(n_random) + 1]] <- spatstat::unmark(beech)
names(reconstructed_beech) <- c(paste0("randomized_", seq_along(n_random)),
                                "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_beech, 
                           filename = "reconstructed_beech.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Ash
ash <- subset.ppp(pattern_2007_living, Species == "Ash")

# reconstruct pattern
# reconstructed_ash <- SHAR::reconstruct_pattern(pattern = ash, 
#                                                n_random = n_random, 
#                                                max_runs = max_runs, 
#                                                fitting = fitting)

reconstructed_ash <- clustermq::Q(fun = reconstruct_pattern,
                                  n_random = n_random,
                                  const = list(pattern = ash,
                                               max_runs = max_runs,
                                               fitting = fitting,
                                               return_input = FALSE,
                                               simplify = TRUE,
                                               verbose = FALSE),
                                  seed = 42,
                                  n_jobs = length(n_random),
                                  template = list(queue = "mpi",
                                                  walltime = "48:00",
                                                  processes = 1))

reconstructed_ash[[length(n_random) + 1]] <- spatstat::unmark(ash)
names(reconstructed_ash) <- c(paste0("randomized_", seq_along(n_random)),
                              "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_ash, 
                           filename = "reconstructed_ash.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Hornbeam
hornbeam <- subset.ppp(pattern_2007_living, Species == "Hornbeam")

# reconstruct pattern
# reconstructed_hornbeam <- SHAR::reconstruct_pattern(pattern = hornbeam, 
#                                                     n_random = n_random, 
#                                                     max_runs = max_runs, 
#                                                     fitting = fitting)

reconstructed_hornbeam <- clustermq::Q(fun = reconstruct_pattern,
                                       n_random = n_random,
                                       const = list(pattern = hornbeam,
                                                    max_runs = max_runs,
                                                    fitting = fitting,
                                                    return_input = FALSE,
                                                    simplify = TRUE,
                                                    verbose = FALSE),
                                       seed = 42,
                                       n_jobs = length(n_random),
                                       template = list(queue = "mpi",
                                                       walltime = "48:00",
                                                       processes = 1))

reconstructed_hornbeam[[length(n_random) + 1]] <- spatstat::unmark(hornbeam)
names(reconstructed_hornbeam) <- c(paste0("randomized_", seq_along(n_random)),
                                   "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_hornbeam, 
                           filename = "reconstructed_hornbeam.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# Sycamore
sycamore <- subset.ppp(pattern_2007_living, Species == "Sycamore")

# reconstruct pattern
# reconstructed_sycamore <- SHAR::reconstruct_pattern(pattern = sycamore, 
#                                                     n_random = n_random, 
#                                                     max_runs = max_runs, 
#                                                     fitting = fitting)

reconstructed_sycamore <- clustermq::Q(fun = reconstruct_pattern,
                                       n_random = n_random,
                                       const = list(pattern = sycamore,
                                                    max_runs = max_runs,
                                                    fitting = fitting,
                                                    return_input = FALSE,
                                                    simplify = TRUE,
                                                    verbose = FALSE),
                                       seed = 42,
                                       n_jobs = length(n_random),
                                       template = list(queue = "mpi",
                                                       walltime = "48:00",
                                                       processes = 1))

reconstructed_sycamore[[length(n_random) + 1]] <- spatstat::unmark(sycamore)
names(reconstructed_sycamore) <- c(paste0("randomized_", seq_along(n_random)),
                                   "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_sycamore, 
                           filename = "reconstructed_sycamore.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

# others
others <- subset.ppp(pattern_2007_living, Species == "others")

# reconstruct pattern
# reconstructed_others <- SHAR::reconstruct_pattern(pattern = others, 
#                                                   n_random = n_random, 
#                                                   max_runs = max_runs, 
#                                                   fitting = fitting)

reconstructed_others <- clustermq::Q(fun = reconstruct_pattern,
                                     n_random = n_random,
                                     const = list(pattern = others,
                                                  max_runs = max_runs,
                                                  fitting = fitting,
                                                  return_input = FALSE,
                                                  simplify = TRUE,
                                                  verbose = FALSE),
                                     seed = 42,
                                     n_jobs = n_random,
                                     template = list(queue = "mpi",
                                                     walltime = "48:00",
                                                     processes = 1))
reconstructed_others[[length(n_random) + 1]] <- spatstat::unmark(others)
names(reconstructed_others) <- c(paste0("randomized_", seq_along(n_random)),
                                 "observed")

# save reconstructed pattern
UtilityFunctions::save_rds(object = reconstructed_others, 
                           filename = "reconstructed_others.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

#### Environmental data ####

environmental_data <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    
    data <- readr::read_rds(x)
    
    environment_raster <- raster::rasterFromXYZ(data)
    
    SHAR::classify_habitats(environment_raster, classes = 5)
  })

#### Habitat associations ####

# reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
# reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))
# reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))
# reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))
# reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))

names_environment <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

# Beech
associations_beech <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_beech,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_beech) <- names_environment

# Ash
associations_ash <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_ash,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_ash) <- names_environment

# Hornbeam
associations_hornbeam <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_hornbeam,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_hornbeam) <- names_environment

# Sycamore
associations_sycamore <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_sycamore,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_sycamore) <- names_environment

# others
associations_others <- purrr::map(environmental_data, function(x) {
  SHAR::results_habitat_association(pattern = reconstructed_others,
                                    raster = x, 
                                    verbose = FALSE)
})

names(associations_others) <- names_environment

#### Save results

UtilityFunctions::save_rds(object = associations_beech, 
                           filename = "associations_beech.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_ash, 
                           filename = "associations_ash.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_hornbeam, 
                           filename = "associations_hornbeam.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_sycamore, 
                           filename = "associations_sycamore.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))

UtilityFunctions::save_rds(object = associations_others, 
                           filename = "associations_others.rds", 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"))
