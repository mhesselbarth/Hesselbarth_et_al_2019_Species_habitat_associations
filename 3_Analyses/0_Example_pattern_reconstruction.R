#### Simulation study ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & sunctions ####

# Packages #
library(NLMR)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '^[0_ 1_]', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Specify future topology ####
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
#                                        processes = 24))
# 
# future::plan(list(login, bsub, future::multiprocess))
# 
# future::plan(list(future::multiprocess, future::multiprocess))
# future::plan(future::multiprocess)
# 


#### 3. Create data ####

simulation_habitats <- NLMR::nlm_mpd(ncol = 30, nrow = 30,
                                     resolution = 20, roughness = 0.3, 
                                     verbose = FALSE) %>%
  SHAR::classify_habitats(classes=5)

simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                number_points = 50, 
                                                alpha = 0.35)
    
names_species <- simulation_pattern$marks$Species %>%
  unique() %>%
  as.character()  

reconstruction_species_1 <- spatstat::subset.ppp(simulation_pattern, Species_code == 1) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 2500, fitting = FALSE)

reconstruction_species_2 <- spatstat::subset.ppp(simulation_pattern, Species_code == 2) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 2500, fitting = TRUE)

detection_spec_3 <-  spatstat::subset.ppp(simulation_pattern, Species_code == 3) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 2500, fitting = FALSE)

detection_spec_3 <-  spatstat::subset.ppp(simulation_pattern, Species_code == 4) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 2500, fitting = TRUE)
