#### Simulation study - Neutral species ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################


#### Install packages ####
toc <- "cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec" # remove for publication!
devtools::install_github("mhesselbarth/SHAR", auth_token=toc, quiet=T)
devtools::install_github("mhesselbarth/UtilityFunctions", auth_token=toc, quiet=T)
rm(toc)

#### Import packages ####
# Packages #
library(SHAR)
library(magrittr)
library(NLMR)
library(UtilityFunctions)
library(stringi)

# Using the R compiler package for just-in-time compiler
compiler::enableJIT(3)
compiler::setCompilerOptions(suppressAll=TRUE)
compiler::setCompilerOptions(optimize=3)

# Set working directory
results <- paste0(getwd(), "/Results")

# Set seed
set.seed(42)

#### Functions for simulation study ####
simulation_habitat_randomization_neutral <- function(number_coloumns, number_rows,
                                             resolution, roughness, number_points,
                                             simulation_runs, number_maps){

  detections_tibble <- tibble::tibble(Correct=numeric(), False=numeric(), Species=numeric(), Alpha=numeric())

  # Create landscape and simulation pattern
  simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                         resolution=resolution, roughness=roughness, verbose=F) %>%
    SHAR::Habitat.Classification(classes=5)

  species_1 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Poisson",
                                         number_points=number_points, species_code=1,
                                         habitat=NULL, alpha=NULL, verbose=F)

  species_2 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Thomas",
                                       number_points=number_points, species_code=2,
                                       habitat=NULL, alpha=NULL, verbose=F)

  simulation_pattern <- spatstat::superimpose(species_1, species_2)

  # Loop over runs
  for(j in 1:simulation_runs){
    # Random number to create new data in 25% of runs
    r <- runif(n=1)
    if(r<=1/8){
      simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                             resolution=resolution, roughness=roughness, verbose=F) %>%
        SHAR::Habitat.Classification(classes=5)

      species_1 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Poisson",
                                             number_points=number_points, species_code=1,
                                             habitat=NULL, alpha=NULL, verbose=F)

      species_2 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Thomas",
                                       number_points=number_points, species_code=2,
                                       habitat=NULL, alpha=NULL, verbose=F)

      simulation_pattern <- spatstat::superimpose(species_1, species_2)
    }

    result <- SHAR::Habitat.Randomization(raster=simulation_habitats,
                                            method="randomization_algorithm",
                                            number_maps=number_maps,
                                            parallel=T, verbose=F) %>%
      SHAR::Results.Habitat.Association(pattern=simulation_pattern,
                                          raster=.,
                                          method="random_raster")

      # Correct and false detections of separated species
      # Species 1: Neutral associations (Poisson)
      detections_tibble[nrow(detections_tibble)+1,] <- c(Correct=ifelse(all(result[[1]]$Significance=="N.S."), 1, 0),
                                                         False=ifelse(all(result[[1]]$Significance=="N.S."), 0, 1),
                                                         Species=1, Association=0)

      # Species 2: Neutral associations (Thomas process)
      detections_tibble[nrow(detections_tibble)+1,] <- c(Correct=ifelse(all(result[[2]]$Significance=="N.S."), 1, 0),
                                                        False=ifelse(all(result[[2]]$Significance=="N.S."), 0, 1),
                                                        Species=2, Association=0)


      # Print progress of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
    }
  return(detections_tibble)
}

simulation_torus_translation_neutral <- function(number_coloumns, number_rows,
                                         resolution, roughness, number_points,
                                         simulation_runs, alpha_sequence){

  detections_tibble <- tibble::tibble(Correct=numeric(), False=numeric(), Species=numeric(), Alpha=numeric())

  # Create landscape and simulation pattern
  simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                       resolution=resolution, roughness=roughness, verbose=F) %>%
    SHAR::Habitat.Classification(classes=5)

  species_1 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Poisson",
                                         number_points=number_points, species_code=1,
                                         habitat=NULL, alpha=NULL, verbose=F)

  species_2 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Thomas",
                                         number_points=number_points, species_code=2,
                                         habitat=NULL, alpha=NULL, verbose=F)

  simulation_pattern <- spatstat::superimpose(species_1, species_2)

  # Loop over runs
  for(j in 1:simulation_runs){
    # Random number to create new data in 25% of runs
    r <- runif(n=1)
    if(r<=1/8){
      simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                           resolution=resolution, roughness=roughness, verbose=F) %>%
        SHAR::Habitat.Classification(classes=5)

      species_1 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Poisson",
                                             number_points=number_points, species_code=1,
                                             habitat=NULL, alpha=NULL, verbose=F)

      species_2 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Thomas",
                                             number_points=number_points, species_code=2,
                                             habitat=NULL, alpha=NULL, verbose=F)

      simulation_pattern <- spatstat::superimpose(species_1, species_2)
    }

    result <- SHAR::Habitat.Randomization(raster=simulation_habitats,
                                          method="torus_translation",
                                          number_maps=number_maps,
                                          parallel=F, verbose=F) %>%
      SHAR::Results.Habitat.Association(pattern=simulation_pattern,
                                        raster=.,
                                        method="random_raster")

    # Correct and false detections of separated species
    # Species 1: Neutral associations (Poisson)
    detections_tibble[nrow(detections_tibble)+1,] <- c(Correct=ifelse(all(result[[1]]$Significance=="N.S."), 1, 0),
                                                       False=ifelse(all(result[[1]]$Significance=="N.S."), 0, 1),
                                                       Species=1, Association=0)

    # Species 2: Neutral associations (Thomas process)
    detections_tibble[nrow(detections_tibble)+1,] <- c(Correct=ifelse(all(result[[2]]$Significance=="N.S."), 1, 0),
                                                       False=ifelse(all(result[[2]]$Significance=="N.S."), 0, 1),
                                                       Species=2, Association=0)


    # Print progress of simulation runs every 10 runs
    if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
  }
  return(detections_tibble)
}

simulation_point_process_neutral <- function(number_coloumns, number_rows,
                                     resolution, roughness, number_points,
                                     simulation_runs, number_pattern, alpha_sequence){

  detections_tibble <- tibble::tibble(Correct=numeric(), False=numeric(), Species=numeric(), Alpha=numeric())

  # Create landscape and simulation pattern
  simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                       resolution=resolution, roughness=roughness, verbose=F) %>%
    SHAR::Habitat.Classification(classes=5)

  species_1 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Poisson",
                                         number_points=number_points, species_code=1,
                                         habitat=NULL, alpha=NULL, verbose=F)

  species_2 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Thomas",
                                         number_points=number_points, species_code=2,
                                         habitat=NULL, alpha=NULL, verbose=F)

  # Loop over runs
  for(j in 1:simulation_runs){
    # Random number to create new data in 25% of runs
    r <- runif(n=1)
    if(r<=1/8){
      simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                           resolution=resolution, roughness=roughness, verbose=F) %>%
        SHAR::Habitat.Classification(classes=5)

      species_1 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Poisson",
                                             number_points=number_points, species_code=1,
                                             habitat=NULL, alpha=NULL, verbose=F)

      species_2 <- Create.Simulation.Species(raster=simulation_habitats, type="neutral", process="Thomas",
                                             number_points=number_points, species_code=2,
                                             habitat=NULL, alpha=NULL, verbose=F)
    }

    # Species 1 - Poisson process
    randomized_pattern_species_1 <- species_1 %>%
      spatstat::unmark() %>%
      spatstat::ppm() %>%
      spatstat::simulate.ppm(nsim=number_pattern, progress=F)

    randomized_pattern_species_1[[length(randomized_pattern_species_1)+1]] <- species_1
    names(randomized_pattern_species_1)[[length(randomized_pattern_species_1)]] <- "Observed"

    # Results using simulated vs. observed data
    result_species_1 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_1,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)


    # Species 2 - Thomas process
    randomized_pattern_species_2 <- species_2 %>%
      spatstat::unmark() %>%
      spatstat::kppm(cluster="Thomas", statistic="pcf",
                       statargs=list(divisor="d")) %>%
      spatstat::simulate.kppm(nsim=number_pattern, verbose=F)

    randomized_pattern_species_2[[length(randomized_pattern_species_2)+1]] <- species_2
    names(randomized_pattern_species_2)[[length(randomized_pattern_species_2)]] <- "Observed"

    result_species_2 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_2,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)

    # Correct and false detections of separated species
    # Species 1: Positive associations (Poisson)
    detections_tibble[nrow(detections_tibble)+1,] <- c(Correct=ifelse(all(result_species_1$Significance=="N.S."), 1, 0),
                                                       False=ifelse(all(result_species_1$Significance=="N.S."), 0, 1),
                                                       Species=1, Association=0)

    # Species 2: Positive associations (Thomas process)
    detections_tibble[nrow(detections_tibble)+1,] <- c(Correct=ifelse(all(result_species_2$Significance=="N.S."), 1, 0),
                                                       False=ifelse(all(result_species_2$Significance=="N.S."), 0, 1),
                                                       Species=2, Association=0)

    # Print progress of simulation runs every 10 runs
    if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
  }

  # Return result list
  return(detections_tibble)
}

#### Define parameters ####
# Number of coloumns and rows for neutral landscape
number_coloumns <- 30
number_rows <- 30
# Resolution of neutral landscape
resolution <- 20
# Roughness of neutral landscape
roughness <- 0.3
# Approxmitated number of points for each species
number_points <- 500 
# Number of runs
simulation_runs <- 20 # 100
# Number of randomized habitat maps / point patterns
number_maps <- 19 # 199
number_pattern <- 19 # 199
# Different association strengths
alpha_sequence <- seq(0.25, 0.75, 0.25) # seq(0, 1, 0.05)

#### Simulation study of different methods to analyze species habitat assocations - Neutral species ####
# Habitat randomization (Harms et al. 2001) #
simulation_study_habitat_randomization_neutral <- simulation_habitat_randomization_neutral(number_coloumns=number_coloumns,
                                                                           number_rows=number_rows,
                                                                           roughness=roughness,
                                                                           resolution=resolution,
                                                                           number_maps=number_maps,
                                                                           number_points=number_points,
                                                                           simulation_runs=simulation_runs)

# Save.Function.rds(object=simulation_study_habitat_randomization_neutral,
#                   file=paste0(results,"/simulation_study_habitat_randomization_neutral.rds"))


# Torus translation (Harms et al. 2001) #
simulation_study_torus_translation_neutral <- simulation_torus_translation_neutral(number_coloumns=number_coloumns,
                                                                   number_rows=number_rows,
                                                                   roughness=roughness,
                                                                   resolution=resolution,
                                                                   number_points=number_points,
                                                                   simulation_runs=simulation_runs)

# Save.Function.rds(object=simulation_study_torus_translation_neutral,
                  # file=paste0(results,"/simulation_study_torus_translation_neutral.rds"))


# Fitting point process (Plotkin et al. 2000) #
simulation_study_point_process_neutral <- simulation_point_process_neutral(number_coloumns=number_coloumns,
                                                           number_rows=number_rows,
                                                           roughness=roughness,
                                                           resolution=resolution,
                                                           number_pattern=number_pattern,
                                                           number_points=number_points,
                                                           simulation_runs=simulation_runs)
# Save.Function.rds(object=simulation_study_point_process_neutral,
#                   file=paste0(results,"/simulation_study_point_process_neutral.rds"))