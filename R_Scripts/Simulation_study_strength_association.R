#### Simulation study - Strength associations #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### Import packages ####
# Packages #
library(SHAR)
library(NLMR)
library(UtilityFunctions)
library(stringi)
library(tidyverse)

# Using the R compiler package for just-in-time compiler
compiler::enableJIT(3)
compiler::setCompilerOptions(suppressAll=TRUE)
compiler::setCompilerOptions(optimize=3)

# Set working directory
results <- paste0(getwd(), "/Results")

# Set seed
set.seed(42)

#### Functions for simulation study #### 
Simulation.Habitat.Randomization <- function(number_coloumns, number_rows, 
                                             resolution, roughness, number_points, 
                                             simulation_runs, number_maps, alpha_sequence){
  
  detections_tibble <- tibble::tibble(Species=numeric(), Alpha=numeric(), Correct=numeric(), False=numeric())
  
  # Loop over association strength
  for(i in 1: length(alpha_sequence)){
    # Create landscape and simulation pattern
    simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                         resolution=resolution, roughness=roughness, verbose=F) %>%
      SHAR::Habitat.Classification(classes=5)
    
    simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                    number_points=number_points, 
                                                    alpha=alpha_sequence[i])
    
    # Loop over runs
    for(j in 1:simulation_runs){
      # Random number to create new data in 25% of runs
      r <- runif(n=1)
      if(r<=1/8){
        simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                             resolution=resolution, roughness=roughness, verbose=F) %>%
          SHAR::Habitat.Classification(classes=5)
        
        simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                        number_points=number_points, 
                                                        alpha=alpha_sequence[i])
      }
      
      result <- SHAR::Habitat.Randomization(raster=simulation_habitats, 
                                            method="randomization_algorithm", 
                                            number_maps=number_maps, 
                                            parallel=T, verbose=F) %>%
        SHAR::Results.Habitat.Association(pattern=simulation_pattern, 
                                          raster=., 
                                          method="random_raster")
      
      # Correct and false detections of separated species
      # Species 1: Positive associations (Poisson)
      habitat_1 <- as.numeric(stringi::stri_sub(names(result)[[1]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=1, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[1]]$Significance[result[[1]]$Habitat==habitat_1] == "Positive", na.rm=T),
                                                         False=sum(result[[1]]$Significance[result$Poisson_positive$Habitat!=habitat_1] == "Positive", na.rm=T))
      
      # Species 2: Positive associations (Thomas process)
      habitat_2 <- as.numeric(stringi::stri_sub(names(result)[[2]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=2, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[2]]$Significance[result[[2]]$Habitat==habitat_2] == "Positive", na.rm=T),
                                                         False=sum(result[[2]]$Significance[result[[2]]$Habitat!=habitat_2] == "Positive", na.rm=T))
      
      # Species 3: Negative associations (Poisson process)
      habitat_3 <- as.numeric(stringi::stri_sub(names(result)[[3]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=3, Alpha=alpha_sequence[i],
                                                         Correct=sum(result[[3]]$Significance[result[[3]]$Habitat==habitat_3] == "Negative", na.rm=T),
                                                         False=sum(result[[3]]$Significance[result[[3]]$Habitat!=habitat_3] == "Negative", na.rm=T))
      
      # Species 4: Negative associations (Thomas process)
      habitat_4 <- as.numeric(stringi::stri_sub(names(result)[[4]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=4, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[4]]$Significance[result[[4]]$Habitat==habitat_4] == "Negative", na.rm=T),
                                                         False=sum(result[[4]]$Significance[result[[4]]$Habitat!=habitat_4] == "Negative", na.rm=T))
      
      # Print progress of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
    }
    
    # Print progress of association strengths
    print(paste0(i , " from ", length(alpha_sequence), " possible alphas done"))
    cat("\n\n")
  }
  return(detections_tibble)
}

Simulation.Torus.Translation <- function(number_coloumns, number_rows, 
                                         resolution, roughness, number_points, 
                                         simulation_runs, alpha_sequence){
  # Data frames for results
  detections_tibble <- tibble::tibble(Species=numeric(), Alpha=numeric(), Correct=numeric(), False=numeric())
  
  # Loop over association strengths
  for(i in 1:length(alpha_sequence)){
    # Create  landscape and simulation pattern
    simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                         resolution=resolution, roughness=roughness, verbose=F) %>%
      SHAR::Habitat.Classification(classes=5)
    
    simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                    number_points=number_points, 
                                                    alpha=alpha_sequence[i])
    
    
    # Loop over simulation runs
    for(j in 1:simulation_runs){
      # Random number to create new simulation data in 25% of simulation runs
      r <- runif(n=1)
      if(r<=1/8){
        simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                             resolution=resolution, roughness=roughness, verbose=F) %>%
          SHAR::Habitat.Classification(classes=5)
        
        simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                        number_points=number_points, 
                                                        alpha=alpha_sequence[i])
      }
      
      result <- SHAR::Habitat.Randomization(raster=simulation_habitats, 
                                            method="torus_translation", 
                                            parallel=F, verbose=F) %>%
        SHAR::Results.Habitat.Association(pattern=simulation_pattern, 
                                          raster=., method='random_raster')
      
      # Correct and false detections of separated species
      # Species 1:  Positive associations (Poisson process) 
      habitat_1 <- as.numeric(stringi::stri_sub(names(result)[[1]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=1, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[1]]$Significance[result[[1]]$Habitat==habitat_1] == "Positive", na.rm=T),
                                                         False=sum(result[[1]]$Significance[result$Poisson_positive$Habitat!=habitat_1] == "Positive", na.rm=T))
      
      # Species 2: Positive associations (Thomas process)
      habitat_2 <- as.numeric(stringi::stri_sub(names(result)[[2]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=2, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[2]]$Significance[result[[2]]$Habitat==habitat_2] == "Positive", na.rm=T),
                                                         False=sum(result[[2]]$Significance[result[[2]]$Habitat!=habitat_2] == "Positive", na.rm=T))
      
      # Species 3: Negative associations (Poisson process) 
      habitat_3 <- as.numeric(stringi::stri_sub(names(result)[[3]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=3, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[3]]$Significance[result[[3]]$Habitat==habitat_3] == "Negative", na.rm=T),
                                                         False=sum(result[[3]]$Significance[result[[3]]$Habitat!=habitat_3] == "Negative", na.rm=T))
      
      # Species 4: Negative associations (Thomas process)
      habitat_4 <- as.numeric(stringi::stri_sub(names(result)[[4]],-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=4, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result[[4]]$Significance[result[[4]]$Habitat==habitat_4] == "Negative", na.rm=T),
                                                         False=sum(result[[4]]$Significance[result[[4]]$Habitat!=habitat_4] == "Negative", na.rm=T))
      
      # Print progess of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
    }
    
    # Print progress of association strength
    print(paste0(i , " from ", length(alpha_sequence), " possible alphas done"))
    cat("\n\n")
  }
  
  # Return result list
  return(detections_tibble)
}

Simulation.Point.Process <- function(number_coloumns, number_rows, 
                                     resolution, roughness, number_points, 
                                     simulation_runs, number_pattern, alpha_sequence){
  # Data frames for results 
  detections_tibble <- tibble::tibble(Species=numeric(), Alpha=numeric(), Correct=numeric(), False=numeric())
  
  # Loop over association strengths
  for(i in 1:length(alpha_sequence)){
    # Create landscape and simulation pattern
    simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                         resolution=resolution, roughness=roughness, verbose=F) %>%
      SHAR::Habitat.Classification(classes=5)
    
    simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                    number_points=number_points, 
                                                    alpha=alpha_sequence[i])
    
    # Loop over simulation runs
    for(j in 1:simulation_runs){
      # Random number to create new simulation data in 25 % of simulation runs
      r <- runif(n=1)
      if(r<=1/8){
        simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                             resolution=resolution, roughness=roughness, verbose=F) %>%
          SHAR::Habitat.Classification(classes=5)
        
        simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                        number_points=number_points, 
                                                        alpha=alpha_sequence[i])      
      }
      
      # Species 1
      pattern_species_1 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==1)
      
      randomized_pattern_species_1 <- pattern_species_1 %>%
        spatstat::unmark() %>%
        spatstat::ppm() %>% 
        spatstat::simulate.ppm(nsim=number_pattern, progress=F)
      
      randomized_pattern_species_1[[length(randomized_pattern_species_1)+1]] <- pattern_species_1
      names(randomized_pattern_species_1)[[length(randomized_pattern_species_1)]] <- "Observed"
      
      # Results using simulated vs. observed data 
      result_species_1 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_1,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)
      
      # Species 2
      pattern_species_2 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==2)
      
      randomized_pattern_species_2 <- pattern_species_2 %>%
        spatstat::unmark() %>%
        spatstat::ppm() %>% 
        spatstat::simulate.ppm(nsim=number_pattern, progress=F)
      
      randomized_pattern_species_2[[length(randomized_pattern_species_2)+1]] <- pattern_species_2
      names(randomized_pattern_species_2)[[length(randomized_pattern_species_2)]] <- "Observed"
      
      result_species_2 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_2,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)
      
      # Species 3 
      pattern_species_3 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==3)
      
      randomized_pattern_species_3 <- pattern_species_3 %>%
        spatstat::unmark() %>%
        spatstat::kppm(cluster="Thomas", statistic="pcf", 
                       statargs=list(divisor="d")) %>% 
        spatstat::simulate.kppm(nsim=number_pattern, verbose=F)
      
      randomized_pattern_species_3[[length(randomized_pattern_species_3)+1]] <- pattern_species_3
      names(randomized_pattern_species_3)[[length(randomized_pattern_species_3)]] <- "Observed"
      
      result_species_3 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_3,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)
      
      # Species 4
      pattern_species_4 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==4)
      
      randomized_pattern_species_4 <- pattern_species_4 %>%
        spatstat::unmark() %>%
        spatstat::ppm() %>% 
        spatstat::simulate.ppm(nsim=number_pattern, progress=F)
      
      randomized_pattern_species_4[[length(randomized_pattern_species_4)+1]] <- pattern_species_4
      names(randomized_pattern_species_4)[[length(randomized_pattern_species_4)]] <- "Observed"
      
      result_species_4 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_4,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)
      
      
      # Correct and false detections of separated species
      # Species 1: Positive associations (Poisson)
      habitat_1 <- as.numeric(stringi::stri_sub(unique(pattern_species_1$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=1, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_1$Significance[result_species_1$Habitat==habitat_1] == "Positive", na.rm=T),
                                                         False=sum(result_species_1$Significance[result_species_1$Habitat!=habitat_1] == "Positive", na.rm=T))
      
      # Species 2: Positive associations (Thomas process)
      habitat_2 <- as.numeric(stringi::stri_sub(unique(pattern_species_2$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=2, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_2$Significance[result_species_2$Habitat==habitat_2] == "Positive", na.rm=T),
                                                         False=sum(result_species_2$Significance[result_species_2$Habitat!=habitat_2] == "Positive", na.rm=T))
      
      # Species 3: Negative associations (Poisson process)
      habitat_3 <- as.numeric(stringi::stri_sub(unique(pattern_species_3$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=3, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_3$Significance[result_species_3$Habitat==habitat_3] == "Negative", na.rm=T),
                                                         False=sum(result_species_3$Significance[result_species_3$Habitat!=habitat_3] == "Negative", na.rm=T))
      
      # Species 4: Negative associations (Thomas process)
      habitat_4 <- as.numeric(stringi::stri_sub(unique(pattern_species_4$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=4, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_4$Significance[result_species_4$Habitat==habitat_4] == "Negative", na.rm=T),
                                                         False=sum(result_species_4$Significance[result_species_4$Habitat!=habitat_4] == "Negative", na.rm=T))
      
      # Print progress of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
    }
    
    # Print progress of association strengths
    print(paste0(i , " from ", length(alpha_sequence), " possible alphas done"))
    cat("\n\n")
  }
  
  # Return result list
  return(detections_tibble)
}

Simulation.Pattern.Reconstruction <- function(number_coloumns, number_rows, 
                                     resolution, roughness, number_points, 
                                     simulation_runs, number_pattern, alpha_sequence){
  # Data frames for results 
  detections_tibble <- tibble::tibble(Species=numeric(), Alpha=numeric(), Correct=numeric(), False=numeric())
  
  # Loop over association strengths
  for(i in 1:length(alpha_sequence)){
    # Create landscape and simulation pattern
    simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                         resolution=resolution, roughness=roughness, verbose=F) %>%
      SHAR::Habitat.Classification(classes=5)
    
    simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                    number_points=number_points, 
                                                    alpha=alpha_sequence[i])
    
    # Loop over simulation runs
    for(j in 1:simulation_runs){
      # Random number to create new simulation data in 25 % of simulation runs
      r <- runif(n=1)
      if(r<=1/8){
        simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows, 
                                             resolution=resolution, roughness=roughness, verbose=F) %>%
          SHAR::Habitat.Classification(classes=5)
        
        simulation_pattern <- Create.Simulation.Pattern(raster=simulation_habitats, 
                                                        number_points=number_points, 
                                                        alpha=alpha_sequence[i])      
      }
      
      # Species 1
      result_species_1 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==1) %>% 
        spatstat::unmark() %>%
        SHAR::Pattern.Reconstruction(number_reconstructions=number_pattern,
                                     max_runs=5000,  
                                     fitting=F, parallel=T, verbose=F) %>%
        SHAR::Results.Habitat.Association(raster=simulation_habitats,
                                          method='random_pattern', only_spatial=T)
      
      # Species 2
      result_species_2 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==2) %>% 
        spatstat::unmark() %>%
        SHAR::Pattern.Reconstruction(number_reconstructions=number_pattern, 
                                     max_runs=5000, 
                                     fitting=T, parallel=T, verbose=F) %>%
        SHAR::Results.Habitat.Association(raster=simulation_habitats,
                                          method='random_pattern', only_spatial=T)
      
      # Species 3 
      result_species_3 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==3) %>% 
        spatstat::unmark() %>%
        SHAR::Pattern.Reconstruction(number_reconstructions=number_pattern,
                                     max_runs=5000,  
                                     fitting=F, parallel=T, verbose=F) %>%
        SHAR::Results.Habitat.Association(raster=simulation_habitats,
                                          method='random_pattern', only_spatial=T)
      
      # Species 4
      result_species_4 <- simulation_pattern %>% 
        spatstat::subset.ppp(Species_code==4) %>% 
        spatstat::unmark() %>%
        SHAR::Pattern.Reconstruction(number_reconstructions=number_pattern,
                                     max_runs=5000,  
                                     fitting=T, parallel=T, verbose=F) %>%
        SHAR::Results.Habitat.Association(raster=simulation_habitats,
                                          method='random_pattern', only_spatial=T)
      
      # Correct and false detections of separated species
      # Species 1: Positive associations (Poisson)
      habitat_1 <- as.numeric(stringi::stri_sub(unique(pattern_species_1$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=1, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_1$Significance[result_species_1$Habitat==habitat_1] == "Positive", na.rm=T),
                                                         False=sum(result_species_1$Significance[result_species_1$Habitat!=habitat_1] == "Positive", na.rm=T))
      
      # Species 2: Positive associations (Thomas process)
      habitat_2 <- as.numeric(stringi::stri_sub(unique(pattern_species_2$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=2, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_2$Significance[result_species_2$Habitat==habitat_2] == "Positive", na.rm=T),
                                                         False=sum(result_species_2$Significance[result_species_2$Habitat!=habitat_2] == "Positive", na.rm=T))
      
      # Species 3: Negative associations (Poisson process)
      habitat_3 <- as.numeric(stringi::stri_sub(unique(pattern_species_3$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=3, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_3$Significance[result_species_3$Habitat==habitat_3] == "Negative", na.rm=T),
                                                         False=sum(result_species_3$Significance[result_species_3$Habitat!=habitat_3] == "Negative", na.rm=T))
      
      # Species 4: Negative associations (Thomas process)
      habitat_4 <- as.numeric(stringi::stri_sub(unique(pattern_species_4$marks$Species),-1))
      detections_tibble[nrow(detections_tibble)+1,] <- c(Species=4, Alpha=alpha_sequence[i], 
                                                         Correct=sum(result_species_4$Significance[result_species_4$Habitat==habitat_4] == "Negative", na.rm=T),
                                                         False=sum(result_species_4$Significance[result_species_4$Habitat!=habitat_4] == "Negative", na.rm=T))
      
      # Print progress of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", simulation_runs, " done"))}
    }
    
    # Print progress of association strengths
    print(paste0(i , " from ", length(alpha_sequence), " possible alphas done"))
    cat("\n\n")
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
simulation_runs <- 100
# Number of randomized habitat maps / point patterns
number_maps <- 199
number_pattern <-  199
# Different association strengths 
alpha_sequence <- seq(0.025, 0.75, 0.025)

#### Simulation study of different methods to analyze species habitat assocations ####
# Habitat randomization (Harms et al. 2001) # 
habitat_randomization <- Simulation.Habitat.Randomization(number_coloumns=number_coloumns,
                                                          number_rows=number_rows, 
                                                          roughness=roughness,
                                                          resolution=resolution, 
                                                          number_maps=number_maps, 
                                                          number_points=number_points,
                                                          alpha_sequence=alpha_sequence, 
                                                          simulation_runs=simulation_runs)

# Save.Function.rds(object=habitat_randomization, 
#                   file=paste0(results,"/strength_association_habitat_randomization.rds"))


# Torus translation (Harms et al. 2001) #
torus_translation<- Simulation.Torus.Translation(number_coloumns=number_coloumns, 
                                                 number_rows=number_rows, 
                                                 roughness=roughness,
                                                 resolution=resolution, 
                                                 number_points=number_points,
                                                 alpha_sequence=alpha_sequence, 
                                                 simulation_runs=simulation_runs)

# Save.Function.rds(object=torus_translation, 
#                   file=paste0(results,"/strength_association_torus_translation.rds"))


# Fitting point process (Plotkin et al. 2000) #
point_process <- Simulation.Point.Process(number_coloumns=number_coloumns, 
                                          number_rows=number_rows, 
                                          roughness=roughness,
                                          resolution=resolution,
                                          number_pattern=number_pattern, 
                                          number_points=number_points,
                                          alpha_sequence=alpha_sequence, 
                                          simulation_runs=simulation_runs)

# Save.Function.rds(object=point_process, 
#                   file=paste0(results,"/strength_association_point_process.rds"))

# Pattern reconstruction #
pattern_recnstruction <- Simulation.Pattern.Reconstruction(number_coloumns=number_coloumns, 
                                                           number_rows=number_rows, 
                                                           roughness=roughness,
                                                           resolution=resolution,
                                                           number_pattern=number_pattern, 
                                                           number_points=number_points,
                                                           alpha_sequence=alpha_sequence, 
                                                           simulation_runs=simulation_runs)

# Save.Function.rds(object=pattern_recnstruction, 
#                   file=paste0(results,"/strength_association_pattern_recnstruction.rds"))
