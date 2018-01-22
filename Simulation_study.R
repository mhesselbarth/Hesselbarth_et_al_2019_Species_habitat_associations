#### Simulation study #### 

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

#### Load libraries and data ####
library(SHAR)
library(NLMR)
library(UtilityFunctions)

# Using the R compiler package for just-in-time compiler
compiler::enableJIT(3)
compiler::setCompilerOptions(suppressAll=TRUE)
compiler::setCompilerOptions(optimize=3)

results <- paste0(getwd(), "/Results")

#### Functions for simulation study #### 
simulation_habitat_randomization <- function(nmbr_clmns, nmbr_rws, rsltn, rghnss, nmbr_pnts, rns_smltn ,nmbr_mps, lph_sqnc){
  # Data frames for results
  species_1_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_2_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_3_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_4_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_5_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_6_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
    
  # Loop over association strength
  for(i in 1: length(lph_sqnc)){
    # Create landscape
    capture.output(raster_mpd <- NLMR::nlm_mpd(ncol=nmbr_clmns, nrow=nmbr_rws, 
                                         resolution=rsltn, roughness=rghnss))
    # Classifiy landscape into discrete habitats
    simulation_habitats <- SHAR::Habitat.Classification(raster=raster_mpd, classes=5)
    # Create simulation pattern
    simulation_pattern <- SHAR::Create.Simulation.Pattern(habitats=simulation_habitats, 
                                                    number_points=nmbr_pnts, alpha=lph_sqnc[i])

    # Loop over runse
    for(j in 1:rns_smltn){
      # Random number to create new data in 25% of runs
      r <- runif(n=1)
      if(r<=1/4){
        # Create new simulation data
        capture.output(raster_mpd <- NLMR::nlm_mpd(ncol=nmbr_clmns, nrow=nmbr_rws, 
                                             resolution=rsltn, roughness=rghnss))
        simulation_habitats <- SHAR::Habitat.Classification(raster=raster_mpd, classes=5)
        simulation_pattern <- SHAR::Create.Simulation.Pattern(habitats=simulation_habitats, 
                                                        number_points=nmbr_pnts, alpha=lph_sqnc[i])      
      }

      # Harms et al. (2001) habitat randomization algorithm
      capture.output(habitat_maps <- SHAR::Habitat.Randomization(raster=simulation_habitats, method="randomization_algorithm", 
                                                                    number_maps=nmbr_mps, parallel=T))

      # Habitat associations using simulated vs. observed data
      result <- SHAR::Results.Habitat.Association(pattern=simulation_pattern, raster=habitat_maps,
                                                  method="random_raster")

      # Correct and false detections of separated species
      # Species 1: No associations (Poisson)
      species_1_df[nrow(species_1_df)+1,] <- c((sum(result$Species_1$Significance == "N.S.", na.rm=T)/5)*100,
                                             (sum(result$Species_1$Significance != "N.S.", na.rm=T)/5)*100,
                                             lph_sqnc[i])

      # Species 2: Positive associations habitat 2 (Poisson process)
      species_2_df[nrow(species_2_df)+1,] <- c((sum(result$Species_2$Significance[result$Species_2$Habitat=="Habitat_2"] == "Positive", na.rm=T)/1)*100,
                                             (sum(result$Species_2$Significance[result$Species_2$Habitat!="Habitat_2"] == "Positive", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 3: Positive associations habitat 3 (Thomas process)
      species_3_df[nrow(species_3_df)+1,] <- c((sum(result$Species_3$Significance[result$Species_3$Habitat=="Habitat_3"] == "Positive", na.rm=T)/1)*100,
                                             (sum(result$Species_3$Significance[result$Species_3$Habitat!="Habitat_3"] == "Positive", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 4: Negative associations habitat 4 (Poisson process)
      species_4_df[nrow(species_4_df)+1,] <- c((sum(result$Species_4$Significance[result$Species_4$Habitat=="Habitat_4"] == "Negative", na.rm=T)/1)*100,
                                             (sum(result$Species_4$Significance[result$Species_4$Habitat!="Habitat_4"] == "Negative", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 5: Negative associations habitat 5 (Thomas process)
      species_5_df[nrow(species_5_df)+1,] <- c((sum(result$Species_5$Significance[result$Species_5$Habitat=="Habitat_5"] == "Negative", na.rm=T)/1)*100,
                                             (sum(result$Species_5$Significance[result$Species_5$Habitat!="Habitat_5"] == "Negative", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 6: No associations (Thomas process)
      species_6_df[nrow(species_6_df)+1,] <- c((sum(result$Species_6$Significance == "N.S.", na.rm=T)/5)*100,
                                             (sum(result$Species_6$Significance!= "N.S.", na.rm=T)/5)*100,
                                             lph_sqnc[i])

      # Print progress of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", rns_smltn, " done"))}
    }
    
    # Print progress of association strengths
    print(paste0(i , " from ", length(lph_sqnc), " possible alphas done"))
    cat("\n\n")
  }
  
  # Combine results in list
  result_list <- list(Species_1=species_1_df, Species_2=species_2_df,
                      Species_3=species_3_df, Species_4=species_4_df,
                      Species_5=species_5_df, Species_6=species_6_df)
  # Return result list
  return(result_list)
}

simulation_torus_translation <- function(nmbr_clmns, nmbr_rws, rsltn, rghnss, nmbr_pnts, rns_smltn ,nmbr_mps, lph_sqnc){
  # Data frames for results
  species_1_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_2_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_3_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_4_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_5_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_6_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())

  # Loop over association strengths
  for(i in 1:length(lph_sqnc)){
    # Create  landscape
    raster_mpd <- NLMR::nlm_mpd(ncol=nmbr_clmns, nrow=nmbr_rws, resolution=rsltn, roughness=rghnss)
    # Classify landscape into discrete habitats
    simulation_habitats <- SHAR::Habitat.Classification(raster=raster_mpd, classes=5)
    # Create simulation pattern
    simulation_pattern <- SHAR::Create.Simulation.Pattern(habitats=simulation_habitats, 
                                                    number_points=nmbr_pnts, alpha=lph_sqnc[i])
    
    # Loop over simulation runs
    for(j in 1:rns_smltn){
      # Random number to create new simulation data in 25% of simulation runs
      r <- runif(n=1)
      if(r<=1/4){
        raster_mpd <- NLMR::nlm_mpd(ncol=nmbr_clmns, nrow=nmbr_rws, resolution=rsltn, roughness=rghnss)
        simulation_habitats <- SHAR::Habitat.Classification(raster=raster_mpd, classes=5)
        simulation_pattern <- SHAR::Create.Simulation.Pattern(habitats=simulation_habitats, 
                                                      number_points=nmbr_pnts, alpha=lph_sqnc[i])
      }
      
      shifted_raster <- SHAR::Habitat.Randomization(raster=simulation_habitats, method="torus_translation")

      # Results of habitat associations using observed vs. simulated data
      result <- SHAR::Results.Habitat.Association(pattern=simulated_pattern, 
                                            raster=shifted_raster, method='random_raster')

      # Correct and false detections of separated species
      # Species 1: No associations (Poisson)
      species_1_df[nrow(species_1_df)+1,] <- c((sum(result$Species_1$Significance == "N.S.", na.rm=T)/5)*100,
                                             (sum(result$Species_1$Significance != "N.S.", na.rm=T)/5)*100,
                                             lph_sqnc[i])

      # Species 2: Positive associations habitat 2 (Poisson process)
      species_2_df[nrow(species_2_df)+1,] <- c((sum(result$Species_2$Significance[result$Species_2$Habitat=="Habitat_2"] == "Positive", na.rm=T)/1)*100,
                                             (sum(result$Species_2$Significance[result$Species_2$Habitat!="Habitat_2"] == "Positive", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 3: Positive associations habitat 3 (Thomas process)
      species_3_df[nrow(species_3_df)+1,] <- c((sum(result$Species_3$Significance[result$Species_3$Habitat=="Habitat_3"] == "Positive", na.rm=T)/1)*100,
                                             (sum(result$Species_3$Significance[result$Species_3$Habitat!="Habitat_3"] == "Positive", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 4: Negative associations habitat 4 (Poisson process)
      species_4_df[nrow(species_4_df)+1,] <- c((sum(result$Species_4$Significance[result$Species_4$Habitat=="Habitat_4"] == "Negative", na.rm=T)/1)*100,
                                             (sum(result$Species_4$Significance[result$Species_4$Habitat!="Habitat_4"] == "Negative", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 5: Negative associations habitat 5 (Thomas process)
      species_5_df[nrow(species_5_df)+1,] <- c((sum(result$Species_5$Significance[result$Species_5$Habitat=="Habitat_5"] == "Negative", na.rm=T)/1)*100,
                                             (sum(result$Species_5$Significance[result$Species_5$Habitat!="Habitat_5"] == "Negative", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 6: No associations (Thomas process)
      species_6_df[nrow(species_6_df)+1,] <- c((sum(result$Species_6$Significance == "N.S.", na.rm=T)/5)*100,
                                             (sum(result$Species_6$Significance!= "N.S.", na.rm=T)/5)*100,
                                             lph_sqnc[i])

      # Print progess of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", rns_smltn, " done"))}
    }
    
    # Print progress of association strength
    print(paste0(i , " from ", length(lph_sqnc), " possible alphas done"))
    cat("\n\n")
  }
  
  # Combine results in one list
  result_list <- list(Species_1=species_1_df, Species_2=species_2_df,
                      Species_3=species_3_df, Species_4=species_4_df,
                      Species_5=species_5_df, Species_6=species_6_df)
  # Return result list
  return(result_list)
}

simulation_point_process <- function(nmbr_clmns, nmbr_rws, rsltn, rghnss, nmbr_pnts, rns_smltn, nmbr_pttrn, lph_sqnc){
  # Data frames for results 
  species_1_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_2_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_3_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_4_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_5_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())
  species_6_df <-  data.frame(Correct=numeric(), False=numeric(), Association=numeric())

  # Loop over association strengths
  for(i in 1: length(lph_sqnc)){
    # Create landscape
    raster_mpd <- NLMR::nlm_mpd(ncol=nmbr_clmns, nrow=nmbr_rws, resolution=rsltn, roughness=rghnss)
    # Classify landscape into discrete habitats
    simulation_habitats <- SHAR::Habitat.Classification(raster=raster_mpd, classes=5)
    # Create simulation pattern
    simulation_pattern <- SHAR::Create.Simulation.Pattern(habitats=simulation_habitats, 
                                                    number_points=nmbr_pnts, alpha=lph_sqnc[i])

    # Loop over simulation runs
    for(j in 1:rns_smltn){
      # Random number to create new simulation data in 25 % of simulation runs
      r <- runif(n=1)
      if(r<=1/4){
        raster_mpd <- NLMR::nlm_mpd(ncol=nmbr_clmns, nrow=nmbr_rws, resolution=rsltn, roughness=rghnss)
        simulation_habitats <- SHAR::Habitat.Classification(raster=raster_mpd, classes=5)
        simulation_pattern <- SHAR::Create.Simulation.Pattern(habitats=simulation_habitats, 
                                                        number_points=nmbr_pnts, alpha=lph_sqnc[i])
      }

      # Subset of species 
      species_1_pattern <- spatstat::subset.ppp(simulation_pattern, Species=="Species_1")
      # Fit point process model to data
      species_1_model <- spatstat::ppm(spatstat::unmark(species_1_pattern))
      # Simulate point pattern using fitted model 
      capture.output(randomized_pattern_species_1 <- spatstat::simulate.ppm(species_1_model, nsim=nmbr_pttrn))
      randomized_pattern_species_1[[length(randomized_pattern_species_1)+1]] <- species_1_pattern
      
      # Results using simulated vs. observed data 
      result_species_1 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_1,
                                                       raster=simulation_habitats,
                                                       method='random_pattern', only_spatial=T)

      species_2_pattern <- spatstat::subset.ppp(simulation_pattern, Species=="Species_2")
      species_2_model <- spatstat::ppm(spatstat::unmark(species_2_pattern))
      capture.output(randomized_pattern_species_2 <- spatstat::simulate.ppm(species_2_model, nsim=nmbr_pttrn))
      randomized_pattern_species_2[[length(randomized_pattern_species_2)+1]] <- species_2_pattern
      
      result_species_2 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_2,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)

      species_3_pattern <- spatstat::subset.ppp(simulation_pattern, Species=="Species_3")
      species_3_model <- spatstat::kppm(spatstat::unmark(species_3_pattern), cluster="Thomas",
                                      statistic="pcf", statargs=list(divisor="d"))
      capture.output(randomized_pattern_species_3 <- spatstat::simulate.kppm(species_3_model, nsim=nmbr_pttrn))
      randomized_pattern_species_3[[length(randomized_pattern_species_3)+1]] <- species_3_pattern
      
      result_species_3 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_3,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)

      species_4_pattern <- spatstat::subset.ppp(simulation_pattern, Species=="Species_4")
      species_4_model <- spatstat::ppm(spatstat::unmark(species_4_pattern))
      capture.output(randomized_pattern_species_4 <- spatstat::simulate.ppm(species_4_model, nsim=nmbr_pttrn))
      randomized_pattern_species_4[[length(randomized_pattern_species_4)+1]] <- species_4_pattern
      
      result_species_4 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_4,
                                                           raster=simulation_habitats,
                                                           method='random_pattern', only_spatial=T)

      species_5_pattern <- spatstat::subset.ppp(simulation_pattern, Species=="Species_5")
      species_5_model <- spatstat::kppm(spatstat::unmark(species_5_pattern), cluster="Thomas",
                                      statistic="pcf", statargs=list(divisor="d"))
      capture.output(randomized_pattern_species_5 <- spatstat::simulate.kppm(species_5_model, nsim=nmbr_pttrn))
      randomized_pattern_species_5[[length(randomized_pattern_species_5)+1]] <- species_5_pattern
      
      result_species_5 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_5,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)

      species_6_pattern <- spatstat::subset.ppp(simulation_pattern, Species=="Species_6")
      species_6_model <- spatstat::kppm(spatstat::unmark(species_6_pattern), cluster="Thomas")
      capture.output(randomized_pattern_species_6 <- spatstat::simulate.kppm(species_6_model, nsim=nmbr_pttrn))
      randomized_pattern_species_6[[length(randomized_pattern_species_6)+1]] <- species_6_pattern
      
      result_species_6 <- SHAR::Results.Habitat.Association(pattern=randomized_pattern_species_6,
                                                            raster=simulation_habitats,
                                                            method='random_pattern', only_spatial=T)
      # Correct and false detections of separated species
      # Species 1: No associations (Poisson)
      species_1_df[nrow(species_1_df)+1,] <- c((sum(result_species_1$Significance == "N.S.", na.rm=T)/5)*100,
                                             (sum(result_species_1$Significance != "N.S.", na.rm=T)/5)*100,
                                             lph_sqnc[i])

      # Species 2: Positive associations habitat 2 (Poisson process)
      species_2_df[nrow(species_2_df)+1,] <- c((sum(result_species_2$Significance[result_species_2$Habitat=="Habitat_2"] == "Positive", na.rm=T)/1)*100,
                                             (sum(result_species_2$Significance[result_species_2$Habitat!="Habitat_2"] == "Positive", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 3: Positive associations habitat 3 (Thomas process)
      species_3_df[nrow(species_3_df)+1,] <- c((sum(result_species_3$Significance[result_species_3$Habitat=="Habitat_3"] == "Positive", na.rm=T)/1)*100,
                                             (sum(result_species_3$Significance[result_species_3$Habitat!="Habitat_3"] == "Positive", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 4: Negative associations habitat 4 (Poisson process)
      species_4_df[nrow(species_4_df)+1,] <- c((sum(result_species_4$Significance[result_species_4$Habitat=="Habitat_4"] == "Negative", na.rm=T)/1)*100,
                                             (sum(result_species_4$Significance[result_species_4$Habitat!="Habitat_4"] == "Negative", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 5: Negative associations habitat 5 (Thomas process)
      species_5_df[nrow(species_5_df)+1,] <- c((sum(result_species_5$Significance[result_species_5$Habitat=="Habitat_5"] == "Negative", na.rm=T)/1)*100,
                                             (sum(result_species_5$Significance[result_species_5$Habitat!="Habitat_5"] == "Negative", na.rm=T)/4)*100,
                                             lph_sqnc[i])

      # Species 6: No associations (Thomas process)
      species_6_df[nrow(species_6_df)+1,] <- c((sum(result_species_6$Significance == "N.S.", na.rm=T)/5)*100,
                                             (sum(result_species_6$Significance!= "N.S.", na.rm=T)/5)*100,
                                             lph_sqnc[i])

      # Print progress of simulation runs every 10 runs
      if(j%%10==0){print(paste0(j , " runs from ", rns_smltn, " done"))}
    }
    
    # Print progress of association strengths
    print(paste0(i , " from ", length(lph_sqnc), " possible alphas done"))
    cat("\n\n")
  }

  # Combine results into one list
  result_list <- list(Species_1=species_1_df, Species_2=species_2_df, 
                      Species_3=species_3_df, Species_4=species_4_df,
                      Species_5=species_5_df, Species_6=species_6_df)
  # Return result list
  return(result_list)
}


#### Define parameters #### 
# Number of coloumns and rows for neutral landscape 
number_coloumns <- 30
number_rows <- 30 
# Resolution of neutral landscape
resolution <- 20
# Roughness of neutral landscape
roughnes <- 0.3
# Approxmitated number of points for each species
number_points <- 500
# Number of runs
simulation_runs <- 10 # ANDERN ZU 100
# Number of randomized habitat maps / point patterns
number_maps <- 19 # ANDERN ZU 199
number_pattern <- 19
# Different association strengths 
alpha_sequence <- seq(0, 1, 0.05)

#### Simulation study of different methods to analyze species habitat assocations ####
# Habitat randomization (Harms et al. 2001) # 
simulation_study_habitat_randomization <- simulation_habitat_randomization(nmbr_clmns=number_coloumns, 
                                                                           nmbr_rws=number_rows, 
                                                                           rghnss=roughnes,
                                                                           rsltn=resolution, 
                                                                           nmbr_mps=number_maps, 
                                                                           nmbr_pnts=number_points,
                                                                           lph_sqnc=alpha_sequence, 
                                                                           rns_smltn=simulation_runs)

# Save.Function.rds(object=simulation_study_habitat_randomization, file=paste0(results,"/simulation_study_habitat_randomization.rds"))


# Torus translation (Harms et al. 2001) #
simulation_study_torus_translation <- simulation_torus_translation(nmbr_clmns=number_coloumns, 
                                                                   nmbr_rws=number_rows, 
                                                                   rghnss=roughnes,
                                                                   rsltn=resolution, 
                                                                   nmbr_mps=number_maps, 
                                                                   nmbr_pnts=number_points,
                                                                   lph_sqnc=alpha_sequence, 
                                                                   rns_smltn=simulation_runs)

# Save.Function.rds(object=simulation_study_torus_translation, file=paste0(results,"/simulation_study_torus_translation.rds"))


# Fitting point process (Plotkin et al. 2000) #
simulation_study_point_process <- simulation_point_process(nmbr_clmns=number_coloumns, 
                                                           nmbr_rws=number_rows, 
                                                           rghnss=roughnes,
                                                           rsltn=resolution, 
                                                           nmbr_pttrn=number_pattern,
                                                           nmbr_pnts=number_points,
                                                           lph_sqnc=alpha_sequence, 
                                                           rns_smltn=simulation_runs)
# Save.Function.rds(object=simulation_study_point_process, file=paste0(results,"/simulation_study_point_process.rds"))

