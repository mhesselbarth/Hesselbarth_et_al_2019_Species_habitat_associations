###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data -Appendix ####

# Load packages #
library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

#### Hypotheses 1 & 2 ####

# Import data 
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))
reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))
reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))
reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))

# Calculate mean energies
full_patterns_list <- list(Beech = reconstructed_beech, 
                           Ash = reconstructed_ash, 
                           Hornbeam = reconstructed_hornbeam, 
                           Sycamore =reconstructed_sycamore,
                           others = reconstructed_others)

mean_energy_full_patterns <- purrr::map_dfr(full_patterns_list, function(x){
  data.frame(mean_energy = shar::calculate_energy(pattern = x, 
                                                  return_mean = TRUE, 
                                                  comp_fast = TRUE, 
                                                  verbose = FALSE))}, .id = "species")

# Plot summary stats
summary_stats_species <- purrr::map_dfr(full_patterns_list, function(current_species) {
  
  summary_stats <- purrr::map_dfr(current_species, function(x) {
    
    gest_result <- spatstat::Gest(X = x, correction = "none")
    
    pcf_result <- shar::estimate_pcf_fast(x,
                                          correction = "none",
                                          method = "c",
                                          spar = 0.5)
    
    gest_df <- as.data.frame(gest_result) # conver to df
    
    names(gest_df)[3] <- "x_r" # unique col names
    
    gest_df$summary_function <- "Nearest Neighbour Distance Function G(r)" # name of method
    
    pcf_df <- as.data.frame(pcf_result) # convert to df
    
    names(pcf_df)[3] <- "x_r" # unique col names
    
    pcf_df$summary_function <- "Pair Correlation Function g(r)" # name of method
    
    dplyr::bind_rows(gest_df, pcf_df) # combine to one df    
  }, .id = "type")

  
  # classify all observed and all randomized repetitions identical
  summary_stats <- dplyr::mutate(summary_stats, type = dplyr::case_when(type == "observed" ~ "observed",
                                                                        TRUE ~ "randomized")) %>%
    dplyr::group_by(summary_function, r, type) %>% 
    dplyr::summarise(lo = stats::quantile(x_r, probs = 0.025),
                     hi = stats::quantile(x_r, probs = 0.975),
                     x_r = mean(x_r),
                     theo = mean(theo))
}, .id = "species")

ggplot() + 
  geom_ribbon(data = dplyr::filter(summary_stats_species, type == "randomized"), 
              aes(x = r, ymin = lo, ymax = hi), 
              alpha = 0.3, col = "grey") +
  geom_line(data = dplyr::filter(summary_stats_species, type == "observed"),
            aes(x = r, y = x_r), col = "black") +
  facet_wrap(~ species + summary_function, scales = "free", nrow = 5, ncol = 2) + 
  theme_bw()
