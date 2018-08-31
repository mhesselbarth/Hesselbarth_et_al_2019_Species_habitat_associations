simulate_point_process_neutral <- function(number_coloumns, number_rows,
                                     resolution, roughness, number_points,
                                     simulation_runs, number_pattern, alpha_sequence){
  
  # Create landscape and simulation pattern
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, 
                                       verbose = FALSE) %>%
    SHAR::classify_habitats(classes = 5)
  
  species_1 <- create_simulation_species(raster = simulation_habitats, type = "neutral", process = "Poisson",
                                         number_points = number_points, species_code = 1,
                                         habitat = NULL, alpha = NULL, verbose = FALSE)
  
  species_2 <- create_simulation_species(raster = simulation_habitats, type = "neutral", process = "Thomas",
                                         number_points = number_points, species_code = 2,
                                         habitat = NULL, alpha = NULL, verbose = FALSE)
  
  results <- furrr::future_map_dfr(1:simulation_runs, function(simulation_run_current){
    
    associations_spec_1 <- species_1 %>%
      SHAR::fit_point_process(process = 'poisson', 
                              number_pattern = number_pattern) %>%
      SHAR::results_habitat_association(raster = simulation_habitats,
                                        method = 'random_pattern', 
                                        only_spatial = TRUE)
    
    detection_spec_1 <- tibble::tibble(Species = as.character(unique(species_1$marks$Species)),
                                       Correct = sum(associations_spec_1$Significance == "N.S.", na.rm = TRUE),
                                       False = sum(associations_spec_1$Significance != "N.S.", na.rm = TRUE))

    associations_spec_2 <- species_2 %>%
      SHAR::fit_point_process(process = 'cluster', 
                              number_pattern = number_pattern) %>% 
      SHAR::results_habitat_association(raster = simulation_habitats,
                                        method = 'random_pattern', 
                                        only_spatial = TRUE)
    
    detection_spec_2 <- tibble::tibble(Species = as.character(unique(species_2$marks$Species)),
                                       Correct = sum(associations_spec_2$Significance == "N.S.", na.rm = TRUE),
                                       False = sum(associations_spec_2$Significance != "N.S.", na.rm = TRUE))
                                          
    
    result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2)
  }, .id = 'Simulation_runs') 
  
  return(results)
}

