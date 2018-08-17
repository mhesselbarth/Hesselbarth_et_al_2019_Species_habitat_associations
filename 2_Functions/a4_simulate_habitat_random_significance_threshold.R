simulate_habitat_random_significance_threshold <- function(number_coloumns, number_rows,
                                                           resolution, roughness,
                                                           number_maps, 
                                                           number_points, alpha,
                                                           threshold_list,
                                                           simulation_runs){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, 
                                       verbose = FALSE) %>%
    SHAR::classify_habitats(classes = 5)
  
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                  number_points = number_points, 
                                                  alpha = alpha)
  
  result <- furrr::future_map_dfr(threshold_list, function(threshold_current){
    
    furrr::future_map_dfr(1:simulation_runs, function(simulation_run_current){
      
      random_habitats <- SHAR::randomize_habitats(raster = simulation_habitats,
                                                  method = 'randomization_algorithm', 
                                                  number_maps = number_maps)
      
      associations <- SHAR::results_habitat_association(pattern = simulation_pattern,
                                                        raster = random_habitats,
                                                        method = 'random_raster', 
                                                        threshold = threshold_current)
      
      detection <- detect_habitat_associations(associations)
    }, .id = 'Simulation_runs')
  }, .id = 'Significance_threshold')
  
  return(result)
}