simulate_torus_trans_number_habitats <- function(number_coloumns, number_rows,
                                                 resolution, roughness, 
                                                 number_points, alpha, 
                                                 number_habitats, 
                                                 simulation_runs){
  
  result <- furrr::future_map_dfr(number_habitats, function(habitats_current){
  
   simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, 
                                       verbose = FALSE) %>%
    SHAR::classify_habitats(classes = habitats_current)
  
    simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                    number_points = number_points, 
                                                    alpha = alpha)
    
    furrr::future_map_dfr(1:simulation_runs, function(run){
      
      random_habitats <- SHAR::randomize_habitats(raster = simulation_habitats,
                                                  method = 'torus_translation')
      
      associations <- SHAR::results_habitat_association(pattern = simulation_pattern,
                                                        raster = random_habitats, 
                                                        method = 'random_raster')
      
      detection <- detect_habitat_associations(associations)
    }, .id = 'Simulation_runs')
  }, .id = 'Number_habitats')
  
  return(result)
}
