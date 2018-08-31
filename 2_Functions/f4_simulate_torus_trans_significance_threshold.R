simulate_torus_trans_significance_threshold <- function(number_coloumns, number_rows,
                                                        resolution, roughness,
                                                        number_null_model,
                                                        number_points, alpha,
                                                        threshold_list){
  
  furrr::future_map_dfr(threshold_list, function(threshold_current){
    
    simulation_habitats <- SHAR::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, 
                                                                 nrow = number_rows,
                                                                 resolution = resolution, 
                                                                 fract_dim = fract_dim, 
                                                                 verbose = FALSE),
                                                   classes = 5)
    
    simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                    number_points = number_points, 
                                                    association_strength = alpha)
    
    names_species <- unique(as.character(simulation_pattern$marks$Species))
    
    random_habitats <- SHAR::randomize_habitats(raster = simulation_habitats,
                                                method = 'torus_translation')
    
    associations <- SHAR::results_habitat_association(pattern = simulation_pattern,
                                                      raster = random_habitats,
                                                      method = 'random_raster', 
                                                      threshold = threshold_current)
    
    detection_species_1 <- detect_habitat_associations(input = associations[[1]],
                                                       species_type = names_species[1], 
                                                       species_code = 1, 
                                                       variable = threshold_current)
    
    detection_species_2 <- detect_habitat_associations(input = associations[[2]], 
                                                       species_type = names_species[2], 
                                                       species_code = 2, 
                                                       variable = threshold_current)
    
    detection_species_3 <- detect_habitat_associations(input = associations[[3]], 
                                                       species_type = names_species[3], 
                                                       species_code = 3, 
                                                       variable = threshold_current)
    
    detection_species_4 <- detect_habitat_associations(input = associations[[4]],
                                                       species_type = names_species[4], 
                                                       species_code = 4, 
                                                       variable = threshold_current)
    
    dplyr::bind_rows(detection_species_1, detection_species_2,
                     detection_species_3, detection_species_4)
  })
}