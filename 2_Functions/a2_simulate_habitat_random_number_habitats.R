simulate_habitat_random_number_habitats <- function(number_coloumns, number_rows,
                                                    resolution, roughness, 
                                                    number_maps, 
                                                    number_points, alpha, 
                                                    number_habitats){
  
  furrr::future_map_dfr(number_habitats, function(habitats_current){
    
    simulation_habitats <- SHAR::classify_habitats(NLMR::nlm_mpd(ncol = number_coloumns, 
                                                                 nrow = number_rows,
                                                                 resolution = resolution, 
                                                                 roughness = roughness, 
                                                                 verbose = FALSE), 
                                                   classes = habitats_current)
      
    simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                    number_points = number_points, 
                                                    association_strength = alpha)
    
    names_species <- unique(as.character(simulation_pattern$marks$Species))
    
    random_habitats <- SHAR::randomize_habitats(raster = simulation_habitats,
                                                method = 'randomization_algorithm', 
                                                number_maps = number_maps)
    
    associations <- SHAR::results_habitat_association(pattern = simulation_pattern,
                                                      raster = random_habitats,
                                                      method = 'random_raster')
    
    detection_species_1 <- detect_habitat_associations(input = associations[[1]],
                                                       species_type = names_species[1], 
                                                       species_code = 1, 
                                                       variable = habitats_current)
    
    detection_species_2 <- detect_habitat_associations(input = associations[[2]], 
                                                       species_type = names_species[2], 
                                                       species_code = 2, 
                                                       variable = habitats_current)
    
    detection_species_3 <- detect_habitat_associations(input = associations[[3]], 
                                                       species_type = names_species[3], 
                                                       species_code = 3, 
                                                       variable = habitats_current)
    
    detection_species_4 <- detect_habitat_associations(input = associations[[4]],
                                                       species_type = names_species[4], 
                                                       species_code = 4, 
                                                       variable = habitats_current)
    
    dplyr::bind_rows(detection_species_1, detection_species_2,
                     detection_species_3, detection_species_4)
  })
}