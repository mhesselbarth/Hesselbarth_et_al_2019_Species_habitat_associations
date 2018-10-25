simulate_habitat_random_association_strength <- function(number_coloumns, number_rows,
                                                         resolution, fract_dim,
                                                         n_random, 
                                                         number_points, 
                                                         association_strength){
  
  simulation_habitats <- NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, 
                                       fract_dim = fract_dim, 
                                       verbose = FALSE) %>%
      SHAR::classify_habitats(classes = 5)
    
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitats,
                                                  number_points = number_points,
                                                  association_strength = association_strength)
  
  names_species <- simulation_pattern$marks$species %>%
    unique() %>%
    as.character()
      
  random_habitats <- SHAR::randomize_raster(raster = simulation_habitats,
                                            n_random = n_random)

  # Species 1
  associations_species_1 <- SHAR::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 1),
                                                              raster = random_habitats)
      
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1], 
                                                     species_code = 1, 
                                                     variable = association_strength)
      
  # Species 2
  associations_species_2 <- SHAR::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 2),
                                                              raster = random_habitats)
  
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2], 
                                                     species_code = 2, 
                                                     variable = association_strength)
  
  # Species 3
  associations_species_3 <- SHAR::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 3),
                                                              raster = random_habitats)
  
  detection_species_3 <- detect_habitat_associations(input = associations_species_3, 
                                                     species_type = names_species[3], 
                                                     species_code = 3, 
                                                     variable = association_strength)
  
  # Species 4
  associations_species_4 <- SHAR::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 4),
                                                              raster = random_habitats)
  
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[4], 
                                                     species_code = 4, 
                                                     variable = association_strength)
      
  dplyr::bind_rows(detection_species_1, detection_species_2,
                   detection_species_3, detection_species_4)

}