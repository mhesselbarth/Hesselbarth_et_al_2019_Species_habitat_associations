simulate_torus_trans_association_strength <- function(number_coloumns, number_rows,
                                                      resolution, fract_dim,
                                                      number_points, association_strength){
  
  # create simulation landscape with 5 discrete classes
  simulation_habitats <- shar::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows,
                                                               resolution = resolution, 
                                                               fract_dim = fract_dim,
                                                               verbose = FALSE,
                                                               cPrintlevel = 0), 
                                                 classes = 5)
  
  # create simulation pattern with 4 species    
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitats,
                                                  number_points = number_points,
                                                  association_strength = association_strength)
      
  # names include association type of species
  names_species <- as.character(unique(simulation_pattern$marks$species))
  
  # randomize raster using torus translation
  random_habitats <- shar::translate_raster(raster = simulation_habitats, 
                                            verbose = FALSE)
     
  # Species 1
  # get habitat associations
  associations_species_1 <- shar::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 1),
                                                              raster = random_habitats, 
                                                              verbose = FALSE)
  
  # count correct/false detections of habitat associations
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1], 
                                                     species_code = 1, 
                                                     variable = association_strength)
  
  # Species 2
  # get habitat associations
  associations_species_2 <- shar::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 2),
                                                              raster = random_habitats,
                                                              verbose = FALSE)
  
  # count correct/false detections of habitat associations
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2], 
                                                     species_code = 2, 
                                                     variable = association_strength)
  
  # Species 3
  # get habitat associations
  associations_species_3 <- shar::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 3),
                                                              raster = random_habitats,
                                                              verbose = FALSE)
  
  # count correct/false detections of habitat associations
  detection_species_3 <- detect_habitat_associations(input = associations_species_3, 
                                                     species_type = names_species[3], 
                                                     species_code = 3, 
                                                     variable = association_strength)
  
  # Species 4
  # get habitat associations
  associations_species_4 <- shar::results_habitat_association(pattern = spatstat::subset.ppp(simulation_pattern, 
                                                                                             species_code == 4),
                                                              raster = random_habitats,
                                                              verbose = FALSE)
  
  # count correct/false detections of habitat associations
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[4], 
                                                     species_code = 4, 
                                                     variable = association_strength)
    
  # combine to one data frame
  dplyr::bind_rows(detection_species_1, detection_species_2,
                   detection_species_3, detection_species_4)
}
