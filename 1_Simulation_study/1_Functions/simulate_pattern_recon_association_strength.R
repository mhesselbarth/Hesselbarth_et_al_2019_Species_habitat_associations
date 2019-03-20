simulate_pattern_recon_association_strength <- function(number_coloumns, 
                                                        number_rows,
                                                        resolution, 
                                                        fract_dim,
                                                        number_points, 
                                                        association_strength,
                                                        n_random, 
                                                        max_runs, 
                                                        comp_fast, 
                                                        no_change){
  
  # create simulation landscape with 5 discrete classes
  simulation_habitats <- shar::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows,
                                                               resolution = resolution, 
                                                               fract_dim = fract_dim,
                                                               cPrintlevel = 0), 
                                                 classes = 5)
  
  # create simulation pattern with 4 species
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitats,
                                                  number_points = number_points,
                                                  association_strength = association_strength)
  
  # association type is included in species name
  names_species <- as.character(unique(simulation_pattern$marks$species))
  
  # Species 1
  # pattern containing only species 1
  species_1 <- spatstat::subset.ppp(simulation_pattern, species_code == 1) 
  
  # randomize pattern using pattern reconstruction
  random_species_1 <- shar::reconstruct_pattern(pattern = species_1, 
                                                n_random = n_random,
                                                max_runs = max_runs, 
                                                fitting = FALSE, 
                                                comp_fast = comp_fast,
                                                no_change = no_change,
                                                verbose = FALSE)
  
  # get habitat associations
  associations_species_1 <- shar::results_habitat_association(pattern = random_species_1, 
                                                              raster = simulation_habitats, 
                                                              verbose = FALSE)
  
  # count correct/false detections
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1], 
                                                     species_code = 1, 
                                                     variable = association_strength)
  
  # Species 2
  # pattern containing only species 2
  species_2 <- spatstat::subset.ppp(simulation_pattern, species_code == 2)
  
  # randomize pattern using pattern reconstruction
  random_species_2 <- shar::reconstruct_pattern(pattern = species_2, 
                                                n_random = n_random,
                                                max_runs = max_runs, 
                                                fitting = TRUE, 
                                                comp_fast = comp_fast,
                                                no_change = no_change,
                                                verbose = FALSE)
  
  # get habitat associations
  associations_species_2 <- shar::results_habitat_association(pattern = random_species_2, 
                                                              raster = simulation_habitats, 
                                                              verbose = FALSE)
  
  # count correct/false detections
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2],
                                                     species_code = 2,
                                                     variable = association_strength)
  
  # Species 3
  # pattern only containing species 3
  species_3 <- spatstat::subset.ppp(simulation_pattern, species_code == 3)
  
  # randomize pattern using pattern reconstruction
  random_species_3 <- shar::reconstruct_pattern(pattern = species_3, 
                                                n_random = n_random,
                                                max_runs = max_runs, 
                                                fitting = FALSE,
                                                comp_fast = comp_fast,
                                                no_change = no_change,
                                                verbose = FALSE)
  
  # get habitat associations
  associations_species_3 <- shar::results_habitat_association(pattern = random_species_3, 
                                                              raster = simulation_habitats, 
                                                              verbose = FALSE)
  
  # count correct/false detections
  detection_species_3 <- detect_habitat_associations(input = associations_species_3,
                                                     species_type = names_species[3],
                                                     species_code = 3,
                                                     variable = association_strength)
  
  # Species 4
  # pattern containing only species 4
  species_4 <- spatstat::subset.ppp(simulation_pattern, species_code == 4)
  
  # randomize pattern using pattern reconstruction
  random_species_4 <- shar::reconstruct_pattern(pattern = species_4, 
                                                n_random = n_random,
                                                max_runs = max_runs, 
                                                fitting = TRUE, 
                                                comp_fast = comp_fast,
                                                no_change = no_change,
                                                verbose = FALSE)
  
  # get habitat associations
  associations_species_4 <- shar::results_habitat_association(pattern = random_species_4,
                                                              raster = simulation_habitats, 
                                                              verbose = FALSE)
  
  # count correct/false detections
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[[4]],
                                                     species_code = 4,
                                                     variable = association_strength)
  
  # combine to one data frame
  dplyr::bind_rows(detection_species_1, detection_species_2, 
                   detection_species_3, detection_species_4)
}