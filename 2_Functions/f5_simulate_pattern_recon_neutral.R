simulate_pattern_recon_neutral <- function(number_coloumns, number_rows, 
                                           resolution, roughness, number_points, 
                                           simulation_runs, number_pattern, alpha_sequence) {
  
  furrr::future_map_dfr(1:simulation_runs, function(simulation_run_current) {
    
    # Create landscape and simulation pattern
    simulation_habitats <- SHAR::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, 
                                                                 nrow = number_rows,
                                                                 resolution = resolution, 
                                                                 fract_dim = fract_dim, 
                                                                 verbose=F), 
                                                   classes=5)
    
    extent_raster <- raster::extent(simulation_habitats)
    owin_overall <- spatstat::owin(xrange = c(extent_raster[1], extent_raster[2]), 
                                   yrange = c(extent_raster[3], extent_raster[4]))
    
    habitats_poly <- sf::as_Spatial(dplyr::summarise(dplyr::group_by(spex::polygonize(simulation_habitats), layer))) 
    
    species_1 <- create_simulation_species(habitats_poly = habitats_poly,
                                           owin_overall = owin_overall, 
                                           type = "neutral", process = "Poisson",
                                           habitat = NULL, association_strength = NULL,
                                           number_points = number_points, 
                                           species_code = 1,
                                           verbose = F)
    
    species_2 <- create_simulation_species(habitats_poly = habitats_poly,
                                           owin_overall = owin_overall, 
                                           type = "neutral", process = "Thomas",
                                           habitat = NULL, association_strength = NULL,
                                           number_points = number_points, 
                                           species_code = 2,
                                           verbose = F)
    
    random_species_1 <- SHAR::reconstruct_pattern(pattern = species_1, 
                                                  number_reconstructions = number_null_model, 
                                                  max_runs = max_runs, 
                                                  fitting = FALSE)
    
    associations_species_1 <- SHAR::results_habitat_association(pattern = random_species_1,
                                                                raster = simulation_habitats,
                                                                method = 'random_pattern', 
                                                                only_spatial = TRUE)
    
    detection_species_1 <- tibble::tibble(Species_code = 1, 
                                          Variable = as.double(NA),
                                          Correct = sum(associations_species_1$Significance == "N.S.", na.rm=T),
                                          False = sum(associations_species_1$Significance != "N.S.", na.rm=T))
    
    
    random_species_2 <- SHAR::reconstruct_pattern(pattern = species_2, 
                                                  number_reconstructions = number_null_model, 
                                                  max_runs = max_runs, 
                                                  fitting = TRUE)
    
    associations_species_2 <- SHAR::results_habitat_association(pattern = random_species_2,
                                                                raster = simulation_habitats,
                                                                method = 'random_pattern', 
                                                                only_spatial = TRUE)
    
    detection_species_2 <- tibble::tibble(Species_code = 2, 
                                          Variable = as.double(NA), 
                                          Correct = sum(associations_species_2$Significance == "N.S.", na.rm=T),
                                          False = sum(associations_species_2$Significance != "N.S.", na.rm=T))
    
    dplyr::bind_rows(detection_species_1, 
                     detection_species_2)
  })
}