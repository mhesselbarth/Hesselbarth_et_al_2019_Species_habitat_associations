# simulate_pattern_recon_significane_threshold <- function(number_coloumns, number_rows,
#                                                      resolution, fract_dim,
#                                                      number_null_model, max_runs,
#                                                      number_points, alpha,
#                                                      threshold_list){
#   
#   furrr::future_map_dfr(threshold_list , function(threshold_current){
#     
#     simulation_habitats <- SHAR::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, 
#                                                                  nrow = number_rows,
#                                                                  resolution = resolution, 
#                                                                  fract_dim = fract_dim, 
#                                                                  verbose = FALSE), 
#                                                    classes = 5)
#     
#     simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
#                                                     number_points = number_points, 
#                                                     association_strength = alpha)
#     
#     names_species <- as.character(unique(simulation_pattern$marks$Species))
#     
#     species_1 <- spatstat::subset.ppp(simulation_pattern, Species_code == 1) 
#     
#     random_species_1 <- SHAR::reconstruct_pattern(pattern = species_1, 
#                                                   number_reconstructions = number_null_model,
#                                                   max_runs = max_runs, fitting = FALSE)
#     
#     associations_species_1 <- SHAR::results_habitat_association(pattern = random_species_1, 
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern',
#                                                                 only_spatial = TRUE, 
#                                                                 threshold = threshold_current)
#     
#     detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
#                                                        species_type = names_species[1], 
#                                                        species_code = 1, 
#                                                        variable = threshold_current)
#     
#     species_2 <- spatstat::subset.ppp(simulation_pattern, Species_code == 2)
#     
#     random_species_2 <- SHAR::reconstruct_pattern(pattern = species_2, 
#                                                   number_reconstructions = number_null_model,
#                                                   max_runs = max_runs, fitting = TRUE)
#     
#     associations_species_2 <- SHAR::results_habitat_association(pattern = random_species_2, 
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern', 
#                                                                 only_spatial = TRUE,
#                                                                 threshold = threshold_current)
#     
#     detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
#                                                        species_type = names_species[2],
#                                                        species_code = 2,
#                                                        variable = threshold_current)
#     
#     species_3 <- spatstat::subset.ppp(simulation_pattern, Species_code == 3)
#     
#     random_species_3 <- SHAR::reconstruct_pattern(pattern = species_3, 
#                                                   number_reconstructions = number_null_model,
#                                                   max_runs = max_runs, fitting = FALSE)
#     
#     associations_species_3 <- SHAR::results_habitat_association(pattern = random_species_3, 
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern',
#                                                                 only_spatial = TRUE, 
#                                                                 threshold = threshold_current)
#     
#     detection_species_3 <- detect_habitat_associations(input = associations_species_3,
#                                                        species_type = names_species[3],
#                                                        species_code = 3,
#                                                        variable = threshold_current)
#     
#     species_4 <- spatstat::subset.ppp(simulation_pattern, Species_code == 4)
#     
#     random_species_4 <- SHAR::reconstruct_pattern(pattern = species_4, 
#                                                   number_reconstructions = number_null_model,
#                                                   max_runs = max_runs, fitting = TRUE)
#     
#     associations_species_4 <- SHAR::results_habitat_association(pattern = random_species_4,
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern',
#                                                                 only_spatial = TRUE, 
#                                                                 threshold = threshold_current)
#     
#     detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
#                                                        species_type = names_species[[4]],
#                                                        species_code = 4,
#                                                        variable = threshold_current)
#     
#     dplyr::bind_rows(detection_species_1, detection_species_2, 
#                      detection_species_3, detection_species_4)
#   })
# }