# simulate_point_process_number_null_model <- function(number_coloumns, number_rows,
#                                                      resolution, fract_dim,
#                                                      number_null_model, 
#                                                      number_points, alpha_sequence){
#   
#   
#   furrr::future_map_dfr(number_null_model, function(null_model_current){
#     
#     simulation_habitats <- SHAR::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, 
#                                                                  nrow = number_rows,
#                                                                  resolution = resolution, 
#                                                                  fract_dim = fract_dim, 
#                                                                  verbose = FALSE), 
#                                                    classes = 5)
#   
#   simulation_pattern <- create_simulation_pattern(raster = simulation_habitats,
#                                                   number_points = number_points,
#                                                   association_strength = alpha)
#   
#   names_species <- unique(as.character(simulation_pattern$marks$Species))
#   
#   random_species_1 <- simulation_pattern %>%
#     spatstat::subset.ppp(Species_code == 1) %>%
#     SHAR::fit_point_process(process = 'poisson',
#                             number_pattern = null_model_current)
#   
#   associations_species_1 <- SHAR::results_habitat_association(pattern = random_species_1,
#                                                               raster = simulation_habitats,
#                                                               method = 'random_pattern',
#                                                               only_spatial = TRUE)
#   
#   detection_species_1 <- detect_habitat_associations(input = associations_species_1,
#                                                      species_type = names_species[1],
#                                                      species_code = 1,
#                                                      variable = null_model_current)
#   
#   random_species_2 <- simulation_pattern %>%
#     spatstat::subset.ppp(Species_code == 2) %>%
#     SHAR::fit_point_process(process = 'cluster',
#                             number_pattern = null_model_current)
#   
#   associations_species_2 <- SHAR::results_habitat_association(pattern = random_species_2,
#                                                               raster = simulation_habitats,
#                                                               method = 'random_pattern',
#                                                               only_spatial = TRUE)
#   
#   detection_species_2 <- detect_habitat_associations(input = associations_species_2,
#                                                      species_type = names_species[2],
#                                                      species_code = 2,
#                                                      variable = null_model_current)
#   
#   random_species_3 <- simulation_pattern %>%
#     spatstat::subset.ppp(Species_code == 3) %>%
#     SHAR::fit_point_process(process = 'poisson',
#                             number_pattern = null_model_current)
#   
#   associations_species_3 <- SHAR::results_habitat_association(pattern = random_species_3,
#                                                               raster = simulation_habitats,
#                                                               method = 'random_pattern',
#                                                               only_spatial = TRUE)
#   
#   detection_species_3 <- detect_habitat_associations(input = associations_species_3,
#                                                      species_type = names_species[3],
#                                                      species_code = 3,
#                                                      variable = null_model_current)
#   
#   random_species_4 <- simulation_pattern %>%
#     spatstat::subset.ppp(Species_code == 4) %>%
#     SHAR::fit_point_process(process = 'cluster',
#                             number_pattern = null_model_current)
#   
#   associations_species_4 <- SHAR::results_habitat_association(pattern = random_species_4,
#                                                               raster = simulation_habitats,
#                                                               method = 'random_pattern',
#                                                               only_spatial = TRUE)
#   
#   detection_species_4 <- detect_habitat_associations(input = associations_species_4,
#                                                      species_type = names_species[[4]],
#                                                      species_code = 4,
#                                                      variable = null_model_current)
#   
#   dplyr::bind_rows(detection_species_1, detection_species_2,
#                    detection_species_3, detection_species_4)
#   })
# }