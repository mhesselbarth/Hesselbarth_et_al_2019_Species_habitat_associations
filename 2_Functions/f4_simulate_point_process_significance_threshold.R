simulate_point_process_significance_threshold <- function(number_coloumns, number_rows,
                                                          resolution, roughness,
                                                          number_pattern, 
                                                          number_points, alpha_sequence,
                                                          threshold_list,
                                                          simulation_runs){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, 
                                       verbose = FALSE) %>%
    SHAR::classify_habitats(classes = 5)
  
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitats,
                                                  number_points = number_points,
                                                  alpha = alpha)
  
  names_species <- simulation_pattern$marks$Species %>%
    unique() %>%
    as.character()
  
  result <- furrr::future_map_dfr(threshold_list, function(threshold_current){
    
    furrr::future_map_dfr(1:simulation_runs, function(simulation_run_current){
      
      detection_spec_1 <- simulation_pattern %>%
        spatstat::subset.ppp(Species_code == 1) %>%
        SHAR::fit_point_process(process = 'poisson', 
                                number_pattern = number_pattern) %>%
        SHAR::results_habitat_association(raster = simulation_habitats,
                                          method = 'random_pattern', 
                                          only_spatial = TRUE, 
                                          threshold = threshold_current) %>%
        list() %>%
        setNames(names_species[[1]]) %>%
        detect_habitat_associations()
      
      detection_spec_2 <- simulation_pattern %>%
        spatstat::subset.ppp(Species_code == 2) %>%
        SHAR::fit_point_process(process = 'cluster', 
                                number_pattern = number_pattern) %>% 
        SHAR::results_habitat_association(raster = simulation_habitats,
                                          method = 'random_pattern', 
                                          only_spatial = TRUE, 
                                          threshold = threshold_current) %>%
        list() %>%
        setNames(names_species[[2]]) %>%
        detect_habitat_associations()
      
      detection_spec_3 <- simulation_pattern %>%
        spatstat::subset.ppp(Species_code == 3) %>%
        SHAR::fit_point_process(process = 'poisson', 
                                number_pattern = number_pattern) %>%
        SHAR::results_habitat_association(raster = simulation_habitats,
                                          method = 'random_pattern',
                                          only_spatial = TRUE,
                                          threshold = threshold_current) %>%
        list() %>%
        setNames(names_species[[3]]) %>%
        detect_habitat_associations()
      
      detection_spec_4 <- simulation_pattern %>%
        spatstat::subset.ppp(Species_code == 4) %>%
        SHAR::fit_point_process(process = 'cluster', 
                                number_pattern = number_pattern) %>%
        SHAR::results_habitat_association(raster = simulation_habitats,
                                          method = 'random_pattern', 
                                          only_spatial = TRUE,
                                          threshold = threshold_current) %>%
        list() %>%
        setNames(names_species[[4]]) %>%
        detect_habitat_associations()
      
      result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2,
                                     detection_spec_3, detection_spec_4)
    }, .id = 'Simulation_runs') 
  }, .id = 'Significance_threshold')
  
  return(result)
}