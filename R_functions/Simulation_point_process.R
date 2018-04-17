Simulation.Point.Process <- function(number_coloumns, number_rows,
                                     resolution, roughness, 
                                     number_points, alpha_sequence, number_pattern,
                                     simulation_runs,
                                     workers = c(1, 1, 1)){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                       resolution=resolution, roughness=roughness, verbose=F) %>%
    SHAR::Habitat.Classification(classes=5)
  
  # future::plan(list(future::tweak(future::multiprocess, workers = workers[[1]]),
  #                   future::tweak(future::multiprocess, workers = workers[[2]]),
  #                   future::tweak(future::multiprocess, workers = workers[[3]])))

  future::plan(future::multiprocess)
  
  simulation_pattern <- alpha_sequence %>%
    furrr::future_map(function(x){
      Create.Simulation.Pattern(raster = simulation_habitats, 
                                number_points = number_points, 
                                alpha = x)
    })
  
  result <- 1:simulation_runs %>%
    furrr::future_map_dfr(function(x){
      simulation_pattern %>% 
        furrr::future_map_dfr(function(x){
          
          names_species <- x$marks$Species %>%
            unique() %>%
            as.character()
          
          pattern_spec_1 <- Fit.Point.Process(input = x, 
                                              species = 1, process = 'Poisson',
                                              number_pattern = number_pattern)
          result_spec_1 <- list(SHAR::Results.Habitat.Association(pattern=pattern_spec_1,
                                                                  raster=simulation_habitats,
                                                                  method='random_pattern', only_spatial=T))
          names(result_spec_1) <- names_species[[1]]
          detection_spec_1 <- SHAR::Detection.Habitat.Association(result_spec_1)
          
    
          pattern_spec_2 <- Fit.Point.Process(input = x, 
                                              species = 2, process = 'Cluster',
                                              number_pattern = number_pattern)
          result_spec_2 <- list(SHAR::Results.Habitat.Association(pattern=pattern_spec_2,
                                                                  raster=simulation_habitats,
                                                                  method='random_pattern', only_spatial=T))
          names(result_spec_2) <- names_species[[2]]
          detection_spec_2 <- SHAR::Detection.Habitat.Association(result_spec_2)
          
          
          pattern_spec_3 <- Fit.Point.Process(input = x, 
                                              species = 3, process = 'Poisson',
                                              number_pattern = number_pattern)
          result_spec_3 <- list(SHAR::Results.Habitat.Association(pattern=pattern_spec_3,
                                                                  raster=simulation_habitats,
                                                                  method='random_pattern', only_spatial=T))
          names(result_spec_3) <- names_species[[3]]
          detection_spec_3 <- SHAR::Detection.Habitat.Association(result_spec_3)
          
          
          pattern_spec_4 <- Fit.Point.Process(input = x, 
                                              species = 4, process = 'Cluster',
                                              number_pattern = number_pattern)
          result_spec_4 <- list(SHAR::Results.Habitat.Association(pattern=pattern_spec_4,
                                                                  raster=simulation_habitats,
                                                                  method='random_pattern', only_spatial=T))
          names(result_spec_4) <- names_species[[4]]
          detection_spec_4 <- SHAR::Detection.Habitat.Association(result_spec_4)
          
          result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2, 
                                         detection_spec_3, detection_spec_4)
          }, .id = 'Association_strength')
      }, .id = 'Simulation_runs')
  
  return(result)
}
