Simulation.Habitat.Randomization.Association.Strength <- function(number_coloumns, number_rows,
                                                                  resolution, roughness, number_maps,
                                                                  number_points, alpha_sequence,
                                                                  simulation_runs){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, verbose = FALSE) %>%
    SHAR::Habitat.Classification(classes = 5)
  
  # simulation_pattern <- alpha_sequence %>%
  #   furrr::future_map(function(x){
  #     Create.Simulation.Pattern(raster = simulation_habitats, 
  #                               number_points = number_points, 
  #                               alpha = x)
  #   })
  
  result <- alpha_sequence %>%
    furrr::future_map_dfr(function(x){
      simulation_pattern <- Create.Simulation.Pattern(raster = simulation_habitats, 
                                                      number_points = number_points, 
                                                      alpha = x)
      
      1:simulation_runs %>%
        furrr::future_map_dfr(function(x){
          random_habitats <- SHAR::Habitat.Randomization(raster = simulation_habitats,
                                                         method = 'randomization_algorithm', 
                                                         number_maps = number_maps)
         
          associations <- SHAR::Results.Habitat.Association(pattern = simulation_pattern,
                                                            raster = random_habitats, 
                                                            method = 'random_raster')
         
          detection <- Detection.Habitat.Association(associations)
      }, .id = 'Simulation_runs')
    }, .id = 'Association_strength')

  # result <- 1:simulation_runs %>%
  #   furrr::future_map_dfr(function(x){
  #     simulation_pattern %>%
  #       furrr::future_map_dfr(function(x){
  #         random_habitats <- SHAR::Habitat.Randomization(raster = simulation_habitats, 
  #                                                        method = 'randomization_algorithm', 
  #                                                        number_maps = number_maps)
  #         
  #         associations <- SHAR::Results.Habitat.Association(pattern = x,
  #                                                           raster = random_habitats, 
  #                                                           method = 'random_raster')
  #         
  #         detection <- Detection.Habitat.Association(associations)
  #       }, .id = 'Association_strength')
  #   }, .id = 'Simulation_runs')
  
  return(result)
}

Simulation.Torus.Translation.Association.Strength <- function(number_coloumns, number_rows,
                                                              resolution, roughness, 
                                                              number_points, alpha_sequence,
                                                              simulation_runs){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, verbose = FALSE) %>%
    SHAR::Habitat.Classification(classes = 5)
  
  # simulation_pattern <- alpha_sequence %>%
  #   furrr::future_map(function(x){
  #     Create.Simulation.Pattern(raster = simulation_habitats,
  #                               number_points = number_points,
  #                               alpha = x)
  #   })
  
  result <- alpha_sequence %>%
    furrr::future_map_dfr(function(x){
      simulation_pattern <- Create.Simulation.Pattern(raster = simulation_habitats, 
                                                      number_points = number_points, 
                                                      alpha = x)
      
      1:simulation_runs %>%
        furrr::future_map_dfr(function(x){
          random_habitats <- SHAR::Habitat.Randomization(raster = simulation_habitats, 
                                                         method = 'torus_translation')
          
          associations <- SHAR::Results.Habitat.Association(pattern = simulation_pattern,
                                                            raster = random_habitats, 
                                                            method = 'random_raster')
          
          detection <- Detection.Habitat.Association(associations)
      }, .id = 'Simulation_runs')
    }, .id = 'Association_strength')
  
  # result <- 1:simulation_runs %>%
  #   furrr::future_map_dfr(function(x){
  #     simulation_pattern %>%
  #       furrr::future_map_dfr(function(x){
  #         random_habitats <- SHAR::Habitat.Randomization(raster = simulation_habitats, 
  #                                                        method = 'torus_translation')
  #         
  #         associations <- SHAR::Results.Habitat.Association(pattern = x,
  #                                                           raster = random_habitats, 
  #                                                           method = 'random_raster')
  #         
  #         detection <- Detection.Habitat.Association(associations)
  #       }, .id = 'Association_strength')
  #   }, .id = 'Simulation_runs')
  
  return(result)
}

Simulation.Point.Process.Association.Strength <- function(number_coloumns, number_rows,
                                                          resolution, roughness, 
                                                          number_points, alpha_sequence, number_pattern,
                                                          simulation_runs,
                                                          workers = c(1, 1, 1)){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, verbose = FALSE) %>%
    SHAR::Habitat.Classification(classes = 5)

  # simulation_pattern <- alpha_sequence %>%
  #   furrr::future_map(function(x){
  #     Create.Simulation.Pattern(raster = simulation_habitats, 
  #                               number_points = number_points, 
  #                               alpha = x)
  #   })
  
  result <- alpha_sequence %>%
    furrr::future_map_dfr(function(x){
      simulation_pattern <- Create.Simulation.Pattern(raster = simulation_habitats,
                                                      number_points = number_points,
                                                      alpha = x)
      
      1:simulation_runs %>%
        furrr::future_map_dfr(function(x){
          names_species <- simulation_pattern$marks$Species %>%
            unique() %>%
            as.character()
          
          detection_spec_1 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 1) %>%
            SHAR::Gamma.Test(process = 'poisson', 
                             number_pattern = number_pattern) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial = TRUE) %>%
            list() %>%
            setNames(names_species[[1]]) %>%
            Detection.Habitat.Association()
              
          detection_spec_2 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 2) %>%
            SHAR::Gamma.Test(process = 'cluster', 
                             number_pattern = number_pattern) %>% 
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial = TRUE) %>%
            list() %>%
            setNames(names_species[[2]]) %>%
            Detection.Habitat.Association()
          
          detection_spec_3 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 3) %>%
            SHAR::Gamma.Test(process = 'poisson', 
                             number_pattern = number_pattern) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial = TRUE) %>%
            list() %>%
            setNames(names_species[[3]]) %>%
            Detection.Habitat.Association()
          
          detection_spec_4 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 4) %>%
            SHAR::Gamma.Test(process = 'cluster', 
                             number_pattern = number_pattern) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial = TRUE) %>%
            list() %>%
            setNames(names_species[[4]]) %>%
            Detection.Habitat.Association()
          
          result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2,
                                         detection_spec_3, detection_spec_4)
      }, .id = 'Simulation_runs') 
    }, .id = 'Association_strength')
  
  # result <- 1:simulation_runs %>%
  #   furrr::future_map_dfr(function(x){
  #     simulation_pattern %>% 
  #       furrr::future_map_dfr(function(x){
  #         
  #         names_species <- x$marks$Species %>%
  #           unique() %>%
  #           as.character()
  #         
  #         detection_spec_1 <- x %>%
  #           spatstat::subset.ppp(Species_code == 1) %>%
  #           SHAR::Gamma.Test(process = 'poisson', 
  #                            number_pattern = number_pattern) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial = TRUE) %>%
  #           list() %>%
  #           setNames(names_species[[1]]) %>%
  #           Detection.Habitat.Association()
  #         
  #         detection_spec_2 <- x %>%
  #           spatstat::subset.ppp(Species_code == 2) %>%
  #           SHAR::Gamma.Test(process = 'cluster', 
  #                            number_pattern = number_pattern) %>% 
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial = TRUE) %>%
  #           list() %>%
  #           setNames(names_species[[2]]) %>%
  #           Detection.Habitat.Association()
  #         
  #         
  #         detection_spec_3 <- x %>%
  #           spatstat::subset.ppp(Species_code == 3) %>%
  #           SHAR::Gamma.Test(process = 'poisson', 
  #                            number_pattern = number_pattern) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial = TRUE) %>%
  #           list() %>%
  #           setNames(names_species[[3]]) %>%
  #           Detection.Habitat.Association()
  #         
  #         
  #         detection_spec_4 <- x %>%
  #           spatstat::subset.ppp(Species_code == 4) %>%
  #           SHAR::Gamma.Test(process = 'cluster', 
  #                            number_pattern = number_pattern) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial = TRUE) %>%
  #           list() %>%
  #           setNames(names_species[[4]]) %>%
  #           Detection.Habitat.Association()
  #         
  #         result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2, 
  #                                        detection_spec_3, detection_spec_4)
  #       }, .id = 'Association_strength')
  #   }, .id = 'Simulation_runs')
  
  return(result)
}


Simulation.Pattern.Reconstruction.Association.Strength <- function(number_coloumns, number_rows,
                                                                   resolution, roughness, number_pattern, 
                                                                   number_points, alpha_sequence,
                                                                   max_runs, simulation_runs){
  
  simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows,
                                       resolution = resolution, roughness = roughness, verbose = FALSE) %>%
    SHAR::Habitat.Classification(classes=5)
  
  result <- alpha_sequence %>%
    furrr::future_map_dfr(function(x){
      simulation_pattern <- Create.Simulation.Pattern(raster = simulation_habitats, 
                                                      number_points = number_points, 
                                                      alpha = x)
      
      1:simulation_runs %>%
        furrr::future_map_dfr(function(x){
          names_species <- simulation_pattern$marks$Species %>%
            unique() %>%
            as.character()
          
          detection_spec_1 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 1) %>%
            SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
                                         max_runs = max_runs, fitting = F) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial=T) %>%
            list() %>%
            setNames(names_species[[1]]) %>% 
            Detection.Habitat.Association()
          
          detection_spec_2 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 2) %>%
            SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
                                         max_runs = max_runs, fitting = T) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial=T) %>%
            list() %>%
            setNames(names_species[[2]]) %>% 
            Detection.Habitat.Association()
          
          detection_spec_3 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 3) %>%
            SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
                                         max_runs = max_runs, fitting = F) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial=T) %>%
            list() %>%
            setNames(names_species[[3]]) %>% 
            Detection.Habitat.Association()
          
          detection_spec_4 <- simulation_pattern %>%
            spatstat::subset.ppp(Species_code == 4) %>%
            SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
                                         max_runs = max_runs, fitting = T) %>%
            SHAR::Results.Habitat.Association(raster = simulation_habitats,
                                              method = 'random_pattern', only_spatial=T) %>%
            list() %>%
            setNames(names_species[[4]]) %>% 
            Detection.Habitat.Association()
          
          result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2, 
                                         detection_spec_3, detection_spec_4)
      }, .id = 'Simulation_runs')
    }, .id = 'Association_strength')
  
  # result <- 1:simulation_runs %>%
  #   furrr::future_map_dfr(function(x){
  #     simulation_pattern %>% 
  #       furrr::future_map_dfr(function(x){
  #         
  #         names_species <- x$marks$Species %>%
  #           unique() %>%
  #           as.character()
  #         
  #         detection_spec_1 <- x %>%
  #           spatstat::subset.ppp(Species_code == 1) %>%
  #           SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
  #                                        max_runs = max_runs, fitting = F) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial=T) %>%
  #           list() %>%
  #           setNames(names_species[[1]]) %>% 
  #           Detection.Habitat.Association()
  #         
  #         
  #         detection_spec_2 <- x %>%
  #           spatstat::subset.ppp(Species_code == 2) %>%
  #           SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
  #                                        max_runs = max_runs, fitting = T) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial=T) %>%
  #           list() %>%
  #           setNames(names_species[[2]]) %>% 
  #           Detection.Habitat.Association()
  #         
  #         
  #         detection_spec_3 <- x %>%
  #           spatstat::subset.ppp(Species_code == 3) %>%
  #           SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
  #                                        max_runs = max_runs, fitting = F) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial=T) %>%
  #           list() %>%
  #           setNames(names_species[[3]]) %>% 
  #           Detection.Habitat.Association()
  #         
  #         
  #         detection_spec_4 <- x %>%
  #           spatstat::subset.ppp(Species_code == 4) %>%
  #           SHAR::Pattern.Reconstruction(number_reconstructions = number_pattern, 
  #                                        max_runs = max_runs, fitting = T) %>%
  #           SHAR::Results.Habitat.Association(raster = simulation_habitats,
  #                                             method = 'random_pattern', only_spatial=T) %>%
  #           list() %>%
  #           setNames(names_species[[4]]) %>% 
  #           Detection.Habitat.Association()
  #         
  #         
  #         result_all <- dplyr::bind_rows(detection_spec_1, detection_spec_2, 
  #                                        detection_spec_3, detection_spec_4)
  #       }, .id = 'Association_strength')
  #   }, .id = 'Simulation_runs')
  
  return(result)
}

