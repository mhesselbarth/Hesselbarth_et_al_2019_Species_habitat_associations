Simulation.Habitat.Randomization.Association.Strength <- function(number_coloumns, number_rows,
                                                                  resolution, roughness, number_maps,
                                                                  number_points, alpha_sequence,
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
          random_habitats <- SHAR::Habitat.Randomization(raster = simulation_habitats, 
                                                         method = 'randomization_algorithm', 
                                                         number_maps = number_maps)
        
          associations <- SHAR::Results.Habitat.Association(pattern = x,
                                                            raster = random_habitats, 
                                                            method = 'random_raster')
          detection <- SHAR::Detection.Habitat.Association(associations)
          }, .id = 'Association_strength')
      }, .id = 'Simulation_runs')
  
  return(result)
}
