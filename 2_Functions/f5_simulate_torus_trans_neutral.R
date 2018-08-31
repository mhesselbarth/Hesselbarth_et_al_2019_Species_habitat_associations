simulate_torus_trans_neutral <- function(number_coloumns, number_rows,
                                         resolution, roughness, number_points,
                                         simulation_runs, alpha_sequence){
  

  # Create landscape and simulation pattern
  simulation_habitats <- NLMR::nlm_mpd(ncol=number_coloumns, nrow=number_rows,
                                       resolution=resolution, roughness=roughness, verbose=F) %>%
    SHAR::classify_habitats(classes=5)
  
  species_1 <- create_simulation_species(raster=simulation_habitats, type="neutral", process="Poisson",
                                         number_points=number_points, species_code=1,
                                         habitat=NULL, alpha=NULL, verbose=F)
  
  species_2 <- create_simulation_species(raster=simulation_habitats, type="neutral", process="Thomas",
                                         number_points=number_points, species_code=2,
                                         habitat=NULL, alpha=NULL, verbose=F)
  
  simulation_pattern <- spatstat::superimpose(species_1, species_2)
  
  result <- furrr::future_map_dfr(1:simulation_runs, function(run){
    
    random_habitats <- SHAR::randomize_habitats(raster = simulation_habitats,
                                                method = 'torus_translation')
    
    associations <- SHAR::results_habitat_association(pattern = simulation_pattern,
                                                      raster = random_habitats, 
                                                      method = 'random_raster')
    
    furrr::future_map_dfr(associations, function(current_species) { 
      
      tibble::tibble(Correct = sum(current_species$Significance == "N.S.", na.rm=T),
                     False = sum(current_species$Significance != "N.S.", na.rm=T))
    }, .id = "Species")
  }, .id = 'Simulation_runs')

return(result)
}