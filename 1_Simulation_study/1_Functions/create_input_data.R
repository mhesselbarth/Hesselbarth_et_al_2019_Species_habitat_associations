create_input_data <- function(number_coloumns, number_rows, 
                              resolution, fract_dim, classes, 
                              number_points, association_strength, 
                              simulation_runs, 
                              threshold = 1/3, 
                              verbose = TRUE) {
  
  
  length_list <- length(association_strength) * simulation_runs

  simulation_habitats_list <- vector("list", length_list)
  simulation_patterns_list <- vector("list", length_list)
  
  counter <- 1
    
  for(i in seq_along(association_strength)) {
  
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
                                                    association_strength = association_strength[[i]])
    
    for(j in seq_len(simulation_runs)) { 
      
      if(runif(n = 1) < threshold) {
        # create simulation landscape with 5 discrete classes
        simulation_habitats_new <- shar::classify_habitats(NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows,
                                                                     resolution = resolution, 
                                                                     fract_dim = fract_dim,
                                                                     verbose = FALSE,
                                                                     cPrintlevel = 0), 
                                                       classes = 5)
        
        # create simulation pattern with 4 species  
        simulation_pattern_new <- create_simulation_pattern(raster = simulation_habitats,
                                                        number_points = number_points,
                                                        association_strength = association_strength[[i]])
        
        simulation_habitats_list[[counter]] <- simulation_habitats_new
        simulation_patterns_list[[counter]] <- simulation_pattern_new
        
      }
      
      else {
        simulation_habitats_list[[counter]] <- simulation_habitats
        simulation_patterns_list[[counter]] <- simulation_pattern
      }
      
      if(verbose) { 
        message("\r> Progress: ", counter, "/", length_list, appendLF = FALSE)
      }
      
      counter <- counter + 1
    }
  }
  
  result <- list(habitats = simulation_habitats_list, patterns = simulation_patterns_list)
  
  return(result)
}
