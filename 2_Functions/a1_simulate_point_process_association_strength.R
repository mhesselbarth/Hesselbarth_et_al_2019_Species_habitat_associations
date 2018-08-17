simulate_point_process_association_strength <- function(number_coloumns, number_rows,
                                                        resolution, fract_dim,
                                                        number_pattern, 
                                                        number_points, alpha_sequence,
                                                        simulation_runs){
  
  # seeds <- rep(c(1,2,3,4,5), each = 10)
  
  furrr::future_map_dfr(alpha_sequence, function(alpha_current){
      
    furrr::future_map_dfr(1:simulation_runs, function(simulation_run_current){
      
      simulation_habitats <- NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows,
                                           resolution = resolution, 
                                           fract_dim = fract_dim, 
                                           verbose = FALSE) %>%
        SHAR::classify_habitats(classes = 5)
        
      simulation_pattern <- create_simulation_pattern(raster = simulation_habitats,
                                                      number_points = number_points,
                                                      association_strength = alpha_current)
        
      names_species <- simulation_pattern$marks$Species %>%
        unique() %>%
        as.character()
      
      random_species_1 <- simulation_pattern %>%
        spatstat::subset.ppp(Species_code == 1) %>%
        SHAR::fit_point_process(process = 'poisson', 
                                number_pattern = number_pattern)
        
      associations_species_1 <- SHAR::results_habitat_association(pattern = random_species_1, 
                                                                  raster = simulation_habitats,
                                                                  method = 'random_pattern',
                                                                  only_spatial = TRUE)
      
      detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                         species_type = names_species[1], 
                                                         species_code = 1, 
                                                         variable = alpha_current)
          
      random_species_2 <- simulation_pattern %>%
          spatstat::subset.ppp(Species_code == 2) %>%
          SHAR::fit_point_process(process = 'cluster', 
                                  number_pattern = number_pattern)
      
      associations_species_2 <- SHAR::results_habitat_association(pattern = random_species_2, 
                                                                  raster = simulation_habitats,
                                                                  method = 'random_pattern', 
                                                                  only_spatial = TRUE)
      
      detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                         species_type = names_species[2], 
                                                         species_code = 2,
                                                         variable = alpha_current)
          
      random_species_3 <- simulation_pattern %>%
          spatstat::subset.ppp(Species_code == 3) %>%
          SHAR::fit_point_process(process = 'poisson', 
                                  number_pattern = number_pattern)
      
      associations_species_3 <- SHAR::results_habitat_association(pattern = random_species_3, 
                                                                  raster = simulation_habitats,
                                                                  method = 'random_pattern',
                                                                  only_spatial = TRUE)
      
      detection_species_3 <- detect_habitat_associations(input = associations_species_3, 
                                                         species_type = names_species[3],
                                                         species_code = 3,
                                                         variable = alpha_current)
      
      random_species_4 <- simulation_pattern %>%
          spatstat::subset.ppp(Species_code == 4) %>%
          SHAR::fit_point_process(process = 'cluster', 
                                  number_pattern = number_pattern) 
      
      associations_species_4 <- SHAR::results_habitat_association(pattern = random_species_4, 
                                                                  raster = simulation_habitats,
                                                                  method = 'random_pattern', 
                                                                  only_spatial = TRUE)
      
      (detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                         species_type = names_species[[4]],
                                                         species_code = 4,
                                                         variable = alpha_current))
        
      dplyr::bind_rows(detection_species_1, detection_species_2,
                       detection_species_3, detection_species_4)
    })
  })
}

# simulate_point_process_association_strength_loop <- function(number_coloumns, number_rows, 
#                                                              resolution, roughness, number_points, 
#                                                              simulation_runs, number_pattern, alpha_sequence){
#   
#   # Loop over association strengths
#   result_tbl_alpha <- listenv::listenv()
#   
#   for(current_alpha in 1:length(alpha_sequence)){
#     
#     result_tbl_alpha[[current_alpha]] <- { # FUTURE HERE
#       
#       # Create landscape and simulation pattern
#       simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows, 
#                                            resolution = resolution, roughness = roughness, verbose = F) %>%
#         SHAR::classify_habitats(classes = 5)
#       
#       simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
#                                                       number_points = number_points, 
#                                                       alpha = alpha_sequence[current_alpha])
#       
#       # Loop over simulation runs
#       result_tbl_runs <- listenv::listenv()
#       
#       for(current_run in 1:simulation_runs){
#         
#         result_tbl_runs[[current_run]] <- { # FUTURE HERE
#           
#           # Random number to create new simulation data in 25 % of simulation runs
#           r <- runif(n = 1)
#           if(r <= 1/8){
#             
#             simulation_habitats <- NLMR::nlm_mpd(ncol = number_coloumns, nrow = number_rows, 
#                                                  resolution = resolution, roughness = roughness, verbose = F) %>%
#               SHAR::classify_habitats(classes = 5)
#             
#             simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
#                                                             number_points = number_points, 
#                                                             alpha = alpha_sequence[current_alpha])      
#           }
#           
#           # Species 1
#           pattern_species_1 <- simulation_pattern %>% 
#             spatstat::subset.ppp(Species_code == 1)
#           
#           randomized_pattern_species_1 <- pattern_species_1 %>%
#             spatstat::unmark() %>%
#             spatstat::ppm() %>% 
#             spatstat::simulate.ppm(nsim = number_pattern, progress = F)
#           
#           randomized_pattern_species_1[[length(randomized_pattern_species_1)+1]] <- pattern_species_1
#           names(randomized_pattern_species_1)[[length(randomized_pattern_species_1)]] <- "Observed"
#           
#           result_species_1 <- SHAR::results_habitat_association(pattern = randomized_pattern_species_1,
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern', only_spatial = T)
#           
#           # positive associations (Poisson process)
#           habitat_1 <- as.numeric(stringi::stri_sub(unique(pattern_species_1$marks$Species),-1))
#           result_summarised_species_1 <- c(Species_code = 1, 
#                                            Variable = alpha_sequence[current_alpha], 
#                                            Correct = sum(result_species_1$Significance[result_species_1$Habitat == habitat_1] ==  "positive", na.rm = T),
#                                            False = sum(result_species_1$Significance[result_species_1$Habitat!= habitat_1] ==  "positive", na.rm = T), 
#                                            Simulation_run = current_run)
#           
#           # Species 2
#           pattern_species_2 <- simulation_pattern %>% 
#             spatstat::subset.ppp(Species_code == 2)
#           
#           randomized_pattern_species_2 <- pattern_species_2 %>%
#             spatstat::unmark() %>%
#             spatstat::ppm() %>% 
#             spatstat::simulate.ppm(nsim = number_pattern, progress = F)
#           
#           randomized_pattern_species_2[[length(randomized_pattern_species_2)+1]] <- pattern_species_2
#           names(randomized_pattern_species_2)[[length(randomized_pattern_species_2)]] <- "Observed"
#           
#           result_species_2 <- SHAR::results_habitat_association(pattern = randomized_pattern_species_2,
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern', only_spatial = T)
#           
#           # positive associations (Thomas process)
#           habitat_2 <- as.numeric(stringi::stri_sub(unique(pattern_species_2$marks$Species),-1))
#           result_summarised_species_2 <- c(Species_code = 2, 
#                                            Variable = alpha_sequence[current_alpha], 
#                                            Correct = sum(result_species_2$Significance[result_species_2$Habitat == habitat_2] ==  "positive", na.rm = T),
#                                            False = sum(result_species_2$Significance[result_species_2$Habitat!= habitat_2] ==  "positive", na.rm = T), 
#                                            Simulation_run = current_run)
#           
#           # Species 3 
#           pattern_species_3 <- simulation_pattern %>% 
#             spatstat::subset.ppp(Species_code == 3)
#           
#           randomized_pattern_species_3 <- pattern_species_3 %>%
#             spatstat::unmark() %>%
#             spatstat::kppm(cluster = "Thomas", statistic = "pcf", 
#                            statargs = list(divisor = "d")) %>% 
#             spatstat::simulate.kppm(nsim = number_pattern, verbose = F)
#           
#           randomized_pattern_species_3[[length(randomized_pattern_species_3)+1]] <- pattern_species_3
#           names(randomized_pattern_species_3)[[length(randomized_pattern_species_3)]] <- "Observed"
#           
#           result_species_3 <- SHAR::results_habitat_association(pattern = randomized_pattern_species_3,
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern', only_spatial = T)
#           
#           # negative associations (Poisson process)
#           habitat_3 <- as.numeric(stringi::stri_sub(unique(pattern_species_3$marks$Species),-1))
#           result_summarised_species_3 <- c(Species_code = 3, 
#                                            Variable = alpha_sequence[current_alpha], 
#                                            Correct = sum(result_species_3$Significance[result_species_3$Habitat == habitat_3] ==  "negative", na.rm = T),
#                                            False = sum(result_species_3$Significance[result_species_3$Habitat!= habitat_3] ==  "negative", na.rm = T), 
#                                            Simulation_run = current_run)
#           
#           # Species 4
#           pattern_species_4 <- simulation_pattern %>% 
#             spatstat::subset.ppp(Species_code == 4)
#           
#           randomized_pattern_species_4 <- pattern_species_4 %>%
#             spatstat::unmark() %>%
#             spatstat::ppm() %>% 
#             spatstat::simulate.ppm(nsim = number_pattern, progress = F)
#           
#           randomized_pattern_species_4[[length(randomized_pattern_species_4)+1]] <- pattern_species_4
#           names(randomized_pattern_species_4)[[length(randomized_pattern_species_4)]] <- "Observed"
#           
#           result_species_4 <- SHAR::results_habitat_association(pattern = randomized_pattern_species_4,
#                                                                 raster = simulation_habitats,
#                                                                 method = 'random_pattern', only_spatial = T)
#           
#           # negative associations (Thomas process)
#           habitat_4 <- as.numeric(stringi::stri_sub(unique(pattern_species_4$marks$Species),-1))
#           result_summarised_species_4 <- c(Species_code = 4, 
#                                            Variable = alpha_sequence[current_alpha], 
#                                            Correct = sum(result_species_4$Significance[result_species_4$Habitat == habitat_4] ==  "negative", na.rm = T),
#                                            False = sum(result_species_4$Significance[result_species_4$Habitat!= habitat_4] ==  "negative", na.rm = T),
#                                            Simulation_run = current_run)
#           
#           dplyr::bind_rows(result_summarised_species_1, 
#                            result_summarised_species_2,
#                            result_summarised_species_3,
#                            result_summarised_species_4)
#         }
#         cat(paste0("\rProgress - Alpha:", current_alpha, "/", length(alpha_sequence), " || Simulation:", current_run, "/", simulation_runs))
#       }
#       Reduce(rbind, result_tbl_runs)
#     }
#   }
#   Reduce(rbind, result_tbl_alpha)
# }
