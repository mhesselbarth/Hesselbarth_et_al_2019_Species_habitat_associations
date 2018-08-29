#### Simulation study - Example methods ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages
source(paste0(getwd(), '/2_Functions/setup_packages.R'))

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = 'a0_', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

set.seed(42, kind = "L'Ecuyer-CMRG")

#### 2. Create example data ####

simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE) %>%
  SHAR::classify_habitats(classes = 5)

simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 250, 
                                                association_strength = 0.35)

example_species <- spatstat::subset.ppp(simulation_pattern, Species_code == 2)

colors_spec <- rev(RColorBrewer::brewer.pal(n = 5, name = "Spectral"))

ggplot() + 
  geom_raster(data = as.data.frame(simulation_landscape, xy = T), 
              aes(x = x, y = y, fill = factor(layer))) + 
  geom_point(data = as.data.frame(example_species),
             aes(x = x, y = y), size = 1.5) +
  scale_fill_manual(values = colors_spec) + 
  theme_void() + 
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#### 3. Point process method ####

gamma_test <- fit_point_process(unmark(example_species), 
                                          process = "cluster", 
                                          number_pattern = 3)

gamma_test_long <- purrr::map_dfr(gamma_test, function(current_pattern) {
  tibble::as.tibble(spatstat::as.data.frame.ppp(current_pattern))
}, .id = "pattern")

gamma_test$pattern <- factor(example_species_kppm_long$pattern, 
                             levels = c("Observed", "Simulation_1", 
                                        "Simulation_2", "Simulation_3"),
                             labels = c("Observed", "Simulation 1", 
                                        "Simulation 2", "Simulation 3"))


#### 4. Habitat randomization ####

habitats_randomized <- SHAR::randomize_habitats(raster = simulation_landscape, 
                                                method = "randomization_algorithm",
                                                number_maps = 3)

habitats_randomized_long <- purrr::map_dfr(habitats_randomized, function(current_raster) {
  tibble::as.tibble(raster::as.data.frame(current_raster, xy = TRUE))
}, .id = "raster")

habitats_randomized_long$raster <- factor(habitats_randomized_long$raster, 
                                          levels = c("Observed", "Randomized_1", 
                                                     "Randomized_2", "Randomized_3"),
                                          labels = c("Observed", "Simulation 1", 
                                                     "Simulation 2", "Simulation 3"))

#### 5. Torus translation ####

habitats_torus <- SHAR::randomize_habitats(raster = simulation_landscape, 
                                           method = "torus_translation")

habitats_torus$Observed <- simulation_landscape

habitats_torus_long <- purrr::map_dfr(habitats_torus[c(25, 750, 1750, 2598)], function(current_raster) {
  tibble::as.tibble(raster::as.data.frame(current_raster, xy = TRUE))
}, .id = "raster")

habitats_torus_long$raster <- factor(habitats_torus_long$raster,
                                     levels = c("Observed", "Randomized_25", 
                                                "Randomized_750", "Randomized_1750"),
                                     labels = c("Observed", "Simulation 1",
                                                "Simulation 2", "Simulation 3"))

#### 6. Save results ####

overwrite <- TRUE

UtilityFunctions::save_rds(object = simulation_landscape,
                           filename = "a00_simulation_landscape.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = example_species,
                           filename = "a00_example_species.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = gamma_test_long,
                           filename = "a00_gamma_test.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = habitats_randomized_long,
                           filename = "a00_habitats_randomized.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = habitats_torus_long,
                           filename = "a00_habitats_torus.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)


