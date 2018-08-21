#### Simulation study - Example methods ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages #
library(NLMR)
library(SHAR)
library(spatstat)
library(tidyverse)
library(UtilityFunctions)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = 'a0_', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

set.seed(42)

#### 2. Create example data ####

simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE) %>%
  SHAR::classify_habitats(classes = 5)

simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 250, 
                                                association_strength = 0.65)

example_species <- spatstat::subset.ppp(simulation_pattern, Species_code == 2)

ggplot() + 
  geom_raster(data = as.data.frame(simulation_landscape, xy = T), 
              aes(x = x, y = y, fill = layer)) + 
  geom_point(data = as.data.frame(example_species), 
             aes(x = x, y = y), size = 1.5) +
  scale_fill_distiller(palette = "Spectral") + 
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#### 3. Point process method ####

example_species_kppm <- fit_point_process(unmark(example_species), 
                                          process = "cluster", 
                                          number_pattern = 3)

example_species_kppm_long <- purrr::map_dfr(example_species_kppm, function(current_pattern) {
  tibble::as.tibble(spatstat::as.data.frame.ppp(current_pattern))
}, .id = "pattern")

example_species_kppm_long$pattern <- factor(example_species_kppm_long$pattern, 
                                            levels = c("Observed", "Simulation_1", 
                                                       "Simulation_2", "Simulation_3"),
                                            labels = c("Observed", "Simulation 1", 
                                                       "Simulation 2", "Simulation 3"))

plot_gamma <- ggplot(data = example_species_kppm_long) + 
  geom_raster(data = as.data.frame(simulation_landscape, xy = T),
              aes(x = x, y = y, fill = layer)) +
  geom_point(aes(x = x, y = y), size = 1.25) +
  facet_wrap(~ pattern, ncol = 4, nrow = 1) +
  scale_fill_distiller(palette = "Spectral") + 
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        text = element_text(size = 25),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

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

plot_random_walk <- ggplot(data = habitats_randomized_long) + 
  geom_raster(aes(x = x, y = y, fill = layer)) +
  geom_point(data = as.data.frame(example_species), 
             aes(x = x, y = y), size = 1.25) +
  facet_wrap(~ raster, ncol = 4, nrow = 1) +
  scale_fill_distiller(palette = "Spectral") + 
  theme(aspect.ratio = 1, 
        legend.position = "none",
        text = element_text(size = 25),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#### 5. Torus translation ####

habitats_torus <- SHAR::randomize_habitats(raster = simulation_landscape, 
                                           method = "torus_translation")

habitats_torus$Observed <- simulation_landscape

habitats_torus_long <- purrr::map_dfr(habitats_torus[c(25, 500, 1000, 2598)], function(current_raster) {
  tibble::as.tibble(raster::as.data.frame(current_raster, xy = TRUE))
}, .id = "raster")

habitats_torus_long$raster <- factor(habitats_torus_long$raster,
                                     levels = c("Observed", "Randomized_25", 
                                                "Randomized_500", "Randomized_1000"),
                                     labels = c("Observed", "Simulation 1",
                                                "Simulation 2", "Simulation 3"))

plot_torus <- ggplot(data = habitats_torus_long) + 
  geom_raster(aes(x = x, y = y, fill = layer)) +
  geom_point(data = as.data.frame(example_species), 
             aes(x = x, y = y), size = 1.25) +
  facet_wrap(~ raster, ncol = 4, nrow = 1) +
  scale_fill_distiller(palette = "Spectral") + 
  theme(aspect.ratio = 1, 
        legend.position = "none", 
        text = element_text(size = 25),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#### 6. Save plot ####

width = 45
heigth = 15 
dpi = 500

UtilityFunctions::save_ggplot(plot = plot_gamma, 
                              filename = "a0_plot_gamma.jpeg", 
                              path = "6_Figures", 
                              overwrite = FALSE, 
                              width = width, 
                              height = heigth, 
                              units = "cm", 
                              dpi = dpi)

UtilityFunctions::save_ggplot(plot = plot_random_walk, 
                              filename = "a0_plot_random_walk.jpeg", 
                              path = "6_Figures", 
                              overwrite = FALSE, 
                              width = width, 
                              height = heigth, 
                              units = "cm", 
                              dpi = dpi)

UtilityFunctions::save_ggplot(plot = plot_torus, 
                              filename = "a0_plot_torus.jpeg", 
                              path = "6_Figures", 
                              overwrite = FALSE, 
                              width = width, 
                              height = heigth, 
                              units = "cm", 
                              dpi = dpi)
