#### Simulation study - Example methods ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages
library(NLMR)
library(RColorBrewer)
library(UtilityFunctions)
library(shar)
library(spatstat)
library(tidyverse)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = 'f0_', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

set.seed(42, kind = "L'Ecuyer-CMRG")

#### 2. Create example data ####

simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE) %>%
  shar::classify_habitats(classes = 5)

simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 100, 
                                                association_strength = 0.35)

example_species <- spatstat::subset.ppp(simulation_pattern, species_code == 2)

colors_spec <- rev(RColorBrewer::brewer.pal(n = 5, name = "Spectral"))

ggplot() +
  geom_raster(data = raster::as.data.frame(simulation_landscape, xy = TRUE),
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

gamma_test <- shar::fit_point_process(example_species, 
                                      n_random = 1,
                                      process = "cluster")

gamma_test_long <- purrr::map_dfr(gamma_test, function(current_pattern) {
  tibble::as.tibble(spatstat::as.data.frame.ppp(current_pattern))
}, .id = "pattern")

gamma_test_long$pattern <- factor(gamma_test_long$pattern, 
                                  levels = c("observed", "randomized_1"),
                                  labels = c("Observed", "(b) Gamma test"))

names(gamma_test_long) <- c("method", "x", "y")


#### 4. Habitat randomization ####

habitats_randomized <- shar::randomize_raster(raster = simulation_landscape, 
                                              n_random  = 1)

habitats_randomized_long <- purrr::map_dfr(habitats_randomized, function(current_raster) {
  tibble::as.tibble(raster::as.data.frame(current_raster, xy = TRUE))
}, .id = "raster")

habitats_randomized_long$raster <- factor(habitats_randomized_long$raster, 
                                          levels = c("observed", "randomized_1"),
                                          labels = c("Observed", "(d) Patch randomization"))

names(habitats_randomized_long) <- c("method", "x", "y", "layer")


#### 5. Torus translation ####

habitats_torus <- shar::translate_raster(raster = simulation_landscape)

habitats_torus$Observed <- simulation_landscape

habitats_torus_long <- purrr::map_dfr(habitats_torus[c(1750, 2598)], function(current_raster) {
  tibble::as.tibble(raster::as.data.frame(current_raster, xy = TRUE))
}, .id = "raster")

habitats_torus_long$raster <- factor(habitats_torus_long$raster,
                                     levels = c("observed", "randomized_1750"),
                                     labels = c("Observed", "(c) Torus translation"))

names(habitats_torus_long) <- c("method", "x", "y", "layer")

#### 6. Pattern reconstruction ####

pattern_reconstruction <- shar::reconstruct_pattern(pattern = example_species, 
                                                    n_random = 1, 
                                                    max_runs = 2500)


pattern_reconstruction_long <- purrr::map_dfr(pattern_reconstruction, function(current_pattern) {
  tibble::as.tibble(spatstat::as.data.frame.ppp(current_pattern))
}, .id = "pattern")

pattern_reconstruction_long$pattern <- factor(pattern_reconstruction_long$pattern,
                                              levels = c("observed", "randomized_1"),
                                              labels = c("Observed", "(e) Pattern reconstruction"))


names(pattern_reconstruction_long) <- c("method", "x", "y")

#### 7. Save results ####

overwrite <- FALSE

UtilityFunctions::save_rds(object = simulation_landscape,
                           filename = "o00_simulation_landscape.rds",
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = example_species,
                           filename = "o00_example_species.rds",
                           path = paste0(getwd(), "/4_Output"),
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = gamma_test_long,
                           filename = "o00_gamma_test.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = habitats_randomized_long,
                           filename = "o00_patch_randomization.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = habitats_torus_long,
                           filename = "o00_torus_translation.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = pattern_reconstruction_long,
                           filename = "o00_pattern_reconstruction.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = overwrite)

