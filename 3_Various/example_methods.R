###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### 1. Import packages & functions ####

# Packages
library(NLMR)
library(UtilityFunctions)
library(shar)
library(spatstat)
library(tidyverse)

# source all functions in R_functions folder
list.files(path = "1_Simulation_study/1_Functions/", full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

#### 2. Create example data ####

# create landscape
simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE) %>%
  shar::classify_habitats(classes = 5)

# create pattern with 4 species
simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 100, 
                                                association_strength = 0.35)

# pick species 2 as example
example_species <- spatstat::subset.ppp(simulation_pattern, species_code == 2)

# plot pattern
ggplot() +
  geom_raster(data = raster::as.data.frame(simulation_landscape, xy = TRUE),
              aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(example_species),
             aes(x = x, y = y), size = 1.5) +
  ggplot2::scale_fill_viridis_d() + 
  theme_void() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#### 3. Point process method ####

# fit clustered pattern to data
gamma_test <- shar::fit_point_process(example_species, 
                                      n_random = 1,
                                      process = "cluster")

# convert to long dataframe
gamma_test <- purrr::map_dfr(gamma_test, function(current_pattern) {
  tibble::as_tibble(spatstat::as.data.frame.ppp(current_pattern))
  }, .id = "pattern")

# convert pattern col to factor
gamma_test$pattern <- factor(gamma_test_long$pattern, 
                             levels = c("observed", "randomized_1"),
                             labels = c("Observed", "(b) Gamma test"))

# set identical names
names(gamma_test) <- c("method", "x", "y")

#### 4. Habitat randomization ####

# randomize habitats
habitats_randomized <- shar::randomize_raster(raster = simulation_landscape, 
                                              n_random  = 1)

# convert to long dataframe
habitats_randomized <- purrr::map_dfr(habitats_randomized, function(current_raster) {
  tibble::as_tibble(raster::as.data.frame(current_raster, xy = TRUE))
  }, .id = "raster")

# convert pattern col to factor\
habitats_randomized$raster <- factor(habitats_randomized$raster, 
                                     levels = c("observed", "randomized_1"),
                                     labels = c("Observed", "(d) Patch randomization"))

# set identical names
names(habitats_randomized) <- c("method", "x", "y", "layer")

#### 5. Torus translation ####

# translate habitats
habitats_torus <- shar::translate_raster(raster = simulation_landscape)

habitats_torus <- purrr::map_dfr(habitats_torus[c(1750, 2598)], function(current_raster) {
  tibble::as_tibble(raster::as.data.frame(current_raster, xy = TRUE))
  }, .id = "raster")

# convert pattern col to factor
habitats_torus$raster <- factor(habitats_torus$raster,
                                levels = c("observed", "randomized_1750"),
                                labels = c("Observed", "(c) Torus translation"))

# set identical names
names(habitats_torus) <- c("method", "x", "y", "layer")

#### 6. Pattern reconstruction ####

# reconstruct pattern
pattern_reconstruction <- shar::reconstruct_pattern(pattern = example_species, 
                                                    n_random = 1, 
                                                    max_runs = 2500)


# convert to long dataframe
pattern_reconstruction <- purrr::map_dfr(pattern_reconstruction, function(current_pattern) {
  tibble::as_tibble(spatstat::as.data.frame.ppp(current_pattern))
}, .id = "pattern")

# convert pattern col to factor
pattern_reconstruction$pattern <- factor(pattern_reconstruction$pattern,
                                         levels = c("observed", "randomized_1"),
                                         labels = c("Observed", "(e) Pattern reconstruction"))

# set identical names
names(pattern_reconstruction) <- c("method", "x", "y")

#### 7. Save results ####

overwrite <- FALSE

UtilityFunctions::save_rds(object = simulation_landscape,
                           filename = "simulation_landscape.rds",
                           path = "3_Various/1_Output",
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = example_species,
                           filename = "species.rds",
                           path = "3_Various/1_Output",
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = gamma_test,
                           filename = "gamma_test.rds",
                           path = "3_Various/1_Output",
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = habitats_randomized,
                           filename = "patch_randomization.rds",
                           path = "3_Various/1_Output",
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = habitats_torus,
                           filename = "torus_translation.rds",
                           path = "3_Various/1_Output",
                           overwrite = overwrite)

UtilityFunctions::save_rds(object = pattern_reconstruction,
                           filename = "pattern_reconstruction.rds",
                           path = "3_Various/1_Output",
                           overwrite = overwrite)

