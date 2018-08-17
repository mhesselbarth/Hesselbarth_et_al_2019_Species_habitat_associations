#### Simulation study - Example methods ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages #
library(furrr)
library(future.batchtools)
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

simulation_landscape <- NLMR::nlm_mpd(ncol = 30, nrow = 30, 
                                      resolution = 20, roughness = 0.3, 
                                      verbose = FALSE) %>%
  SHAR::classify_habitats(classes = 5)

simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 100, alpha=0.35)

species_4 <- spatstat::subset.ppp(simulation_pattern, Species_code == 4)

plot_observed <- ggplot() + 
  geom_raster(data = as.data.frame(simulation_landscape, xy = T), 
              aes(x = x, y = y, fill = factor(layer))) + 
  geom_point(data = as.data.frame(species_4), 
             aes(x = x, y = y), size = 2.5) +
  scale_fill_viridis_d() +
  theme_classic() + 
  theme(aspect.ratio = 1, legend.position = "none") +
  labs(title="Observed data")


#### 3. Point process method ####

species_4_kppm <- spatstat::kppm(unmark(species_4), 
                                 cluster = "Thomas", statistic = "pcf", 
                                 statargs = list(divisor = "d")) %>%
  simulate.kppm(nsim = 3)

gamma_test <- purrr::map(1:length(species_4_kppm), function(i){
  
  ggplot() + 
    geom_raster(data = as.data.frame(simulation_landscape, xy = T), 
                aes(x = x, y = y, fill = factor(layer))) + 
    geom_point(data = spatstat::as.data.frame.ppp(species_4_kppm[[i]]), 
               aes(x = x, y = y), size = 2.5) +
    scale_fill_viridis_d() +
    theme_classic() + 
    theme(aspect.ratio = 1, legend.position = "none") +
    labs(title=paste0("Randomzation ", i))
})

plot_gamma_test <- plot_observed + gamma_test[[1]] +
  gamma_test[[2]] + gamma_test[[3]] + 
  plot_layout(ncol=4, nrow=1)

#### 4. Habitat randomization ####

habitats_randomized <- SHAR::randomize_habitats(raster = simulation_landscape, 
                                                method = "randomization_algorithm",
                                                number_maps = 3)

random_test <- purrr::map(1:(length(habitats_randomized)-1), function(i) {
  
  ggplot() + 
    geom_raster(data = as.data.frame(habitats_randomized[[i]], xy = T), 
                aes(x = x, y = y, fill = factor(layer))) + 
    geom_point(data = spatstat::as.data.frame.ppp(species_4), 
               aes(x = x, y = y), size = 2.5) +
    scale_fill_viridis_d() +
    theme_classic() + 
    theme(aspect.ratio = 1, legend.position = "none") +
    labs(title=paste0("Randomzation ", i))
})

plot_random_test <- plot_observed + random_test[[1]] +
  random_test[[2]] + random_test[[3]] + 
  plot_layout(ncol=4, nrow=1)

#### 5. Torus translation ####

habitats_torus <- SHAR::randomize_habitats(raster = simulation_landscape, 
                                           method = "torus_translation")

torus_test <- purrr::map(c(10,20,30), function(i) {
  
  ggplot() + 
    geom_raster(data = as.data.frame(habitats_torus[[i]], xy = T), 
                aes(x = x, y = y, fill = factor(layer))) + 
    geom_point(data = spatstat::as.data.frame.ppp(species_4), 
               aes(x = x, y = y), size = 2.5) +
    scale_fill_viridis_d() +
    theme_classic() + 
    theme(aspect.ratio = 1, legend.position = "none") +
    labs(title=paste0("Randomzation ", i))
})

plot_torus_test <- plot_observed + torus_test[[1]] +
  torus_test[[2]] + torus_test[[3]] + 
  plot_layout(ncol=4, nrow=1)

plot_observed + gamma_test[[1]] + gamma_test[[2]] + gamma_test[[3]] + 
  plot_observed + random_test[[1]] + random_test[[2]] + random_test[[3]] +
  plot_observed + torus_test[[1]] +  torus_test[[2]] + torus_test[[3]] + 
  plot_layout(ncol=4, nrow=3)

