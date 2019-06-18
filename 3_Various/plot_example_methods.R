###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### 1. Import packages and data ####

# Packages #
library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(patchwork)
library(raster)
library(spatstat)
library(tidyverse)

#### 1. Input data ####

# import data
results <- list.files(path = "3_Various/1_Output/", full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

# get names of input
results_names <- list.files(path = "3_Various/1_Output/", full.names = FALSE) %>%
  purrr::map_chr(function(files) files)

# assign names to result list
names(results) <- stringr::str_sub(results_names, start = 1, end = -5)

#### Plot results ####

# set point size
point_size <- 1
col <- "black"
text_size <- 12.5

# Observed
# convert to data frame
simulation_landscape <- raster::as.data.frame(results$simulation_landscape, xy = TRUE)

# set method
simulation_landscape$method <- "(a) Observed"

plot_observed <- ggplot() +
  geom_raster(data = simulation_landscape, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species),
             aes(x = x, y = y), size = point_size + 0.25, col = col) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_viridis_d() +
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = text_size),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.line = element_blank())

# Gamma test #
gamma_test <- dplyr::filter(results$gamma_test,
                            method == "(b) gamma-test")

plot_gamma_test <- ggplot(data = gamma_test) + 
  geom_raster(data = as.data.frame(results$simulation_landscape, xy = TRUE),
              aes(x = x, y = y, fill = factor(layer))) +
  geom_point(aes(x = x, y = y), size = point_size, col = col) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_viridis_d() +
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = text_size),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.line = element_blank())

# Torus translation test
torus_translation <- dplyr::filter(results$torus_translation,
                                   method == "(c) Torus translation")

plot_torus_translation_test <- ggplot(data = torus_translation) + 
  geom_raster(aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species), 
             aes(x = x, y = y), size = point_size, col = col) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_viridis_d() +
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = text_size),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.line = element_blank())


# Patch randomization test #
patch_randomization <- dplyr::filter(results$patch_randomization,
                                     method == "(d) Randomized habitats")

plot_patch_randomization_algorithm <- ggplot(data = patch_randomization) + 
  geom_raster(aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species), 
             aes(x = x, y = y), size = point_size, col = col) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_viridis_d() +
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none",
        text = element_text(size = text_size),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.line = element_blank())


# Pattern reconstruction test #
pattern_reconstruction <- dplyr::filter(results$pattern_reconstruction,
                                        method == "(e) Pattern reconstruction")

plot_pattern_reconstruction <- ggplot(data = pattern_reconstruction) + 
  geom_raster(data = as.data.frame(results$simulation_landscape, xy = TRUE),
              aes(x = x, y = y, fill = factor(layer))) +
  geom_point(aes(x = x, y = y), size = point_size, col = col) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_viridis_d() +
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = text_size),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.line = element_blank())


# Plot overall

# set plot size for overall
width_plot <- 1
width_spacer <- 0

plot_overall <- plot_observed + {plot_gamma_test + plot_torus_translation_test + 
    plot_patch_randomization_algorithm + plot_pattern_reconstruction}

# plot_overall <- plot_observed + plot_spacer() + 
#   plot_gamma_test + plot_spacer() +
#   plot_torus_translation_test + plot_spacer() +
#   plot_patch_randomization_algorithm + plot_spacer() +
#   plot_pattern_reconstruction +
#   plot_layout(nrow = 1, 
#               widths = c(width_plot, width_spacer,
#                          width_plot, width_spacer,
#                          width_plot, width_spacer,
#                          width_plot, width_spacer,
#                          width_plot)) 

#### 6. Save plot ####

# set plot saving parameters
width <- 210
heigth <- 120 
overwrite <- FALSE

helpeR::save_ggplot(plot = plot_overall,
                    filename = "plot_methods.png",
                    path = "3_Various/2_Figures",
                    width = width,
                    height = heigth,
                    units = "mm", 
                    overwrite = overwrite)

# helpeR::save_ggplot(plot = plot_gamma_test, 
#                               filename = "p00_plot_gamma_test.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
# 
# helpeR::save_ggplot(plot = plot_pattern_reconstruction, 
#                               filename = "p00_plot_pattern_reconstruction.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
# 
# helpeR::save_ggplot(plot = plot_habitat_randomization_test, 
#                               filename = "p00_plot_habitat_randomization_test.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
# 
# helpeR::save_ggplot(plot = plot_torus_translation_test, 
#                               filename = "p00_plot_torus_translation_test.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
