#### Examples methods #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
library(patchwork)
library(RColorBrewer)
library(spatstat)
library(tidyverse)

#### 1. Input data ####
results <- list.files(paste0(getwd(), '/4_Output'), pattern = 'o00_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

results_names <- list.files(paste0(getwd(), '/4_Output'), pattern = 'o00_', full.names = FALSE) %>%
  purrr::map_chr(function(files) files)

results_names <- stringr::str_sub(results_names, start = 1, end = -5)
names_split <- stringr::str_split(results_names, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3])
names(results) <- names_combined

colors_spec <- rev(RColorBrewer::brewer.pal(n = 5, name = "Spectral"))

#### Plot results ####

point_size = 3.5

# Observed

simulation_landscape <- as.data.frame(results$simulation_landscape, xy = TRUE)
simulation_landscape$Method <- "Observed"

plot_observed <- ggplot() +
  geom_raster(data = simulation_landscape, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species),
             aes(x = x, y = y), size = point_size) +
  facet_wrap(~ Method, ncol = 1, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = 30),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Gamma test #
gamma_test <- dplyr::filter(results$gamma_test,
                            method == "(I) Gamma test")

plot_gamma_test <- ggplot(data = gamma_test) + 
  geom_raster(data = as.data.frame(results$simulation_landscape, xy = TRUE),
              aes(x = x, y = y, fill = factor(layer))) +
  geom_point(aes(x = x, y = y), size = point_size) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = 30),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Torus translation test
torus_translation <- dplyr::filter(results$torus_translation,
                                   method == "(II) Torus translation")

plot_torus_translation_test <- ggplot(data = torus_translation) + 
  geom_raster(aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species), 
             aes(x = x, y = y), size = point_size) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = 30),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Patch randomization test #
patch_randomization <- dplyr::filter(results$patch_randomization,
                                     method == "(III) Patch randomization")

plot_patch_randomization_algorithm <- ggplot(data = patch_randomization) + 
  geom_raster(aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species), 
             aes(x = x, y = y), size = point_size) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none",
        text = element_text(size = 30),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Pattern reconstruction test #
pattern_reconstruction <- dplyr::filter(results$pattern_reconstruction,
                                        method == "(IV) Pattern reconstruction")

plot_pattern_reconstruction <- ggplot(data = pattern_reconstruction) + 
  geom_raster(data = as.data.frame(results$simulation_landscape, xy = TRUE),
              aes(x = x, y = y, fill = factor(layer))) +
  geom_point(aes(x = x, y = y), size = point_size) +
  facet_wrap(~ method, ncol = 1, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = 30),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Plot overall

width_plot <- 1
width_spacer <- 0.25

plot_overall <- plot_observed + plot_spacer() + 
  plot_gamma_test + plot_spacer() +
  plot_torus_translation_test + plot_spacer() +
  plot_patch_randomization_algorithm + plot_spacer() +
  plot_pattern_reconstruction +
  plot_layout(nrow = 1, 
              widths = c(width_plot, width_spacer,
                         width_plot, width_spacer,
                         width_plot, width_spacer,
                         width_plot, width_spacer,
                         width_plot)) 

#### 6. Save plot ####

width <- 700
heigth <- 180 
overwrite <- FALSE

UtilityFunctions::save_ggplot(plot = plot_overall,
                              filename = "p00_plot_methods.png",
                              path = "6_Figures",
                              overwrite = overwrite,
                              width = width,
                              height = heigth,
                              units = "mm")

# UtilityFunctions::save_ggplot(plot = plot_gamma_test, 
#                               filename = "p00_plot_gamma_test.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
# 
# UtilityFunctions::save_ggplot(plot = plot_pattern_reconstruction, 
#                               filename = "p00_plot_pattern_reconstruction.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
# 
# UtilityFunctions::save_ggplot(plot = plot_habitat_randomization_test, 
#                               filename = "p00_plot_habitat_randomization_test.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
# 
# UtilityFunctions::save_ggplot(plot = plot_torus_translation_test, 
#                               filename = "p00_plot_torus_translation_test.png", 
#                               path = "6_Figures", 
#                               overwrite = overwrite, 
#                               width = width, 
#                               height = heigth, 
#                               units = "mm")
