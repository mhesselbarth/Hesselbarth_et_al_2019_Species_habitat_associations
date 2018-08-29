#### Examples methods #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
source(paste0(getwd(), '/2_Functions/setup_packages.R'))

#### 1. Input data ####
results <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a00_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

results_names <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a00_', full.names = FALSE) %>%
  purrr::map_chr(function(files) files)

results_names <- stringr::str_sub(results_names, start = 1, end = -5)
names_split <- stringr::str_split(results_names, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3])
names(results) <- names_combined

colors_spec <- rev(RColorBrewer::brewer.pal(n = 5, name = "Spectral"))

#### Plot results ####

# Gamma test #
plot_gamma_test <- ggplot(data = results$gamma_test) + 
  geom_raster(data = as.data.frame(results$simulation_landscape, xy = T),
              aes(x = x, y = y, fill = factor(layer))) +
  geom_point(aes(x = x, y = y), size = 2) +
  facet_wrap(~ pattern, ncol = 4, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = 40),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Patch randomization test #
plot_habitat_randomization_test <- ggplot(data = results$habitats_randomized) + 
  geom_raster(aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species), 
             aes(x = x, y = y), size = 2) +
  facet_wrap(~ raster, ncol = 4, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none",
        text = element_text(size = 40),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Torus translation test
plot_torus_translation_test <- ggplot(data = results$habitats_torus) + 
  geom_raster(aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = as.data.frame(results$example_species), 
             aes(x = x, y = y), size = 2) +
  facet_wrap(~ raster, ncol = 4, nrow = 1) +
  scale_fill_manual(values = colors_spec) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        panel.spacing = unit(15, "mm"),
        legend.position = "none", 
        text = element_text(size = 40),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#### 6. Save plot ####

width <- 425
heigth <- 100 
overwrite <- TRUE

UtilityFunctions::save_ggplot(plot = plot_gamma_test, 
                              filename = "p00_plot_gamma_test.png", 
                              path = "6_Figures", 
                              overwrite = overwrite, 
                              width = width, 
                              height = heigth, 
                              units = "mm")

UtilityFunctions::save_ggplot(plot = plot_habitat_randomization_test, 
                              filename = "p00_plot_habitat_randomization_test.png", 
                              path = "6_Figures", 
                              overwrite = overwrite, 
                              width = width, 
                              height = heigth, 
                              units = "mm")

UtilityFunctions::save_ggplot(plot = plot_torus_translation_test, 
                              filename = "p00_plot_torus_translation_test.png", 
                              path = "6_Figures", 
                              overwrite = overwrite, 
                              width = width, 
                              height = heigth, 
                              units = "mm")
