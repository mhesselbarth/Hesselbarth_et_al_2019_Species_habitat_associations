###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Classify habitats using MRT ####

#### Load packages ####
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(raster)
library(spatstat)
library(tidyverse)

# import data
plot_area_sp <- readr::read_rds("2_Real_world_data/1_Data/05_plot_area_sp.rds")

pattern_2007 <- readr::read_rds("2_Real_world_data/1_Data/02_pattern_2007_ppp.rds")

mrt_model_species <- readr::read_rds("2_Real_world_data/3_Results/mrt_model_species.rds")

# convert to data frame
pattern_2007_df_living <- tibble::as_tibble(pattern_2007) %>%
  dplyr::filter(type == "living")

#### Plot results ####
plot_area_matrix <- as.data.frame(raster::geom(plot_area_sp))

classification_raster_list <- readr::read_rds("2_Real_world_data/3_Results/classification_raster_list.rds")

classification_raster_df <- purrr::map_dfr(classification_raster_list, function(x) {
  raster::as.data.frame(x, xy = TRUE) %>%
    dplyr::filter(!is.na(layer)) %>%
    dplyr::mutate(layer = as.factor(layer))  
}, .id = "type")

# classification_raster_df$type <- factor(classification_raster_df$type, 
#                                         levels = c("species", "size", "status"),
#                                         labels = c("Species", "Size classes", "Tree status"))

plot_classified <- ggplot2::ggplot() + 
  ggplot2::geom_raster(data = dplyr::filter(classification_raster_df,
                                            type == "species"), 
                       ggplot2::aes(x = x, y = y, fill = layer)) + 
  ggplot2::geom_polygon(data = plot_area_matrix,
                        aes(x = plot_area_matrix[, 5], y = plot_area_matrix[, 6]),
                        fill = NA, col = "black", size = 1) +
  # ggplot2::facet_wrap(~ type) +
  ggplot2::coord_equal() + 
  ggplot2::scale_fill_viridis_d(name = "Habitat") + 
  ggplot2::labs(x = "x coordinate", y = "y coordinate") +
  ggplot2::theme_bw(base_size = 15)

suppoRt::save_ggplot(plot = plot_classified, 
                     filename = "plot_classified.png", 
                     path = "2_Real_world_data/4_Figures", 
                     dpi = 300, width = 210, height = 120, units = "mm")

#### Results classification

# number of cells in each habitat
raster::values(classification_raster_list$species) %>% 
  table()

raster::values(classification_raster_list$species) %>% 
  table() %>% 
  sum()

# area in ha
raster::values(classification_raster_list$species) %>% 
  table() %>% 
  magrittr::multiply_by(prod(raster::res(classification_raster_list$species))) %>% 
  magrittr::divide_by(10000)

raster::values(classification_raster_list$species) %>% 
  table() %>% 
  magrittr::multiply_by(prod(raster::res(classification_raster_list$species))) %>% 
  magrittr::divide_by(10000) %>% 
  sum()

# trees in each habitat
raster::extract(x = classification_raster_list$species, 
                y = pattern_2007_df_living[, 1:2]) %>% 
  table(useNA = "ifany")

raster::extract(x = classification_raster_list$species, 
                y = pattern_2007_df_living[, 1:2]) %>% 
  table(useNA = "ifany") %>% 
  sum()
