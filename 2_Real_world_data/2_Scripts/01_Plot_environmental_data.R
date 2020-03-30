###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Classify habitats using MRT ####

#### Load packages ####
library(extrafont)
library(patchwork)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(raster)
library(tidyverse)

# import data #
plot_area <- readr::read_rds("2_Real_world_data/1_Data/05_plot_area_owin.rds") %>% 
  as.data.frame()

plot_area_sp <- readr::read_rds("2_Real_world_data/1_Data/05_plot_area_sp.rds")

# import environmental data as raster
environmental_data_raster <- list.files("2_Real_world_data/1_Data",
                                        pattern = '03_', full.names = TRUE) %>%
  purrr::map(function(x) {
    data <- readr::read_rds(x)
    data_raster <- raster::rasterFromXYZ(data)
    raster::mask(x = data_raster,
                 mask = plot_area_sp)
  })

# get names of environmental data
environmental_data_names <- list.files("2_Real_world_data/1_Data/", 
                                       pattern = "03_")

environmental_data_names <- c("Soil_acidity", "Continentality", "Light_conditions",    
                              "Soil_nitrogen", "Soil_depth", "Plant_available_water",        
                              "Soil_water_content_spring", "Soil_water_content_summer")

# set names
names(environmental_data_raster) <- environmental_data_names

# stack list
environmental_data_raster <- raster::stack(environmental_data_raster)


# conver to dataframe
environmental_data_raster <- raster::as.data.frame(environmental_data_raster, 
                                                   xy = TRUE) %>% 
  tidyr::pivot_longer(cols = -c(x, y), names_to = "measure") %>% 
  dplyr::mutate(value_scl = as.numeric(scale(value))) %>% 
  dplyr::arrange(measure, x, y)

plot_function <- function(df, title) {
  
  title <- gsub(title, patter = "_", replacement = " ")
  
  ggplot(data = df) + 
    geom_raster(aes(x = x, y = y, fill = value_scl)) + 
    # geom_polygon(data = plot_area, aes(x = x, y = y), fill = NA, col = "black") +
    scale_fill_viridis_c(na.value = "white", guide = FALSE) +
    coord_equal() + 
    labs(title = title) + 
    theme_void(base_size = 12.5) + 
    theme(text = element_text(family = "Calibri Light"))
}

environmental_data_temp <- dplyr::group_by(environmental_data_raster, measure) %>% 
  tidyr::nest() %>% 
  mutate(ggplots = map2(data, measure, plot_function)) 

ggplot_environ <- gridExtra::grid.arrange(grobs = environmental_data_temp$ggplots) %>% 
  patchwork::wrap_ggplot_grob()

suppoRt::save_ggplot(plot = ggplot_environ, 
                     filename = "ggplot_environ.png", 
                     path = "C:/Users/Maximilian/ownCloud/13_Thesis_defense/Figures/", 
                     dpi = 300, width = 250, height = 250, units = "mm", 
                     overwrite = FALSE)


