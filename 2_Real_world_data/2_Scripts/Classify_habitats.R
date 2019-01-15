###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Classify habitats using MRT ####

#### Load packages ####
library(mvpart)
library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(raster)
library(rgeos)
library(spatstat)
library(tidyverse)

#### Extract species abundance ####

# import data 
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# convert to data frame
pattern_2007_df <- spatstat::as.data.frame.ppp(pattern_2007)

plot_area <- readr::read_rds(paste0(getwd(), 
                                    "/2_Real_world_data/1_Data/plot_area.rds"))

# import environmental data as raster
environmental_data_raster <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), 
                                        pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    data <- readr::read_rds(x)
    data_raster <- raster::rasterFromXYZ(data)
    raster::mask(x = data_raster,
                 mask = rgeos::gBuffer(plot_area, width = 10))
  })

# get names of environmental data
environmental_data_names <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

environmental_data_names <- purrr::map_chr(environmental_data_names, 
                                           function(x) stringr::str_sub(x, 
                                                                        start = 3, 
                                                                        end = -5))

# set names
names(environmental_data_raster) <- environmental_data_names

# stack list
environmental_data_raster <- raster::stack(environmental_data_raster)

# import DEM
environmental_data_DEM <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/4_DEM.rds")) %>%
  raster::crop(environmental_data_raster) %>% 
  raster::setExtent(ext = raster::extent(environmental_data_raster)) %>%
  raster::mask(mask = rgeos::gBuffer(plot_area, width = 10))

environmental_data_raster <- raster::stack(environmental_data_raster, 
                                           environmental_data_DEM)

# convert to data frame
environmental_data_df <- raster::as.data.frame(environmental_data_raster)

# extract cell id of each tree location
pattern_2007_cell_id <- raster::extract(x = environmental_data_raster, 
                                        y = pattern_2007_df[, 1:2], 
                                        cellnumbers = TRUE, df = TRUE, along = TRUE)

# data frame including only  cell id, coords, and species
pattern_2007_cell_id <- cbind(pattern_2007_cell_id[, 2], 
                              pattern_2007_df[, c(1:2,10)])

# set names
names(pattern_2007_cell_id)[1] <- "cell_id"

# count species abundance in each cell
species_abundance <- table(factor(pattern_2007_cell_id$cell_id, 
                                  levels = seq_len(raster::ncell(environmental_data_raster))),
                           pattern_2007_cell_id$Species) %>%
  as.data.frame() %>%
  purrr::set_names(c("cell_id", "species", "abundance")) %>%
  tidyr::spread(key = species, value = abundance)

#### MRT classification ####
species_abundance_matrix <- data.matrix(species_abundance[, -1])

# formula <- species_abundance_matrix ~ acidity + continentality + light_conditions + 
#   nitrogen + soil_depth + water_content_spring + water_content_summer + water_content

formula_soil <- scale(species_abundance_matrix) ~ acidity + continentality + light_conditions +
  nitrogen + soil_depth + water_content_spring + water_content_summer + water_content

# formula_DEM <- scale(species_abundance_matrix) ~ Aspect + Elevation + Slope

# Set a new seed for random numbers to ensure results are reproducible
set.seed(42)

# run mrt classification
mrt_model_soil <- mvpart::mvpart(form = formula_soil, 
                                 data = environmental_data_df, 
                                 xv = "pick")

# mrt_model_DEM <- mvpart::mvpart(form = formula_DEM, 
#                                 data = environmental_data_df, 
#                                 xv = "pick")

raster_coords <- raster::xyFromCell(environmental_data_raster$acidity, 
                                    cell = raster::Which(!is.na(environmental_data_raster$acidity), 
                                                         cells = TRUE))

classification_df_soil <- tibble::tibble(x = raster_coords[,1], 
                                         y = raster_coords[, 2],
                                         class = factor(mrt_model_soil$where))

# classification_df_DEM <- tibble::tibble(x = raster_coords[,1], 
#                                         y = raster_coords[, 2],
#                                         class = factor(mrt_model_DEM$where))

#### Plot results ####
table(classification_df_soil$class)
length(table(classification_df_soil$class))
sum(table(classification_df_soil$class))

# table(classification_df_DEM$class)
# length(table(classification_df_DEM$class))
# sum(table(classification_df_DEM$class))

ggplot2::ggplot(data = classification_df_soil, ggplot2::aes(x = x, y = y)) + 
  ggplot2::geom_raster(ggplot2::aes(fill = class)) + 
  ggplot2::coord_equal() + 
  ggplot2::scale_fill_viridis_d() + 
  ggplot2::theme_bw()

# ggplot2::ggplot(data = classification_df_DEM, ggplot2::aes(x = x, y = y)) + 
#   ggplot2::geom_raster(ggplot2::aes(fill = class)) + 
#   ggplot2::coord_equal() + 
#   ggplot2::scale_fill_viridis_d() + 
#   ggplot2::theme_bw()

#### Save results ####
soil_mrt_classified <- raster::rasterFromXYZ(classification_df_soil)

UtilityFunctions::save_rds(object = soil_mrt_classified,
                           path = paste0(getwd(), "/2_Real_world_data/3_Results"),
                           filename = "soil_mrt_classified.rds", 
                           overwrite = FALSE)

