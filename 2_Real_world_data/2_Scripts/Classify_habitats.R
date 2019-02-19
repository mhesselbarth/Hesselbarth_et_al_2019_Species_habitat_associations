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
# 
# # import data
# pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))
# 
# # convert to data frame
# pattern_2007_df <- spatstat::as.data.frame.ppp(pattern_2007) %>%
#   dplyr::filter(Type == "living")
# 
# # pattern_2007_df$ID_new <- 1:nrow(pattern_2007_df)
# 
# plot_area <- readr::read_rds(paste0(getwd(),
#                                     "/2_Real_world_data/1_Data/plot_area.rds"))
# 
# # import environmental data as raster
# environmental_data_raster <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"),
#                                         pattern = '3_', full.names = TRUE) %>%
#   purrr::map(function(x) {
#     data <- readr::read_rds(x)
#     data_raster <- raster::rasterFromXYZ(data)
#     raster::mask(x = data_raster,
#                  mask = plot_area)
#   })
# 
# # get names of environmental data
# environmental_data_names <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')
# 
# environmental_data_names <- purrr::map_chr(environmental_data_names,
#                                            function(x) stringr::str_sub(x,
#                                                                         start = 3,
#                                                                         end = -5))
# 
# # set names
# names(environmental_data_raster) <- environmental_data_names
# 
# # stack list
# environmental_data_raster <- raster::stack(environmental_data_raster)
# 
# # import DEM
# # environmental_data_DEM <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/4_DEM.rds")) %>%
# #   raster::crop(environmental_data_raster) %>%
# #   raster::setExtent(ext = raster::extent(environmental_data_raster)) %>%
# #   raster::mask(mask = rgeos::gBuffer(plot_area, width = 10))
# 
# # stack all layers to one RasterStack
# # environmental_data_raster <- raster::stack(environmental_data_raster,
# #                                            environmental_data_DEM)
# 
# # only data that is inside plot
# # environmental_data_raster <- raster::intersect(x = environmental_data_raster,
# #                                                y = plot_area)
# 
# # convert to data frame
# environmental_data_df <- raster::as.data.frame(environmental_data_raster)
# 
# # only non-NA environmental data
# environmental_data_df <- environmental_data_df[complete.cases(environmental_data_df), ]
# 
# # cell IDs of all non-NA cells
# cell_ids <- as.numeric(row.names(environmental_data_df))
# 
# # scale data
# # environmental_data_df <- as.data.frame(apply(environmental_data_df, MARGIN = 2, FUN = scale))
# 
# # extract cell id of each tree location
# pattern_2007_cell_id <- raster::extract(x = environmental_data_raster,
#                                         y = pattern_2007_df[, 1:2],
#                                         cellnumbers = TRUE, df = TRUE, along = TRUE)
# 
# # combine data to get cell id and species information
# pattern_2007_cell_id <- cbind(pattern_2007_cell_id,
#                               pattern_2007_df)
# 
# # only non NA data
# pattern_2007_cell_id <- pattern_2007_cell_id[complete.cases(pattern_2007_cell_id), ]
# 
# # data frame including only  cell id, and species
# pattern_2007_cell_id <- pattern_2007_cell_id[, c(2, 15, 20)]
# 
# # Calculate importance value for each species in each cell
# species_iv <- dplyr::mutate(pattern_2007_cell_id, basal_area = (pi / 4) * DBH_07 ^ 2) %>%
#   dplyr::group_by(cells, Species) %>%
#   dplyr::summarise(n = n(),
#                    ba_sum = sum(basal_area)) %>%
#   dplyr::mutate(n_rel = n / sum(n),
#                 ba_rel = ba_sum / sum(ba_sum),
#                 importance_value = n_rel + ba_rel) %>%
#   dplyr::select(cells, Species, importance_value) %>%
#   tidyr::spread(key = Species, value = importance_value,
#                 fill = 0) %>%
#   dplyr::arrange(cells) %>%
#   dplyr::mutate(Beech = tidyr::replace_na(Beech, 0),
#                 Ash = tidyr::replace_na(Ash, 0),
#                 Hornbeam = tidyr::replace_na(Hornbeam, 0),
#                 Sycamore = tidyr::replace_na(Sycamore, 0),
#                 others = tidyr::replace_na(others, 0))
# 
# # count species abundance in each cell
# # species_abundance <- table(factor(pattern_2007_cell_id$cell_id,
# #                                   levels = cell_ids),
# #                            pattern_2007_cell_id$Species) %>%
# #   as.data.frame() %>%
# #   purrr::set_names(c("cell_id", "species", "abundance")) %>%
# #   tidyr::spread(key = species, value = abundance)
# 
# #### MRT classification ####
# # convert to matrix without cell id
# # species_abundance_matrix <- data.matrix(species_abundance[, -1])
# 
# # convert to matrix without cell id
# species_iv_matrix <- data.matrix(species_iv[, -1])
# 
# # formula_soil <- scale(species_abundance_matrix) ~ acidity + continentality + light_conditions +
# #   nitrogen + soil_depth + water_content_spring + water_content_summer + water_content
# 
# formula_soil <- species_iv_matrix ~ acidity  + light_conditions +
#   nitrogen + soil_depth + water_content_spring + water_content_summer + water_content
# 
# # formula_DEM <- scale(species_abundance_matrix) ~ Aspect + Elevation + Slope
# 
# # Set a new seed for random numbers to ensure results are reproducible
# set.seed(42)
# 
# # run mrt classification
# # ==> selected 4 groups
# mrt_model_soil <- mvpart::mvpart(form = formula_soil,
#                                  data = environmental_data_df,
#                                  size = 4) # xv = "pick")
# 
# # mrt_model_DEM <- mvpart::mvpart(form = formula_DEM,
# #                                 data = environmental_data_df,
# #                                 xv = "pick")
# 
# raster_coords <- raster::xyFromCell(environmental_data_raster,
#                                     cell = cell_ids)
# 
# classification_df_soil <- tibble::tibble(x = raster_coords[,1],
#                                          y = raster_coords[, 2],
#                                          class = factor(mrt_model_soil$where))
# 
# class_ids <- sort(unique(classification_df_soil$class))
# 
# classification_df_soil <- dplyr::mutate(classification_df_soil,
#                                         class = dplyr::case_when(class == class_ids[1] ~ 1,
#                                                                  class == class_ids[2] ~ 2,
#                                                                  class == class_ids[3] ~ 3,
#                                                                  class == class_ids[4] ~ 4,
#                                                                  class == class_ids[5] ~ 5,
#                                                                  class == class_ids[6] ~ 6),
#                                         class = factor(class))
# 
# soil_mrt_classified <- raster::rasterFromXYZ(classification_df_soil)
# 
# # classification_df_DEM <- tibble::tibble(x = raster_coords[,1],
# #                                         y = raster_coords[, 2],
# #                                         class = factor(mrt_model_DEM$where))
# 
# 
# #### Add values on edge ####
# soil_mrt_classified <- landscapemetrics::pad_raster(soil_mrt_classified,
#                                                     pad_raster_value = NA)[[1]]
# 
# cells_na <- raster::Which(is.na(soil_mrt_classified),
#                           cells = TRUE)
# 
# neighbours <- raster::adjacent(x = soil_mrt_classified,
#                                cells = cells_na,
#                                directions = 8)
# 
# neighbours_value <- tibble::tibble(focal = neighbours[, 1],
#                                    neighbour = neighbours[, 2],
#                                    x = soil_mrt_classified[neighbours[, 2]]) %>%
#   dplyr::filter(!is.na(x)) %>%
#   dplyr::group_by(neighbour, focal) %>%
#   dplyr::summarise(x = as.numeric(names(which.max(table(x)))))
# 
# soil_mrt_classified[neighbours_value$focal] <- neighbours_value$x
# 
# UtilityFunctions::save_rds(object = soil_mrt_classified,
#                            path = paste0(getwd(), "/2_Real_world_data/3_Results"),
#                            filename = "soil_mrt_classified.rds", 
#                            overwrite = FALSE)
# 
#### Plot results ####
soil_mrt_classified <- readr::read_rds("2_Real_world_data/3_Results/soil_mrt_classified.rds")

plot_area <- readr::read_rds(paste0(getwd(),
                                    "/2_Real_world_data/1_Data/plot_area.rds"))

plot_area_matrix <- as.data.frame(raster::geom(plot_area))

soil_mrt_classified_df <- raster::as.data.frame(soil_mrt_classified, xy = TRUE) %>%
  dplyr::filter(!is.na(layer)) %>%
  dplyr::mutate(layer = as.factor(layer))

plot_classified <- ggplot2::ggplot(data = soil_mrt_classified_df, ggplot2::aes(x = x, y = y)) + 
  ggplot2::geom_raster(ggplot2::aes(fill = layer)) + 
  ggplot2::geom_polygon(data = plot_area_matrix, 
                        aes(x = plot_area_matrix[, 5], y = plot_area_matrix[, 6]), 
                        fill = NA, col = "black", size = 1) + 
  ggplot2::coord_equal() + 
  ggplot2::scale_fill_viridis_d(name = "Habitat") + 
  ggplot2::labs(x = "x coordinate", y = "y coordinate") +
  ggplot2::theme_bw(base_size = 15)

# ggplot2::ggplot(data = classification_df_DEM, ggplot2::aes(x = x, y = y)) +
#   ggplot2::geom_raster(ggplot2::aes(fill = class)) +
#   ggplot2::coord_equal() +
#   ggplot2::scale_fill_viridis_d() +
#   ggplot2::theme_bw()

UtilityFunctions::save_ggplot(plot = plot_classified, 
                              filename = "plot_classified.png", 
                              path = "2_Real_world_data/4_Figures", 
                              dpi = 300, width = 15, height = 15, units = "cm",
                              overwrite = FALSE)

#### Results classification

# number of cells in each habitat
table(soil_mrt_classified_df$layer)

sum(table(soil_mrt_classified_df$layer))

# area in ha
(table(soil_mrt_classified_df$layer) * prod(raster::res(soil_mrt_classified))) / 10000
(sum(table(soil_mrt_classified_df$layer)) * prod(raster::res(soil_mrt_classified))) / 10000


# trees in each habitat
raster::extract(x = soil_mrt_classified, 
                y = pattern_2007_df[, 1:2]) %>% 
  factor(levels = levels(soil_mrt_classified_df$layer)) %>%
  table()

# table(classification_df_DEM$class)
# length(table(classification_df_DEM$class))
# sum(table(classification_df_DEM$class))

