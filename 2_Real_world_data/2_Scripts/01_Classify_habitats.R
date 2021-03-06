###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Classify habitats using MRT ####

#### Load packages ####
library(mvpart) # devtools::install_github("mvignon/mvpart")
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(raster)
library(rgeos)
library(rpart)
library(spatstat)
library(tidyverse)

#### Extract species abundance ####

# import data
pattern_2007 <- readr::read_rds("2_Real_world_data/1_Data/02_pattern_2007_ppp.rds")

# convert to data frame

# all living trees
pattern_2007_df <- tibble::as_tibble(pattern_2007) %>%
  dplyr::filter(type == "living")

# # only small trees (beech only)
# pattern_2007_df_small <-  tibble::as_tibble(pattern_2007) %>%
#   dplyr::filter(type == "living", dbh_group_07 == "small", species == "beech")
# 
# # only medium trees (beech only)
# pattern_2007_df_medium <- spatstat::as.data.frame.ppp(pattern_2007) %>%
#   dplyr::filter(type == "living", dbh_group_07 == "medium", species == "beech")
# 
# # only large trees (beech only)
# pattern_2007_df_large <- spatstat::as.data.frame.ppp(pattern_2007) %>%
#   dplyr::filter(type == "living", dbh_group_07 == "large", species == "beech")
# 
# # # only living trees (beech only)
# pattern_2007_df_living <- spatstat::as.data.frame.ppp(pattern_2007) %>%
#   dplyr::filter(type == "living", species == "beech")
# 
# # # only dead trees (beech only)
# pattern_2007_df_dead <- spatstat::as.data.frame.ppp(pattern_2007) %>%
#   dplyr::filter(type == "dead", species == "beech")

# pattern_2007_df$ID_new <- 1:nrow(pattern_2007_df)

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

environmental_data_names <- purrr::map_chr(environmental_data_names,
                                           function(x) stringr::str_sub(x,
                                                                        start = 4,
                                                                        end = -5))

# set names
names(environmental_data_raster) <- environmental_data_names

# stack list
environmental_data_raster <- raster::stack(environmental_data_raster)

# import DEM
# environmental_data_DEM <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/4_DEM.rds")) %>%
#   raster::crop(environmental_data_raster) %>%
#   raster::setExtent(ext = raster::extent(environmental_data_raster)) %>%
#   raster::mask(mask = rgeos::gBuffer(plot_area, width = 10))

# stack all layers to one RasterStack
# environmental_data_raster <- raster::stack(environmental_data_raster,
#                                            environmental_data_DEM)

# only data that is inside plot
# environmental_data_raster <- raster::intersect(x = environmental_data_raster,
#                                                y = plot_area)

# convert to data frame
environmental_data_df <- raster::as.data.frame(environmental_data_raster)

# only non-NA environmental data
environmental_data_df <- environmental_data_df[complete.cases(environmental_data_df), ]

# cell IDs of all non-NA cells
environmental_data_df$cells <- as.numeric(row.names(environmental_data_df))

# scale data
# environmental_data_df <- as.data.frame(apply(environmental_data_df, MARGIN = 2, FUN = scale))

#### All trees ####

# extract cell id of each tree location
pattern_2007_cell_id <- raster::extract(x = environmental_data_raster,
                                        y = pattern_2007_df[, 1:2],
                                        cellnumbers = TRUE, df = TRUE, along = TRUE)

# combine data to get cell id and species information
pattern_2007_cell_id <- cbind(pattern_2007_cell_id,
                              pattern_2007_df)

# only non NA data
pattern_2007_cell_id <- pattern_2007_cell_id[complete.cases(pattern_2007_cell_id), ]

# data frame including only  cell id, DHB and species
pattern_2007_cell_id <- pattern_2007_cell_id[, c(2, 14, 16)]

# Calculate importance value for each species in each cell
species_iv <- dplyr::mutate(pattern_2007_cell_id,
                            basal_area = (pi / 4) * dbh_07 ^ 2) %>%
  dplyr::group_by(cells, species) %>%
  dplyr::summarise(n = n(),
                   ba_sum = sum(basal_area)) %>%
  dplyr::mutate(n_rel = n / sum(n),
                ba_rel = ba_sum / sum(ba_sum),
                importance_value = n_rel + ba_rel) %>%
  dplyr::select(cells, species, importance_value) %>%
  tidyr::spread(key = species, value = importance_value,
                fill = 0) %>%
  dplyr::arrange(cells) %>%
  dplyr::mutate(Beech = tidyr::replace_na(beech, 0),
                Ash = tidyr::replace_na(ash, 0),
                Hornbeam = tidyr::replace_na(hornbeam, 0),
                Sycamore = tidyr::replace_na(sycamore, 0),
                others = tidyr::replace_na(others, 0)) %>%
  dplyr::left_join(x = environmental_data_df, 
                   y = ., 
                   by = "cells")

#### Small trees ####
# 
# # extract cell id of each tree location
# pattern_2007_cell_id_small <- raster::extract(x = environmental_data_raster,
#                                               y = pattern_2007_df_small[, 1:2],
#                                               cellnumbers = TRUE, df = TRUE, along = TRUE)
# 
# # combine data to get cell id and species information
# pattern_2007_cell_id_small <- cbind(pattern_2007_cell_id_small,
#                                     pattern_2007_df_small)
# 
# # only non NA data
# pattern_2007_cell_id_small <- pattern_2007_cell_id_small[complete.cases(pattern_2007_cell_id_small), ]
# 
# # data frame including only  cell id, and DBH
# pattern_2007_cell_id_small <- pattern_2007_cell_id_small[, c(2, 16)]
# 
# # Calculate importance value for each species in each cell
# species_iv_small <- dplyr::mutate(pattern_2007_cell_id_small, 
#                             basal_area = (pi / 4) * dbh_07 ^ 2) %>%
#   dplyr::group_by(cells) %>%
#   dplyr::summarise(n = n(),
#                    ba_sum = sum(basal_area)) %>%
#   dplyr::mutate(n_rel = n / sum(n),
#                 ba_rel = ba_sum / sum(ba_sum),
#                 importance_value = n_rel + ba_rel) %>%
#   dplyr::select(cells, importance_value) %>%
#   dplyr::arrange(cells) %>% 
#   dplyr::left_join(x = environmental_data_df, 
#                    y = ., 
#                    by = "cells") %>% 
#   dplyr::mutate(importance_value = tidyr::replace_na(importance_value, replace = 0))
# 
#### Medium trees ####
# 
# # extract cell id of each tree location
# pattern_2007_cell_id_medium <- raster::extract(x = environmental_data_raster,
#                                               y = pattern_2007_df_medium[, 1:2],
#                                               cellnumbers = TRUE, df = TRUE, along = TRUE)
# 
# # combine data to get cell id and species information
# pattern_2007_cell_id_medium <- cbind(pattern_2007_cell_id_medium,
#                                     pattern_2007_df_medium)
# 
# # only non NA data
# pattern_2007_cell_id_medium <- pattern_2007_cell_id_medium[complete.cases(pattern_2007_cell_id_medium), ]
# 
# # data frame including only  cell id, and DBH
# pattern_2007_cell_id_medium <- pattern_2007_cell_id_medium[, c(2, 16)]
# 
# # Calculate importance value for each species in each cell
# species_iv_medium <- dplyr::mutate(pattern_2007_cell_id_medium, 
#                                   basal_area = (pi / 4) * dbh_07 ^ 2) %>%
#   dplyr::group_by(cells) %>%
#   dplyr::summarise(n = n(),
#                    ba_sum = sum(basal_area)) %>%
#   dplyr::mutate(n_rel = n / sum(n),
#                 ba_rel = ba_sum / sum(ba_sum),
#                 importance_value = n_rel + ba_rel) %>%
#   dplyr::select(cells, importance_value) %>%
#   dplyr::arrange(cells) %>%
#   dplyr::left_join(x = environmental_data_df, 
#                    y = ., 
#                    by = "cells") %>% 
#   dplyr::mutate(importance_value = tidyr::replace_na(importance_value, replace = 0))
# 
#### Large trees ####
# 
# # extract cell id of each tree location
# pattern_2007_cell_id_large <- raster::extract(x = environmental_data_raster,
#                                                y = pattern_2007_df_large[, 1:2],
#                                                cellnumbers = TRUE, df = TRUE, along = TRUE)
# 
# # combine data to get cell id and species information
# pattern_2007_cell_id_large <- cbind(pattern_2007_cell_id_large,
#                                      pattern_2007_df_large)
# 
# # only non NA data
# pattern_2007_cell_id_large <- pattern_2007_cell_id_large[complete.cases(pattern_2007_cell_id_large), ]
# 
# # data frame including only  cell id, and DBH
# pattern_2007_cell_id_large <- pattern_2007_cell_id_large[, c(2, 16)]
# 
# # Calculate importance value for each species in each cell
# species_iv_large <- dplyr::mutate(pattern_2007_cell_id_large, 
#                                    basal_area = (pi / 4) * dbh_07 ^ 2) %>%
#   dplyr::group_by(cells) %>%
#   dplyr::summarise(n = n(),
#                    ba_sum = sum(basal_area)) %>%
#   dplyr::mutate(n_rel = n / sum(n),
#                 ba_rel = ba_sum / sum(ba_sum),
#                 importance_value = n_rel + ba_rel) %>%
#   dplyr::select(cells, importance_value) %>%
#   dplyr::arrange(cells) %>% 
#   dplyr::left_join(x = environmental_data_df, 
#                    y = ., 
#                    by = "cells") %>% 
#   dplyr::mutate(importance_value = tidyr::replace_na(importance_value, replace = 0))
# 
#### Living trees (beech only)
# 
# # extract cell id of each tree location
# pattern_2007_cell_id_living <- raster::extract(x = environmental_data_raster,
#                                                y = pattern_2007_df_living[, 1:2],
#                                                cellnumbers = TRUE, df = TRUE, along = TRUE)
# 
# # combine data to get cell id and species information
# pattern_2007_cell_id_living <- cbind(pattern_2007_cell_id_living,
#                                     pattern_2007_df_living)
# 
# # only non NA data
# pattern_2007_cell_id_living <- pattern_2007_cell_id_living[complete.cases(pattern_2007_cell_id_living), ]
# 
# # data frame including only  cell id, and DBH
# pattern_2007_cell_id_living <- pattern_2007_cell_id_living[, c(2, 16)]
# 
# # Calculate importance value for each species in each cell
# species_iv_living <- dplyr::mutate(pattern_2007_cell_id_living, 
#                                   basal_area = (pi / 4) * dbh_07 ^ 2) %>%
#   dplyr::group_by(cells) %>%
#   dplyr::summarise(n = n(),
#                    ba_sum = sum(basal_area)) %>%
#   dplyr::mutate(n_rel = n / sum(n),
#                 ba_rel = ba_sum / sum(ba_sum),
#                 importance_value = n_rel + ba_rel) %>%
#   dplyr::select(cells, importance_value) %>%
#   dplyr::arrange(cells) %>%
#   dplyr::left_join(x = environmental_data_df, 
#                    y = ., 
#                    by = "cells") %>% 
#   dplyr::mutate(importance_value = tidyr::replace_na(importance_value, replace = 0))
# 
#### Dead trees ####
# 
# # extract cell id of each tree location
# pattern_2007_cell_id_dead <- raster::extract(x = environmental_data_raster,
#                                                y = pattern_2007_df_dead[, 1:2],
#                                                cellnumbers = TRUE, df = TRUE, along = TRUE)
# 
# # combine data to get cell id and species information
# pattern_2007_cell_id_dead <- cbind(pattern_2007_cell_id_dead,
#                                      pattern_2007_df_dead)
# 
# # only non NA data
# pattern_2007_cell_id_dead <- pattern_2007_cell_id_dead[!is.na(pattern_2007_cell_id_dead$dbh_group_07), ]
# 
# # data frame including only  cell id, and DBH
# pattern_2007_cell_id_dead <- pattern_2007_cell_id_dead[, c(2, 16)]
# 
# # Calculate importance value for each species in each cell
# species_iv_dead <- dplyr::mutate(pattern_2007_cell_id_dead, 
#                                    basal_area = (pi / 4) * dbh_07 ^ 2) %>%
#   dplyr::group_by(cells) %>%
#   dplyr::summarise(n = n(),
#                    ba_sum = sum(basal_area)) %>%
#   dplyr::mutate(n_rel = n / sum(n),
#                 ba_rel = ba_sum / sum(ba_sum),
#                 importance_value = n_rel + ba_rel) %>%
#   dplyr::select(cells, importance_value) %>%
#   dplyr::arrange(cells) %>% 
#   dplyr::left_join(x = environmental_data_df, 
#                    y = ., 
#                    by = "cells") %>% 
#   dplyr::mutate(importance_value = tidyr::replace_na(importance_value, replace = 0))
# 
#### MRT classification ####

# Set a new seed for random numbers to ensure results are reproducible
set.seed(42)

raster_coords <- raster::xyFromCell(environmental_data_raster,
                                    cell = environmental_data_df$cells)

# convert to matrix without cell id
species_iv <- data.matrix(species_iv[, -(1:9)])

# species_iv_size <- matrix(c(species_iv_small$importance_value, 
#                             species_iv_medium$importance_value,
#                             species_iv_large$importance_value), ncol = 3)

# species_iv_status <- matrix(c(species_iv_living$importance_value, 
#                               species_iv_dead$importance_value), ncol = 2)

# Run MRT models
mrt_model_species <- mvpart::mvpart(form = species_iv ~ acidity + light_conditions + 
                                      nitrogen + soil_depth + water_content_spring + 
                                      water_content_summer + water_content,
                                    data = environmental_data_df,
                                    size = 4, 
                                    xvmult = 1000, 
                                    xval = 100)

suppoRt::save_rds(object = mrt_model_species, 
                  filename = "mrt_model_species.rds", 
                  path = "2_Real_world_data/3_Results/")

# mrt_model_size <- mvpart::mvpart(form = species_iv_size ~ acidity + light_conditions + 
#                                    nitrogen + soil_depth + water_content_spring + 
#                                    water_content_summer + water_content,
#                                  data = environmental_data_df,
#                                  size = 4,
#                                  xvmult = 1000, 
#                                  xval = 100)
# 
# mrt_model_status <- mvpart::mvpart(form = species_iv_status ~ acidity + light_conditions + 
#                                      nitrogen + soil_depth + water_content_spring + 
#                                      water_content_summer + water_content,
#                                  data = environmental_data_df,
#                                  size = 4, 
#                                  xvmult = 1000, 
#                                  xval = 100)

# mrt_model_list <- list(mrt_model_species, mrt_model_size, mrt_model_status)

classification_raster_list <- purrr::map(list(mrt_model_species),  function(current_mrt) {
  
  classification_df <- tibble::tibble(x = raster_coords[, 1],
                                      y = raster_coords[, 2],
                                      class = current_mrt[[2]])
  
  present_classes <- sort(unique(classification_df$class))
  
  classification_df <- dplyr::mutate(classification_df, 
                                     class = dplyr::case_when(class == present_classes[[1]] ~ 1, 
                                                              class == present_classes[[2]] ~ 2, 
                                                              class == present_classes[[3]] ~ 3, 
                                                              class == present_classes[[4]] ~ 4))
  
  
  classification_raster <- raster::rasterFromXYZ(classification_df)
  
  classification_raster <- landscapemetrics::pad_raster(classification_raster,
                                                        pad_raster_value = NA)[[1]]
  
  cells_na <- raster::Which(is.na(classification_raster),
                            cells = TRUE)
  
  neighbours <- raster::adjacent(x = classification_raster,
                                 cells = cells_na,
                                 directions = 8)
  
  neighbours_value <- tibble::tibble(focal = neighbours[, 1],
                                     neighbour = neighbours[, 2],
                                     x = classification_raster[neighbours[, 2]]) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::group_by(neighbour, focal) %>%
    dplyr::summarise(x = as.numeric(names(which.max(table(x)))))
  
  classification_raster[neighbours_value$focal] <- neighbours_value$x
  
  return(classification_raster)
})

# names(classification_raster_list) <- c("species", "size", "status")
names(classification_raster_list) <- "species"

landscapetools::show_landscape(raster::stack(classification_raster_list))

suppoRt::save_rds(object = classification_raster_list,
                 path = "2_Real_world_data/3_Results",
                 filename = "classification_raster_list.rds")
