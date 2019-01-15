###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Classify habitats using MRT ####

#### Load packages ####
library(UtilityFunctions) # devtools::install_github("mhesselbarth/UtilityFunctions)
library(raster)
library(spatstat)
library(tidyverse)

#### Extract species abundance ####

# import data 
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# convert to data frame
pattern_2007_df <- spatstat::as.data.frame.ppp(pattern_2007)

# import environmental data as raster
environmental_data_raster <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), 
                                        pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    data <- readr::read_rds(x)
    raster::rasterFromXYZ(data)
  })

# get names of environmental data
environmental_data_names <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

environmental_data_names <- purrr::map_chr(environmental_data_names, 
                                           function(x) stringr::str_sub(x, 
                                                                        start = 3, 
                                                                        end = -5))

# set names
names(environmental_data_raster) <- environmental_data_names

# extract cell id of each tree location
pattern_2007_cell_id <- raster::extract(x = raster::stack(environmental_data_raster), 
                                        y = pattern_2007_df[, 1:2], 
                                        cellnumbers = TRUE, df = TRUE, along = TRUE)

# data frame including only  cell id, coords, and species
pattern_2007_cell_id <- cbind(pattern_2007_cell_id[, 2], 
                              pattern_2007_df[, c(1:2,10)])

# set names
names(pattern_2007_cell_id)[1] <- "cell_id"

# count species abundance in each cell
species_abundance <- table(factor(pattern_2007_cell_id$cell_id, 
                                  levels = seq_len(1155)),
                           pattern_2007_cell_id$Species) %>%
  as.data.frame() %>%
  purrr::set_names(c("cell_id", "species", "abundance")) %>%
  tidyr::spread(key = species, value = abundance)

#### Environmental data ####

# import data 
environmental_data_df <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), 
                                    pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) {
    data <- readr::read_rds(x)
    data[, 3]
  })

# combine to one dataframe
environmental_data_df <- dplyr::bind_cols(environmental_data_df)

# get names of environmental data
environmental_data_names <- list.files(paste0(getwd(), "/2_Real_world_data/1_Data"), pattern = '3_')

environmental_data_names <- purrr::map_chr(environmental_data_names, 
                                           function(x) stringr::str_sub(x, 
                                                                        start = 3, 
                                                                        end = -5))

# set names
names(environmental_data_df) <- environmental_data_names

#### MRT classification ####




total_df <- cbind(species_abundance, environmental_data)
