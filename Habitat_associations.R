#### Habitat associations - Real word data set #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### Install packages ####
toc <- "cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec"

devtools::install_github("mhesselbarth/SHAR", auth_token=toc, quiet=T)
devtools::install_github("mhesselbarth/UtilityFunctions", auth_token=toc, quiet=T)

#### Load packages and functions ####

library(ggplot2)
library(UtilityFunctions)
library(raster)
library(SHAR)
library(spatstat)

data <- paste0(getwd(), "/Data")

#### Functions ####

foo <- function(pattern, species, raster, number_reconstructions, max_runs, fitting, parallel){
  
  if(class(raster)!="list" || is.null(names(raster))){
    print("Please provide named list - No associations analysed")
    associations <- NULL
  }
  
  else{
    reconstructed_pattern <- Pattern.Reconstruction(pattern=spatstat::subset.ppp(pattern,Species==species), 
                                                  number_reconstructions=number_reconstructions, 
                                                  max_runs = max_runs, fitting=fitting, parallel=parallel)
  

    associations <- list()
    for(i in 1:length(raster)){
      associations[[i]] <- Results.Habitat.Association(pattern=reconstructed_pattern, raster=raster[[i]], 
                                                     method="random_pattern", only_spatial=T)
      names(associations)[[i]] <- names(raster)[[i]]
    }
  }
  
  return(associations)
}

### Load data ####

# Point pattern #
pattern_1999 <- readRDS(paste0(data, "/pattern_1999.rds"))
pattern_2007 <- readRDS(paste0(data, "/pattern_2007.rds"))

pattern_1999_living <- subset(pattern_1999, Type!="dead")
pattern_2007_living <- subset(pattern_2007, Type!="dead")

pattern_2007_small <- spatstat::subset.ppp(pattern_2007_living, DBH_group=="small")
pattern_2007_medium <- spatstat::subset.ppp(pattern_2007_living, DBH_group=="medium")
pattern_2007_large <-  spatstat::subset.ppp(pattern_2007_living, DBH_group=="large")

# Environmental data
acidity <- readRDS(paste0(data, "/acidity.rds"))
available_water_content <- readRDS(paste0(data, "/available_water_content.rds"))
continentality <- readRDS(paste0(data, "/continentality.rds"))
light_conditions <- readRDS(paste0(data, "/light_conditions.rds"))
nitrogen <- readRDS(paste0(data, "/nitrogen.rds"))
soil_depth <- readRDS(paste0(data, "/soil_depth.rds"))
water_content_spring <- readRDS(paste0(data, "/water_content_spring.rds"))
water_content_summer <- readRDS(paste0(data, "/water_content_summer.rds"))

DEM <- readRDS(paste0(data, "/DEM.rds"))

# Classify into habitats
raster_acidity <- Habitat.Classification(raster=rasterFromXYZ(acidity))
raster_available_water_content <- Habitat.Classification(raster=rasterFromXYZ(available_water_content))
raster_continentality <- Habitat.Classification(raster=rasterFromXYZ(continentality))
raster_light_conditions <- Habitat.Classification(raster=rasterFromXYZ(light_conditions))
raster_nitrogen <- Habitat.Classification(raster=rasterFromXYZ(nitrogen))
raster_soil_depth <- Habitat.Classification(raster=rasterFromXYZ(soil_depth))
raster_water_content_spring <- Habitat.Classification(raster=rasterFromXYZ(water_content_spring))
raster_water_content_summer <- Habitat.Classification(raster=rasterFromXYZ(water_content_summer))

raster_list <- list(raster_acidity=raster_acidity, 
                    raster_available_water_content=raster_available_water_content, 
                    raster_continentality=raster_continentality, 
                    raster_light_conditions=raster_light_conditions, 
                    raster_nitrogen=raster_nitrogen, 
                    raster_soil_depth=raster_soil_depth,
                    raster_water_content_spring=raster_water_content_spring,
                    raster_water_content_summer=raster_water_content_summer)

raster_aspect <- Habitat.Classification(raster=rasterFromXYZ(DEM$Aspect))
raster_slope <- Habitat.Classification(raster=rasterFromXYZ(DEM$Slope))
raster_elevation <- Habitat.Classification(raster=rasterFromXYZ(DEM$Elevation))


#### Pattern reconstruction ####
# Set parameters #
number_reconstructions <- 19
max_runs <- 10
fitting <- T
parallel <- T

habitat_associations_ash <- foo(pattern=pattern_2007, species="Ash", raster=raster_list,
                                number_reconstructions=number_reconstructions, max_runs=max_runs,
                                fitting=fitting, parallel=parallel)



#### ENDE ####
