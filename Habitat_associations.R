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

rm(toc)

#### Load packages and functions ####

library(ggplot2)
library(UtilityFunctions)
library(raster)
library(SHAR)
library(spatstat)

data <- paste0(getwd(), "/Data")
results <- paste0(getwd(), "/Results")

set.seed(42)

#### Functions ####

Function.Habitat.Associations <- function(pattern, species=NULL, raster, 
                                          number_reconstructions=1, max_runs=10000, 
                                          fitting=F, parallel=F){
  
  if(class(raster)!="list" || is.null(names(raster))){
    print("Please provide named list - No associations analysed")
    associations <- NULL
  }
  
  else{
    if(is.null(species)){
      print("Starting pattern reconstruction - No species provided")
      reconstructed_pattern <- Pattern.Reconstruction(pattern=pattern, 
                                                      number_reconstructions=number_reconstructions, 
                                                      max_runs=max_runs, fitting=fitting, parallel=parallel)
      
    }
    else{
      print(paste0("Starting pattern reconstruction - Species: ", species))
      reconstructed_pattern <- Pattern.Reconstruction(pattern=spatstat::subset.ppp(pattern,Species==species), 
                                                      number_reconstructions=number_reconstructions, 
                                                      max_runs=max_runs, fitting=fitting, parallel=parallel)
    }
    
    print("Starting analysing habitat associations")
    pb <- utils::txtProgressBar(max=length(raster), style=3)
    associations <- list()
    for(i in 1:length(raster)){
      associations[[i]] <- Results.Habitat.Association(pattern=reconstructed_pattern, raster=raster[[i]], 
                                                       method="random_pattern", only_spatial=T)
      names(associations)[[i]] <- names(raster)[[i]]
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  result_list <- list(Associations=associations, Pattern=reconstructed_pattern)
  return(result_list)
}

### Load data ####

# Point pattern #
pattern_2007 <- readRDS(paste0(data, "/pattern_2007.rds"))

pattern_2007_living <- subset(pattern_2007, Type!="dead")
pattern_2007_dead <- subset(pattern_2007, Type=="dead")

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

raster_aspect <- Habitat.Classification(DEM$Aspect)
raster_slope <- Habitat.Classification(DEM$Slope)
raster_elevation <- Habitat.Classification(DEM$Elevation)

raster_list <- list(raster_acidity=raster_acidity, 
                    raster_available_water_content=raster_available_water_content, 
                    raster_continentality=raster_continentality, 
                    raster_light_conditions=raster_light_conditions, 
                    raster_nitrogen=raster_nitrogen, 
                    raster_soil_depth=raster_soil_depth,
                    raster_water_content_spring=raster_water_content_spring,
                    raster_water_content_summer=raster_water_content_summer,
                    raster_aspect=raster_aspect, 
                    raster_slope=raster_slope,
                    raster_elevation=raster_elevation)



#### Pattern reconstruction ####
# Set parameters #
number_reconstructions <- 19
max_runs <- 100
fitting <- T
parallel <- T

# Species #

habitat_associations_ash <- Function.Habitat.Associations(pattern=pattern_2007, species="Ash", raster=raster_list,
                                                          number_reconstructions=number_reconstructions, max_runs=max_runs,
                                                          fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_ash, file=paste0(results, "/habitat_associations_ash.rds"))

habitat_associations_beech <- Function.Habitat.Associations(pattern=pattern_2007, species="Beech", raster=raster_list,
                                                            number_reconstructions=number_reconstructions, max_runs=max_runs,
                                                            fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_beech, file=paste0(results, "/habitat_associations_beech.rds"))


habitat_associations_hornbeam <- Function.Habitat.Associations(pattern=pattern_2007, species="Hornbeam", raster=raster_list,
                                                               number_reconstructions=number_reconstructions, max_runs=max_runs,
                                                               fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_hornbeam, file=paste0(results, "/habitat_associations_hornbeam.rds"))


habitat_associations_others <- Function.Habitat.Associations(pattern=pattern_2007, species="others", raster=raster_list,
                                                             number_reconstructions=number_reconstructions, max_runs=max_runs,
                                                             fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_others, file=paste0(results, "/habitat_associations_others.rds"))


habitat_associations_sycamore <- Function.Habitat.Associations(pattern=pattern_2007, species="Sycamore", raster=raster_list,
                                                               number_reconstructions=number_reconstructions, max_runs=max_runs,
                                                               fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_sycamore, file=paste0(results, "/habitat_associations_sycamore.rds"))


# Height classes #
habitat_associations_beech_small <- Function.Habitat.Associations(pattern=pattern_2007_small, species="Beech", 
                                                                  raster=raster_list,
                                                                  number_reconstructions=number_reconstructions,
                                                                  max_runs=max_runs,
                                                                  fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_beech_small, file=paste0(results, "/habitat_associations_beech_small.rds"))


habitat_associations_beech_medium <- Function.Habitat.Associations(pattern=pattern_2007_medium, species="Beech", 
                                                                   raster=raster_list,
                                                                   number_reconstructions=number_reconstructions,
                                                                   max_runs=max_runs,
                                                                   fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_beech_medium, file=paste0(results, "/habitat_associations_beech_medium.rds"))


habitat_associations_beech_large <- Function.Habitat.Associations(pattern=pattern_2007_large, species="Beech", 
                                                                  raster=raster_list,
                                                                  number_reconstructions=number_reconstructions,
                                                                  max_runs=max_runs,
                                                                  fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_beech_large, file=paste0(results, "/habitat_associations_beech_large.rds"))

# Dead #
habitat_associations_beech_dead <- Function.Habitat.Associations(pattern=pattern_2007_dead, species="Beech", 
                                                                  raster=raster_list,
                                                                  number_reconstructions=number_reconstructions,
                                                                  max_runs=max_runs,
                                                                  fitting=fitting, parallel=parallel)
Save.Function.rds(object=habitat_associations_beech_dead, file=paste0(results, "/habitat_associations_beech_dead.rds"))


#### ENDE ####
