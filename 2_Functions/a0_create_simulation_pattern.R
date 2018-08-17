#' Create simulation pattern
#'
#' Algorithm to create simulation pattern with 6 species with different species-habitat associations.
#' 
#' \cr Species 1: Positive associations (Poisson process)
#' \cr Species 2: Positive associations (Thomas process)
#' \cr Species 3: Negative associations (Poisson process)
#' \cr Species 4: Negative associations (Thomas process)
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package with habitats
#' @param number_points [\code{numeric(1)}]\cr Number of points for each species (association_strength=0)
#' @param association_strength [\code{numeric(1)}]\cr Strength of species-habitat association
#' @return ppp object of the spatstat package with simulated species

#' @export
create_simulation_pattern <- function(raster, number_points = 100, association_strength = 0.3){
  
  owin_raster <- raster %>%
    raster::rasterToPolygons(fun = function(x) !is.na(x), na.rm = TRUE, dissolve = TRUE) %>%
    maptools::unionSpatialPolygons(ID = rep(1, times = length(.))) %>%
    maptools::as.owin.SpatialPolygons()
  
  # Species 1: Positive associations (Poisson)
  habitat_1 <- sample(x = seq(min(raster::values(raster)):max(raster::values(raster))), size = 1)
  species_1 <- create_simulation_species(raster = raster, type = "positive", process = "Poisson",
                                         habitat = habitat_1, number_points = number_points, association_strength = association_strength,
                                         species_code = 1, verbose = FALSE)
  
  # Species 2: Positive associations (Thomas process)
  habitat_2 <- sample(x=seq(min(raster::values(raster)):max(raster::values(raster))), size=1)
  species_2 <- create_simulation_species(raster = raster, type = "positive", process = "Thomas",
                                         habitat = habitat_2, number_points = number_points, association_strength = association_strength,
                                         species_code = 2, verbose = FALSE)
  
  # Species 3: Negative associations (Poisson)
  habitat_3 <- sample(x = seq(min(raster::values(raster)):max(raster::values(raster))), size = 1)
  species_3 <- create_simulation_species(raster = raster, type = "negative", process = "Poisson",
                                         habitat = habitat_3, number_points = number_points, association_strength = association_strength,
                                         species_code = 3, verbose = FALSE)
  
  # Species 4: Negative associations habitat 4 (Thomas process)
  habitat_4 <- sample(x = seq(min(raster::values(raster)):max(raster::values(raster))), size = 1)
  species_4 <- create_simulation_species(raster = raster, type = "negative", process = "Thomas",
                                         habitat = habitat_4, number_points = number_points, association_strength = association_strength,
                                         species_code = 4, verbose = FALSE)
  
  simulation_pattern <- spatstat::superimpose(species_1, species_2,
                                              species_3, species_4,
                                              W = owin_raster)
  
  return(simulation_pattern)
}