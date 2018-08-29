#' Create simulation species
#'
#' Algorithm to create simulation species with choosen characteristics
#' 
#' @param habitats_poly [\code{sf(1)}]\cr sf object with habitats
#' @param type [\code{string(1)}]\cr 'positive' or 'negative' associations
#' @param process [\code{string(1)}]\cr Process type to chose. Either 'Poisson' or 'Thomas'
#' @param habitat [\code{numeric(1)}]\cr Habitat to which species is associated
#' @param number_points [\code{numeric(1)}]\cr Number of points for each species (association_strength = 0)
#' @param association_strength [\code{numeric(1)}]\cr Strength of species-habitat association
#' @param species_code [\code{numeric{1}}]\cr Species code to number species
#' @param verbose [\code{logical(1)}]\cr Print advanced error message
#'
#' @return ppp object of the spatstat package with simulated species

#' @export
create_simulation_species <- function(habitats_poly, owin_overall,  type, process, habitat, number_points = 100, association_strength = 0.3,
                                      species_code = 0, verbose = TRUE){
  
  scale <- mean(diff(owin_overall$yrange), diff(owin_overall$xrange)) / 25
  
  if(type=="positive"){
    
    if(process=="Poisson"){
      
      owin_pattern <- maptools::as.owin.SpatialPolygons(habitats_poly[habitats_poly$layer == habitat,])
      
      # owin_pattern <- habitats_poly %>% 
      #   dplyr::filter(layer == habitat) %>%
      #   # sf::as_Spatial() %>%
      #   as("Spatial") %>%
      #   maptools::as.owin.SpatialPolygons()
      
      pattern_a <- mobsim::sim_poisson_community(s_pool = 1, 
                                                 n_sim = number_points, 
                                                 xrange = owin_overall$xrange,
                                                 yrange = owin_overall$yrange)
      
      pattern_a <- spatstat::ppp(x = pattern_a$census$x,
                                 y = pattern_a$census$y,
                                 window = owin_overall)
      
      pattern_b <- spatstat::runifpoint(n = floor(pattern_a$n * association_strength), win = owin_pattern)
      
      pattern <- spatstat::superimpose.ppp(pattern_a, pattern_b, W = owin_overall)
      
      marks_pattern <- data.frame(Species = rep(paste0("Poisson_positive_", habitat), pattern$n),
                                  Species_code = species_code,
                                  Habitat = rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else if(process=="Thomas"){
      
      # owin_pattern <- habitats_poly %>% 
      #   dplyr::filter(layer == habitat) %>%
      #   sf::as_Spatial() %>%
      #   maptools::as.owin.SpatialPolygons() 
      
      owin_pattern <- maptools::as.owin.SpatialPolygons(habitats_poly[habitats_poly$layer == habitat,])
      
      pattern_a <- mobsim::sim_thomas_community(s_pool = 1, 
                                                n_sim = number_points, 
                                                sigma = scale, 
                                                cluster_points = 5, 
                                                xrange = owin_overall$xrange, 
                                                yrange = owin_overall$yrange)
      
      pattern_a <- spatstat::ppp(x = pattern_a$census$x,
                                 y = pattern_a$census$y,
                                 window = owin_overall)
      
      pattern_b <- spatstat::runifpoint(n = floor(pattern_a$n * association_strength), win = owin_pattern)
      
      pattern <- spatstat::superimpose(pattern_a, pattern_b, W = owin_overall)
      
      marks_pattern <- data.frame(Species = rep(paste0("Thomas_positive_", habitat), pattern$n),
                                  Species_code = species_code,
                                  Habitat = rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else{
      
      if(verbose == TRUE){print("Please select either 'Poisson', 'Thomas' as process")}
      pattern <- NULL
    }
  }
  
  else if (type=="negative"){
    
    if(process=="Poisson"){
      
      # owin_pattern <- habitats_poly %>% 
      #   dplyr::filter(layer == habitat) %>%
      #   sf::as_Spatial() %>%
      #   maptools::as.owin.SpatialPolygons()
      
      owin_pattern <- maptools::as.owin.SpatialPolygons(habitats_poly[habitats_poly$layer == habitat,])
      
      pattern_a <- mobsim::sim_poisson_community(s_pool = 1, 
                                                 n_sim = number_points, 
                                                 xrange = owin_overall$xrange,
                                                 yrange = owin_overall$yrange)
      
      pattern_a <- spatstat::ppp(x = pattern_a$census$x,
                                 y = pattern_a$census$y,
                                 window = owin_overall)
      
      pattern_b <- pattern_a[!spatstat::inside.owin(x = pattern_a, w = owin_pattern)]
      pattern_c <- spatstat::rthin(pattern_a[spatstat::inside.owin(x = pattern_a, w = owin_pattern)], 1 - association_strength)
      
      pattern <- spatstat::superimpose(pattern_b, pattern_c, W = owin_overall)
      
      marks_pattern <- data.frame(Species = rep(paste0("Poisson_negative_", habitat), pattern$n),
                                  Species_code = species_code,
                                  Habitat = rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else if(process=="Thomas"){
      
      # owin_pattern <- habitats_poly %>% 
      #   dplyr::filter(layer == habitat) %>%
      #   sf::as_Spatial() %>%
      #   maptools::as.owin.SpatialPolygons()
      
      owin_pattern <- maptools::as.owin.SpatialPolygons(habitats_poly[habitats_poly$layer == habitat,])
      
      pattern_a <- mobsim::sim_thomas_community(s_pool = 1, 
                                                n_sim = number_points, 
                                                sigma = scale, 
                                                cluster_points = 5, 
                                                xrange = owin_overall$xrange, 
                                                yrange = owin_overall$yrange)
      
      pattern_a <- spatstat::ppp(x = pattern_a$census$x,
                                 y = pattern_a$census$y,
                                 window = owin_overall)
      
      pattern_b <- pattern_a[!spatstat::inside.owin(x = pattern_a, w = owin_pattern)]
      pattern_c <- spatstat::rthin(pattern_a[spatstat::inside.owin(x = pattern_a, w = owin_pattern)], 1 - association_strength)
      
      pattern <- spatstat::superimpose(pattern_b, pattern_c, W = owin_overall)
      
      marks_pattern <- data.frame(Species = rep(paste0("Thomas_negative_", habitat), pattern$n),
                                  Species_code = species_code,
                                  Habitat = rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else{
      
      if(verbose == TRUE){print("Please select either 'Poisson', 'Thomas' or as process")}
      pattern <- NULL
    }
  }
  
  else if(type == "neutral"){
    
    if(process == "Poisson"){
      
      pattern <- spatstat::runifpoint(n = number_points, win = owin_overall)
      marks_pattern <- data.frame(Species = rep("Poisson_neutral", pattern$n),
                                  Species_code = species_code)
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else if(process == "Thomas"){
      
      pattern <- spatstat::rThomas(kappa = lambda/5, scale = scale, mu = 5, win = owin_overall)
      marks_pattern <- data.frame(Species = rep("Thomas_neutral", pattern$n),
                                  Species_code = species_code)
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else{
      
      if(verbose == TRUE){print("Please select either 'Poisson', 'Thomas' or 'Complex' as process")}
      pattern <- NULL
    }
  }
  
  else{
    
    if(verbose == TRUE){print("Please select either 'positive', 'negative' or 'neutral' as type")}
    pattern <- NULL
  }
  
  return(pattern)
}