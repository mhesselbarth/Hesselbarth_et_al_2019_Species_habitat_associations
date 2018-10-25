#' create_simulation_species
#'
#' @details 
#' Algorithm to create simulation species with choosen characteristics
#' 
#' @param habitats_poly sf object with habitats
#' @param habitat Habitat to which species is associated
#' @param owin_overall owin object with whole observation window
#' @param type 'positive' or 'negative' associations
#' @param process Process type to chose. Either 'Poisson' or 'Thomas'
#' @param association_strength Strength of species-habitat association
#' @param number_points Number of points for each species (association_strength = 0)
#' @param species_code Species code to number species
#' @param verbose Print advanced error message
#'
#' @return ppp object of the spatstat package with simulated species

#' @export
create_simulation_species <- function(habitats_poly, habitat,  owin_overall, 
                                      type, process, association_strength = 0.3,
                                      number_points = 100, species_code = 0, 
                                      verbose = TRUE){
  
  scale <- mean(diff(owin_overall$yrange), diff(owin_overall$xrange)) / 25
  
  if(type == "positive"){
    
    if(process == "Poisson"){
      
      owin_pattern <- maptools::as.owin.SpatialPolygons(habitats_poly[habitats_poly$layer == habitat,])
      
      pattern_a <- mobsim::sim_poisson_community(s_pool = 1, 
                                                 n_sim = number_points, 
                                                 xrange = owin_overall$xrange,
                                                 yrange = owin_overall$yrange)
      
      pattern_a <- spatstat::ppp(x = pattern_a$census$x,
                                 y = pattern_a$census$y,
                                 window = owin_overall)
      
      pattern_b <- spatstat::runifpoint(n = floor(pattern_a$n * association_strength), win = owin_pattern)
      
      pattern <- spatstat::superimpose.ppp(pattern_a, pattern_b, W = owin_overall)
      
      marks_pattern <- data.frame(species = rep(paste0("poisson_positive_", habitat), pattern$n),
                                  species_code = species_code,
                                  habitat = rep(habitat, pattern$n))
      
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else if(process == "Thomas"){
      
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
      
      marks_pattern <- data.frame(species = rep(paste0("thomas_positive_", habitat), pattern$n),
                                  species_code = species_code,
                                  habitat = rep(habitat, pattern$n))
      
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else{
      
      if(verbose == TRUE){print("Please select either 'Poisson', 'Thomas' as process")}
      pattern <- NULL
    }
  }
  
  else if (type == "negative"){
    
    spatstat.options(fastthin=FALSE)
    p_retain <- 1 - association_strength
    
    if(process == "Poisson"){
      
      owin_pattern <- maptools::as.owin.SpatialPolygons(habitats_poly[habitats_poly$layer == habitat,])
      
      pattern_a <- mobsim::sim_poisson_community(s_pool = 1, 
                                                 n_sim = number_points, 
                                                 xrange = owin_overall$xrange,
                                                 yrange = owin_overall$yrange)
      
      pattern_a <- spatstat::ppp(x = pattern_a$census$x,
                                 y = pattern_a$census$y,
                                 window = owin_overall)
      
      pattern_b <- pattern_a[!spatstat::inside.owin(x = pattern_a, w = owin_pattern)]
      pattern_c <- spatstat::rthin(X = pattern_a[spatstat::inside.owin(x = pattern_a, w = owin_pattern)], P = p_retain)
      
      pattern <- spatstat::superimpose(pattern_b, pattern_c, W = owin_overall)
      
      marks_pattern <- data.frame(species = rep(paste0("poisson_negative_", habitat), pattern$n),
                                  species_code = species_code,
                                  habitat = rep(habitat, pattern$n))
      
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else if(process=="Thomas"){
      
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
      pattern_c <- spatstat::rthin(X = pattern_a[spatstat::inside.owin(x = pattern_a, w = owin_pattern)], P = p_retain)
      
      pattern <- spatstat::superimpose(pattern_b, pattern_c, W = owin_overall)
      
      marks_pattern <- data.frame(species = rep(paste0("thomas_negative_", habitat), pattern$n),
                                  species_code = species_code,
                                  habitat = rep(habitat, pattern$n))
      
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else{
      
      if(verbose == TRUE){print("Please select either 'Poisson', 'Thomas' or as process")}
      pattern <- NULL
    }
  }
  
  else if(type == "neutral"){
    
    if(process == "Poisson"){
      
      pattern <- mobsim::sim_poisson_community(s_pool = 1, 
                                               n_sim = number_points, 
                                               xrange = owin_overall$xrange,
                                               yrange = owin_overall$yrange)
      
      pattern <- spatstat::ppp(x = pattern$census$x,
                               y = pattern$census$y,
                               window = owin_overall)
      
      marks_pattern <- data.frame(species = rep("poisson_neutral", pattern$n),
                                  species_code = species_code)
      
      spatstat::marks(pattern) <- marks_pattern
    }
    
    else if(process == "Thomas"){
      
      pattern <- mobsim::sim_thomas_community(s_pool = 1, 
                                              n_sim = number_points, 
                                              sigma = scale, 
                                              cluster_points = 5,
                                              xrange = owin_overall$xrange, 
                                              yrange = owin_overall$yrange)
      
      pattern <- spatstat::ppp(x = pattern$census$x,
                               y = pattern$census$y,
                               window = owin_overall)
      
      marks_pattern <- data.frame(species = rep("thomas_neutral", pattern$n),
                                  species_code = species_code)
      
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