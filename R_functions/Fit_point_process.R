Fit.Point.Process <- function(input, species, 
                              process='Poisson', number_pattern=199){
  
  pattern_species <- input %>%
    spatstat::subset.ppp(Species_code == species) %>%
    spatstat::unmark()
  
  if(process == 'Poisson'){
    pattern_random <- pattern_species %>% 
      spatstat::ppm() %>%
      spatstat::simulate.ppm(nsim=number_pattern, progress=F)
  }
  
  else if(process == 'Cluster'){
    pattern_random <- pattern_species %>%
      spatstat::kppm(cluster = 'Thomas', statistic = 'pcf') %>%
      spatstat::simulate.kppm(nsim=number_pattern, verbose=F)
  }
  
  else{stop('Please select either Poisson or Cluster as process')}
  
  pattern_random[[length(pattern_random)+1]] <- pattern_species
  names(pattern_random)[[length(pattern_random)]] <- "Observed"
  
  return(pattern_random)
}
