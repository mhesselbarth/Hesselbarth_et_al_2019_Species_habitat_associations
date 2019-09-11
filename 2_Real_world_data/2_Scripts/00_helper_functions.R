calc_energy_helper <- function(pattern_randomized, pattern_observed, 
                               weights = c(0.5, 0.5), 
                               return_mean = FALSE, comp_fast = 1000, 
                               verbose = TRUE) {
  
  # calculate r sequence
  r <- seq(from = 0,
           to = spatstat::rmax.rule(W = pattern_observed$window,
                                    lambda = spatstat::intensity.ppp(pattern_observed)),
           length.out = 250)
  
  # check if weights make sense
  if (sum(weights) > 1 || sum(weights) == 0) {
    stop("The sum of 'weights' must be 0 < sum(weights) <= 1.", call. = FALSE)
  }
    
  # check if number of points exceed comp_fast limit
  if (pattern_observed$n > comp_fast) {
    comp_fast <- TRUE
  }
    
  else {
    comp_fast <- FALSE
  }
  
  # calculate summary functions for observed pattern
  if (comp_fast) {
    
    gest_observed <- spatstat::Gest(X = pattern_observed,
                                    correction = "none",
                                    r = r)
    
    pcf_observed <- shar::estimate_pcf_fast(pattern = pattern_observed,
                                            correction = "none",
                                            method = "c",
                                            spar = 0.5,
                                            r = r)
  }
  
  else{
    
    gest_observed <- spatstat::Gest(X = pattern_observed,
                                    correction = "han",
                                    r = r)
    
    pcf_observed <- spatstat::pcf(X = pattern_observed,
                                  correction = "best",
                                  divisor = "d",
                                  r = r)
  }
    
  # loop through all reconstructed patterns
  result <- vapply(seq_along(pattern_randomized), function(x) {
    
    # fast computation of summary stats
    if (comp_fast) {
      
      gest_reconstruction <- spatstat::Gest(X = pattern_randomized[[x]],
                                            correction = "none",
                                            r = r)
      
      pcf_reconstruction <- shar::estimate_pcf_fast(pattern = pattern_randomized[[x]],
                                                    correction = "none",
                                                    method = "c",
                                                    spar = 0.5,
                                                    r = r)
    }
    
    # normal computation of summary stats
    else{
      
      gest_reconstruction <- spatstat::Gest(X = pattern_randomized[[x]],
                                            correction = "han",
                                            r = r)
      
      pcf_reconstruction <- spatstat::pcf(X = pattern_randomized[[x]],
                                          correction = "best",
                                          divisor = "d",
                                          r = r)
    }
    
    # difference between observed and reconstructed pattern
    energy <- (mean(abs(gest_observed[[3]] - gest_reconstruction[[3]]), na.rm = TRUE) * weights[[1]]) +
      (mean(abs(pcf_observed[[3]] - pcf_reconstruction[[3]]), na.rm = TRUE) * weights[[2]])
    
    # print progress
    if (verbose) {
      message("\r> Progress: ", x, "/", length(pattern_randomized), "\t\t",
              appendLF = FALSE)
    }
    
    return(energy)
    
  }, FUN.VALUE = numeric(1))
    
  # set names
  names(result) <- paste0("randomized_", seq_along(result))
  
  # return mean for all reconstructed patterns
  if (return_mean) {
    result <- mean(result)
  }
  
  # write result in new line if progress was printed
  if (verbose) {
    message("\r")
  }
  
  return(result)
}

create_random_pat <- function(x, method) {
  
  randomized <- x[names(x) != "observed"]
  observed <- x$observed
  
  # combine to one list
  random_pat <- list(randomized = randomized,
                     observed = observed,
                     method = method,
                     energy_df = "NA",
                     stop_criterion = "NA",
                     iterations = NA)
  
  # set class of result
  class(random_pat) <- "rd_pat"
  
  return(random_pat)
}
