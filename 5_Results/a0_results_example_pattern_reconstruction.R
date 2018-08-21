#### Results Pattern reconstruction #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
library(UtilityFunctions)
library(SHAR)
library(tidyverse)

#### 1. Input data ####
results <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a0_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

results_names <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a0_', full.names = FALSE) %>%
  purrr::map_chr(function(files) files)

results_names <- stringr::str_sub(results_names, start = 1, end = -5)
names_split <- stringr::str_split(results_names, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3], "_", names_split[, 4])

names(results) <- names_combined

### 2. Calculate pcf ####

results_df_long <- purrr::map_dfr(results, function(current_species) {
  purrr::map_dfr(current_species, function(current_pattern) { 
    tibble::as.tibble(as.data.frame(spatstat::pcf(current_pattern, divisor = "d", correction = "Ripley")))
  }, .id = "pattern")
}, .id = "species")

results_df_long <- results_df_long %>%
  dplyr::mutate(pattern = dplyr::case_when(pattern == "Observed" ~ "Observed", 
                                           pattern != "Observed" ~ "Simulation"), 
                species = dplyr::case_when(species == "reconstruction_species_1" ~ "Species 1", 
                                           species == "reconstruction_species_2" ~ "Species 2", 
                                           species == "reconstruction_species_3" ~ "Species 3",
                                           species == "reconstruction_species_4" ~ "Species 4")) 

pcf_observed <- results_df_long %>%
  dplyr::filter(pattern == "Observed")

pcf_null_model <- results_df_long %>%
  dplyr::filter(pattern == "Simulation") %>% 
  dplyr::group_by(species, r) %>% 
  dplyr::summarise(lo=stats::quantile(iso, probs = 0.025),
                   hi=stats::quantile(iso, probs = 0.975))


#### 3. Plot results ###

ggplot() + 
  geom_line(data = pcf_observed, 
            aes(x = r, y = iso)) + 
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(data = pcf_null_model, 
              aes(x= r, ymin = lo, ymax = hi), 
              alpha = 0.3) + 
  facet_wrap(~ species, scales = "free") +
  labs(x = "r [m]", y = "g(r)") +
  theme_bw(base_size = 15)
