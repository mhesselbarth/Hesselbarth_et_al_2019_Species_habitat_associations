###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################


#### 1. Import packages and data ####

# Packages #
library(dplyr)
library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(shar)
library(spatstat)
library(tidyverse)

#### 1. Input data ####

# import data
results <- list.files(path = "3_Various/1_Results/", pattern = "02_", full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

# get name of data
results_names <- list.files(path = "3_Various/1_Results/", pattern = "02_", full.names = FALSE) %>%
  purrr::map_chr(function(files) files)

# add names
names(results) <- stringr::str_sub(results_names, start = 4, end = -5)

# split results according to method
results_fitting <- results$example_fitted_pattern

results_reconstruction <- results$example_reconstructed_pattern

### 2. Calculate pcf ####

results_fitting <- purrr::map_dfr(results_fitting, function(current_species) {
  purrr::map_dfr(current_species, function(current_pattern) { 
    tibble::as_tibble(
      spatstat::pcf.ppp(current_pattern, 
                        divisor = "d", 
                        correction = "Ripley", 
                        r = seq(from = 0, to = 100, length.out = 515))
    )
  }, .id = "pattern")
}, .id = "species")

# add method to df
results_fitting$method = "gamma-test"

results_reconstruction <- purrr::map_dfr(results_reconstruction, function(current_species) {
  purrr::map_dfr(current_species, function(current_pattern) { 
    tibble::as_tibble(
      spatstat::pcf.ppp(current_pattern, 
                        divisor = "d", 
                        correction = "Ripley", 
                        r = seq(from = 0, to = 100, length.out = 515))
    )
  }, .id = "pattern")
}, .id = "species")

# add method to df
results_reconstruction$method = "Pattern reconstruction"

# combine to one df
results <- dplyr::bind_rows(results_fitting, results_reconstruction) %>% 
  dplyr::mutate(pattern = dplyr::case_when(pattern == "observed" ~ "observed", 
                                           pattern != "observed" ~ "simulation"))

# get value of observed pattern
pcf_observed <- dplyr::filter(results, 
                              pattern == "observed")

# convert species col to factor
pcf_observed$species <- factor(pcf_observed$species, 
                               levels = c("species_1",
                                          "species_2", 
                                          "species_3",
                                          "species_4"), 
                               labels = c("Species 1", 
                                          "Species 2", 
                                          "Species 3",
                                          "Species 4"))

# calculate quantiles of simulated patterns grouped by species & method
pcf_null_model <- dplyr::filter(results, 
                                pattern == "simulation") %>% 
  dplyr::group_by(species, method, r) %>% 
  dplyr::summarise(lo = quantile(iso, probs = 0.025),
                   hi = quantile(iso, probs = 0.975))

# convert species col to factor
pcf_null_model$species <- factor(pcf_null_model$species, 
                                 levels = c("species_1",
                                            "species_2", 
                                            "species_3",
                                            "species_4"), 
                                 labels = c("Species 1", 
                                            "Species 2", 
                                            "Species 3",
                                            "Species 4"))

# covert method col to factor
pcf_null_model$method <- factor(pcf_null_model$method, 
                                levels = c("gamma-test",
                                           "Pattern reconstruction"))

#### 3. Plot results ###
plot_method_comparison <- ggplot() + 
  geom_ribbon(data = pcf_null_model, aes(x = r, ymin = lo, ymax = hi, fill = method), size = 1, alpha = 0.3) +
  geom_line(data = pcf_observed, aes(x = r, y = theo, col = "CSR"), linetype = 2, size = 1) +
  geom_line(data = pcf_null_model, aes(x = r, y = lo, col = method), size = 1) +
  geom_line(data = pcf_null_model, aes(x = r, y = hi, col = method), size = 1) +
  geom_line(data = pcf_observed, aes(x = r, y = iso, col = "Observed"), size = 1) +
  scale_color_manual(values = c("gamma-test" = "#1b9e77",
                                "Pattern reconstruction" = "#7570b3", 
                                "Observed" = "black", 
                                "CSR" = "grey"), 
                     name = "") +
  scale_fill_manual(values = c("gamma-test" = "#1b9e77",
                               "Pattern reconstruction" = "#7570b3"), 
                     name = "") +
  facet_wrap(~ species, scales = "free") +
  labs(x = "r [m]", y = "g(r)") +
  theme_classic(base_size = 15) +  
  guides(color = guide_legend(nrow = 2, byrow = TRUE), fill = FALSE) +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5, "cm"))

#### 4. Save result ####
helpeR::save_ggplot(plot = plot_method_comparison, 
                    path = "3_Various/2_Figures",
                    filename = "gamma_vs_reconstruction.png",
                    dpi = 300, 
                    width = 210, height = 160, units = "mm")