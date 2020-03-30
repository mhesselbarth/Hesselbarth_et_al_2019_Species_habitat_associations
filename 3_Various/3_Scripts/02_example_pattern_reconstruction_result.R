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
library(extrafont)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
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
results_fitting <- results$gamma_test
results_reconstruction <- results$pattern_reconstruction

### 2. Calculate pcf ####

# get value of observed pattern
pcf_observed <- spatstat::pcf.ppp(results_fitting$observed, 
                                  divisor = "d", 
                                  correction = "Ripley", 
                                  r = seq(from = 0, to = 250, length.out = 515)) %>% 
  tibble::as_tibble()

# gamma test
results_fitting <- purrr::map_dfr(results_fitting$randomized, function(current_pattern) { 
    tibble::as_tibble(
      spatstat::pcf.ppp(current_pattern, 
                        divisor = "d", 
                        correction = "Ripley", 
                        r = seq(from = 0, to = 250, length.out = 515))
    )}, .id = "pattern")

# add method to df
results_fitting$method = "gamma-test"

results_reconstruction <- purrr::map_dfr(results_reconstruction$randomized, function(current_pattern) { 
    tibble::as_tibble(
      spatstat::pcf.ppp(current_pattern, 
                        divisor = "d", 
                        correction = "Ripley", 
                        r = seq(from = 0, to = 250, length.out = 515))
    )}, .id = "pattern")

# add method to df
results_reconstruction$method = "Pattern reconstruction"

# combine to one df
pcf_null_model <- dplyr::bind_rows(results_fitting, results_reconstruction) %>% 
  dplyr::group_by(method, r) %>% 
  dplyr::summarise(lo = quantile(iso, probs = 0.025),
                   hi = quantile(iso, probs = 0.975)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(method = factor(method, 
                                levels = c("gamma-test",
                                           "Pattern reconstruction")))

#### 3. Plot results ###
plot_method_comparison <- ggplot() + 
  geom_ribbon(data = pcf_null_model, aes(x = r, ymin = lo, ymax = hi, fill = method), size = 1, alpha = 0.25) +
  geom_line(data = pcf_null_model, aes(x = r, y = lo, col = method), size = 0.5) +
  geom_line(data = pcf_null_model, aes(x = r, y = hi, col = method), size = 0.5) +
  geom_line(data = pcf_observed, aes(x = r, y = theo), col = "grey", linetype = 2, size = 0.75) +
  geom_line(data = pcf_observed, aes(x = r, y = iso), col = "black", size = 1) +
  scale_color_manual(name = "", values = c("#0D0887FF", "#CC4678FF")) + 
  scale_fill_manual(name = "", values = c("#0D0887FF", "#CC4678FF")) + 
  labs(x = "r [m]", y = expression(italic("g(r)"))) +
  theme_classic(base_size = 12.5) +  
  theme(legend.position = "bottom", 
        text = element_text(family = "Calibri Light"))

#### 4. Save result ####
suppoRt::save_ggplot(plot = plot_method_comparison, 
                     filename = "gamma_vs_reconstruction.png",
                     path = "C:/Users/Maximilian/ownCloud/13_Disputation/Figures/",
                     dpi = 300, units = "mm",
                     width = 225, height = 150, 
                     overwrite = FALSE)
