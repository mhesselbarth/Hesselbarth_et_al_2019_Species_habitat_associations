#### Results Pattern reconstruction #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
library(dplyr)
library(RColorBrewer)
library(shar)
library(spatstat)
library(tidyverse)

#### 1. Input data ####
results <- list.files(paste0(getwd(), '/4_Output'), pattern = 'o01_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

results_names <- list.files(paste0(getwd(), '/4_Output'), pattern = 'o01_', full.names = FALSE) %>%
  purrr::map_chr(function(files) files)

results_names <- stringr::str_sub(results_names, start = 1, end = -5)
names_split <- stringr::str_split(results_names, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3], "_", names_split[, 4])

names(results) <- names_combined

### 2. Calculate pcf ####
results_df_long <- purrr::map_dfr(results, function(current_species) {
  purrr::map_dfr(current_species, function(current_pattern) { 
    tibble::as.tibble(
      spatstat::pcf.ppp(current_pattern, 
                        divisor = "d", 
                        correction = "Ripley", 
                        r = seq(from = 0, to = 100, length.out = 515))
    )
  }, .id = "pattern")
}, .id = "species")

results_df_long_reclassified <- dplyr::mutate(results_df_long, 
                                              pattern = dplyr::case_when(pattern == "observed" ~ "observed", 
                                                                         pattern != "observed" ~ "simulation"), 
                                              method = dplyr::case_when(pattern == "observed" ~ "Observed",
                                                                        str_detect(species, "^reconstruction") ~ "Pattern reconstruction",
                                                                        str_detect(species, "^fitted") ~ "Gamma test"), 
                                              species = dplyr::case_when(str_detect(species, "species_1") ~ "species_1",
                                                                         str_detect(species, "species_2") ~ "species_2", 
                                                                         str_detect(species, "species_3") ~ "species_3", 
                                                                         str_detect(species, "species_4") ~ "species_4"))

pcf_observed <- dplyr::filter(results_df_long_reclassified, 
                              pattern == "observed")

pcf_observed$species <- factor(pcf_observed$species, 
                               levels = c("species_1",
                                          "species_2", 
                                          "species_3",
                                          "species_4"), 
                               labels = c("Species 1", 
                                          "Species 2", 
                                          "Species 3",
                                          "Species 4"))

pcf_null_model <- dplyr::filter(results_df_long_reclassified, 
                                pattern == "simulation") %>% 
  dplyr::group_by(species, method, r) %>% 
  dplyr::summarise(lo = quantile(iso, probs = 0.025),
                   hi = quantile(iso, probs = 0.975))

pcf_null_model$species <- factor(pcf_null_model$species, 
                                 levels = c("species_1",
                                            "species_2", 
                                            "species_3",
                                            "species_4"), 
                                 labels = c("Species 1", 
                                            "Species 2", 
                                            "Species 3",
                                            "Species 4"))

pcf_null_model$method <- factor(pcf_null_model$method, 
                                levels = c("Gamma test",
                                           "Pattern reconstruction"))

#### 3. Plot results ###
plot_method_comparison <- ggplot(data = pcf_null_model) + 
  geom_ribbon(data = pcf_null_model, aes(x = r, ymin = lo, ymax = hi, fill = method)) +
  geom_line(data = pcf_observed, aes(x = r, y = theo, linetype = "Complete spatial randomness"), size = 1, col = "grey35") +
  geom_line(data = pcf_observed, aes(x = r, y = iso, linetype = "Observed"), size = 1, col = "grey35") +
  scale_linetype_manual(name = "", values = c("Observed" = 1, "Complete spatial randomness" = 2)) +
  scale_fill_viridis_d(name = "") +
  facet_wrap(~ species) +
  labs(x = "r [m]", y = "g(r)") +
  theme_classic(base_size = 15) +  
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5, "cm"))

#### 4. Save result ####
UtilityFunctions::save_ggplot(plot = plot_method_comparison, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "a0_plot_method_comparison.png",
                              width = 325, height = 250, units = "mm", 
                              overwrite = FALSE)
