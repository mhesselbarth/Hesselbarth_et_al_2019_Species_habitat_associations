#### Results Pattern reconstruction #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
source(paste0(getwd(), '/2_Functions/setup_packages.R'))

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
      as.data.frame(
      spatstat::pcf.ppp(current_pattern, 
                        divisor = "d", 
                        correction = "Ripley", 
                        r = seq(from = 0, to = 100, length.out = 515))
      )
    )
  }, .id = "pattern")
}, .id = "species")

results_df_long_reclassified <- results_df_long %>%
  dplyr::mutate(pattern = dplyr::case_when(pattern == "Observed" ~ "Observed", 
                                           pattern != "Observed" ~ "Simulation"), 
                method = dplyr::case_when(str_detect(species, "^reconstruction") ~ "Pattern reconstruction",
                                          str_detect(species, "^fitted") ~ "Gamma test"), 
                species = dplyr::case_when(str_detect(species, "species_1") ~ "species_1",
                                           str_detect(species, "species_2") ~ "species_2", 
                                           str_detect(species, "species_3") ~ "species_3", 
                                           str_detect(species, "species_4") ~ "species_4"))

pcf_observed <- results_df_long_reclassified %>%
  dplyr::filter(pattern == "Observed", 
                species == "species_1")

pcf_null_model <- results_df_long_reclassified %>%
  dplyr::filter(pattern == "Simulation") %>% 
  dplyr::group_by(species, method, r) %>% 
  dplyr::summarise(lo=stats::quantile(iso, probs = 0.025),
                   hi=stats::quantile(iso, probs = 0.975)) %>% 
  dplyr::filter(species == "species_1")

pcf_null_model$method <- factor(pcf_null_model$method, 
                                levels = c("Gamma test",
                                           "Pattern reconstruction"))

#### 3. Plot results ###

colors_spec <- rev(RColorBrewer::brewer.pal(n = 3, name = "Spectral"))

plot_method_comparison <- ggplot(data = pcf_observed) + 
  geom_ribbon(data = pcf_null_model, aes(x= r, ymin = lo, ymax = hi, fill = "Null model")) +
  geom_line(aes(x = r, y = theo, color = "Complete spatial randomness"), linetype = 2, size = 1.25) +
  geom_line(aes(x = r, y = iso, color = "Observed"), size = 1.25) +
  scale_color_manual(name = "", values = c("Complete spatial randomness" = colors_spec[1], "Observed" = colors_spec[3])) +
  scale_fill_manual(name = "", values = c("Null model" = "grey")) +
  facet_wrap(~ method, ncol = 1) +
  labs(x = "r [m]", y = "g(r)") +
  theme_classic(base_size = 30) +  
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position="bottom")

#### 4. Save result ####
UtilityFunctions::save_ggplot(plot = plot_method_comparison, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "a0_plot_method_comparison.png",
                              width = 325, height = 250, units = "mm", 
                              overwrite = TRUE)
