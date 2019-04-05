###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Hypothesis 1 & 2 ####

# Load packages #

# library(clustermq)

library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(onpoint) # devtools::install_github("mhesselbarth/onpoint")
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)
library(patchwork)

#### Import data ####

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")

pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")

# split into species
beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Beech"))
beech_dead <- spatstat::subset.ppp(pattern_2007_dead, Species == "Beech")
ash <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Ash"))
hornbeam <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Hornbeam"))
sycamore <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "Sycamore"))
others <- spatstat::unmark(subset.ppp(pattern_2007_living, Species == "others"))

#### Forest structure ####

# Species abundance
# abundance <- table(pattern_2007_living$marks$Species) %>%
#   as.data.frame() %>% 
#   purrr::set_names(c("species", "abundance")) %>% 
#   dplyr::mutate(abundance_rel = abundance / sum(abundance) * 100, 
#                 species = as.factor(species))

# position_dodge(width = 1) <- ggplot2::ggplot(data = abundance) + 
#   ggplot2::geom_bar(ggplot2::aes(x = species, y = abundance_rel), stat = "identity") + 
#   ggplot2::geom_text(ggplot2::aes(x = species, y = abundance_rel, label = paste0("n = ", abundance)), 
#                      vjust = -1) +
#   ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 100, by = 20), limits = c(0, 100)) + 
#   ggplot2::scale_x_discrete(labels = c("Beech" = "F. sylvatica", "Ash" = "F. exelcsior ", 
#                                        "Hornbeam" = "C. betulus ", "Sycamore" = "A. pseudoplatanus ", 
#                                        "others" = "others")) +
#   ggplot2::labs(x = "Species", y = "Relative abundance [%]") + 
#   ggplot2::theme_bw(base_size = 15)

abundance <- pattern_2007_living$marks %>% 
  as.data.frame() %>% 
  dplyr::group_by(Species) %>%
  dplyr::summarise(n = n(), 
                   ba = sum(((DBH_07/2) ^ 2) * pi)) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100, 
                ba_rel = (ba / sum(ba)) * 100) %>% 
  tidyr::gather(key = "type", value = "value", -Species)

species_abundance <- ggplot2::ggplot(data = dplyr::filter(abundance, type == "n_rel" | type == "ba_rel" )) + 
  ggplot2::geom_bar(ggplot2::aes(x = Species, y = value, 
                                 group = factor(type, levels = c("n_rel", "ba_rel")),
                                 fill = factor(type, levels = c("n_rel", "ba_rel"))), 
                    stat = "identity", position = "dodge") + 
  ggplot2::geom_text(ggplot2::aes(x = Species, y = value, label = paste0(round(value, 1), "%"), 
                                  group = factor(type, levels = c("n_rel", "ba_rel"))),
                     position = position_dodge(width = 1), vjust = -1) +
  ggplot2::scale_fill_viridis_d(name = "", labels = c("n_rel" = "Number of stems", "ba_rel" = "Basal area")) + 
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 100, by = 20), limits = c(0, 100)) + 
  ggplot2::scale_x_discrete(labels = c("Beech" = "F. sylvatica", "Ash" = "F. exelcsior ", 
                                       "Hornbeam" = "C. betulus ", "Sycamore" = "A. pseudoplatanus ", 
                                       "others" = "others")) +
  ggplot2::labs(x = "Species", y = "Relative value [%]") + 
  ggplot2::theme_bw(base_size = 15)

helpeR::save_ggplot(plot = species_abundance, 
                    path = "2_Real_world_data/4_Figures", 
                    filename = "species_abundance.png", 
                    dpi = 300, width = 22, height = 12, units = "cm",
                    overwrite = TRUE)

# DBH distribution 

dbh_distribution <- ggplot2::ggplot(data = data.frame(pattern_2007_living$marks)) + 
  ggplot2::geom_histogram(ggplot2::aes(x = DBH_07), 
                          binwidth = 1, fill = "black", col = "white") + 
  ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0, 100)) +
  ggplot2::labs(x = "DBH [cm]", y = "Count") + 
  ggplot2::theme_bw(base_size = 15)

helpeR::save_ggplot(plot = dbh_distribution, 
                    path = "2_Real_world_data/4_Figures", 
                    filename = "dbh_distribution.png", 
                    dpi = 300, width = 15, height = 10, units = "cm",
                    overwrite = TRUE)


#### PPA ####

nsim <- 199

envelope_all <- spatstat::envelope(pattern_2007_living, 
                                   fun = pcfinhom, 
                                   funargs = list(divisor = "d", 
                                                  correction = "Ripley"), 
                                   nsim = nsim)

plot_all <- onpoint::plot_quantums(envelope_all, title = "All species", 
                                   ylab = "g(r)", xlab = "r [m]", quantum_position = 0)

envelope_beech <- spatstat::envelope(beech, fun = pcfinhom, 
                                     funargs = list(divisor = "d", 
                                                    correction = "Ripley"), 
                                     nsim = nsim)

plot_beech <- onpoint::plot_quantums(envelope_beech, title = "F. sylvatica", 
                                     legend_position = "none", ylab = "g(r)", xlab = "r [m]")

envelope_ash <- spatstat::envelope(ash, fun = pcfinhom, 
                                   funargs = list(divisor = "d", 
                                                  correction = "Ripley"),
                                   nsim = nsim)

plot_ash <- onpoint::plot_quantums(envelope_ash, title = "F. excelsior", 
                                   legend_position = "none", ylab = "g(r)", xlab = "r [m]")

envelope_hornbeam <- spatstat::envelope(hornbeam, fun = pcfinhom, 
                                   funargs = list(divisor = "d", 
                                                  correction = "Ripley"),
                                   nsim = nsim)

plot_hornbeam <- onpoint::plot_quantums(envelope_hornbeam, title = "C. betulus", 
                                        legend_position = "none", ylab = "g(r)", xlab = "r [m]")

envelope_sycamore <- spatstat::envelope(sycamore, fun = pcfinhom, 
                                        funargs = list(divisor = "d", 
                                                       correction = "Ripley"),
                                        nsim = nsim)

plot_sycamore <- onpoint::plot_quantums(envelope_sycamore, title = "A. pseudoplatanus", 
                                        legend_position = "none", ylab = "g(r)", xlab = "r [m]")


envelope_others <- spatstat::envelope(others, fun = pcfinhom, 
                                        funargs = list(divisor = "d", 
                                                       correction = "Ripley"),
                                        nsim = nsim)

plot_others <- onpoint::plot_quantums(envelope_others, title = "others", 
                                      legend_position = "none", ylab = "g(r)", xlab = "r [m]")

plot_overall <- plot_all + {plot_beech + plot_ash + plot_hornbeam + plot_sycamore + plot_others} + plot_layout(nrow = 2)

helpeR::save_ggplot(plot = plot_overall, filename = "pcf_overall.png", 
                    path = "2_Real_world_data/4_Figures", 
                    width = 210, height = 297, units = "mm", 
                    dpi = 300,
                    overwrite = TRUE)

# Beech dead 
envelope_beech_dead <- spatstat::envelope(beech_dead, 
                                          fun = pcfinhom, 
                                          funargs = list(divisor = "d", 
                                                         correction = "Ripley"), 
                                          nsim = nsim)

plot_beech_dead <- onpoint::plot_quantums(envelope_beech_dead, title = "Beech (dead trees)",
                                          ylab = "g(r)", xlab = "r [m]")

helpeR::save_ggplot(plot = plot_beech_dead, filename = "pcf_beech_dead.png", 
                    path = "2_Real_world_data/4_Figures", 
                    dpi = 300, width = 20, height = 12, units = "cm",
                    overwrite = TRUE)

#### Hypotheses 1 & 2 ####

# Import data 
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))
reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))
reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))
reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))

# Calculate mean energies
full_patterns_list <- list(Beech = reconstructed_beech, 
                           Ash = reconstructed_ash, 
                           Hornbeam = reconstructed_hornbeam, 
                           Sycamore =reconstructed_sycamore,
                           others = reconstructed_others)

mean_energy_full_patterns <- purrr::map_dfr(full_patterns_list, function(x){
  data.frame(mean_energy = shar::calculate_energy(pattern = x, 
                                                  return_mean = TRUE, 
                                                  comp_fast = TRUE, 
                                                  verbose = FALSE))}, .id = "species")

helpeR::save_rds(object = mean_energy_full_patterns, 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results/Appendix"), 
                           filename = "mean_energy_full_patterns.rds")

# Plot summary stats
summary_stats_species <- purrr::map_dfr(full_patterns_list, function(current_species) {
  
  summary_stats <- purrr::map_dfr(current_species, function(x) {
    
    gest_result <- spatstat::Gest(X = x, correction = "none")
    
    pcf_result <- shar::estimate_pcf_fast(x,
                                          correction = "none",
                                          method = "c",
                                          spar = 0.5)
    
    gest_df <- as.data.frame(gest_result) # conver to df
    
    names(gest_df)[3] <- "x_r" # unique col names
    
    gest_df$summary_function <- "Nearest Neighbour Distance Function G(r)" # name of method
    
    pcf_df <- as.data.frame(pcf_result) # convert to df
    
    names(pcf_df)[3] <- "x_r" # unique col names
    
    pcf_df$summary_function <- "Pair Correlation Function g(r)" # name of method
    
    dplyr::bind_rows(gest_df, pcf_df) # combine to one df    
  }, .id = "type")

  
  # classify all observed and all randomized repetitions identical
  summary_stats <- dplyr::mutate(summary_stats, type = dplyr::case_when(type == "observed" ~ "observed",
                                                                        TRUE ~ "randomized")) %>%
    dplyr::group_by(summary_function, r, type) %>% 
    dplyr::summarise(lo = stats::quantile(x_r, probs = 0.025),
                     hi = stats::quantile(x_r, probs = 0.975),
                     x_r = mean(x_r),
                     theo = mean(theo))
}, .id = "species")

ggplot_full_patterns <- ggplot() + 
  geom_ribbon(data = dplyr::filter(summary_stats_species, type == "randomized"), 
              aes(x = r, ymin = lo, ymax = hi), 
              alpha = 0.3, col = "grey") +
  geom_line(data = dplyr::filter(summary_stats_species, type == "observed"),
            aes(x = r, y = x_r), col = "black") +
  facet_wrap(~ species + summary_function, scales = "free", nrow = 5, ncol = 2) + 
  theme_bw()

helpeR::save_ggplot(plot = ggplot_full_patterns, 
                              path = paste0(getwd(), "/2_Real_world_data/3_Results/Appendix"), 
                              filename = "ggplot_full_patterns.png")
