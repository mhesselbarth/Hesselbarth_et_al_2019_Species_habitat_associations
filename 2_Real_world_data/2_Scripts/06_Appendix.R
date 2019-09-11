###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Real-world data - Appendix ####

# Load packages #

library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt)
library(onpoint) # devtools::install_github("mhesselbarth/onpoint")
library(raster)
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)
library(patchwork)

#### Import data ####

# import point pattern data
pattern_2007 <- readr::read_rds("2_Real_world_data/1_Data/02_pattern_2007_ppp.rds")

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, type != "dead")

pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, type == "dead")

# split into species
beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, 
                                               species == "beech"))

beech_dead <- spatstat::subset.ppp(pattern_2007_dead, 
                                   species == "beech")

ash <- spatstat::unmark(subset.ppp(pattern_2007_living, 
                                   species == "ash"))

hornbeam <- spatstat::unmark(subset.ppp(pattern_2007_living, 
                                        species == "hornbeam"))

sycamore <- spatstat::unmark(subset.ppp(pattern_2007_living, 
                                        species == "sycamore"))

others <- spatstat::unmark(subset.ppp(pattern_2007_living, 
                                      species == "others"))

#### Forest structure ####

# Species abundance
# abundance <- table(pattern_2007_living$marks$species) %>%
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
  tibble::as_tibble() %>% 
  dplyr::group_by(species) %>%
  dplyr::summarise(n = n(), 
                   ba = sum(((dbh_07 / 2) ^ 2) * pi)) %>%
  dplyr::mutate(n_rel = (n / sum(n)) * 100, 
                ba_rel = (ba / sum(ba)) * 100,
                species = factor(species, 
                                 levels = c("beech", "ash", "hornbeam", 
                                            "sycamore", "others"))) %>% 
  tidyr::gather(key = "type", value = "value", -species)

species_abundance <- ggplot2::ggplot(data = dplyr::filter(abundance, 
                                                          type == "n_rel" | 
                                                            type == "ba_rel" )) + 
  ggplot2::geom_bar(ggplot2::aes(x = species, y = value, 
                                 group = factor(type, levels = c("n_rel", "ba_rel")),
                                 fill = factor(type, levels = c("n_rel", "ba_rel"))), 
                    stat = "identity", position = "dodge") + 
  ggplot2::geom_text(ggplot2::aes(x = species, y = value, label = paste0(round(value, 1), "%"), 
                                  group = factor(type, levels = c("n_rel", "ba_rel"))),
                     position = position_dodge(width = 1), vjust = -1) +
  ggplot2::scale_fill_viridis_d(name = "", labels = c("n_rel" = "Number of stems", "ba_rel" = "Basal area")) + 
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 100, by = 20), limits = c(0, 100)) + 
  ggplot2::scale_x_discrete(labels = c("beech" = "F. sylvatica", "ash" = "F. exelcsior ", 
                                       "hornbeam" = "C. betulus ", "sycamore" = "A. pseudoplatanus ", 
                                       "others" = "others")) +
  ggplot2::labs(x = "Species", y = "Relative value [%]") + 
  ggplot2::theme_bw(base_size = 15) + 
  ggplot2::theme(legend.position = "bottom")

suppoRt::save_ggplot(plot = species_abundance, 
                    path = "2_Real_world_data/4_Figures/Appendix/", 
                    filename = "species_abundance.png", 
                    dpi = 300, width = 22, height = 12, units = "cm")

# DBH distribution 
max_dbh <- ceiling(max(pattern_2007_living$marks$dbh_07))

dbh <- pattern_2007_living$marks %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(dbh_class = cut(dbh_07, breaks = 0:max_dbh)) %>%
  dplyr::group_by(dbh_class) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(n_rel = n / sum(n) * 100, 
                smaller_than = as.numeric(dbh_class), 
                stage = dplyr::case_when(smaller_than <= 2.5 ~ "small", 
                                         smaller_than > 2.5 & smaller_than <= 10 ~ "medium", 
                                         smaller_than > 10 ~ "large"))

dbh_distribution <- ggplot2::ggplot(data = dbh) + 
  ggplot2::geom_bar(ggplot2::aes(x = smaller_than, y = n, 
                                 fill = factor(stage, 
                                               levels = c("small", "medium", "large"))), 
                    stat = "identity", col = "white") +  
  ggplot2::scale_fill_viridis_d(name = "Life history stage") + 
  ggplot2::scale_x_continuous(breaks = seq(from = 0, to = max(dbh$smaller_than), by = 10), 
                              limits = c(0, 100)) + 
  ggplot2::labs(x = "DBH [cm]", y = "Count") + 
  ggplot2::theme_bw(base_size = 15) + 
  ggplot2::theme(legend.position = "bottom")

# dbh_distribution <- ggplot2::ggplot(data = data.frame(pattern_2007_living$marks)) + 
#   ggplot2::geom_histogram(ggplot2::aes(x = DBH_07), 
#                           binwidth = 1, fill = "black", col = "white") + 
#   ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(0, 100)) +
#   ggplot2::labs(x = "DBH [cm]", y = "Count") + 
#   ggplot2::theme_bw(base_size = 15)

suppoRt::save_ggplot(plot = dbh_distribution, 
                    path = "2_Real_world_data/4_Figures/Appendix/", 
                    filename = "dbh_distribution.png", 
                    dpi = 300, width = 22, height = 12, units = "cm")

#### PPA ####

# # calculate PCF #
# nsim <- 199
# 
# envelope_all <- spatstat::envelope(pattern_2007_living, 
#                                    fun = pcf, 
#                                    funargs = list(divisor = "d", 
#                                                   correction = "Ripley"), 
#                                    nsim = nsim)
# 
# envelope_beech <- spatstat::envelope(beech, fun = pcf, 
#                                      funargs = list(divisor = "d", 
#                                                     correction = "Ripley"), 
#                                      nsim = nsim)
# 
# envelope_ash <- spatstat::envelope(ash, fun = pcf, 
#                                    funargs = list(divisor = "d", 
#                                                   correction = "Ripley"),
#                                    nsim = nsim)
# 
# envelope_hornbeam <- spatstat::envelope(hornbeam, fun = pcf, 
#                                    funargs = list(divisor = "d", 
#                                                   correction = "Ripley"),
#                                    nsim = nsim)
# 
# envelope_sycamore <- spatstat::envelope(sycamore, fun = pcf, 
#                                         funargs = list(divisor = "d", 
#                                                        correction = "Ripley"),
#                                         nsim = nsim)
# 
# envelope_others <- spatstat::envelope(others, fun = pcf, 
#                                         funargs = list(divisor = "d", 
#                                                        correction = "Ripley"),
#                                         nsim = nsim)
# 
# # save results of envelope function # 
# suppoRt::save_rds(object = envelope_all, 
#                   filename = "envelope_all.rds", 
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_beech, 
#                   filename = "envelope_beech.rds", 
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_ash, 
#                   filename = "envelope_ash.rds", 
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_sycamore, 
#                   filename = "envelope_sycamore.rds", 
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_hornbeam, 
#                   filename = "envelope_hornbeam.rds", 
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_others, 
#                   filename = "envelope_others.rds", 
#                   path = "2_Real_world_data/3_Results/Appendix/")

# read pcf data #
envelope_all <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_all.rds")
envelope_beech <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_beech.rds")
envelope_ash <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_ash.rds")
envelope_sycamore <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_sycamore.rds")
envelope_hornbeam <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_hornbeam.rds")
envelope_others <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_others.rds")

# create plot of single species
plot_all <- onpoint::plot_quantums(envelope_all, title = "All species", 
                                   ylab = "g(r)", xlab = "r [m]", quantum_position = 0)

plot_beech <- onpoint::plot_quantums(envelope_beech, title = "F. sylvatica", 
                                     legend_position = "none", ylab = "g(r)", xlab = "r [m]")

plot_ash <- onpoint::plot_quantums(envelope_ash, title = "F. excelsior", 
                                   legend_position = "none", ylab = "g(r)", xlab = "r [m]")

plot_hornbeam <- onpoint::plot_quantums(envelope_hornbeam, title = "C. betulus", 
                                        legend_position = "none", ylab = "g(r)", xlab = "r [m]")

plot_sycamore <- onpoint::plot_quantums(envelope_sycamore, title = "A. pseudoplatanus", 
                                        legend_position = "none", ylab = "g(r)", xlab = "r [m]")

plot_others <- onpoint::plot_quantums(envelope_others, title = "others", 
                                      legend_position = "none", ylab = "g(r)", xlab = "r [m]")

# combine to one large plot
plot_overall <- plot_all + {plot_beech + plot_ash + plot_hornbeam + plot_sycamore + plot_others} + plot_layout(nrow = 2)

# save plot
suppoRt::save_ggplot(plot = plot_overall, filename = "pcf_overall.png", 
                     path = "2_Real_world_data/4_Figures/Appendix/", 
                     width = 210, height = 297, units = "mm", 
                     dpi = 300)

# Beech dead 
# envelope_beech_dead <- spatstat::envelope(beech_dead, 
#                                           fun = pcf, 
#                                           funargs = list(divisor = "d", 
#                                                          correction = "Ripley"), 
#                                           nsim = nsim)
# 
# suppoRt::save_rds(object = envelope_beech_dead,
#                   filename = "envelope_beech_dead.rds",
#                   path = "2_Real_world_data/3_Results/Appendix/")

# load pcf dead beech
envelope_beech_dead <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_beech_dead.rds")

# create ggplot
plot_beech_dead <- onpoint::plot_quantums(envelope_beech_dead, title = "Beech (dead trees)",
                                          ylab = "g(r)", xlab = "r [m]")

# save ggplot
suppoRt::save_ggplot(plot = plot_beech_dead, filename = "pcf_beech_dead.png", 
                    path = "2_Real_world_data/4_Figures/Appendix/", 
                    dpi = 300, width = 20, height = 12, units = "cm")

#### Quality reconstruction vs. fitted ####

source("2_Real_world_data/2_Scripts/00_helper_functions.R")

#### Hypotheses 1 & 2 ####

# Import data 
reconstructed_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_beech.rds"))
reconstructed_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_ash.rds"))
reconstructed_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_hornbeam.rds"))
reconstructed_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_sycamore.rds"))
reconstructed_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/reconstructed_others.rds"))

fitted_beech <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_beech.rds"))
fitted_ash <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_ash.rds"))
fitted_hornbeam <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_hornbeam.rds"))
fitted_sycamore <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_sycamore.rds"))
fitted_others <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/3_Results/fitted_others.rds"))

# Calculate mean energies
species_rec_list <- list(beech = reconstructed_beech, 
                         ash = reconstructed_ash, 
                         hornbeam = reconstructed_hornbeam, 
                         sycamore = reconstructed_sycamore,
                         others = reconstructed_others)

species_fit_list <- list(beech = fitted_beech, 
                         ash = fitted_ash, 
                         hornbeam = fitted_hornbeam, 
                         sycamore = fitted_sycamore,
                         others = fitted_others)

energy_species_rec <- purrr::map_dfr(species_rec_list, function(x) {
  
  random <- x[names(x) != "observed"]
  observed <- x$observed
  
  data.frame(mean_energy = calc_energy_helper(pattern_observed = observed,
                                              pattern_randomized = random,
                                              return_mean = TRUE, 
                                              comp_fast = TRUE, 
                                              verbose = FALSE))}, .id = "species") %>% 
  dplyr::mutate(method = "Pattern reconstruction")

energy_species_fit <- purrr::map_dfr(species_fit_list, function(x){
  
  random <- x$randomized
  observed <- x$observed
  
  data.frame(mean_energy = calc_energy_helper(pattern_observed = observed,
                                              pattern_randomized = random,
                                              return_mean = TRUE, 
                                              comp_fast = TRUE, 
                                              verbose = FALSE))}, .id = "species") %>% 
  dplyr::mutate(method = "Point process fitting")

energy_species_all <- dplyr::bind_rows(energy_species_rec, energy_species_fit)

suppoRt::save_rds(object = energy_species_all, 
                           path = paste0(getwd(), "/2_Real_world_data/3_Results/Appendix"), 
                           filename = "energy_species_all.rds")

# # Plot summary stats
# summary_stats_species <- purrr::map_dfr(species_rec_list, function(current_species) {
#   
#   summary_stats <- purrr::map_dfr(current_species, function(x) {
#     
#     gest_result <- spatstat::Gest(X = x, correction = "none")
#     
#     pcf_result <- onpoint::estimate_pcf_fast(x,
#                                              correction = "none",
#                                              method = "c",
#                                              spar = 0.5)
#     
#     gest_df <- as.data.frame(gest_result) # conver to df
#     
#     names(gest_df)[3] <- "x_r" # unique col names
#     
#     gest_df$summary_function <- "Nearest Neighbour Distance Function G(r)" # name of method
#     
#     pcf_df <- as.data.frame(pcf_result) # convert to df
#     
#     names(pcf_df)[3] <- "x_r" # unique col names
#     
#     pcf_df$summary_function <- "Pair Correlation Function g(r)" # name of method
#     
#     dplyr::bind_rows(gest_df, pcf_df) # combine to one df    
#   }, .id = "type")
# 
#   
#   # classify all observed and all randomized repetitions identical
#   summary_stats <- dplyr::mutate(summary_stats, type = dplyr::case_when(type == "observed" ~ "observed",
#                                                                         TRUE ~ "randomized")) %>%
#     dplyr::group_by(summary_function, r, type) %>% 
#     dplyr::summarise(lo = stats::quantile(x_r, probs = 0.025),
#                      hi = stats::quantile(x_r, probs = 0.975),
#                      x_r = mean(x_r),
#                      theo = mean(theo))
# }, .id = "species")
# 
# summary_stats_species <- dplyr::mutate(summary_stats_species, 
#                                        species = factor(species, 
#                                                         levels = c("beech", "ash", 
#                                                                    "hornbeam", "sycamore", 
#                                                                    "others"), 
#                                                         labels = c("F. sylvatica", "F. exelcsior ", 
#                                                                    "C. betulus ", "A. pseudoplatanus ", 
#                                                                    "others")))
# 
# ggplot_reconstruction_species <- ggplot() + 
#   geom_ribbon(data = dplyr::filter(summary_stats_species, type == "randomized"), 
#               aes(x = r, ymin = lo, ymax = hi), 
#               alpha = 0.3, col = "grey") +
#   geom_line(data = dplyr::filter(summary_stats_species, type == "observed"),
#             aes(x = r, y = x_r), col = "black") +
#   labs(x = "r [m]", y = "f(r)") + 
#   facet_wrap(~ species + summary_function, scales = "free", nrow = 5, ncol = 2) + 
#   theme_bw(base_size = 15)
# 
# suppoRt::save_ggplot(plot = ggplot_reconstruction_species, 
#                      path = paste0(getwd(), "/2_Real_world_data/4_Figures/Appendix"), 
#                      filename = "ggplot_reconstruction_species.png", 
#                      width = 210, height = 297, units = "mm", 
#                      dpi = 300)
