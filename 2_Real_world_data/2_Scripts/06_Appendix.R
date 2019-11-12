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

#### Set parameters ####
base_size <- 12.5
dpi = 300
width = 210 
height = 297
units = "mm"
overwrite = FALSE

nsim = 199

#### Forest structure ####

# Species abundance
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
  ggplot2::theme_bw(base_size = base_size) + 
  ggplot2::theme(legend.position = "bottom")

suppoRt::save_ggplot(plot = species_abundance, 
                    path = "2_Real_world_data/4_Figures/Appendix/", 
                    filename = "species_abundance.png", 
                    dpi = dpi, width = width, height = height * 1/3, units = units, 
                    overwrite = overwrite)

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
  ggplot2::labs(x = "dbh [cm]", y = "Count") + 
  ggplot2::theme_bw(base_size = base_size) + 
  ggplot2::theme(legend.position = "bottom")

suppoRt::save_ggplot(plot = dbh_distribution, 
                    path = "2_Real_world_data/4_Figures/Appendix/", 
                    filename = "dbh_distribution.png", 
                    dpi = dpi, width = width, height = height * 1/3, units = units, 
                    overwrite = overwrite)

#### PPA ####
# # calculate PCF #
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
                                   ylab = expression(italic("g(r)")), xlab = "r [m]", 
                                   legend_position = "none",
                                   quantum_position = 0, 
                                   base_size = base_size)

plot_beech <- onpoint::plot_quantums(envelope_beech, title = expression(italic("F. sylvatica")), 
                                     legend_position = "none", 
                                     ylab = expression(italic("g(r)")), xlab = "r [m]", 
                                     base_size = base_size)

plot_ash <- onpoint::plot_quantums(envelope_ash, title = expression(italic("F. excelsior")), 
                                   legend_position = "none", 
                                   ylab = expression(italic("g(r)")), xlab = "r [m]", 
                                   base_size = base_size)

plot_hornbeam <- onpoint::plot_quantums(envelope_hornbeam, title = expression(italic("C. betulus")), 
                                        legend_position = "none", 
                                        ylab = expression(italic("g(r)")), xlab = "r [m]", 
                                        base_size = base_size)

plot_sycamore <- onpoint::plot_quantums(envelope_sycamore, title = expression(italic("A. pseudoplatanus")), 
                                        legend_position = "none", 
                                        ylab = expression(italic("g(r)")), xlab = "r [m]", 
                                        base_size = base_size)

plot_others <- onpoint::plot_quantums(envelope_others, title = "others", 
                                      legend_position = "none", 
                                      ylab = expression(italic("g(r)")), xlab = "r [m]", 
                                      base_size = base_size)

# combine to one large plot
plot_overall <- plot_all + {plot_beech + plot_ash + plot_hornbeam + plot_sycamore + plot_others} + plot_layout(nrow = 2)

# save plot
suppoRt::save_ggplot(plot = plot_overall, filename = "pcf_overall.png", 
                     path = "2_Real_world_data/4_Figures/Appendix/", 
                     width = width, height = height * 3/4, units = "mm", 
                     dpi = dpi, overwrite = overwrite)

#### Random labeling 
# beech_full <- spatstat::subset.ppp(pattern_2007, species == "beech", select = type)
# 
# beech_rl <- spatstat::rlabel(beech_full, nsim = nsim)
# 
# envelope_beech_dl <- spatstat::envelope(beech_full, fun = markconnect,
#                                         nsim = nsim, simulate = beech_rl,
#                                         funargs = list(i = "dead", j = "living",
#                                                        correction = "Ripley"))
# 
# envelope_beech_ld <- spatstat::envelope(beech_full, fun = markconnect,
#                                         nsim = nsim, simulate = beech_rl,
#                                         funargs = list(i = "living", j = "dead",
#                                                        correction = "Ripley"))
# 
# envelope_beech_dd <- spatstat::envelope(beech_full, fun = markconnect,
#                                         nsim = nsim, simulate = beech_rl,
#                                         funargs = list(i = "dead", j = "dead",
#                                                        correction = "Ripley"))
# 
# envelope_beech_ll <- spatstat::envelope(beech_full, fun = markconnect,
#                                         nsim = nsim, simulate = beech_rl,
#                                         funargs = list(i = "living", j = "living",
#                                                        correction = "Ripley"))
# 
# # save results of envelope function #
# suppoRt::save_rds(object = envelope_beech_dl,
#                   filename = "envelope_beech_dl.rds",
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_beech_ld,
#                   filename = "envelope_beech_ld.rds",
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_beech_dd,
#                   filename = "envelope_beech_dd.rds",
#                   path = "2_Real_world_data/3_Results/Appendix/")
# 
# suppoRt::save_rds(object = envelope_beech_ll,
#                   filename = "envelope_beech_ll.rds",
#                   path = "2_Real_world_data/3_Results/Appendix/")

# load envelope results #
envelope_beech_dl <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_beech_dl.rds")
envelope_beech_ld <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_beech_ld.rds")
envelope_beech_dd <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_beech_dd.rds")
envelope_beech_ll <- readr::read_rds("2_Real_world_data/3_Results/Appendix/envelope_beech_ll.rds")

plot_beech_dl <- onpoint::plot_quantums(envelope_beech_dl, title = "",
                                        ylab = expression(italic(paste(rho["dead,living"], "(r)"))), xlab = "r [m]", 
                                        labels = c("Positive correlation", "No correlation", "Negative correlation"), 
                                        legend_position = "none",
                                        base_size = base_size)

plot_beech_ld <- onpoint::plot_quantums(envelope_beech_ld, title = "",
                                        ylab = expression(italic(paste(rho["living,dead"], "(r)"))), xlab = "r [m]", 
                                        labels = c("Positive correlation", "No correlation", "Negative correlation"), 
                                        legend_position = "none",
                                        base_size = base_size)

plot_beech_dd <- onpoint::plot_quantums(envelope_beech_dd, title = "",
                                        ylab = expression(italic(paste(rho["dead,dead"], "(r)"))), xlab = "r [m]", 
                                        labels = c("Positive correlation", "No correlation", "Negative correlation"), 
                                        legend_position = "none",
                                        base_size = base_size)

plot_beech_ll <- onpoint::plot_quantums(envelope_beech_ll, title = "",
                                        ylab = expression(italic(paste(rho["living,living"], "(r)"))), xlab = "r [m]", 
                                        labels = c("Positive correlation", "No correlation", "Negative correlation"), 
                                        legend_position = "none",
                                        base_size = base_size)

plot_labelling_overall <- plot_beech_ll + plot_beech_ld + 
  plot_beech_dl + plot_beech_dd + plot_layout(nrow = 2, ncol = 2)

suppoRt::save_ggplot(plot = plot_labelling_overall, filename = "plot_labelling_overall.png",
                    path = "2_Real_world_data/4_Figures/Appendix/",
                    dpi = dpi, width = width, height = height * 1/2, units = "mm", 
                    overwrite = overwrite)
