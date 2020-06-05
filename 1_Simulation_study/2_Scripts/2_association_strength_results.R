###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Simulation study - Results ####

# Packages #
library(shar)
library(spatstat)
library(suppoRt) # devtools::install_github("mhesselbarth/suppoRt")
library(tidyverse)

# import results simulation study
results <- list.files(path = "1_Simulation_study/3_Results/", 
                      pattern = "*50_runs*", full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

# get names of results
names_result <- list.files(path = "1_Simulation_study/3_Results/", 
                           pattern = "*50_runs*", full.names = FALSE)

# add names to result
names(results) <- stringr::str_sub(names_result, start = 1, end = -13)

#### 2. Preprocessing data ####

# summarise the results
results <- purrr::map_dfr(results, function(current_result) {
  
  dplyr::mutate(current_result,
                species_type = dplyr::case_when(
                  species_code == 1 ~ "(a) CSR (positive association)",
                  species_code == 2 ~ "(b) Cluster process (positive association)",
                  species_code == 3 ~ "(c) CSR (negative association)",
                  species_code == 4 ~ "(d) Cluster process (negative association)")) %>%
    dplyr::group_by(species_type, variable) %>%
    dplyr::summarise(correct_mean = mean(correct),
                     correct_hi = mean(correct) + (stats::sd(correct, na.rm = TRUE) / sqrt(length(correct))),
                     correct_lo = mean(correct) - (stats::sd(correct, na.rm = TRUE) / sqrt(length(correct))),
                     
                     false_mean = mean(false),
                     false_hi = mean(false) + (stats::sd(false, na.rm = TRUE) / sqrt(length(false))),
                     false_lo = mean(false) - (stats::sd(false, na.rm = TRUE) / sqrt(length(false))),
                     .groups = "keep")
  }, .id = "method")

# Rename method for nicer plotting
results <- dplyr::mutate(results, method = dplyr::case_when(method == "gamma_test" ~ "(I) gamma test", 
                                                            method == "torus_translation" ~ "(II) Torus-translation test", 
                                                            method == "habitat_randomization" ~ "(III) Patch randomization test", 
                                                            method == "pattern_reconstruction" ~ "(IV) Pattern reconstruction"))

# convert the results col as factor
results$method <- factor(results$method, 
                         levels = c("(I) gamma test",               
                                    "(II) Torus-translation test", 
                                    "(III) Patch randomization test", 
                                    "(IV) Pattern reconstruction"))

# convert species type col as factor
results$species_type <- factor(results$species_type, 
                               levels = c("(a) CSR (positive association)", 
                                          "(b) Cluster process (positive association)",
                                          "(c) CSR (negative association)",
                                          "(d) Cluster process (negative association)"))


#### 3. Plotting data ####

# plot correct detections
strength_association_correct_ggplot <- ggplot(data = results) +
  geom_line(aes(x = variable, y = correct_mean, col = method, group = method), size = 1) +
  geom_ribbon(aes(x = variable, ymin = correct_lo, ymax = correct_hi, fill = method, group = method), alpha = 0.25) +
  geom_hline(yintercept = 0.5, linetype = 3, col = "grey", size = 0.75) + 
  facet_wrap(~ species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(0.05, 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_viridis_d(name = "", option = "C") +
  scale_colour_viridis_d(name = "", option = "C") +
  labs(x = expression(paste("Association strength ", alpha)), y = "Correct detection rate") +
  guides(fill = guide_legend(ncol = 2, nrow = 2)) + 
  theme_classic(base_size = 12.5) + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(12.5, "mm"))

# plot wrong detections
strength_association_false_ggplot <- ggplot(data = results) +
  geom_line(aes(x = variable, y = false_mean, col = method, group = method), size = 1) +
  geom_ribbon(aes(x = variable, ymin = false_lo, ymax = false_hi, fill = method, group = method), alpha = 0.25) +
  geom_hline(yintercept = 0.5, linetype = 3, col = "grey", size = 0.75) + 
  facet_wrap(~ species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(0.05, 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_viridis_d(name = "", option = "C") +
  scale_colour_viridis_d(name = "", option = "C") +
  labs(x = expression(paste("Association strength ", alpha)), y = "False detection rate") +
  guides(fill = guide_legend(ncol = 2, nrow = 2)) + 
  theme_classic(base_size = 12.5) + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(12.5, "mm"))

# plot together
overall_ggplot <- ggplot(data = results) +
  geom_ribbon(aes(x = variable, ymin = correct_lo, ymax = correct_hi, fill = method, group = method), alpha = 0.25) +
  geom_ribbon(aes(x = variable, ymin = false_lo, ymax = false_hi, fill = method, group = method), alpha = 0.25) +
  geom_line(aes(x = variable, y = correct_mean, col = method, group = method, linetype = "Correct"), size = 1) +
  geom_line(aes(x = variable, y = false_mean, col = method, group = method, linetype = "False"), size = 1) +
  geom_hline(yintercept = 0.5, linetype = 3, col = "grey", size = 0.75) + 
  facet_wrap(~ species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(0.05, 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_viridis_d(name = "", option = "C") +
  scale_colour_viridis_d(name = "", option = "C") +
  scale_linetype_manual(name = "", values = c("Correct" = 1, "False" = 2)) +
  labs(x = expression(paste("Association strength ", alpha)), y = "Detection rate") +
  guides(fill = guide_legend(ncol = 2, nrow = 2), 
         linetype = guide_legend(nrow = 2)) + 
  theme_classic(base_size = 12.5) + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(12.5, "mm"))

#### 4. Save plots ####

# set parameters for plot
width <- 210
height <- 297 * 1/2
units <- "mm"
dpi <- 300

overwrite <- FALSE

# save plots
suppoRt::save_ggplot(plot = strength_association_correct_ggplot, 
                     path = "1_Simulation_study/4_Figures",
                     filename = "simulation_study_correct.png",
                     width = width, height = height, units = units,
                     dpi = dpi,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = strength_association_false_ggplot, 
                     path = "1_Simulation_study/4_Figures",
                     filename = "simulation_study_false.png",
                     width = width, height = height, units = units,
                     dpi = dpi,
                     overwrite = overwrite)

suppoRt::save_ggplot(plot = overall_ggplot, 
                     path = "1_Simulation_study/4_Figures",
                     filename = "overall_ggplot.png",
                     width = width, height = height, units = units,
                     dpi = dpi,
                     overwrite = overwrite)
