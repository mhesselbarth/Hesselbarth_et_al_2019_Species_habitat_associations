###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Simulation study - Results ####

# Packages #
library(helpeR)
library(shar)
library(spatstat)
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
                species_type = dplyr::case_when(species_code == 1 ~ "CSR (positive association)",
                species_code == 2 ~ "Cluster process (positive association)",
                species_code == 3 ~ "CSR (negative association)",
                species_code == 4 ~ "Cluster process (negative association)")) %>%
    dplyr::group_by(species_type, variable) %>%
    dplyr::summarise(correct_mean = mean(correct),
                     correct_hi = mean(correct) + (stats::sd(correct, na.rm=T)/sqrt(length(correct))),
                     correct_lo = mean(correct) - (stats::sd(correct, na.rm=T)/sqrt(length(correct))),
                     
                     false_mean = mean(false),
                     false_hi = mean(false) + (stats::sd(false, na.rm=T)/sqrt(length(false))),
                     false_lo = mean(false) - (stats::sd(false, na.rm=T)/sqrt(length(false)))
                     )
  }, .id = "method")

# Rename method for nicer plotting
results <- dplyr::mutate(results, method = dplyr::case_when(method == "gamma_test" ~ "(I) Gamma test", 
                                                            method == "torus_translation" ~ "(II) Torus-translation test", 
                                                            method == "habitat_randomization" ~ "(III) Patch randomization test", 
                                                            method == "pattern_reconstruction" ~ "(IV) Pattern reconstruction"))

# convert the results col as factor
results$method <- factor(results$method, 
                         levels = c("(I) Gamma test",               
                                    "(II) Torus-translation test", 
                                    "(III) Patch randomization test", 
                                    "(IV) Pattern reconstruction"))

# convert species type col as factor
results$species_type <- factor(results$species_type, 
                               levels = c("CSR (positive association)", 
                                          "Cluster process (positive association)",
                                          "CSR (negative association)",
                                          "Cluster process (negative association)"))


#### 3. Plotting data ####

# plot correct detections
strength_association_correct_ggplot <- ggplot(data = results) +
  geom_line(aes(x = variable, y = correct_mean, col = method, group = method), size = 1.5) +
  geom_ribbon(aes(x = variable, ymin = correct_lo, ymax = correct_hi, fill = method, group = method), alpha = 0.3) +
  geom_hline(yintercept = 0.5, linetype = 2, col = "grey") + 
  facet_wrap(~ species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_fill_viridis_d(name = "") +
  scale_colour_viridis_d(name = "") +
  labs(x = expression(paste("Association strength ", alpha)), y = "Correct detections rate") +
  guides(fill = guide_legend(ncol = 2, nrow = 2)) + 
  theme_classic(base_size = 28.5) + 
  theme(legend.position = "bottom", 
        panel.spacing.x = unit(25, "mm"), 
        panel.spacing.y = unit(15, "mm"), 
        legend.key.size = unit(15, "mm"))

# plot wrong detections
strength_association_false_ggplot <- ggplot(data = results) +
  geom_line(aes(x = variable, y = false_mean, col = method, group = method), size = 1) +
  geom_ribbon(aes(x = variable, ymin = false_lo, ymax = false_hi, fill = method, group = method), alpha = 0.3) +
  facet_wrap(~ species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_viridis_d(name = "") +
  scale_colour_viridis_d(name = "") +
  labs(x = expression(paste("Association strength ", alpha)), y = "False detections rate") +
  guides(fill = guide_legend(ncol = 2, nrow = 2)) + 
  theme_classic(base_size = 28.5) + 
  theme(legend.position = "bottom", 
        panel.spacing.x = unit(50, "mm"), 
        panel.spacing.y = unit(25, "mm"), 
        legend.key.size = unit(25, "mm"))

#### 4. Save plots ####

# set parameters for plot
width <- 400

height <- 300

overwrite <- FALSE

# save plots
helpeR::save_ggplot(plot = strength_association_correct_ggplot, 
                    path = "1_Simulation_study/4_Figures/",
                    filename = "simulation_study_correct.png",
                    width = width, height = height, units = "mm", 
                    overwrite = overwrite)

helpeR::save_ggplot(plot = strength_association_false_ggplot, 
                    path = "1_Simulation_study/4_Figures/",
                    filename = "simulation_study_false.png",
                    width = width, height = height, units = "mm",
                    overwrite = overwrite)

