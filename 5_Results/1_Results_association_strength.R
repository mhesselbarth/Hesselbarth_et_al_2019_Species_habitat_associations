#### Results simulation study #### 

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

# Data #
results <- list.files(paste0(getwd(), '/4_Output'), pattern = '1_', full.names = TRUE) %>%
  purrr::map(function(x) readr::read_rds(x))

names_result <- list.files(paste0(getwd(), '/4_Output'), pattern = '1_', full.names = FALSE)
names_result_short <- stringr::str_sub(names_result, start = 3, end = -7)

names(results) <- names_result_short

alpha_sequence <- readr::read_rds(paste0(getwd(), '/4_Output/alpha_sequence.rds'))


#### 2. Preprocessing data ####

results_summarised <- purrr::map_dfr(results, function(current_result) {
  
  names_split <- stringr::str_split(current_result$Species, "_", simplify = TRUE)[, 1:2]
  names_combined <- paste0(names_split[, 1], "_", names_split[, 2])
  
  current_result$Type <- names_combined
  
  current_result_grouped <- dplyr::group_by(current_result, Type, Association_strength)

  current_result_means <- dplyr::summarise(current_result_grouped,
                                           Correct_mean = mean(Correct),
                                           Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                                           Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                                           False_mean = mean(False),
                                           False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                                           False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False))))
}, .id = "Method")

results_summarised$Method <- as.factor(results_summarised$Method)
results_summarised$Type <- as.factor(results_summarised$Type)
results_summarised$Association_strength <- alpha_sequence


#### 3. Plotting data ####

strength_association_correct_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Association_strength, y = Correct_mean, 
                col = Method, group = Method), size = 0.75) +
  geom_ribbon(aes(x = Association_strength, ymin = Correct_lo, ymax = Correct_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = alpha_sequence) +
  scale_fill_viridis_d(name = '') +
  scale_colour_viridis_d(name = '') +
  labs(x = expression(paste("Association strength ", alpha)), y = "Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

strength_association_false_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Association_strength, y = False_mean, 
                col = Method, group = Method), size = 0.75) +
  geom_ribbon(aes(x = Association_strength, ymin = False_lo, ymax = False_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = alpha_sequence) +
  scale_fill_viridis_d(name = '') +
  scale_colour_viridis_d(name = '') +
  labs(x = expression(paste("Association strength ", alpha)), y = "Mean false detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")


#### 4. Save plots ####

UtilityFunctions::save_ggplot(plot = strength_association_correct_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "1_association_strength_correct.png",
                              width = 145, height = 120, units = "mm", dpi = 500, 
                              overwrite = FALSE)

UtilityFunctions::save_ggplot(plot = strength_association_false_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "1_association_strength_false.png",
                              width = 145, height = 120, units = "mm", dpi = 500, 
                              overwrite = FALSE)

