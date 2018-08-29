#### Results simulation study - Number null models #### 

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
results <- list.files(paste0(getwd(), '/4_Output'), pattern = '3_', full.names = TRUE) %>%
  purrr::map(function(x) readr::read_rds(x))

names_result <- list.files(paste0(getwd(), '/4_Output'), pattern = '3_', full.names = FALSE)
names_split <- stringr::str_split(names_result, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3])

names(results) <- names_combined

number_null_models <- readr::read_rds(paste0(getwd(), '/4_Output/number_null_models.rds'))

#### 2. Preprocessing data ####

results_summarised <- purrr::map_dfr(results, function(current_result) {
  
  names_split <- stringr::str_split(current_result$Species, "_", simplify = TRUE)[, 1:2]
  names_combined <- paste0(names_split[, 1], "_", names_split[, 2])
  
  current_result$Type <- names_combined
  
  current_result_grouped <- dplyr::group_by(current_result, Type, Number_null_models)
  
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
results_summarised$Number_null_models <- number_null_models

#### 3. Plotting data ####

number_null_models_correct_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Number_null_models, y = Correct_mean, 
                col = Method, group = Method), size = 0.75) +
  geom_ribbon(aes(x = Number_null_models, ymin = Correct_lo, ymax = Correct_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = number_null_models) +
  scale_fill_viridis_d(name = '') +
  scale_colour_viridis_d(name = '') +
  labs(x = expression(paste("Number null models", gamma)), y = "Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

number_null_models_false_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Number_null_models, y = False_mean, 
                col = Method, group = Method), size = 0.75) +
  geom_ribbon(aes(x = Number_null_models, ymin = False_lo, ymax = False_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = number_null_models) +
  scale_fill_viridis_d(name = '') +
  scale_colour_viridis_d(name = '') +
  labs(x = expression(paste("Number null models", gamma)), y = "Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")


#### 4. Save plots ####

UtilityFunctions::save_ggplot(plot = number_null_models_correct_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "3_number_null_models_correct.png",
                              width = 145, height = 120, units = "mm", dpi = 500, 
                              overwrite = FALSE)

UtilityFunctions::save_ggplot(plot = number_null_models_false_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "3_number_null_models_false.png",
                              width = 145, height = 120, units = "mm", dpi = 500, 
                              overwrite = FALSE)

