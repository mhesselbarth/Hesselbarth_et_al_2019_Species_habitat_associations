#### Results simulation study - Neutral species #### 

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
results <- list.files(paste0(getwd(), '/4_Output'), pattern = '5_', full.names = TRUE) %>%
  purrr::map(function(x) readr::read_rds(x))

names_result <- list.files(paste0(getwd(), '/4_Output'), pattern = '5_', full.names = FALSE)
names_split <- stringr::str_split(names_result, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3])

names(results) <- names_combined

#### 2. Preprocessing data ####

results_summarised <- purrr::map_dfr(results, function(current_result) {
  
  current_result_grouped <- dplyr::group_by(current_result, Species)
  
  current_result_means <- dplyr::summarise(current_result_grouped,
                                           n = n(),
                                           Correct_mean = mean(Correct / n),
                                           Correct_hi = Correct_mean + (stats::sd(Correct / n, na.rm=T)/sqrt(n)),
                                           Correct_lo = Correct_mean - (stats::sd(Correct / n, na.rm=T)/sqrt(n)),
                                           False_mean = mean(False / n),
                                           False_hi = False_mean + (stats::sd(False / n, na.rm=T)/sqrt(n)),
                                           False_lo = False_mean - (stats::sd(False / n, na.rm=T)/sqrt(n)))
}, .id = "Method")

results_summarised$Method <- as.factor(results_summarised$Method)
results_summarised$Species <- as.factor(results_summarised$Species)

#### 3. Plotting data ####

neutral_species_correct_ggplot <- ggplot(data = results_summarised) +
  geom_bar(aes(x = Method, y = Correct_mean), 
           stat="identity") +
  geom_errorbar(aes(x = Method,
                    ymin = Correct_lo, ymax = Correct_hi),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Species) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x="", y="Mean correct detections") +
  theme_bw() 

neutral_species_false_ggplot <- ggplot(data = results_summarised) +
  geom_bar(aes(x = Method, y = False_mean), 
           stat="identity") +
  geom_errorbar(aes(x = Method,
                    ymin = False_lo, ymax = False_hi),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Species) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x="", y="Mean false detections") +
  theme_bw() 

#### 4. Save plots ####

UtilityFunctions::save_ggplot(plot = neutral_species_correct_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "4_significane_threshold_correct.png",
                              width = 145, height = 120, units = "mm", dpi = 500, 
                              overwrite = FALSE)

UtilityFunctions::save_ggplot(plot = neutral_species_false_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "4_significane_threshold_false.png",
                              width = 145, height = 120, units = "mm", dpi = 500, 
                              overwrite = FALSE)

