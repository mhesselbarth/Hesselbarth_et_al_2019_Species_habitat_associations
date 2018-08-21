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
results <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a1_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

names_result <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a1_', full.names = FALSE)
names_split <- stringr::str_split(names_result, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3])

names(results) <- names_combined

alpha_sequence <- readr::read_rds(paste0(getwd(), '/4_Output/alpha_sequence.rds'))

#### 2. Preprocessing data ####

results_summarised <- purrr::map_dfr(results, function(current_result) {

  species_type_df <- dplyr::mutate(current_result,
                                   Species_type=factor(
                                     dplyr::case_when(Species_code == 1 ~ "Poisson process (positive association)",
                                                      Species_code == 2 ~ "Thomas process (positive association)",
                                                      Species_code == 3 ~ "Poisson procces (negative association)",
                                                      Species_code == 4 ~ "Thomas process (negative association)")))
  
  species_type_df_grouped <- dplyr::group_by(species_type_df, Species_type, Variable)
  
  dplyr::summarise(species_type_df_grouped,
                   Correct_mean = mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   
                   False_mean = mean(False),
                   False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                   False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False)))
                   )
}, .id = "Method")

results_summarised <- dplyr::mutate(results_summarised, Method = dplyr::case_when(Method == "point_process" ~ "(I) Gamma test", 
                                                                                  Method == "torus_translation" ~ "(II) Torus-translation test"))

#### 3. Plotting data ####

strength_association_correct_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Variable, y = Correct_mean, 
                col = Method, group = Method), size = 1) +
  geom_ribbon(aes(x = Variable, ymin = Correct_lo, ymax = Correct_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_viridis_d(name = '') +
  scale_colour_viridis_d(name = '') +
  labs(x = expression(paste("Association strength ", alpha)), y = "Mean correct detections") +
  theme_bw(base_size = 25) + 
  theme(legend.position = "bottom")

strength_association_false_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Variable, y = False_mean, 
                col = Method, group = Method), size = 0.75) +
  geom_ribbon(aes(x = Variable, ymin = False_lo, ymax = False_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_viridis_d(name = '') +
  scale_colour_viridis_d(name = '') +
  labs(x = expression(paste("Association strength ", alpha)), y = "Mean false detections") +
  theme_bw(base_size = 18.5) + 
  theme(legend.position = "bottom")


#### 4. Save plots ####

width = 650
height = 250
dpi = 900

UtilityFunctions::save_ggplot(plot = strength_association_correct_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "a1_association_strength_correct.png",
                              width = width, height = height, units = "mm", dpi = dpi, 
                              overwrite = TRUE)

UtilityFunctions::save_ggplot(plot = strength_association_false_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "a1_association_strength_false.png",
                              width = width, height = height, units = "mm", dpi = dpi, 
                              overwrite = FALSE)

