#### Results simulation study #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages and data ####

# Packages #
source(paste0(getwd(), '/2_Functions/setup_packages.R'))

# Data #
results <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a1_', full.names = TRUE) %>%
  purrr::map(function(files) readr::read_rds(files))

names_result <- list.files(paste0(getwd(), '/4_Output'), pattern = 'a1_', full.names = FALSE)
names_short <- stringr::str_sub(names_result, start = 1, end = -5)
names_split <- stringr::str_split(names_short, pattern = "_", simplify = TRUE)
names_combined <- paste0(names_split[, 2], "_", names_split[, 3], 
                         "_", names_split[, 4], "_", names_split[, 5])

names(results) <- names_combined

#### 2. Preprocessing data ####

pattern <- "_100_100"

results_filter <- results[stringr::str_detect(names(results), pattern = pattern)]
names(results_filter)

results_summarised <- purrr::map_dfr(results_filter, function(current_result) {

  species_type_df <- dplyr::mutate(current_result,
                                   Species_type= dplyr::case_when(Species_code == 1 ~ "Complete spatial randomness (positive association)",
                                                                  Species_code == 2 ~ "Cluster process (positive association)",
                                                                  Species_code == 3 ~ "Complete spatial randomness (negative association)",
                                                                  Species_code == 4 ~ "Cluster process (negative association)"))
  
  species_type_df_grouped <- dplyr::group_by(species_type_df, Species_type, Variable)
  
  dplyr::summarise(species_type_df_grouped,
                   Correct_mean = mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct)) * 1.96),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct)) * 1.96),
                   
                   False_mean = mean(False),
                   False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                   False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False)))
                   )
}, .id = "Method")

results_summarised <- dplyr::mutate(results_summarised, Method = dplyr::case_when(Method == paste0("point_process", pattern) ~ "(I) Gamma test", 
                                                                                  Method == paste0("torus_translation", pattern) ~ "(II) Torus-translation test", 
                                                                                  Method == paste0("habitat_randomization", pattern) ~ "(III) Patch randomization test", 
                                                                                  Method == paste0("pattern_reconstruction", pattern) ~ "(IV) Pattern reconstruction"))

results_summarised$Method <- factor(results_summarised$Method, 
                                    levels = c("(I) Gamma test",               
                                               "(II) Torus-translation test", 
                                               "(III) Patch randomization test", 
                                               "(IV) Pattern reconstruction"))

results_summarised$Species_type <- factor(results_summarised$Species_type, 
                                          levels = c("Complete spatial randomness (positive association)", 
                                                     "Cluster process (positive association)",
                                                     "Complete spatial randomness (negative association)",
                                                     "Cluster process (negative association)"))


#### 3. Plotting data ####

colors_spec <- rev(RColorBrewer::brewer.pal(n = 4, name = "Spectral"))

strength_association_correct_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Variable, y = Correct_mean, col = Method, group = Method), size = 1.5) +
  geom_ribbon(aes(x = Variable, ymin = Correct_lo, ymax = Correct_hi, fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = colors_spec, name = '') +
  scale_colour_manual(values = colors_spec, name = '') +
  labs(x = expression(paste("Association strength ", alpha)), y = "Mean correct detections") +
  theme_classic(base_size = 40) + 
  theme(legend.position = "bottom")

strength_association_false_ggplot <- ggplot(data = results_summarised) +
  geom_line(aes(x = Variable, y = False_mean, 
                col = Method, group = Method), size = 1) +
  geom_ribbon(aes(x = Variable, ymin = False_lo, ymax = False_hi, 
                  fill = Method, group = Method), alpha = 0.3) +
  facet_wrap(~ Species_type, nrow = 2, ncol = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = colors_spec, name = '') +
  scale_colour_manual(values = colors_spec, name = '') +
  labs(x = expression(paste("Association strength ", alpha)), y = "Mean false detections") +
  theme_classic(base_size = 40) + 
  theme(legend.position = "bottom")


#### 4. Save plots ####

width <- 700
height <- 400
overwrite <- TRUE

UtilityFunctions::save_ggplot(plot = strength_association_correct_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "p1_association_strength_correct.png",
                              width = width, height = height, units = "mm", 
                              overwrite = overwrite)

UtilityFunctions::save_ggplot(plot = strength_association_false_ggplot, 
                              path = paste0(getwd(), "/6_Figures"),
                              filename = "p1_association_strength_false.png",
                              width = width, height = height, units = "mm",
                              overwrite = overwrite)

