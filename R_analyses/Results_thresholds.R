#### Results simulation study - Threshold #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### Install packages ####
# toc <- "cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec"
# devtools::install_github("mhesselbarth/SHAR", auth_token=toc, quiet=F)
# devtools::install_github("mhesselbarth/UtilityFunctions", auth_token=toc, quiet=F)
# rm(toc)

#### Import packages and data ####
# Packages #
library(ggplot2)
library(UtilityFunctions)
library(SHAR)
library(tidyverse)
library(viridis)

# Working directories
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")

# Import data #
habitat_randomization <- readRDS(paste0(results, "/thresholds_habitat_randomization.rds"))
torus_translation <- readRDS(paste0(results, "/thresholds_torus_translation.rds"))
point_process <- readRDS(paste0(results, "/thresholds_point_process.rds"))

#### Preprocessing data ####
habitat_randomization$Method <- "Randomized habitats"
torus_translation$Method <- "Torus translation"
point_process$Method <- "Gamma test"

overall_thresholds <- dplyr::bind_rows(habitat_randomization, 
                                                 torus_translation, 
                                                 point_process) %>%
  dplyr::mutate(Method=factor(Method, levels=c("Gamma test", 
                                               "Torus translation", 
                                               "Randomized habitats")),
                Species_type=factor(dplyr::case_when(Species==1 ~ "Poisson process (positive)",
                                                     Species==2 ~ "Thomas process (positive)",
                                                     Species==3 ~ "Poisson procces (negative)",
                                                     Species==4 ~ "Thomas process (negative)"),
                                    levels=c("Poisson process (positive)", "Poisson procces (negative)",
                                             "Thomas process (positive)", "Thomas process (negative)"))) %>%
  dplyr::group_by(Species_type, Threshold, Method) %>%
  dplyr::summarise(Alpha=mean(Alpha),
                   Correct_mean = mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   False_mean = mean(False),
                   False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                   False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False))))


overall_thresholds %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Alpha_mean=mean(Alpha))

thresholds_correct_ggplot <- ggplot(data=overall_thresholds, 
                                    aes(x=factor(Threshold), y=Correct_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Correct_lo, ymax=Correct_hi), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Species_type) +
  scale_fill_viridis(discrete=T, name='') +
  labs(x=expression(paste("Approximated significance value ", rho)), y="Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

# UtilityFunctions::Save.Function.ggplot(plot=thresholds_correct_ggplot,
#                                        path=figures, 
#                                        filename="Threshold_correct.png",
#                                        width=145, height=120, units="mm", dpi=500)

thresholds_false_ggplot <- ggplot(data=overall_thresholds, 
                                    aes(x=factor(Threshold), y=False_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=False_lo, ymax=False_hi), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Species_type) +
  scale_fill_viridis(discrete=T, name='') +
  labs(x=expression(paste("Approximated significance value ", rho)), y="Mean false detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

# UtilityFunctions::Save.Function.ggplot(plot=thresholds_false_ggplot,
#                                        path=figures, 
#                                        filename="Threshold_false.png",
#                                        width=145, height=120, units="mm", dpi=500)