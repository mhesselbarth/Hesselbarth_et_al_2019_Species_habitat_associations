#### Results simulation study - Neutral species #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

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
habitat_randomization <- readRDS(paste0(results, "/neutral_habitat_randomization.rds"))
torus_translation <- readRDS(paste0(results, "/neutral_torus_translation.rds"))
point_process <- readRDS(paste0(results, "/neutral_point_process.rds"))

habitat_randomization$Method <- "Randomized\nhabitats"
torus_translation$Method <- "Torus\ntranslation"
point_process$Method <- "Gamma\ntest"

overall_neutral_species <- rbind(habitat_randomization, 
                                 torus_translation, 
                                 point_process) %>%
  dplyr::mutate(Method=factor(Method, levels=c("Gamma\ntest", 
                                               "Torus\ntranslation", 
                                               "Randomized\nhabitats")),
                Species_type=factor(dplyr::case_when(Species==1 ~ "Poisson process",
                                                     Species==2 ~ "Thomas process"))) %>%
  dplyr::group_by(Species_type, Method) %>%
  dplyr::summarise(Correct_mean=mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))))

overall_neutral_species

neutral_species_ggplot <- ggplot(data=overall_neutral_species, 
                               aes(x=Method, y=Correct_mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Correct_lo, ymax=Correct_hi), width=.2,
                position=position_dodge(.9)) + 
  facet_wrap(~Species_type) +
  labs(x="", y="Mean correct detections") +
  theme_bw() 

UtilityFunctions::Save.Function.ggplot(plot=neutral_species_ggplot,
       path=figures, filename="Neutral_species.png",
       width=145, height=120, units="mm", dpi=500)
