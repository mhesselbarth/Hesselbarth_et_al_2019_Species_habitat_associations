#### Results simulation study #### 

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

# Working directories

# Import data #
habitat_randomization <- readRDS(paste0(results, "/strength_association_habitat_randomization.rds"))
torus_translation <- readRDS(paste0(results, "/strength_association_torus_translation.rds"))
point_process <- readRDS(paste0(results, "/strength_association_point_process.rds"))

#### Preprocessing data ####
habitat_randomization$Method <- "Randomized habitats"
torus_translation$Method <- "Torus translation"
point_process$Method <- "Gamma test"

overall_strength_association <- dplyr::bind_rows(habitat_randomization, 
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
  dplyr::group_by(Species_type, Alpha, Method) %>%
  dplyr::summarise(Correct_mean = mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   False_mean = mean(False),
                   False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                   False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False))))

#### Plotting data ####
strength_association_correct_ggplot <- ggplot(data=overall_strength_association) +
  geom_line(aes(x=Alpha, y=Correct_mean, col=Method, group=Method), size=0.75) +
  geom_ribbon(aes(x=Alpha, ymin=Correct_lo, ymax=Correct_hi, fill=Method, group=Method), alpha=0.3) +
  facet_wrap(~Species_type, nrow=2, ncol=2) + 
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  scale_x_continuous(limits=c(0.025, 0.75), breaks=seq(0.05, 0.75, 0.1)) +
  scale_fill_viridis(discrete=T, name='') +
  scale_color_viridis(discrete=T, name='') +
  labs(x=expression(paste("Association strength ", alpha)), y="Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")
UtilityFunctions::Save.Function.ggplot(plot=strength_association_correct_ggplot,
                                       path=figures, 
                                       filename="Association_strength_correct.png",
                                       width=145, height=120, units="mm", dpi=500)

strength_association_false_ggplot <- ggplot(data=overall_strength_association) +
  geom_line(aes(x=Alpha, y=False_mean, col=Method, group=Method), size=0.75) +
  geom_ribbon(aes(x=Alpha, ymin=False_lo, ymax=False_hi, fill=Method, group=Method), alpha=0.3) +
  facet_wrap(~Species_type, nrow=2, ncol=2) + 
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  scale_x_continuous(limits=c(0.025, 0.75), breaks=seq(0.05, 0.75, 0.1)) +
  scale_fill_viridis(discrete=T, name='') +
  scale_color_viridis(discrete=T, name='') +
  labs(x=expression(paste("Association strength ", alpha)), y="Mean false detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")
UtilityFunctions::Save.Function.ggplot(plot=strength_association_false_ggplot,
       path=figures, filename="Association_strength_false.png",
       width=145, height=120, units="mm", dpi=500)
