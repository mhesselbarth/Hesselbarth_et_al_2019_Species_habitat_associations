#### Results simulation study - Number null model#### 

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
habitat_randomization <- readRDS(paste0(results, "/number_null_model_habitat_randomization.rds"))
point_process <- readRDS(paste0(results, "/number_null_model_point_process.rds"))

habitat_randomization$Method <- "Randomized habitats"
point_process$Method <- "Gamma test"

overall_number_null_model <- rbind(habitat_randomization, 
                                 point_process) %>%
  dplyr::mutate(Method=factor(Method, levels=c("Gamma test", 
                                               "Randomized habitats")),
                Species_type=factor(dplyr::case_when(Species==1 ~ "Poisson process (positive)",
                                                     Species==2 ~ "Thomas process (positive)",
                                                     Species==3 ~ "Poisson process (negative)",
                                                     Species==4 ~ "Thomas process (negative)"),
                                    levels=c("Poisson process (positive)", "Poisson process (negative)",
                                             "Thomas process (positive)", "Thomas process (negative)"))) %>%
  dplyr::group_by(Species_type, Null_model, Method) %>%
  dplyr::summarise(Alpha= mean(Alpha),
                   Correct_mean = mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   False_mean = mean(False),
                   False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                   False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False))))

overall_number_null_model %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Alpha_mean=mean(Alpha))

number_habitats_correct_ggplot <- ggplot(data=overall_number_null_model, 
                               aes(x=factor(Null_model), y=Correct_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Correct_lo, ymax=Correct_hi), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Species_type) +
  scale_fill_viridis(discrete=T, name="") +
  labs(x=expression(paste("Number of null model data ", nu)), y="Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

UtilityFunctions::Save.Function.ggplot(plot=plot_number_habitats_correct,
       path=figures, filename="Number_null_model_correct.png",
       width=145, height=120, units="mm", dpi=500)

number_habitats_false_ggplot <- ggplot(data=overall_number_null_model, 
                                       aes(x=factor(Null_model), y=False_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=False_lo, ymax=False_hi), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Species_type) +
  scale_fill_viridis(discrete=T, name="") +
  labs(x=expression(paste("Number of null model data ", nu)), y="Mean false detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

number_habitats_false_thomas_ggplot <- ggplot(data=dplyr::filter(overall_number_null_model, 
                                                               Species_type=='Thomas process (negative)'), 
                                       aes(x=factor(Null_model), y=False_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=False_lo, ymax=False_hi), width=.2,
                position=position_dodge(.9)) +
  scale_fill_viridis(discrete=T, name="") +
  labs(x=expression(paste("Number of null model data ", nu)), y="Mean false detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

UtilityFunctions::Save.Function.ggplot(plot=plot_number_habitats_false_thomas,
       path=figures, filename="Number_null_model_false_Thomas.png",
       width=145, height=120, units="mm", dpi=500)

