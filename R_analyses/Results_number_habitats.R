#### Results simulation study - Number habitats #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### Install packages ####
# toc <- "cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec"
# devtools::install_github("mhesselbarth/SHAR", auth_token=toc, quiet=T)
# devtools::install_github("mhesselbarth/UtilityFunctions", auth_token=toc, quiet=T)
# rm(toc)

#### Import packages and data ####
# Packages #
library(ggplot2)
library(magrittr)
library(UtilityFunctions)
library(SHAR)
library(viridis)
library(patchwork)

# Working directories
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")

# Import data #
habitat_randomization <- readRDS(paste0(results, "/number_habitats_habitat_randomization.rds"))
torus_translation <- readRDS(paste0(results, "/number_habitats_torus_translation.rds"))
point_process <- readRDS(paste0(results, "/number_habitats_point_process.rds"))

habitat_randomization$Method <- "Randomized habitats"
torus_translation$Method <- "Torus translation"
point_process$Method <- "Gamma test"

overall_number_habitats <- rbind(habitat_randomization, 
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
  dplyr::group_by(Species_type, Habitats, Method) %>%
  dplyr::summarise(Alpha= mean(Alpha),
                   Correct_mean = mean(Correct),
                   Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                   False_mean = mean(False),
                   False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                   False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False))))


overall_number_habitats %>%
  dplyr::group_by(Method) %>%
  dplyr::summarise(Alpha_mean=mean(Alpha))

plot_number_habitats_correct <- ggplot(data=overall_number_habitats, 
                               aes(x=factor(Habitats), y=Correct_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Correct_lo, ymax=Correct_hi), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Species_type) +
  scale_fill_viridis(discrete=T, name='') +
  labs(x="Number of habitats", y="Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

UtilityFunctions::Save.Function.ggplot(plot=plot_number_habitats_correct,
       path=figures, filename="Number_habitats_correct.png",
       width=145, height=120, units="mm", dpi=500)

plot_number_habitats_false <- ggplot(data=overall_number_habitats, 
                                       aes(x=factor(Habitats), y=False_mean, fill=Method, group=Method)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=False_lo, ymax=False_hi), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Species_type) +
  scale_fill_viridis(discrete=T, name="") +
  labs(x="Number of habitats", y="Mean correct detections") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom")

# UtilityFunctions::Save.Function.ggplot(plot=plot_number_habitats_false,
#        path=figures, filename="Number_habitats_false.png",
#        width=145, height=120, units="mm", dpi=500)

