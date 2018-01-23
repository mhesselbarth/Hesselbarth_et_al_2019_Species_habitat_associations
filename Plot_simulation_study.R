#### Plot simulation study - Strength Associations #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################


#### Install packages ####
toc <- "cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec"

devtools::install_github("mhesselbarth/SHAR", auth_token=toc, quiet=T)
devtools::install_github("mhesselbarth/UtilityFunctions", auth_token=toc, quiet=T)

#### Import packages and data ####
# Packages #
library(ggplot2)
library(UtilityFunctions)
library(SHAR)

# Working directories
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")

# Import data #
simulation_study_habitat_randomization <- readRDS(paste0(results, "/simulation_study_habitat_randomization.rds"))
simulation_study_torus_translation <- readRDS(paste0(results, "/simulation_study_torus_translation.rds"))
simulation_study_point_process <- readRDS(paste0(results, "/simulation_study_point_process.rds"))

#### Preprocessing data ####
habitat_randomization_aggregated <- Aggregate.Result.List(result_list=simulation_study_habitat_randomization, id="Association")
torus_translation_aggregated <- Aggregate.Result.List(result_list=simulation_study_torus_translation, id="Association")
point_process_aggregated <- Aggregate.Result.List(result_list=simulation_study_point_process, id="Association")

habitat_randomization_aggregated$Method <- "Habitat_randomization"
torus_translation_aggregated$Method <- "Torus_translation"
point_process_aggregated$Method <- "Point_process"

overall_association <- rbind(habitat_randomization_aggregated,
                             torus_translation_aggregated, 
                             point_process_aggregated)
overall_association$Method <- as.factor(overall_association$Method)
overall_association$Type <- factor(overall_association$Type, 
                                   levels=c("Poisson process (neutral)", "Poisson process (positive)", "Poisson process (negative)",
                                            "Thomas process (neutral)", "Thomas process (positive)", "Thomas process (negative)"))

#### Plotting data ####
p_strength_asso <- ggplot(data=subset(overall_association, Measure=="Correct")) +
  geom_ribbon(aes(x=Association, ymin=Mean-SE, ymax=Mean+SE, fill=Method), alpha=0.3) +
  geom_line(aes(x=Association, y=Mean, col=Method), linetype=1) +
  facet_wrap(~Type, nrow=2, ncol=3) + 
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100, 20)) +
  scale_x_continuous(limits=c(0, 1), breaks=seq(0,1, 0.2)) +
  labs(x="Strength of association", y="Mean correct detection [%]") +
  theme_bw() +
  theme(legend.position="none")
p_strength_asso

# Save.Function.ggplot(plot=p_strength_asso, file=paste0(figures, "/Fig_3.jpeg"), dpi=1000)

