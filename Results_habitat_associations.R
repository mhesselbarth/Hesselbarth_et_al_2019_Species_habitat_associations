#### Results Habitat associations #### 

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
rm(toc)

#### Import packages and data ####
# Packages #
library(ggplot2)
library(UtilityFunctions)
library(SHAR)

# Working directories #
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")

# Import data # 
habitat_associations_ash <- readRDS(file=paste0(results, "/habitat_associations_ash.rds"))
habitat_associations_beech <- readRDS(file=paste0(results, "/habitat_associations_beech.rds"))
habitat_associations_hornbeam <- readRDS(file=paste0(results, "/habitat_associations_hornbeam.rds"))
habitat_associations_others <- readRDS(file=paste0(results, "/habitat_associations_others.rds"))
habitat_associations_sycamore <- readRDS(file=paste0(results, "/habitat_associations_sycamore.rds"))

habitat_associations_beech_small <- readRDS(file=paste0(results, "/habitat_associations_beech_small.rds"))
habitat_associations_beech_medium <- readRDS(file=paste0(results, "/habitat_associations_beech_medium.rds"))
habitat_associations_beech_large <- readRDS(file=paste0(results, "/habitat_associations_beech_large.rds"))

habitat_associations_beech_dead <- readRDS(file=paste0(results, "/habitat_associations_beech_dead.rds"))

#### Plot pattern reconstruction ####
plot_pattern_reconstruction_ash <- Plot.Reconstructed.Pattern(habitat_associations_ash$Pattern, only_spatial=T)
plot_pattern_reconstruction_beech <- Plot.Reconstructed.Pattern(habitat_associations_beech$Pattern, only_spatial=T)
plot_pattern_reconstruction_hornbeam <- Plot.Reconstructed.Pattern(habitat_associations_hornbeam$Pattern, only_spatial=T)
plot_pattern_reconstruction_others <- Plot.Reconstructed.Pattern(habitat_associations_others$Pattern, only_spatial=T)
plot_pattern_reconstruction_sycamore <- Plot.Reconstructed.Pattern(habitat_associations_sycamore$Pattern, only_spatial=T)

plot_pattern_reconstruction_beech_small <- Plot.Reconstructed.Pattern(habitat_associations_beech_small$Pattern, only_spatial=T)
plot_pattern_reconstruction_beech_medium <- Plot.Reconstructed.Pattern(habitat_associations_beech_medium$Pattern, only_spatial=T)
plot_pattern_reconstruction_beech_large <- Plot.Reconstructed.Pattern(habitat_associations_beech_large$Pattern, only_spatial=T)

plot_pattern_reconstruction_beech_dead <- Plot.Reconstructed.Pattern(habitat_associations_beech_dead$Pattern, only_spatial=T)


#### Habitat associations ####
habitat_assocaiations_ash$Associations
habitat_assocaiations_beech$Associations
habitat_assocaiations_hornbeam$Associations
habitat_assocaiations_others$Associations
habitat_assocaiations_sycamore$Associations


#### Habitat associations - Size classes #### 
habitat_associations_beech_small

size_class <- rep(c("small","medium","large"), each=2)
association <- rep(c("Positive association", "Negative association"), times=3)
count <- c(1,3,2,6,7,12)

size_classes_df <- data.frame(size_class, association, count)
size_classes_df$size_class <- factor(size_classes_df$size_class, levels=c("small", "medium", "large"))
size_classes_df$association <- factor(size_classes_df$association, levels=c("Positive association", 
                                                                            "Negative association"))
ggplot(data=size_classes_df, aes(x=size_class, y=count)) + 
  geom_bar(aes(fill=association), position=position_dodge(width = 0.9), stat="identity") + 
  geom_text(aes(label=count, group=association), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  scale_y_continuous(breaks=seq(0,12,4)) + 
  scale_fill_viridis(name="", discrete=T) +
  labs(x="Size class", y="Count") +
  theme_bw() + 
  theme(text=element_text(size=20))
