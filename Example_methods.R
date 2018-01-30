#### Example methods ####

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
library(ggplot2)
library(NLMR)
library(UtilityFunctions)
library(raster)
library(SHAR)
library(spatstat)
library(viridis)

figures <- paste0(getwd(), "/Figures")

#### Create example data ####

landscape_observed <- nlm_mpd(ncol=30, nrow=30, resolution=20, roughness=0.3)
landscape_observed <- Habitat.Classification(raster=landscape_observed, classes=5)
pattern_observed <- Create.Simulation.Pattern(raster=landscape_observed, number_points=50, alpha=0.3)

#### Point process method ####
model_kppm<- kppm(unmark(pattern_observed), 
                  cluster="Thomas", statistic="pcf", statargs=list(divisor="d"))
simulated_pattern <- simulate.kppm(model_kppm, nsim=3)

plot_observed <- ggplot() + 
  geom_raster(data=as.data.frame(landscape_observed, xy=T), aes(x=x, y=y, fill=factor(layer))) + 
  geom_point(data=as.data.frame(pattern_observed), aes(x=x, y=y), pch=1, size=1.5) +
  scale_fill_viridis(discrete=T) +
  theme(aspect.ratio=1) +
  theme_classic() + 
  theme(legend.position="none") + 
  labs(title="Observed data")

plot_point_process <- list()
for(i in 1:length(simulated_pattern)){
  plot_point_process[[i]] <- ggplot() + 
    geom_raster(data=as.data.frame(landscape_observed, xy=T), aes(x=x, y=y, fill=factor(layer))) + 
    geom_point(data=as.data.frame(simulated_pattern[[i]]), aes(x=x, y=y), pch=1, size=1.5) +
    scale_fill_viridis(discrete=T) +
    theme(aspect.ratio=1) +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title="Randomization data")
}

plot_overall_point_process <- gridExtra::grid.arrange(plot_observed, plot_point_process[[1]], 
                                                      plot_point_process[[2]], plot_point_process[[3]])
Save.Function.ggplot(plot=plot_overall_point_process, file=paste0(figures, "/Point_process.jpeg"), dpi=1000)

#### Pattern reconstruction ####
reconstructed_pattern <- Pattern.Reconstruction(pattern=pattern_observed,
                                                number_reconstructions=3, max_runs=10000, fitting=T)
plot_pattern_reconstruction <- list()
for(i in c(1:3)){
  plot_pattern_reconstruction[[i]] <- ggplot() + 
    geom_raster(data=as.data.frame(landscape_observed, xy=T), aes(x=x, y=y, fill=factor(layer))) + 
    geom_point(data=as.data.frame(reconstructed_pattern[[i]]), aes(x=x, y=y), pch=1, size=1.5) +
    scale_fill_viridis(discrete=T) +
    theme(aspect.ratio=1) +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title="Randomization data")
}
plot_overall_reconstruction <- gridExtra::grid.arrange(plot_observed, plot_pattern_reconstruction[[1]], 
                                                       plot_pattern_reconstruction[[2]], plot_pattern_reconstruction[[3]])
 Save.Function.ggplot(plot=plot_overall_reconstruction, file=paste0(figures, "/Pattern_reconstruction.jpeg"), dpi=1000)


#### Habitat randomization ####
simulated_habitat <- Habitat.Randomization(raster=landscape_observed, method="randomization_algorithm",
                                                    number_maps=3)

plot_habitat_randomization <- list()
for(i in 1:length(simulated_habitat)){
  plot_habitat_randomization[[i]] <- ggplot() + 
    geom_raster(data=as.data.frame(simulated_habitat[[i]], xy=T), aes(x=x, y=y, fill=factor(layer))) + 
    geom_point(data=as.data.frame(pattern_observed), aes(x=x, y=y), pch=1, size=1.5) +
    scale_fill_viridis(discrete=T) +
    theme(aspect.ratio=1) +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title="Randomization data")
}

plot_overall_habitat_randomization <- gridExtra::grid.arrange(plot_observed, plot_habitat_randomization[[1]], 
                                                              plot_habitat_randomization[[2]], plot_habitat_randomization[[3]])
Save.Function.ggplot(plot=plot_overall_habitat_randomization, file=paste0(figures, "/Habitat_randomization.jpeg"), dpi=1000)


#### Torus translation ####
torus_habitats <- Habitat.Randomization(raster=landscape_observed, method="torus_translation")

plot_torus_translation <- list()
for(i in c(5,10,20)){
  plot_torus_translation[[length(plot_torus_translation)+1]] <- ggplot() + 
    geom_raster(data=as.data.frame(torus_habitats[[i]], xy=T), aes(x=x, y=y, fill=factor(layer))) + 
    geom_point(data=as.data.frame(pattern_observed), aes(x=x, y=y), pch=1, size=1.5) +
    scale_fill_viridis(discrete=T) +
    theme(aspect.ratio=1) +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title="Randomization data")
}

plot_overall_torus_translation <- gridExtra::grid.arrange(plot_observed, plot_torus_translation[[1]], 
                                                          plot_torus_translation[[2]], plot_torus_translation[[3]])
Save.Function.ggplot(plot=plot_overall_torus_translation, file=paste0(figures, "/Torus_translation.jpeg"), dpi=1000)
