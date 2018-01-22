
devtools::install_github("mhesselbarth/SHAR", auth_token="cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec")

library(ggplot2)
library(NLMR)
library(UtilityFunctions)
library(raster)
library(SHAR)
library(spatstat)
library(viridis)

figures <- paste0(getwd(), "/Figures")

landscape_observed <- nlm_mpd(ncol=30, nrow=30, resolution=20, roughness=0.3)
landscape_observed <- Habitat.Classification(landscape_observed)

pattern_observed <- Create.Simulation.Pattern(habitats=landscape_observed, number_points=50, alpha=0.3)

model_kppm<- kppm(unmark(pattern_observed), 
                  cluster="Thomas", statistic="pcf", statargs=list(divisor="d"))
simulated_pattern <- simulate.kppm(model_kppm, nsim=3)

plot_observed <- ggplot() + 
  geom_raster(data=as.data.frame(landscape_observed, xy=T), aes(x=x, y=y, fill=factor(layer))) + 
  geom_point(data=as.data.frame(pattern_observed), aes(x=x, y=y), pch=1, size=1.5) +
  scale_fill_brewer(palette="RdYlBu", direction=-1) +
  theme(aspect.ratio=1) +
  theme_classic() + 
  theme(legend.position="none") + 
  labs(title="Observed data")

plot_point_process <- list()
for(i in 1:length(simulated_pattern)){
  plot_point_process[[i]] <- ggplot() + 
    geom_raster(data=as.data.frame(landscape_observed, xy=T), aes(x=x, y=y, fill=factor(layer))) + 
    geom_point(data=as.data.frame(simulated_pattern[[i]]), aes(x=x, y=y), pch=1, size=1.5) +
    scale_fill_brewer(palette="RdYlBu", direction=-1) +
    theme(aspect.ratio=1) +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title="Randomization data")
}

plot_overall_point_process <- gridExtra::grid.arrange(plot_observed, plot_point_process[[1]], 
                                                      plot_point_process[[2]], plot_point_process[[3]])
# Save.Function.ggplot(plot=plot_overall_point_process, file=paste0(figures, "/Fig_1.jpeg"), dpi=1000)

simulated_habitat <- Parallel.Habitat.Randomization(raster=landscape_observed, method="Random.Neighbour",
                                                    no.maps=3)

plot_habitat_randomization <- list()
for(i in 1:length(simulated_habitat)){
  plot_habitat_randomization[[i]] <- ggplot() + 
    geom_raster(data=as.data.frame(simulated_habitat[[i]], xy=T), aes(x=x, y=y, fill=factor(layer))) + 
    geom_point(data=as.data.frame(pattern_observed), aes(x=x, y=y), pch=1, size=1.5) +
    scale_fill_brewer(palette="RdYlBu", direction=-1) +
    theme(aspect.ratio=1) +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title="Randomization data")
    
  
}

plot_overall_habitat_randomization <- gridExtra::grid.arrange(plot_observed, plot_habitat_randomization[[1]], 
                                                              plot_habitat_randomization[[2]], plot_habitat_randomization[[3]])
# Save.Function.ggplot(plot=plot_overall_habitat_randomization, file=paste0(figures, "/Fig_2.jpeg"), dpi=1000)



