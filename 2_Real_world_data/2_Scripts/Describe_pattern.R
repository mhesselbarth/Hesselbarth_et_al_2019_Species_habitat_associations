###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### General summary pattern####

# Load packages #

library(helpeR) # devtools::install_github("mhesselbarth/helpeR)
library(onpoint) # devtools::install_github("mhesselbarth/onpoint")
library(patchwork) # devtools::install_github("thomasp85/patchwork")
library(shar) # devtools::install_github("r-spatialecology/shar")
library(spatstat)
library(tidyverse)

#### Import data ####

# import point pattern data
pattern_2007 <- readr::read_rds(paste0(getwd(), "/2_Real_world_data/1_Data/2_pattern_2007.rds"))

# split into living and dead
pattern_2007_living <- spatstat::subset.ppp(pattern_2007, Type != "dead")

pattern_2007_dead <- spatstat::subset.ppp(pattern_2007, Type == "dead")


#### PCF ####

# Beech
beech <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Beech"))

pcf_beech <- spatstat::envelope(beech, fun = onpoint::estimate_pcf_fast, 
                                nsim = 199, nrank = 5,
                                funargs = list(correction = "Ripley", 
                                               stoyan = 0.25))

plot_beech <- onpoint::plot_quantums(pcf_beech, 
                                     xlab = "r [m]", ylab = "g(r)", title = "F. sylvatica",
                                     line_size = 1, 
                                     legend_position = "none")

# Beech dead 
beech_dead <- spatstat::subset.ppp(pattern_2007_dead, Species == "Beech")

pcf_beech_dead <- spatstat::envelope(beech_dead, fun = "pcf", nsim = 199, nrank = 5,
                                     funargs = list(divisor = "d", 
                                                    correction = "Ripley", 
                                                    stoyan = 0.25))

plot_beech_dead <- onpoint::plot_quantums(pcf_beech_dead, 
                                          xlab = "r [m]", ylab = "g(r)", title = "F. sylvatica (dead)",
                                          line_size = 1,  
                                          legend_position = "none")

# Ash
ash <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Ash"))

pcf_ash <- spatstat::envelope(ash, fun = "pcf", nsim = 199, nrank = 5,
                              funargs = list(divisor = "d", 
                                             correction = "Ripley", 
                                             stoyan = 0.25))

plot_ash <- onpoint::plot_quantums(pcf_ash, 
                                   xlab = "r [m]", ylab = "g(r)", title = "F. excelsior",
                                   line_size = 1, 
                                   legend_position = "none")

# Sycamore
sycamore <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Sycamore"))

pcf_sycamore <- spatstat::envelope(sycamore, fun = "pcf", nsim = 199, nrank = 5,
                                   funargs = list(divisor = "d", 
                                                  correction = "Ripley", 
                                                  stoyan = 0.25))

plot_sycamore <- onpoint::plot_quantums(pcf_sycamore,
                                        xlab = "r [m]", ylab = "g(r)", title = "A. pseudoplatanus",
                                        line_size = 1, 
                                        legend_position = "none")

# Hornbeam
hornbeam <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Hornbeam"))

pcf_hornbeam <- spatstat::envelope(hornbeam, fun = "pcf", nsim = 199, nrank = 5,
                                   funargs = list(divisor = "d", 
                                                  correction = "Ripley", 
                                                  stoyan = 0.25))

plot_hornbeam <- onpoint::plot_quantums(pcf_hornbeam, 
                                        xlab = "r [m]", ylab = "g(r)", title = "C. betulus",
                                        line_size = 1, 
                                        legend_position = "none")

# others
others <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "others"))

pcf_others <- spatstat::envelope(others, fun = "pcf", nsim = 199, nrank = 5,
                                   funargs = list(divisor = "d", 
                                                  correction = "Ripley", 
                                                  stoyan = 0.25))

plot_others <- onpoint::plot_quantums(pcf_others, 
                                      xlab = "r [m]", ylab = "g(r)", title = "others",
                                      line_size = 1, 
                                      legend_position = "none")

# combine plots 
plot_overall <- plot_beech + plot_ash + 
  plot_sycamore + plot_hornbeam + 
  plot_layout(ncol = 2, nrow = 2)
