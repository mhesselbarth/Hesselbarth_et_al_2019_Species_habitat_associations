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

pcf_beech <- spatstat::pcf.ppp(beech, divisor = "d", correction = "Ripley", stoyan = 0.25)

plot(pcf_beech, main = "Pair correlation function Beech")

# Ash
ash <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Ash"))

pcf_ash <- spatstat::pcf.ppp(ash, divisor = "d", correction = "Ripley", stoyan = 0.25)

plot(pcf_ash, main = "Pair correlation function Ash")

# Sycamore
sycamore <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Sycamore"))

pcf_sycamore <- spatstat::pcf.ppp(sycamore, divisor = "d", correction = "Ripley", stoyan = 0.25)

plot(pcf_sycamore, main = "Pair correlation function Sycamore")

# Hornbeam
hornbeam <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "Hornbeam"))

pcf_hornbeam <- spatstat::pcf.ppp(hornbeam, divisor = "d", correction = "Ripley", stoyan = 0.25)

plot(pcf_hornbeam, main = "Pair correlation function Hornbeam")

# others
others <- spatstat::unmark(spatstat::subset.ppp(pattern_2007_living, Species == "others"))

pcf_others <- spatstat::pcf.ppp(others, divisor = "d", correction = "Ripley", stoyan = 0.25)

plot(pcf_others, main = "Pair correlation function others")