#### Real-world data ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages #
# library(clustermq)
library(UtilityFunctions)
library(SHAR)
library(spatstat)
library(tidyverse)

# pattern_1999 <- readr::read_rds(paste0(getwd(), "/1_Data/pattern_1999.rds"))
pattern_2007 <- readr::read_rds(paste0(getwd(), "/1_Data/pattern_2007.rds"))

#### 2. Preprocessing data ####
pattern_2007_living <- subset.ppp(pattern_2007, Type != 'dead')
pattern_2007_dead <- subset.ppp(pattern_2007, Type == 'dead')

# Beech
beech <- subset.ppp(pattern_2007_living, Species == "Beech")
beech_small <- subset.ppp(beech, DBH_group == "small")
beech_medium <- subset.ppp(beech, DBH_group == "medium")
beech_large <- subset.ppp(beech, DBH_group == "large")
beech_dead <- subset.ppp(pattern_2007_dead, Species == "Beech")

# Ash
ash <- subset.ppp(pattern_2007_living, Species == "Ash")
ash_small <- subset.ppp(ash, DBH_group == "small")
ash_medium <- subset.ppp(ash, DBH_group == "medium")
ash_large <- subset.ppp(ash, DBH_group == "large")
ash_dead <- subset.ppp(pattern_2007_dead, Species == "Ash")

# Hornbeam
hornbeam <- subset.ppp(pattern_2007_living, Species == "Hornbeam")
hornbeam_small <- subset.ppp(hornbeam, DBH_group == "small")
hornbeam_medium <- subset.ppp(hornbeam, DBH_group == "medium")
hornbeam_large <- subset.ppp(hornbeam, DBH_group == "large")
hornbeam_dead <- subset.ppp(pattern_2007_dead, Species == "Hornbeam")

# Sycamore
sycamore <- subset.ppp(pattern_2007_living, Species == "Sycamore")
sycamore_small <- subset.ppp(sycamore, DBH_group == "small")
sycamore_medium <- subset.ppp(sycamore, DBH_group == "medium")
sycamore_large <- subset.ppp(sycamore, DBH_group == "large")
sycamore_dead <- subset.ppp(pattern_2007_dead, Species == "Sycamore")

# others
others <- subset.ppp(pattern_2007_living, Species == "others")
others_small <- subset.ppp(others, DBH_group == "small")
others_medium <- subset.ppp(others, DBH_group == "medium")
others_large <- subset.ppp(others, DBH_group == "large")
others_dead <- subset.ppp(pattern_2007_dead, Species == "others")

#### 3. Pattern reconstruction ####

n_random <- 199
max_runs <- 10000

# Beech
# plot(pcf(beech, divisor = "d", correction = "Ripley"))
beech_reconstructed <- SHAR::reconstruct_pattern(pattern = beech, 
                                                 n_random = n_random, 
                                                 max_runs = max_runs, 
                                                 fitting = TRUE, 
                                                 comp_fast = TRUE, 
                                                 verbose = TRUE)

# Ash
# plot(pcf(ash, divisor = "d", correction = "Ripley"))
ash_reconstructed <- SHAR::reconstruct_pattern(pattern = ash, 
                                               n_random = n_random, 
                                               max_runs = max_runs, 
                                               fitting = TRUE,
                                               verbose = TRUE)

