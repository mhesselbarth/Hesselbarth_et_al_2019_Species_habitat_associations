#### Simulation study ####

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### 1. Import packages & functions ####

# Packages #
library(future.batchtools)
library(NLMR)
library(SHAR)
library(tidyverse)
library(UtilityFunctions)

# Source all functions in R_functions folder
list.files(paste0(getwd(), '/2_Functions'), pattern = '^[a0_ a1_]', full.names = TRUE) %>%
  purrr::walk(function(x) source(x))

#### 2. Specify future topology ####
# 
# future_map for 1) alpha (x) 2) simulation runs (y) 3) within null model function
# login node -> { cluster nodes } -> { multiple cores }
# 
# login <- future::tweak(remote, workers = "gwdu101.gwdg.de", user = 'hesselbarth3')
# bsub <- future::tweak(future.batchtools::batchtools_lsf, template = 'lsf.tmpl',
#                       resources = list(job.name = 'pattern_recon_example',
#                                        log.file = 'pattern_recon_example.log',
#                                        queue = 'mpi-short',
#                                        walltime = '02:00',
#                                        processes = 24))
# 
# future::plan(list(login, bsub, future::multiprocess))
# 
# future::plan(list(future::multiprocess, future::multiprocess))
# future::plan(future::multiprocess)
# 
# 
#### 3. Create data ####

set.seed(42)

simulation_habitats <- NLMR::nlm_fbm(ncol = 30, nrow = 30,
                                     resolution = 20, fract_dim = 1.5, 
                                     verbose = FALSE) %>%
  SHAR::classify_habitats(classes = 5)

simulation_pattern <- create_simulation_pattern(raster = simulation_habitats, 
                                                number_points = 250, 
                                                association_strength = 0.35)
    
names_species <- simulation_pattern$marks$Species %>%
  unique() %>%
  as.character()  

#### 4. Pattern reconstruction ####

reconstruction_species_1 %<-% {spatstat::subset.ppp(simulation_pattern, Species_code == 1) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 5000, fitting = FALSE)}

# future::resolved(future::futureOf(reconstruction_species_1))
# while (TRUE) {
# 
#   Sys.sleep(300)
#   
#   if(future::resolved(future::futureOf(reconstruction_species_1))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(),
#                       "\n\nFile '", deparse(substitute(reconstruction_species_1)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }

reconstruction_species_2 %<-% {spatstat::subset.ppp(simulation_pattern, Species_code == 2) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 5000, fitting = TRUE)}

# future::resolved(future::futureOf(reconstruction_species_2))
# while (TRUE) {
# 
#   Sys.sleep(300)
# 
#   if(future::resolved(future::futureOf(reconstruction_species_2))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(),
#                       "\n\nFile '", deparse(substitute(reconstruction_species_2)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }

reconstruction_species_3 %<-% {spatstat::subset.ppp(simulation_pattern, Species_code == 3) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 19, 
                            max_runs = 5000, fitting = FALSE)}

# future::resolved(future::futureOf(reconstruction_species_3))
# while (TRUE) {
# 
#   Sys.sleep(300)
# 
#   if(future::resolved(future::futureOf(reconstruction_species_3))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(),
#                       "\n\nFile '", deparse(substitute(reconstruction_species_3)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }

reconstruction_species_4 %<-% {spatstat::subset.ppp(simulation_pattern, Species_code == 4) %>%
  SHAR::reconstruct_pattern(number_reconstructions = 199, 
                            max_runs = 5000, fitting = TRUE)}

# future::resolved(future::futureOf(reconstruction_species_4))
# while (TRUE) {
# 
#   Sys.sleep(300)
# 
#   if(future::resolved(future::futureOf(reconstruction_species_4))) {
#     gmailr::send_message(
#       gmailr::mime(
#         To = "maximilian.hesselbarth@uni-goettingen.de",
#         From = "hesselbarth.maximilian.gmail.com",
#         Subject = "RStudio Server",
#         body = paste0("Time: ", Sys.time(),
#                       "\n\nFile '", deparse(substitute(reconstruction_species_4)), "' written!
#                       \n\nHuuurray!!1!1!!"))
#     )
#     break()
#   }
# }

#### 5. Point process ####
fitted_species_1 <- fit_point_process(reconstruction_species_1$Observed, 
                                      process = "poisson", number_pattern = 199)

fitted_species_2 <- fit_point_process(reconstruction_species_2$Observed, 
                                      process = "cluster", number_pattern = 199)

fitted_species_3 <- fit_point_process(reconstruction_species_3$Observed, 
                                      process = "poisson", number_pattern = 199)

fitted_species_4 <- fit_point_process(reconstruction_species_4$Observed, 
                                      process = "cluster", number_pattern = 199)




#### 6. Save results ####

# Species 1
UtilityFunctions::save_rds(object = reconstruction_species_1,
                           filename = "a0_reconstruction_species_1.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = fitted_species_1,
                           filename = "a0_fitted_species_1.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

# Species 2
UtilityFunctions::save_rds(object = reconstruction_species_2,
                           filename = "a0_reconstruction_species_2.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = fitted_species_2,
                           filename = "a0_fitted_species_2.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

# Species 3
UtilityFunctions::save_rds(object = reconstruction_species_3,
                           filename = "a0_reconstruction_species_3.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = fitted_species_3,
                           filename = "a0_fitted_species_3.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

# Species 4
UtilityFunctions::save_rds(object = reconstruction_species_4,
                           filename = "a0_reconstruction_species_4.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)

UtilityFunctions::save_rds(object = fitted_species_4,
                           filename = "a0_fitted_species_4.rds",
                           path = paste0(getwd(), "/4_Output"), 
                           overwrite = FALSE)