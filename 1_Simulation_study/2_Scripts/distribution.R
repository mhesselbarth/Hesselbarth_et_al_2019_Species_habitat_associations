###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

# Import packages
library(dplyr)
library(ggplot2)
library(onpoint)
library(purrr)
library(raster)
library(shar)
library(spatstat)


# classify landscape
landscape_classified <- shar::classify_habitats(shar::landscape, classes = 5)

# calculate CSR envelopes input pattern
pattern_csr <- spatstat::envelope(Y = shar::species_b, fun = pcf, nsim = 199, 
                                  funargs = list(divisor = "d", correction = "Ripley", 
                                                 rank = 5))

# plot results
onpoint::plot_quantums(pattern_csr)

# gamma test randomization
pattern_random <- shar::fit_point_process(shar::species_b, n_random = 1999, 
                                          process = "cluster")

# extract points
points_extracted <- purrr::map_dfr(pattern_random, function(x) {
  shar::extract_points(raster = landscape_classified,
                       pattern = x)}, .id = "pattern")

# only random points
points_random <- dplyr::filter(points_extracted, 
                               pattern != "observed", 
                               habitat == 2) 

thresholds <- quantile(points_random$count, probs = c(0.025, 0.975))

points_count <- dplyr::group_by(points_random, count) %>% 
  dplyr::summarise(n = n())

# only observed points
points_observed <- dplyr::filter(points_extracted, 
                                 pattern == "observed", 
                                 habitat == 2)

ggplot2::ggplot() + 
  # ggplot2::geom_bar(data = points_count, stat = "identity",
  #                   ggplot2::aes(x = count, y = n)) +
  ggplot2::geom_density(data = points_random, 
                        ggplot2::aes(x = count,  y = ..density..), 
                        col = "black", fill = "grey", alpha = 1) + 
  # ggplot2::geom_segment(ggplot2::aes(x = thresholds[[1]], xend = thresholds[[1]],
  #                                    y = 0.04, yend = 0.001), col = "#21908CFF",
  #                       arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  # ggplot2::geom_segment(ggplot2::aes(x = thresholds[[2]], xend = thresholds[[2]],
  #                                    y = 0.04, yend = 0.001), col = "#21908CFF",
  #                       arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  # ggplot2::geom_segment(ggplot2::aes(x = points_observed$count, xend = points_observed$count,
  #                                    y = 0.04, yend = 0.001), col = "#440154FF",
  #                       arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  ggplot2::scale_x_continuous(limits = c(10, 80), 
                              breaks = seq(from = 10, to = 80, by = 5)) + 
  ggplot2::labs(x = "Individuals within habitat", y = "n (randomizations)") + 
  ggplot2::theme_bw(base_size = 15)


ggplot2::ggplot() + 
  # ggplot2::geom_bar(data = points_count, stat = "identity",
  #                   ggplot2::aes(x = count, y = n)) +
  ggplot2::geom_density(data = points_random, 
                        ggplot2::aes(x = count,  y = ..density..), 
                        col = "black", fill = "grey", alpha = 1) + 
  ggplot2::geom_segment(ggplot2::aes(x = thresholds[[1]], xend = thresholds[[1]],
                                     y = 0.04, yend = 0.001), col = "#21908CFF",
                        arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  ggplot2::geom_segment(ggplot2::aes(x = thresholds[[2]], xend = thresholds[[2]],
                                     y = 0.04, yend = 0.001), col = "#21908CFF",
                        arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  # ggplot2::geom_segment(ggplot2::aes(x = points_observed$count, xend = points_observed$count,
  #                                    y = 0.04, yend = 0.001), col = "#440154FF",
  #                       arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  ggplot2::scale_x_continuous(limits = c(10, 80), 
                              breaks = seq(from = 10, to = 80, by = 5)) + 
  ggplot2::labs(x = "Individuals within habitat", y = "n (randomizations)") + 
  ggplot2::theme_bw(base_size = 15)

ggplot2::ggplot() + 
  # ggplot2::geom_bar(data = points_count, stat = "identity",
  #                   ggplot2::aes(x = count, y = n)) +
  ggplot2::geom_density(data = points_random, 
                        ggplot2::aes(x = count,  y = ..density..), 
                        col = "black", fill = "grey", alpha = 1) + 
  ggplot2::geom_segment(ggplot2::aes(x = thresholds[[1]], xend = thresholds[[1]],
                                     y = 0.04, yend = 0.001), col = "#21908CFF",
                        arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  ggplot2::geom_segment(ggplot2::aes(x = thresholds[[2]], xend = thresholds[[2]],
                                     y = 0.04, yend = 0.001), col = "#21908CFF",
                        arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  ggplot2::geom_segment(ggplot2::aes(x = points_observed$count, xend = points_observed$count,
                                     y = 0.04, yend = 0.001), col = "#440154FF",
                        arrow = arrow(length = unit(0.5, "cm")), size = 2) +
  ggplot2::scale_x_continuous(limits = c(10, 80), 
                              breaks = seq(from = 10, to = 80, by = 5)) + 
  ggplot2::labs(x = "Individuals within habitat", y = "n (randomizations)") + 
  ggplot2::theme_bw(base_size = 15)


