# Helper functions for tests
# This file is automatically loaded by testthat before running tests

library(sf)
library(prioritizr)
library(dplyr)
library(terra)

# Helper function to create test data using prioritizr simulation data
create_test_data <- function() {

  # Use prioritizr simulation data as per minpatch.Rmd
  dat <- c(prioritizr::get_sim_pu_raster(), prioritizr::get_sim_features()) %>%
    terra::as.polygons(dissolve = FALSE, values = TRUE) %>%
    sf::st_as_sf() %>%
    dplyr::rename(cost = layer)

  sf::st_crs(dat) <- NA

  # Get feature column names
  features <- colnames(dat) %>%
    stringr::str_subset("feature_")

  # Create prioritizr problem
  p <- prioritizr::problem(dat, features = features, cost_column = "cost") %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_relative_targets(0.3) %>%
    prioritizr::add_binary_decisions()

  # Solve the problem
  s <- solve(p)

  # Create targets data frame for internal use
  targets <- data.frame(
    feature_id = seq_along(features),
    target = rep(0.3, length(features))  # 30% targets
  )

  return(list(
    planning_units = dat,
    prioritizr_problem = p,
    prioritizr_solution = s,
    solution = s$solution_1,
    targets = targets,
    features = features
  ))
}
