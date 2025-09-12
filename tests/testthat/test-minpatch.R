# Test file for MinPatch R implementation
library(testthat)
library(sf)

# Helper function to create simple test data
create_test_data <- function() {
  # Create 3x3 grid of planning units
  coords_list <- list()
  for (i in 1:3) {
    for (j in 1:3) {
      coords <- matrix(c(
        i - 0.5, j - 0.5,
        i + 0.5, j - 0.5,
        i + 0.5, j + 0.5,
        i - 0.5, j + 0.5,
        i - 0.5, j - 0.5
      ), ncol = 2, byrow = TRUE)
      coords_list[[(i-1)*3 + j]] <- st_polygon(list(coords))
    }
  }
  
  planning_units <- st_sf(
    unit_id = 1:9,
    cost = rep(1, 9),
    geometry = st_sfc(coords_list)
  )
  
  # Create simple feature data
  features <- data.frame(
    pu_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 3, 5, 7, 9),
    feature_id = c(rep(1, 9), rep(2, 5)),
    amount = c(rep(1, 9), rep(0.5, 5))
  )
  
  # Create targets
  targets <- data.frame(
    feature_id = c(1, 2),
    target = c(3, 1)
  )
  
  return(list(
    planning_units = planning_units,
    features = features,
    targets = targets
  ))
}

test_that("Input validation works correctly", {
  test_data <- create_test_data()
  
  # Valid inputs should not throw error
  expect_silent(validate_inputs(
    solution = c(1, 1, 0, 1, 0, 0, 0, 0, 0),
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ))
  
  # Invalid solution should throw error
  expect_error(validate_inputs(
    solution = c(1, 2, 0),  # Invalid values
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ))
  
  # Mismatched lengths should throw error
  expect_error(validate_inputs(
    solution = c(1, 0, 1),  # Wrong length
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ))
  
  # Negative min_patch_size should throw error
  expect_error(validate_inputs(
    solution = c(1, 1, 0, 1, 0, 0, 0, 0, 0),
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = -1.0,  # Invalid
    patch_radius = 1.5,
    boundary_penalty = 0
  ))
})

test_that("Data structure initialization works", {
  test_data <- create_test_data()
  solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)
  
  minpatch_data <- initialize_minpatch_data(
    solution = solution,
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  )
  
  # Check that all required components are present
  expect_true(all(c("unit_dict", "area_dict", "cost_dict", "boundary_matrix", 
                   "abundance_matrix", "target_dict") %in% names(minpatch_data)))
  
  # Check unit_dict structure
  expect_equal(length(minpatch_data$unit_dict), 9)
  expect_equal(minpatch_data$unit_dict[["1"]]$status, 1)
  expect_equal(minpatch_data$unit_dict[["3"]]$status, 0)
  
  # Check area_dict
  expect_equal(length(minpatch_data$area_dict), 9)
  expect_true(all(minpatch_data$area_dict > 0))
  
  # Check target_dict
  expect_equal(length(minpatch_data$target_dict), 2)
  expect_equal(minpatch_data$target_dict[["1"]]$target, 3)
  expect_equal(minpatch_data$target_dict[["2"]]$target, 1)
})

test_that("Patch identification works correctly", {
  test_data <- create_test_data()
  solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)  # Units 1, 2, 4 selected
  
  minpatch_data <- initialize_minpatch_data(
    solution, test_data$planning_units, test_data$features, 
    test_data$targets, NULL, 1.0, 1.5, 0
  )
  
  patch_dict <- make_patch_dict(minpatch_data)
  
  # Should identify connected components
  expect_true(length(patch_dict) >= 1)
  
  # Check patch statistics calculation
  patch_stats <- calculate_patch_stats(patch_dict, minpatch_data$area_dict, 1.0)
  
  expect_true(all(c("all_patch_count", "valid_patch_count", "all_patch_area") %in% names(patch_stats)))
  expect_true(patch_stats$all_patch_count >= 0)
  expect_true(patch_stats$valid_patch_count >= 0)
  expect_true(patch_stats$all_patch_area >= 0)
})

test_that("Feature conservation calculation works", {
  test_data <- create_test_data()
  solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)
  
  minpatch_data <- initialize_minpatch_data(
    solution, test_data$planning_units, test_data$features, 
    test_data$targets, NULL, 1.0, 1.5, 0
  )
  
  feature_amounts <- calculate_feature_conservation(
    minpatch_data$unit_dict, 
    minpatch_data$abundance_matrix, 
    minpatch_data$target_dict
  )
  
  # Should have amounts for both features
  expect_equal(length(feature_amounts), 2)
  expect_true(all(names(feature_amounts) %in% c("1", "2")))
  expect_true(all(feature_amounts >= 0))
  
  # Feature 1 should have amount from units 1, 2, 4
  expect_equal(feature_amounts["1"], 3)  # 1 + 1 + 1
  
  # Feature 2 should have amount from unit 1
  expect_equal(feature_amounts["2"], 0.5)
})

test_that("Small patch removal works", {
  test_data <- create_test_data()
  # Create solution with isolated small patches
  solution <- c(1, 0, 1, 0, 0, 0, 0, 0, 0)  # Units 1 and 3 (isolated)
  
  minpatch_data <- initialize_minpatch_data(
    solution, test_data$planning_units, test_data$features, 
    test_data$targets, NULL, 2.0, 1.5, 0  # min_patch_size = 2.0
  )
  
  patch_dict <- make_patch_dict(minpatch_data)
  
  # Remove small patches
  updated_unit_dict <- remove_small_patches_from_solution(
    minpatch_data$unit_dict, patch_dict, minpatch_data$area_dict, 2.0
  )
  
  # Small patches should be removed
  # (assuming each unit has area 1, both patches are too small)
  expect_true(updated_unit_dict[["1"]]$status == 0 || updated_unit_dict[["3"]]$status == 0)
})

test_that("Cost calculation works", {
  test_data <- create_test_data()
  solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)
  
  minpatch_data <- initialize_minpatch_data(
    solution, test_data$planning_units, test_data$features, 
    test_data$targets, NULL, 1.0, 1.5, 0.1  # boundary_penalty = 0.1
  )
  
  cost_summary <- calculate_cost_summary(
    minpatch_data$unit_dict, 
    minpatch_data$cost_dict, 
    minpatch_data$boundary_matrix, 
    0.1
  )
  
  # Check cost components
  expect_true(all(c("total_unit_cost", "total_boundary_cost", "total_cost") %in% names(cost_summary)))
  expect_equal(cost_summary$total_unit_cost, 3)  # 3 units selected, cost = 1 each
  expect_true(cost_summary$total_boundary_cost >= 0)
  expect_equal(cost_summary$total_cost, cost_summary$total_unit_cost + cost_summary$total_boundary_cost)
})

test_that("Complete MinPatch run works", {
  test_data <- create_test_data()
  solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)
  
  # Run MinPatch
  result <- run_minpatch(
    solution = solution,
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0,
    verbose = FALSE
  )
  
  # Check result structure
  expect_true(all(c("solution", "patch_stats", "cost_summary") %in% names(result)))
  
  # Check solution is binary
  expect_true(all(result$solution %in% c(0, 1)))
  expect_equal(length(result$solution), 9)
  
  # Check patch stats
  expect_true(all(c("initial", "final") %in% names(result$patch_stats)))
  
  # Check cost summary
  expect_true(all(c("total_cost", "total_unit_cost") %in% names(result$cost_summary)))
})

test_that("Feature representation calculation works", {
  test_data <- create_test_data()
  solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)
  
  minpatch_data <- initialize_minpatch_data(
    solution, test_data$planning_units, test_data$features, 
    test_data$targets, NULL, 1.0, 1.5, 0
  )
  
  feature_rep <- calculate_feature_representation(
    minpatch_data$unit_dict,
    minpatch_data$abundance_matrix,
    minpatch_data$target_dict
  )
  
  # Check structure
  expect_true(is.data.frame(feature_rep))
  expect_true(all(c("feature_id", "target", "conserved", "proportion_met", "target_met") %in% names(feature_rep)))
  expect_equal(nrow(feature_rep), 2)
  
  # Check values
  expect_equal(feature_rep$target[1], 3)
  expect_equal(feature_rep$conserved[1], 3)
  expect_true(feature_rep$target_met[1])
})

test_that("Example data creation works", {
  example_data <- create_example_data(n_units = 25, n_features = 3, grid_size = 5)
  
  # Check structure
  expect_true(all(c("planning_units", "features", "targets") %in% names(example_data)))
  
  # Check planning units
  expect_equal(nrow(example_data$planning_units), 25)
  expect_true(inherits(example_data$planning_units, "sf"))
  
  # Check features
  expect_true(is.data.frame(example_data$features))
  expect_true(all(c("pu_id", "feature_id", "amount") %in% names(example_data$features)))
  
  # Check targets
  expect_equal(nrow(example_data$targets), 3)
  expect_true(all(c("feature_id", "target") %in% names(example_data$targets)))
})

test_that("Solution comparison works", {
  test_data <- create_test_data()
  original_solution <- c(1, 1, 0, 1, 0, 0, 0, 0, 0)
  
  result <- run_minpatch(
    solution = original_solution,
    planning_units = test_data$planning_units,
    features = test_data$features,
    targets = test_data$targets,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  comparison <- compare_solutions(original_solution, result)
  
  # Check structure
  expect_true(is.data.frame(comparison))
  expect_true(all(c("Metric", "Original", "MinPatch", "Change") %in% names(comparison)))
  expect_true(nrow(comparison) > 0)
})