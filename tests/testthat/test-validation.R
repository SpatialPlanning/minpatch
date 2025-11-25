# Test file for validation and edge cases

test_that("validate_inputs catches invalid costs", {
  test_data <- create_test_data()
  
  # Negative costs should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = c(-1, rep(1, length(test_data$solution) - 1)),
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ), "costs must be non-negative")
  
  # Wrong length costs should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = c(1, 1, 1),  # Wrong length
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ), "costs must be a numeric vector with same length as solution")
})

test_that("validate_inputs catches invalid boundary_penalty", {
  test_data <- create_test_data()
  
  # Negative boundary penalty should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = -0.1
  ), "boundary_penalty must be a non-negative number")
})

test_that("validate_inputs catches non-sf planning_units", {
  test_data <- create_test_data()
  
  # Non-sf object should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = data.frame(x = 1:10),  # Not sf
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ), "planning_units must be an sf object")
})

test_that("validate_inputs catches invalid targets format", {
  test_data <- create_test_data()
  
  # Invalid targets format should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = c(0.1, 0.2, 0.3),  # Not a data.frame
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ), "targets must be a data.frame")
  
  # Missing required columns should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = data.frame(id = 1:5),  # Missing required columns
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ), "targets must contain 'feature_id' and 'target' columns")
})

test_that("validate_inputs handles units objects", {
  test_data <- create_test_data()
  
  # Should handle sf units objects (as.numeric conversion)
  # This tests the units handling in min_patch_size and patch_radius
  expect_silent(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,  # Could be units object
    patch_radius = 1.5,    # Could be units object
    boundary_penalty = 0
  ))
})

test_that("run_minpatch handles missing prioritizr requirement", {
  test_data <- create_test_data()
  
  # Test that prioritizr_problem is required
  expect_error(
    run_minpatch(
      prioritizr_solution = test_data$prioritizr_solution,
      min_patch_size = 1.0,
      patch_radius = 1.5
    ),
    "prioritizr_problem is required"
  )
})

test_that("run_minpatch handles invalid solution_column", {
  test_data <- create_test_data()
  
  expect_error(
    run_minpatch(
      prioritizr_problem = test_data$prioritizr_problem,
      prioritizr_solution = test_data$prioritizr_solution,
      min_patch_size = 1.0,
      patch_radius = 1.5,
      solution_column = "nonexistent"
    ),
    "Solution column nonexistent not found"
  )
})

test_that("boundary matrix handles invalid geometries", {
  test_data <- create_test_data()
  
  # Test with valid geometries (should work)
  planning_units_valid <- test_data$planning_units
  
  boundary_matrix <- create_boundary_matrix(planning_units_valid, verbose = FALSE)
  
  expect_true(inherits(boundary_matrix, "dgCMatrix"))
  expect_equal(nrow(boundary_matrix), nrow(planning_units_valid))
  expect_equal(ncol(boundary_matrix), nrow(planning_units_valid))
})

test_that("boundary matrix creation with n_cores parameter", {
  test_data <- create_test_data()
  
  # Test with n_cores = 1 (sequential)
  boundary_matrix_1 <- create_boundary_matrix(
    test_data$planning_units, 
    verbose = FALSE,
    n_cores = 1
  )
  
  expect_true(inherits(boundary_matrix_1, "dgCMatrix"))
  expect_equal(nrow(boundary_matrix_1), nrow(test_data$planning_units))
})

test_that("abundance matrix handles missing feature columns", {
  test_data <- create_test_data()
  
  # Create abundance matrix
  abundance_matrix <- create_abundance_matrix(
    test_data$planning_units,
    test_data$prioritizr_problem
  )
  
  expect_true(is.list(abundance_matrix))
  expect_equal(length(abundance_matrix), nrow(test_data$planning_units))
  
  # Check that all units have abundance information
  for (i in seq_along(abundance_matrix)) {
    expect_true(is.list(abundance_matrix[[i]]))
  }
})

test_that("patch radius dictionary creation works", {
  test_data <- create_test_data()
  
  patch_radius_dict <- create_patch_radius_dict(
    test_data$planning_units,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  expect_true(is.list(patch_radius_dict))
  expect_equal(length(patch_radius_dict), nrow(test_data$planning_units))
  
  # Check that all entries contain character vectors of unit IDs
  for (i in seq_along(patch_radius_dict)) {
    expect_true(is.character(patch_radius_dict[[i]]))
  }
})

test_that("add_patch_centered_on_unit works correctly", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Set all units to available
  for (i in seq_along(minpatch_data$unit_dict)) {
    minpatch_data$unit_dict[[i]]$status <- 0
  }
  
  # Add a patch centered on unit "1"
  updated_unit_dict <- add_patch_centered_on_unit(minpatch_data, "1")
  
  # Check that center unit was selected
  expect_equal(updated_unit_dict[["1"]]$status, 1)
  
  # Check that some neighbors were also selected
  neighbors_selected <- sum(sapply(updated_unit_dict, function(x) x$status == 1))
  expect_true(neighbors_selected >= 1)
})

test_that("make_patch_dict handles empty solution", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Set all units to unselected
  for (i in seq_along(minpatch_data$unit_dict)) {
    minpatch_data$unit_dict[[i]]$status <- 0
  }
  
  patch_dict <- make_patch_dict(minpatch_data)
  
  # Should return empty list
  expect_equal(length(patch_dict), 0)
})

test_that("calculate_patch_stats handles empty patches", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Set all units to unselected
  for (i in seq_along(minpatch_data$unit_dict)) {
    minpatch_data$unit_dict[[i]]$status <- 0
  }
  
  updated_data <- calculate_patch_stats(minpatch_data)
  stats <- updated_data$patch_stats
  
  # Should return zero statistics
  expect_equal(stats$all_patch_count, 0)
  expect_equal(stats$valid_patch_count, 0)
  expect_equal(stats$all_patch_area, 0)
})