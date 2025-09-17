# Test file for data_structures.R

test_that("Input validation works correctly", {
  test_data <- create_test_data()

  # Valid inputs should not throw error
  expect_silent(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
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
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ))

  # Negative min_patch_size should throw error
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = -1.0,  # Invalid
    patch_radius = 1.5,
    boundary_penalty = 0
  ))
})

test_that("Data structure initialization works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0,
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution
  )

  # Check that all required components are present
  expect_true(all(c("unit_dict", "area_dict", "cost_dict", "boundary_matrix",
                   "abundance_matrix", "target_dict") %in% names(minpatch_data)))

  # Check unit_dict structure
  n_units <- nrow(test_data$planning_units)
  expect_equal(length(minpatch_data$unit_dict), n_units)
  expect_true(all(sapply(minpatch_data$unit_dict, function(x) x$status %in% c(0, 1))))

  # Check area_dict
  expect_equal(length(minpatch_data$area_dict), n_units)
  expect_true(all(minpatch_data$area_dict > 0))

  # Check target_dict
  expect_equal(length(minpatch_data$target_dict), length(test_data$features))
  expect_true(all(sapply(minpatch_data$target_dict, function(x) x$target > 0)))
})

test_that("MinPatch data structure contains all required components", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  # Check that minpatch_data contains prioritizr objects
  expect_true("prioritizr_problem" %in% names(minpatch_data))
  expect_true("prioritizr_solution" %in% names(minpatch_data))

  # Check that abundance matrix was created correctly
  expect_true("abundance_matrix" %in% names(minpatch_data))
  expect_equal(length(minpatch_data$abundance_matrix), nrow(test_data$planning_units))

  # Check that target_dict was created correctly
  expect_true("target_dict" %in% names(minpatch_data))
  expect_equal(length(minpatch_data$target_dict), length(test_data$features))
})
