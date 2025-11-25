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

test_that("boundary matrix creation with small datasets", {
  test_data <- create_test_data()
  
  # Test with n_cores = 1 explicitly
  boundary_matrix <- create_boundary_matrix(
    test_data$planning_units,
    verbose = FALSE,
    n_cores = 1
  )
  
  expect_true(inherits(boundary_matrix, "dgCMatrix"))
  expect_equal(nrow(boundary_matrix), nrow(test_data$planning_units))
  expect_equal(ncol(boundary_matrix), nrow(test_data$planning_units))
  
  # Check that diagonal contains perimeters
  diag_values <- Matrix::diag(boundary_matrix)
  expect_true(all(diag_values > 0))
})

test_that("boundary matrix handles symmetry", {
  test_data <- create_test_data()
  
  boundary_matrix <- create_boundary_matrix(
    test_data$planning_units,
    verbose = FALSE
  )
  
  # Matrix should be symmetric
  diff <- boundary_matrix - Matrix::t(boundary_matrix)
  expect_true(all(abs(diff@x) < 1e-10))
})

test_that("create_abundance_matrix handles all features", {
  test_data <- create_test_data()
  
  abundance_matrix <- create_abundance_matrix(
    test_data$planning_units,
    test_data$prioritizr_problem
  )
  
  # Check structure
  expect_true(is.list(abundance_matrix))
  expect_equal(length(abundance_matrix), nrow(test_data$planning_units))
  
  # Check that feature IDs are strings
  for (unit_id in names(abundance_matrix)) {
    if (length(abundance_matrix[[unit_id]]) > 0) {
      expect_true(all(sapply(names(abundance_matrix[[unit_id]]), is.character)))
    }
  }
})

test_that("create_patch_radius_dict optimization works", {
  test_data <- create_test_data()
  
  # Test with different radius values
  radius1 <- create_patch_radius_dict(
    test_data$planning_units,
    patch_radius = 0.5,
    verbose = FALSE
  )
  
  radius2 <- create_patch_radius_dict(
    test_data$planning_units,
    patch_radius = 2.0,
    verbose = FALSE
  )
  
  # Larger radius should generally have more neighbors
  avg_neighbors1 <- mean(sapply(radius1, length))
  avg_neighbors2 <- mean(sapply(radius2, length))
  
  expect_true(avg_neighbors2 >= avg_neighbors1)
})

test_that("validate_inputs handles zero patch_radius", {
  test_data <- create_test_data()
  
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 1.0,
    patch_radius = 0,
    boundary_penalty = 0
  ), "patch_radius must be a positive number")
})

test_that("validate_inputs handles zero min_patch_size", {
  test_data <- create_test_data()
  
  expect_error(validate_inputs(
    solution = test_data$solution,
    planning_units = test_data$planning_units,
    targets = test_data$targets,
    costs = NULL,
    min_patch_size = 0,
    patch_radius = 1.5,
    boundary_penalty = 0
  ), "min_patch_size must be a positive number")
})

test_that("initialize_minpatch_data creates all required components", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0.1, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Check all required components exist
  required_components <- c(
    "unit_dict", "area_dict", "cost_dict", "boundary_matrix",
    "abundance_matrix", "target_dict", "patch_radius_dict",
    "min_patch_size", "patch_radius", "boundary_penalty",
    "prioritizr_problem", "prioritizr_solution"
  )
  
  expect_true(all(required_components %in% names(minpatch_data)))
  
  # Check that parameters are stored correctly
  expect_equal(minpatch_data$min_patch_size, 1.0)
  expect_equal(minpatch_data$patch_radius, 1.5)
  expect_equal(minpatch_data$boundary_penalty, 0.1)
})

test_that("initialize_minpatch_data handles custom costs", {
  test_data <- create_test_data()
  
  custom_costs <- rep(2.0, length(test_data$solution))
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    custom_costs, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Check that custom costs were used
  expect_equal(minpatch_data$unit_dict[[1]]$cost, 2.0)
  expect_equal(minpatch_data$cost_dict[[1]], 2.0)
})

test_that("boundary matrix handles adjacency correctly", {
  test_data <- create_test_data()
  
  boundary_matrix <- create_boundary_matrix(
    test_data$planning_units,
    verbose = FALSE
  )
  
  # Non-zero entries indicate adjacency or self
  n_units <- nrow(test_data$planning_units)
  
  # Each unit should at least have itself (diagonal)
  for (i in seq_len(n_units)) {
    expect_true(boundary_matrix[i, i] > 0)
  }
})
