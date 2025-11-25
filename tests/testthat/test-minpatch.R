# Test file for minpatch.R

test_that("Complete MinPatch run works", {
  test_data <- create_test_data()

  # Run MinPatch with new signature
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0,
    verbose = FALSE
  )

  # Check result structure
  expect_true(all(c("solution", "patch_stats", "cost_summary") %in% names(result)))

  # Check solution is sf object with minpatch column
  expect_true(inherits(result$solution, "sf"))
  expect_true("minpatch" %in% names(result$solution))
  expect_true(all(result$solution$minpatch %in% c(0, 1)))
  expect_equal(nrow(result$solution), nrow(test_data$planning_units))

  # Check patch stats
  expect_true(all(c("initial", "final") %in% names(result$patch_stats)))

  # Check cost summary
  expect_true(all(c("total_cost", "cost") %in% names(result$cost_summary)))
})

test_that("MinPatch handles different solution columns", {
  test_data <- create_test_data()

  # Test with default solution column
  result1 <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    solution_column = "solution_1",
    verbose = FALSE
  )

  expect_true("minpatch" %in% names(result1$solution))
  expect_true(all(result1$solution$minpatch %in% c(0, 1)))
})

test_that("MinPatch handles different stage combinations", {
  test_data <- create_test_data()
  
  # Test with only stage 1 (remove small patches) - expect warning about unmet targets
  expect_warning(
    result1 <- run_minpatch(
      prioritizr_problem = test_data$prioritizr_problem,
      prioritizr_solution = test_data$prioritizr_solution,
      min_patch_size = 1.0,
      patch_radius = 1.5,
      remove_small_patches = TRUE,
      add_patches = FALSE,
      whittle_patches = FALSE,
      verbose = FALSE
    ),
    "conservation targets are no longer met"
  )
  
  expect_true(all(c("solution", "patch_stats", "cost_summary") %in% names(result1)))
  
  # Test with only stage 2 (add patches)
  result2 <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    remove_small_patches = FALSE,
    add_patches = TRUE,
    whittle_patches = FALSE,
    verbose = FALSE
  )
  
  expect_true(all(c("solution", "patch_stats", "cost_summary") %in% names(result2)))
})

test_that("MinPatch validates required parameters", {
  test_data <- create_test_data()

  # Missing prioritizr_problem should throw error
  expect_error(run_minpatch(
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5
  ))

  # Invalid solution column should throw error
  expect_error(run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    solution_column = "nonexistent_column"
  ))
})

test_that("MinPatch handles boundary penalty correctly", {
  test_data <- create_test_data()
  
  # Test with zero boundary penalty
  result1 <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0,
    verbose = FALSE
  )
  
  expect_equal(result1$cost_summary$boundary_cost, 0)
  
  # Test with positive boundary penalty
  result2 <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0.1,
    verbose = FALSE
  )
  
  expect_true(result2$cost_summary$boundary_cost >= 0)
})

test_that("MinPatch verbose output works", {
  test_data <- create_test_data()
  
  # Capture verbose output
  output <- capture.output(
    result <- run_minpatch(
      prioritizr_problem = test_data$prioritizr_problem,
      prioritizr_solution = test_data$prioritizr_solution,
      min_patch_size = 1.0,
      patch_radius = 1.5,
      verbose = TRUE
    )
  )
  
  # Check that output contains stage messages
  expect_true(any(grepl("Stage", output)))
  expect_true(any(grepl("complete", output, ignore.case = TRUE)))
})

test_that("MinPatch handles relative targets correctly", {
  test_data <- create_test_data()
  
  # Run with relative targets (which are converted internally)
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Check that targets were processed
  expect_true("target_dict" %in% names(result$minpatch_data))
  expect_true(length(result$minpatch_data$target_dict) > 0)
  
  # Check that all targets have positive values
  target_values <- sapply(result$minpatch_data$target_dict, function(x) x$target)
  expect_true(all(target_values > 0))
})

test_that("MinPatch preserves prioritizr and minpatch columns", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Check both columns exist
  expect_true("prioritizr" %in% names(result$solution))
  expect_true("minpatch" %in% names(result$solution))
  
  # Check they are binary
  expect_true(all(result$solution$prioritizr %in% c(0, 1)))
  expect_true(all(result$solution$minpatch %in% c(0, 1)))
})

test_that("MinPatch with only stage 3 (whittle only)", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    remove_small_patches = FALSE,
    add_patches = FALSE,
    whittle_patches = TRUE,
    verbose = FALSE
  )
  
  expect_true(all(c("solution", "patch_stats", "cost_summary") %in% names(result)))
})

test_that("MinPatch returns planning_units in result", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  expect_true("planning_units" %in% names(result))
  expect_true(inherits(result$planning_units, "sf"))
  expect_true("prioritizr" %in% names(result$planning_units))
})

test_that("MinPatch returns minpatch_data in result", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  expect_true("minpatch_data" %in% names(result))
  expect_true(all(c("unit_dict", "boundary_matrix", "target_dict") %in%
                  names(result$minpatch_data)))
})

test_that("MinPatch handles different min_patch_size values", {
  test_data <- create_test_data()
  
  # Very small threshold - should keep most patches
  result_small <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 0.1,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Large threshold - should remove more patches
  result_large <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 5.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Both should complete successfully
  expect_true(all(c("solution", "patch_stats") %in% names(result_small)))
  expect_true(all(c("solution", "patch_stats") %in% names(result_large)))
})

test_that("MinPatch respects locked-in constraints", {
  test_data <- create_test_data()
  
  # Add locked-in constraints to some planning units
  locked_in_units <- c(1, 2, 3, 10, 20)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_in_constraints(locked_in_units)
  
  # Solve with locked constraints
  s_locked <- solve(p_locked)
  
  # Run MinPatch
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = s_locked,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Check that locked-in units are selected in the result
  for (unit in locked_in_units) {
    expect_equal(result$solution$minpatch[unit], 1,
                 info = paste("Unit", unit, "should be locked-in and selected"))
  }
  
  # Check that locked-in units are marked as status = 2 in unit_dict
  for (unit in locked_in_units) {
    expect_equal(result$minpatch_data$unit_dict[[as.character(unit)]]$status, 2,
                 info = paste("Unit", unit, "should have status 2 (conserved)"))
  }
})

test_that("MinPatch respects locked-out constraints", {
  test_data <- create_test_data()
  
  # Add locked-out constraints to some planning units
  locked_out_units <- c(5, 15, 25, 35, 45)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_out_constraints(locked_out_units)
  
  # Solve with locked constraints
  s_locked <- solve(p_locked)
  
  # Run MinPatch
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = s_locked,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Check that locked-out units are NOT selected in the result
  for (unit in locked_out_units) {
    expect_equal(result$solution$minpatch[unit], 0,
                 info = paste("Unit", unit, "should be locked-out and not selected"))
  }
  
  # Check that locked-out units are marked as status = 3 in unit_dict
  for (unit in locked_out_units) {
    expect_equal(result$minpatch_data$unit_dict[[as.character(unit)]]$status, 3,
                 info = paste("Unit", unit, "should have status 3 (excluded)"))
  }
})

test_that("MinPatch handles both locked-in and locked-out constraints together", {
  test_data <- create_test_data()
  
  # Add both types of constraints
  locked_in_units <- c(1, 2, 3)
  locked_out_units <- c(50, 60, 70)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_in_constraints(locked_in_units) %>%
    prioritizr::add_locked_out_constraints(locked_out_units)
  
  # Solve with locked constraints
  s_locked <- solve(p_locked)
  
  # Run MinPatch
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = s_locked,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Check locked-in units are selected
  for (unit in locked_in_units) {
    expect_equal(result$solution$minpatch[unit], 1)
  }
  
  # Check locked-out units are not selected
  for (unit in locked_out_units) {
    expect_equal(result$solution$minpatch[unit], 0)
  }
})

test_that("Locked-in units are not removed in Stage 1 (small patch removal)", {
  test_data <- create_test_data()
  
  # Create a scenario with small isolated locked-in units
  empty_solution <- test_data$prioritizr_solution
  empty_solution$solution_1 <- rep(0, nrow(empty_solution))
  
  # Add a few locked-in units that would form a small patch
  locked_in_units <- c(1, 2)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_in_constraints(locked_in_units)
  
  # Run MinPatch with high min_patch_size to test if locked units are protected
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = empty_solution,
    min_patch_size = 10.0,
    patch_radius = 1.5,
    remove_small_patches = TRUE,
    verbose = FALSE
  )
  
  # Locked-in units should still be selected despite forming a small patch
  for (unit in locked_in_units) {
    expect_equal(result$solution$minpatch[unit], 1,
                 info = paste("Locked-in unit", unit, "should not be removed even if patch is small"))
  }
})

test_that("Locked-out units are never selected in Stage 2 (patch addition)", {
  test_data <- create_test_data()
  
  # Start with empty solution
  empty_solution <- test_data$prioritizr_solution
  empty_solution$solution_1 <- rep(0, nrow(empty_solution))
  
  # Lock out units that would otherwise be good candidates
  locked_out_units <- c(10, 11, 12, 13, 14)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_out_constraints(locked_out_units)
  
  # Run MinPatch with patch addition enabled
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = empty_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    remove_small_patches = FALSE,
    add_patches = TRUE,
    verbose = FALSE
  )
  
  # Locked-out units should never be selected
  for (unit in locked_out_units) {
    expect_equal(result$solution$minpatch[unit], 0,
                 info = paste("Locked-out unit", unit, "should never be selected in Stage 2"))
  }
})

test_that("Locked-in units are not removed in Stage 3 (whittling)", {
  test_data <- create_test_data()
  
  # Create solution with some locked-in edge units
  locked_in_units <- c(1, 10, 20)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_in_constraints(locked_in_units)
  
  # Solve with locked constraints
  s_locked <- solve(p_locked)
  
  # Run MinPatch with whittling enabled
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = s_locked,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    whittle_patches = TRUE,
    verbose = FALSE
  )
  
  # Locked-in units should remain selected after whittling
  for (unit in locked_in_units) {
    expect_equal(result$solution$minpatch[unit], 1,
                 info = paste("Locked-in unit", unit, "should not be removed during whittling"))
  }
})

test_that("Warning issued when locked-in patch is smaller than min_patch_size", {
  test_data <- create_test_data()
  
  # Lock in just one or two small units
  locked_in_units <- c(1)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_in_constraints(locked_in_units)
  
  # Solve with locked constraints
  s_locked <- solve(p_locked)
  
  # Should issue warning about small locked-in patch
  expect_warning(
    result <- run_minpatch(
      prioritizr_problem = p_locked,
      prioritizr_solution = s_locked,
      min_patch_size = 100.0,
      patch_radius = 1.5,
      verbose = TRUE
    ),
    "smaller than min_patch_size"
  )
})

test_that("Locked constraint information is stored in result", {
  test_data <- create_test_data()
  
  locked_in_units <- c(1, 2, 3)
  locked_out_units <- c(50, 60, 70)
  
  p_locked <- test_data$prioritizr_problem %>%
    prioritizr::add_locked_in_constraints(locked_in_units) %>%
    prioritizr::add_locked_out_constraints(locked_out_units)
  
  s_locked <- solve(p_locked)
  
  result <- run_minpatch(
    prioritizr_problem = p_locked,
    prioritizr_solution = s_locked,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Check that locked constraint info is stored
  expect_true("locked_in_indices" %in% names(result$minpatch_data))
  expect_true("locked_out_indices" %in% names(result$minpatch_data))
  
  expect_equal(sort(result$minpatch_data$locked_in_indices), sort(locked_in_units))
  expect_equal(sort(result$minpatch_data$locked_out_indices), sort(locked_out_units))
})
