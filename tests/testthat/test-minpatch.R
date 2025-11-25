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
