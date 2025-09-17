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
