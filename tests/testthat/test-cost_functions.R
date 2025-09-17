# Test file for cost_functions.R

test_that("Cost calculation works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0.1, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  # Create minpatch column before calling calculate_cost_summary
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data$unit_dict)

  cost_summary <- calculate_cost_summary(minpatch_data)

  # Check cost components
  expect_true(all(c("cost", "boundary_cost", "total_cost") %in% names(cost_summary)))
  expect_true(cost_summary$cost > 0)
  expect_true(cost_summary$boundary_cost >= 0)
  expect_equal(cost_summary$total_cost, cost_summary$cost + cost_summary$boundary_cost)
})

# test_that("Feature representation calculation works", {
#   test_data <- create_test_data()
#
#   minpatch_data <- initialize_minpatch_data(
#     test_data$solution, test_data$planning_units, test_data$targets,
#     NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
#   )
#
#   feature_rep <- calculate_feature_representation(minpatch_data)
#
#   # Check structure
#   expect_true(is.data.frame(feature_rep))
#   expect_true(all(c("feature_id", "target", "conserved", "proportion_met", "target_met") %in% names(feature_rep)))
#   expect_equal(nrow(feature_rep), length(test_data$features))
#
#   # Check values are reasonable
#   expect_true(all(feature_rep$target > 0))
#   expect_true(all(feature_rep$conserved >= 0))
#   expect_true(all(feature_rep$proportion_met >= 0))
# })

test_that("Solution vector creation works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  solution_vector <- create_solution_vector(minpatch_data$unit_dict)

  # Check solution vector properties
  expect_true(is.numeric(solution_vector))
  expect_true(all(solution_vector %in% c(0, 1)))
  expect_equal(length(solution_vector), nrow(test_data$planning_units))
})

# test_that("MinPatch report generation works", {
#   test_data <- create_test_data()
#
#   # Run MinPatch to get result
#   result <- run_minpatch(
#     prioritizr_problem = test_data$prioritizr_problem,
#     prioritizr_solution = test_data$prioritizr_solution,
#     min_patch_size = 1.0,
#     patch_radius = 1.5,
#     verbose = FALSE
#   )
#
#   # Generate report
#   report <- generate_minpatch_report(result)
#
#   # Check report structure
#   expect_true(all(c("summary_stats", "feature_summary", "feature_representation",
#                    "patch_stats", "cost_breakdown") %in% names(report)))
#
#   # Check summary stats
#   expect_true(all(c("initial_patches", "final_patches", "total_cost") %in% names(report$summary_stats)))
#
#   # Check feature summary
#   expect_true(all(c("total_features", "targets_met", "targets_unmet") %in% names(report$feature_summary)))
# })

test_that("Cost summary handles different boundary penalties", {
  test_data <- create_test_data()

  # Test with zero boundary penalty
  solution_data <- test_data$prioritizr_solution[c("solution_1")]

  minpatch_data_0 <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  minpatch_data_1 <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0.1, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  # Create minpatch columns before calling calculate_cost_summary
  minpatch_data_0$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data_0$unit_dict)
  minpatch_data_1$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data_1$unit_dict)

  cost_summary_0 <- calculate_cost_summary(minpatch_data_0)
  cost_summary_1 <- calculate_cost_summary(minpatch_data_1)

  # Boundary cost should be 0 when penalty is 0
  expect_equal(cost_summary_0$boundary_cost, 0)
  
  # Boundary cost should be >= 0 when penalty > 0
  expect_true(cost_summary_1$boundary_cost >= 0)
})
