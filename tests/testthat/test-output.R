# Test file for output.R

test_that("generate_minpatch_report works correctly", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  report <- generate_minpatch_report(result)
  
  # Check report structure
  expect_true(all(c("features", "patch_stats", "cost") %in% names(report)))
  
  # Check features data frame
  expect_true(is.data.frame(report$features))
  expect_true(all(c("feature", "met", "total_amount", "absolute_held", 
                    "absolute_target", "relative_held", "relative_target") %in% 
                    names(report$features)))
  
  # Check patch stats
  expect_true(is.data.frame(report$patch_stats))
  expect_equal(nrow(report$patch_stats), 2)  # Initial and final
  expect_true("time" %in% names(report$patch_stats))
  
  # Check cost summary
  expect_true(is.data.frame(report$cost))
  expect_true(all(c("cost", "boundary_cost", "total_cost") %in% names(report$cost)))
})

test_that("print_minpatch_summary works correctly", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Capture output
  output <- capture.output(print_minpatch_summary(result))
  
  # Check that output contains expected sections
  expect_true(any(grepl("MinPatch Processing Summary", output)))
  expect_true(any(grepl("Patch Statistics", output)))
  expect_true(any(grepl("Cost Breakdown", output)))
  expect_true(any(grepl("Feature Representation", output)))
})

test_that("plot_prioritizr works correctly", {
  skip_if_not_installed("ggplot2")
  
  test_data <- create_test_data()
  
  # Plot with default parameters
  plot1 <- plot_prioritizr(test_data$prioritizr_solution)
  expect_true(inherits(plot1, "ggplot"))
  expect_equal(plot1$labels$title, "prioritizr Solution")
  
  # Plot with custom title
  plot2 <- plot_prioritizr(test_data$prioritizr_solution, 
                           title = "Custom Title")
  expect_equal(plot2$labels$title, "Custom Title")
  
  # Plot with custom column
  plot3 <- plot_prioritizr(test_data$prioritizr_solution, 
                           col = "solution_1")
  expect_true(inherits(plot3, "ggplot"))
})

test_that("visualize_minpatch_results handles different scenarios", {
  skip_if_not_installed("ggplot2")
  
  test_data <- create_test_data()
  
  # Test with all three stages
  result1 <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    remove_small_patches = TRUE,
    add_patches = TRUE,
    whittle_patches = TRUE,
    verbose = FALSE
  )
  
  plot1 <- plot_minpatch(result1)
  expect_true(inherits(plot1, "ggplot"))
  
  # Test with minimal changes
  result2 <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 0.1,
    patch_radius = 1.5,
    remove_small_patches = FALSE,
    add_patches = FALSE,
    whittle_patches = FALSE,
    verbose = FALSE
  )
  
  plot2 <- plot_minpatch(result2)
  expect_true(inherits(plot2, "ggplot"))
})

test_that("compare_solutions handles various result types", {
  test_data <- create_test_data()
  
  # Test with boundary penalty
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    boundary_penalty = 0.1,
    verbose = FALSE
  )
  
  comparison <- compare_solutions(result)
  
  # Check that boundary costs are included
  boundary_row <- comparison$overall[comparison$overall$Metric == "Boundary Cost", ]
  expect_true(nrow(boundary_row) > 0)
  expect_true(boundary_row$MinPatch >= 0)
  
  # Check that percentage changes are calculated
  expect_true(all(is.numeric(comparison$overall$Percent_Change) | 
                  is.na(comparison$overall$Percent_Change)))
})

test_that("compare_solutions calculates feature changes correctly", {
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.5,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  comparison <- compare_solutions(result)
  
  # Check feature comparison calculations
  expect_true(all(comparison$features$Area_Change == 
                  comparison$features$MinPatch_Area - comparison$features$Original_Area))
  
  # Check target proportions
  expect_true(all(comparison$features$Original_Proportion >= 0 | 
                  is.na(comparison$features$Original_Proportion)))
  expect_true(all(comparison$features$MinPatch_Proportion >= 0 | 
                  is.na(comparison$features$MinPatch_Proportion)))
})

test_that("Output functions handle empty or minimal solutions", {
  test_data <- create_test_data()
  
  # Create a result with minimal changes
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 0.01,  # Very small
    patch_radius = 1.5,
    remove_small_patches = FALSE,
    add_patches = FALSE,
    whittle_patches = FALSE,
    verbose = FALSE
  )
  
  # Should still generate valid report
  report <- generate_minpatch_report(result)
  expect_true(all(c("features", "patch_stats", "cost") %in% names(report)))
  
  # Should still print summary
  output <- capture.output(print_minpatch_summary(result))
  expect_true(length(output) > 0)
  
  # Should still create comparison
  comparison <- compare_solutions(result)
  expect_true(all(c("overall", "features", "summary") %in% names(comparison)))
})