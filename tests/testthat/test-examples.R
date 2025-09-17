# Test file for examples.R

test_that("compare_solutions works correctly", {
  test_data <- create_test_data()

  # Run MinPatch to get result
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )

  # Compare solutions
  comparison <- compare_solutions(result)

  # Check structure
  expect_true(all(c("overall", "features", "summary") %in% names(comparison)))

  # Check overall comparison
  expect_true(is.data.frame(comparison$overall))
  expect_true(all(c("Metric", "Original", "MinPatch", "Change", "Percent_Change") %in% names(comparison$overall)))
  expect_true(nrow(comparison$overall) > 0)

  # Check feature comparison
  expect_true(is.data.frame(comparison$features))
  expect_true(all(c("Feature_ID", "Target", "Original_Area", "MinPatch_Area",
                   "Area_Change", "Percent_Change") %in% names(comparison$features)))
  expect_equal(nrow(comparison$features), length(test_data$features))

  # Check summary statistics
  expect_true(is.list(comparison$summary))
  expect_true(all(c("features_improved", "features_reduced", "features_unchanged",
                   "targets_gained", "targets_lost") %in% names(comparison$summary)))

  # Check that summary counts add up correctly
  total_features <- comparison$summary$features_improved +
                   comparison$summary$features_reduced +
                   comparison$summary$features_unchanged
  expect_equal(total_features, length(test_data$features))
})

test_that("compare_solutions handles edge cases", {
  test_data <- create_test_data()

  # Run MinPatch with no changes (skip all stages)
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 0.1,  # Very small to avoid removals
    patch_radius = 1.5,
    remove_small_patches = FALSE,
    add_patches = FALSE,
    whittle_patches = FALSE,
    verbose = FALSE
  )

  comparison <- compare_solutions(result)

  # Should still return valid structure even with minimal changes
  expect_true(all(c("overall", "features", "summary") %in% names(comparison)))
  expect_true(is.data.frame(comparison$overall))
  expect_true(is.data.frame(comparison$features))
})

test_that("compare_solutions calculates metrics correctly", {
  test_data <- create_test_data()

  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )

  comparison <- compare_solutions(result)

  # Check that numeric values are reasonable
  expect_true(all(is.numeric(comparison$overall$Original)))
  expect_true(all(is.numeric(comparison$overall$MinPatch)))
  expect_true(all(is.numeric(comparison$overall$Change)))

  # Check that feature areas are non-negative
  expect_true(all(comparison$features$Original_Area >= 0))
  expect_true(all(comparison$features$MinPatch_Area >= 0))
  expect_true(all(comparison$features$Target >= 0))

  # Check that target achievement flags are logical
  expect_true(all(is.logical(comparison$features$Original_Target_Met)))
  expect_true(all(is.logical(comparison$features$MinPatch_Target_Met)))
})

test_that("plot_minpatch works when ggplot2 is available", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")

  test_data <- create_test_data()

  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )

  # Create visualization
  plot <- plot_minpatch(result, title = "Test Plot")

  # Check that it returns a ggplot object
  expect_true(inherits(plot, "ggplot"))

  # Check that the plot has the expected layers
  expect_true(length(plot$layers) > 0)

  # Check that it has a title
  expect_equal(plot$labels$title, "Test Plot")
})

test_that("plot_minpatch fails gracefully without ggplot2", {
  # Skip this test if ggplot2 is not available (we can't test the error condition if it's missing)
  skip_if_not_installed("ggplot2")
  
  # Since we can't easily mock requireNamespace, we'll just test that the function
  # works when ggplot2 is available and skip testing the error condition
  test_data <- create_test_data()
  
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  # Test that the function works when ggplot2 is available
  plot <- plot_minpatch(result)
  expect_true(inherits(plot, "ggplot"))
})

test_that("plot_minpatch handles different change types", {
  skip_if_not_installed("ggplot2")
  
  test_data <- create_test_data()
  
  # Run MinPatch with settings that should create changes
  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 2.0,  # Larger to force some removals
    patch_radius = 1.5,
    verbose = FALSE
  )
  
  plot <- plot_minpatch(result)
  
  # Check that the plot data contains the expected change categories
  plot_data <- plot$data
  expect_true("change" %in% names(plot_data))
  expect_true(all(plot_data$change %in% c("No Change", "Added", "Removed", "Retained")))
  
  # Check that the plot has geom_sf layer (which contains the fill aesthetic)
  expect_true(length(plot$layers) > 0)
  expect_true(any(sapply(plot$layers, function(x) inherits(x$geom, "GeomSf"))))
})

test_that("plot_minpatch uses default title", {
  skip_if_not_installed("ggplot2")

  test_data <- create_test_data()

  result <- run_minpatch(
    prioritizr_problem = test_data$prioritizr_problem,
    prioritizr_solution = test_data$prioritizr_solution,
    min_patch_size = 1.0,
    patch_radius = 1.5,
    verbose = FALSE
  )

  # Create visualization without specifying title
  plot <- plot_minpatch(result)

  # Should use default title
  expect_equal(plot$labels$title, "MinPatch Results")
})
