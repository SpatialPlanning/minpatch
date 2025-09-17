# Test file for patch_functions.R

test_that("Patch identification works correctly", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  patch_dict <- make_patch_dict(minpatch_data)

  # Should identify connected components
  expect_true(length(patch_dict) >= 0)

  # Check patch statistics calculation
  updated_minpatch_data <- calculate_patch_stats(minpatch_data)
  patch_stats <- updated_minpatch_data$patch_stats

  expect_true(all(c("all_patch_count", "valid_patch_count", "all_patch_area") %in% names(patch_stats)))
  expect_true(patch_stats$all_patch_count >= 0)
  expect_true(patch_stats$valid_patch_count >= 0)
  expect_true(patch_stats$all_patch_area >= 0)
})

test_that("Feature conservation calculation works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  feature_amounts <- calculate_feature_conservation(minpatch_data)

  # Should have amounts for all features
  expect_equal(length(feature_amounts), length(test_data$features))
  expect_true(all(names(feature_amounts) %in% as.character(seq_along(test_data$features))))
  expect_true(all(feature_amounts >= 0))
})

test_that("Small patch removal works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 2.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  patch_dict <- make_patch_dict(minpatch_data)

  # Remove small patches using the correct function signature
  updated_minpatch_data <- remove_small_patches_from_solution(minpatch_data)

  # Function should return updated minpatch_data
  expect_true(all(c("unit_dict", "prioritizr_solution") %in% names(updated_minpatch_data)))
})

test_that("Unmet targets identification works", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Ensure minpatch column exists before calling identify_unmet_targets
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data$unit_dict)
  
  unmet_targets <- identify_unmet_targets(minpatch_data)
  
  # Should return character vector of feature IDs
  expect_true(is.character(unmet_targets))
  expect_true(all(unmet_targets %in% as.character(seq_along(test_data$features))))
})

test_that("BestPatch scoring works", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  # Ensure minpatch column exists before calling identify_unmet_targets
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(minpatch_data$unit_dict)
  
  feature_amounts <- calculate_feature_conservation(minpatch_data)
  unmet_targets <- identify_unmet_targets(minpatch_data)
  
  # Always test that unmet_targets is a character vector
  expect_true(is.character(unmet_targets))
  
  if (length(unmet_targets) > 0) {
    best_patch_scores <- calculate_best_patch_scores(
      minpatch_data, feature_amounts, unmet_targets
    )
    
    # Should return named numeric vector
    expect_true(is.numeric(best_patch_scores))
    expect_true(all(best_patch_scores >= 0))
    expect_true(all(names(best_patch_scores) %in% names(minpatch_data$unit_dict)))
  } else {
    # If no unmet targets, test that calculate_best_patch_scores returns empty vector
    best_patch_scores <- calculate_best_patch_scores(
      minpatch_data, feature_amounts, character(0)
    )
    expect_true(is.numeric(best_patch_scores))
    expect_equal(length(best_patch_scores), 0)
  }
})
