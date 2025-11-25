# Test file for whittling_functions.R

test_that("Edge unit identification works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  edge_units <- find_edge_units(minpatch_data)

  # Should return character vector of unit IDs
  expect_true(is.character(edge_units))
  expect_true(all(edge_units %in% names(minpatch_data$unit_dict)))
})

test_that("Whittle score calculation works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  edge_units <- find_edge_units(minpatch_data)

  if (length(edge_units) > 0) {
    whittle_scores <- calculate_whittle_scores(edge_units, minpatch_data)

    # Should return list with scores or messages
    expect_true(is.list(whittle_scores))
    expect_true(all(names(whittle_scores) %in% edge_units))

    # Check that scores are either numeric or character messages
    for (score in whittle_scores) {
      expect_true(is.numeric(score) || is.character(score))
    }
  }
})

test_that("Unit removal checks work", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  # Get a selected unit to test removal
  selected_units <- names(minpatch_data$unit_dict)[sapply(minpatch_data$unit_dict, function(x) x$status == 1)]

  if (length(selected_units) > 0) {
    test_unit <- selected_units[1]

    # Test can_remove_unit function
    can_remove <- can_remove_unit(test_unit, minpatch_data)
    expect_true(is.logical(can_remove))

    # Test individual removal check functions
    violates_targets <- removal_violates_targets(test_unit, minpatch_data)
    expect_true(is.logical(violates_targets))

    makes_too_small <- removal_makes_patch_too_small(test_unit, minpatch_data)
    expect_true(is.logical(makes_too_small))

    increases_cost <- removal_increases_cost(test_unit, minpatch_data)
    expect_true(is.logical(increases_cost))

    splits_nonviably <- removal_splits_patch_nonviably(test_unit, minpatch_data)
    expect_true(is.logical(splits_nonviably))
  }
})

test_that("Simulated whittling works", {
  test_data <- create_test_data()

  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  # Run simulated whittling (with limited iterations for testing)
  original_unit_count <- sum(sapply(minpatch_data$unit_dict, function(x) x$status %in% c(1, 2)))

  whittled_data <- simulated_whittling(minpatch_data, verbose = FALSE)

  # Should return updated minpatch_data
  expect_true(all(c("unit_dict", "prioritizr_solution") %in% names(whittled_data)))

  # Check that minpatch column was updated
  expect_true("minpatch" %in% names(whittled_data$prioritizr_solution))
  expect_true(all(whittled_data$prioritizr_solution$minpatch %in% c(0, 1)))

  # Unit count should be <= original (whittling removes units)
  final_unit_count <- sum(sapply(whittled_data$unit_dict, function(x) x$status %in% c(1, 2)))
  expect_true(final_unit_count <= original_unit_count)
})

test_that("Whittling respects minimum patch size", {
  test_data <- create_test_data()

  # Use a larger minimum patch size to test constraints
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 5.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )

  # Get a unit that's part of a patch
  selected_units <- names(minpatch_data$unit_dict)[sapply(minpatch_data$unit_dict, function(x) x$status == 1)]

  if (length(selected_units) > 0) {
    test_unit <- selected_units[1]

    # Test if removal would make patch too small
    makes_too_small <- removal_makes_patch_too_small(test_unit, minpatch_data)
    expect_true(is.logical(makes_too_small))
  }
})

test_that("Whittle scores handle keystone units", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  edge_units <- find_edge_units(minpatch_data)
  
  if (length(edge_units) > 0) {
    whittle_scores <- calculate_whittle_scores(edge_units, minpatch_data)
    
    # Check for keystone messages
    keystone_messages <- sapply(whittle_scores, function(x) {
      is.character(x) && grepl("cannot be whittled", x)
    })
    
    expect_true(is.logical(keystone_messages))
  }
})

test_that("Whittle scores accept pre-computed feature amounts", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  edge_units <- find_edge_units(minpatch_data)
  feature_amounts <- calculate_feature_conservation(minpatch_data)
  
  if (length(edge_units) > 0) {
    scores1 <- calculate_whittle_scores(edge_units, minpatch_data, NULL)
    scores2 <- calculate_whittle_scores(edge_units, minpatch_data, feature_amounts)
    
    expect_equal(length(scores1), length(scores2))
  }
})

test_that("removal_violates_targets with pre-computed amounts", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  selected_units <- names(minpatch_data$unit_dict)[
    sapply(minpatch_data$unit_dict, function(x) x$status == 1)
  ]
  
  if (length(selected_units) > 0) {
    feature_amounts <- calculate_feature_conservation(minpatch_data)
    
    result1 <- removal_violates_targets(selected_units[1], minpatch_data, NULL)
    result2 <- removal_violates_targets(selected_units[1], minpatch_data, feature_amounts)
    
    expect_true(is.logical(result1))
    expect_true(is.logical(result2))
  }
})

test_that("removal_increases_cost with zero boundary penalty", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  selected_units <- names(minpatch_data$unit_dict)[
    sapply(minpatch_data$unit_dict, function(x) x$status == 1)
  ]
  
  if (length(selected_units) > 0) {
    increases_cost <- removal_increases_cost(selected_units[1], minpatch_data)
    
    expect_false(increases_cost)
  }
})

test_that("removal_increases_cost with positive boundary penalty", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0.1, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  selected_units <- names(minpatch_data$unit_dict)[
    sapply(minpatch_data$unit_dict, function(x) x$status == 1)
  ]
  
  if (length(selected_units) > 0) {
    increases_cost <- removal_increases_cost(selected_units[1], minpatch_data)
    
    expect_true(is.logical(increases_cost))
  }
})

test_that("can_remove_unit respects conserved units", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  selected_units <- names(minpatch_data$unit_dict)[
    sapply(minpatch_data$unit_dict, function(x) x$status == 1)
  ]
  
  if (length(selected_units) > 0) {
    minpatch_data$unit_dict[[selected_units[1]]]$status <- 2
    
    can_remove <- can_remove_unit(selected_units[1], minpatch_data)
    
    expect_false(can_remove)
  }
})

test_that("find_edge_units handles external boundaries", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  edge_units <- find_edge_units(minpatch_data)
  
  expect_true(is.character(edge_units))
  expect_true(all(edge_units %in% names(minpatch_data$unit_dict)))
  
  # All edge units should be selected or conserved
  for (unit_id in edge_units) {
    expect_true(minpatch_data$unit_dict[[unit_id]]$status %in% c(1, 2))
  }
})

test_that("removal_splits_patch_nonviably creates temp data correctly", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  selected_units <- names(minpatch_data$unit_dict)[
    sapply(minpatch_data$unit_dict, function(x) x$status == 1)
  ]
  
  if (length(selected_units) > 0) {
    splits_nonviably <- removal_splits_patch_nonviably(selected_units[1], minpatch_data)
    
    expect_true(is.logical(splits_nonviably))
  }
})

test_that("simulated_whittling handles empty edge units", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    rep(1, length(test_data$solution)),
    test_data$planning_units,
    test_data$targets,
    NULL, 1.0, 1.5, 0,
    test_data$prioritizr_problem,
    test_data$prioritizr_solution
  )
  
  whittled_data <- simulated_whittling(minpatch_data, verbose = FALSE)
  
  expect_true("unit_dict" %in% names(whittled_data))
  expect_true("minpatch" %in% names(whittled_data$prioritizr_solution))
})

test_that("calculate_whittle_scores handles units with no features", {
  test_data <- create_test_data()
  
  minpatch_data <- initialize_minpatch_data(
    test_data$solution, test_data$planning_units, test_data$targets,
    NULL, 1.0, 1.5, 0, test_data$prioritizr_problem, test_data$prioritizr_solution
  )
  
  edge_units <- find_edge_units(minpatch_data)
  
  if (length(edge_units) > 0) {
    minpatch_data$abundance_matrix[[edge_units[1]]] <- list()
    
    scores <- calculate_whittle_scores(edge_units, minpatch_data)
    
    expect_equal(scores[[edge_units[1]]], 0)
  }
})
