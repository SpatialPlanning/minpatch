#' Create patch dictionary from unit dictionary
#'
#' Identifies connected components (patches) in the current solution
#'
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Named list where each patch contains area, unit count, and unit IDs
#' @keywords internal
make_patch_dict <- function(minpatch_data) {

  unit_dict <- minpatch_data$unit_dict
  boundary_matrix <- minpatch_data$boundary_matrix

  # Get all selected planning units (status = 1 or 2)
  selected_units <- names(unit_dict)[sapply(unit_dict, function(x) x$status %in% c(1, 2))]


  if (length(selected_units) == 0) {
    return(list())
  }

  patch_dict <- list()
  patch_id <- 1
  remaining_units <- selected_units

  while (length(remaining_units) > 0) {
    # Start a new patch with the first remaining unit
    current_patch <- character(0)
    units_to_check <- remaining_units[1]


    # Find all connected units using breadth-first search
    while (length(units_to_check) > 0) {
      current_unit <- units_to_check[1]
      units_to_check <- units_to_check[-1]

      if (!current_unit %in% current_patch) {
        current_patch <- c(current_patch, current_unit)

        # Find neighbors of current unit that are also selected
        neighbors <- names(boundary_matrix[[current_unit]])
        selected_neighbors <- intersect(neighbors, remaining_units)
        selected_neighbors <- selected_neighbors[!selected_neighbors %in% current_patch]


        units_to_check <- unique(c(units_to_check, selected_neighbors))
      }
    }


    # Store patch information
    patch_dict[[as.character(patch_id)]] <- list(
      area = 0,  # Will be calculated later
      unit_count = length(current_patch),
      unit_ids = current_patch
    )

    # Remove processed units from remaining units
    remaining_units <- setdiff(remaining_units, current_patch)
    patch_id <- patch_id + 1
  }

  return(patch_dict)
}

#' Calculate patch statistics
#'
#' Calculates summary statistics for patches including areas and counts
#'
#' @param patch_dict Named list of patches
#' @param area_dict Named vector of planning unit areas
#' @param min_patch_size Minimum patch size threshold
#'
#' @return List containing patch statistics
#' @keywords internal
calculate_patch_stats <- function(patch_dict, area_dict, min_patch_size) {

  if (length(patch_dict) == 0) {
    return(list(
      all_patch_count = 0,
      all_patch_area = 0,
      median_all_patch = 0,
      valid_patch_count = 0,
      valid_patch_area = 0,
      median_valid_patch = 0
    ))
  }

  # Calculate patch areas
  all_areas <- numeric(length(patch_dict))
  valid_areas <- numeric(0)

  for (i in seq_along(patch_dict)) {
    patch <- patch_dict[[i]]
    patch_area <- sum(area_dict[patch$unit_ids])
    all_areas[i] <- patch_area

    # Update patch_dict with calculated area
    patch_dict[[i]]$area <- patch_area

    if (as.numeric(patch_area) >= as.numeric(min_patch_size)) {
      valid_areas <- c(valid_areas, patch_area)
    }
  }

  return(list(
    all_patch_count = length(all_areas),
    all_patch_area = sum(all_areas),
    median_all_patch = ifelse(length(all_areas) > 0, stats::median(all_areas), 0),
    valid_patch_count = length(valid_areas),
    valid_patch_area = sum(valid_areas),
    median_valid_patch = ifelse(length(valid_areas) > 0, stats::median(valid_areas), 0)
  ))
}

#' Remove small patches from solution
#'
#' Stage 1 of MinPatch: Remove patches smaller than minimum size threshold
#'
#' @param minpatch_data List containing all MinPatch data structures
#' @param patch_dict Named list of patches
#' @param min_patch_size Minimum patch size threshold
#'
#' @return Updated minpatch_data with small patches removed
#' @keywords internal
remove_small_patches_from_solution <- function(minpatch_data, patch_dict, min_patch_size) {

  unit_dict <- minpatch_data$unit_dict
  area_dict <- minpatch_data$area_dict

  if (length(patch_dict) == 0) {
    return(minpatch_data)
  }

  for (patch_id in names(patch_dict)) {
    patch <- patch_dict[[patch_id]]

    # Calculate patch area
    patch_area <- sum(area_dict[patch$unit_ids])

    # Calculate patch area and use proper floating-point comparison
    area_num <- as.numeric(patch_area)
    min_size_num <- as.numeric(min_patch_size)

    # Use proper floating-point comparison with tolerance to handle precision issues
    tolerance <- 1e-10
    should_remove <- (area_num + tolerance) < min_size_num

    if (should_remove) {
      # Remove this patch by setting status to 0 (available)
      for (unit_id in patch$unit_ids) {
        # Only remove if it wasn't originally conserved (status 2)
        if (unit_dict[[unit_id]]$status == 1) {
          unit_dict[[unit_id]]$status <- 0
        }
      }
    }
  }

  # Update the unit_dict in minpatch_data
  minpatch_data$unit_dict <- unit_dict

  # Update the minpatch column in the prioritizr solution object
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(unit_dict)

  return(minpatch_data)
}

#' Add new patches to meet conservation targets
#'
#' Stage 2 of MinPatch: Add new patches using BestPatch algorithm
#'
#' @param minpatch_data List containing all MinPatch data structures (including prioritizr objects)
#' @param patch_radius Radius for adding new patches
#' @param verbose Logical, whether to print progress
#'
#' @return List containing updated unit_dict and unmet targets
#' @keywords internal
add_new_patches <- function(minpatch_data, patch_radius, verbose = TRUE) {

  unit_dict <- minpatch_data$unit_dict
  target_dict <- minpatch_data$target_dict
  abundance_matrix <- minpatch_data$abundance_matrix
  patch_radius_dict <- minpatch_data$patch_radius_dict

  # Calculate current feature conservation amounts
  feature_amounts <- calculate_feature_conservation(minpatch_data)

  # Identify unmet targets using prioritizr functions
  unmet_targets <- identify_unmet_targets(minpatch_data)

  if (verbose) {
    cat("  Initial unmet targets:", length(unmet_targets), "\n")
    if (length(unmet_targets) > 0) {
      cat("  Unmet feature IDs:", paste(unmet_targets, collapse = ", "), "\n")
    }
  }

  iteration <- 0
  max_iterations <- 1000  # Prevent infinite loops

  while (length(unmet_targets) > 0 && iteration < max_iterations) {
    iteration <- iteration + 1

    if (verbose && (iteration <= 5 || iteration %% 10 == 0)) {
      cat("  Iteration", iteration, "- Unmet targets:", length(unmet_targets), "\n")
    }

    # Calculate BestPatch scores for all available planning units
    best_patch_scores <- calculate_best_patch_scores(
      minpatch_data, feature_amounts, unmet_targets
    )

    if (verbose && iteration <= 5) {
      cat("    Found", length(best_patch_scores), "potential patches with scores\n")
      if (length(best_patch_scores) > 0) {
        cat("    Best score:", max(best_patch_scores), "for unit", names(best_patch_scores)[which.max(best_patch_scores)], "\n")
      }
    }

    if (length(best_patch_scores) == 0 || max(best_patch_scores) <= 0) {
      # No more beneficial patches can be added
      if (verbose) cat("    No more beneficial patches found. Stopping.\n")
      break
    }

    # Select the planning unit with the highest BestPatch score
    best_unit_id <- names(best_patch_scores)[which.max(best_patch_scores)]

    # Add the patch centered on this planning unit
    # Update minpatch_data with the new unit_dict before calling add_patch_centered_on_unit
    minpatch_data$unit_dict <- unit_dict
    unit_dict <- add_patch_centered_on_unit(minpatch_data, best_unit_id)

    if (verbose && iteration <= 5) {
      cat("    Added patch centered on unit", best_unit_id, "\n")
    }

    # Recalculate feature conservation amounts
    feature_amounts <- calculate_feature_conservation(minpatch_data)

    # Update the minpatch column with current solution and check unmet targets
    minpatch_data$unit_dict <- unit_dict
    minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(unit_dict)
    unmet_targets <- identify_unmet_targets(minpatch_data)
  }

  if (iteration >= max_iterations) {
    warning("Maximum iterations reached in add_new_patches")
  }

  # Update minpatch_data with final unit_dict
  minpatch_data$unit_dict <- unit_dict
  minpatch_data$prioritizr_solution$minpatch <- create_solution_vector(unit_dict)

  return(minpatch_data)
}

#' Calculate current feature conservation amounts
#'
#' Calculates how much of each feature is currently conserved
#'
#' @param minpatch_data List containing all MinPatch data structures
#'
#' @return Named vector of conserved amounts for each feature
#' @keywords internal
calculate_feature_conservation <- function(minpatch_data) {

  target_dict <- minpatch_data$target_dict
  unit_dict <- minpatch_data$unit_dict
  abundance_matrix <- minpatch_data$abundance_matrix

  feature_amounts <- numeric(length(target_dict))
  names(feature_amounts) <- names(target_dict)

  # Initialize all features to 0
  for (feat_id in names(target_dict)) {
    feature_amounts[feat_id] <- 0
  }

  # Sum up conserved amounts from selected planning units
  for (unit_id in names(unit_dict)) {
    if (unit_dict[[unit_id]]$status %in% c(1, 2)) {  # Selected or conserved
      unit_abundances <- abundance_matrix[[unit_id]]

      for (feat_id in names(unit_abundances)) {
        if (feat_id %in% names(feature_amounts)) {
          feature_amounts[feat_id] <- feature_amounts[feat_id] + unit_abundances[[feat_id]]
        }
      }
    }
  }

  return(feature_amounts)
}

#' Identify features with unmet targets
#'
#' Uses prioritizr functions to identify unmet targets from minpatch_data
#'
#' @param minpatch_data List containing all MinPatch data structures including prioritizr objects
#'
#' @return Character vector of feature IDs with unmet targets
#' @keywords internal
identify_unmet_targets <- function(minpatch_data) {

  # Use prioritizr's eval_target_coverage_summary with the minpatch column
  target_coverage <- prioritizr::eval_target_coverage_summary(
    minpatch_data$prioritizr_problem,
    minpatch_data$prioritizr_solution %>% dplyr::select("minpatch")
  )

  # Return feature IDs where targets are not met
  unmet_indices <- which(!target_coverage$met)
  return(as.character(unmet_indices))
}

#' Calculate BestPatch scores for all available planning units
#'
#' Implements the BestPatch scoring algorithm from the original paper
#'
#' @param minpatch_data List containing all MinPatch data structures
#' @param feature_amounts Named vector of current conservation amounts
#' @param unmet_targets Character vector of features with unmet targets
#'
#' @return Named vector of BestPatch scores
#' @keywords internal
calculate_best_patch_scores <- function(minpatch_data, feature_amounts, unmet_targets) {

  unit_dict <- minpatch_data$unit_dict
  abundance_matrix <- minpatch_data$abundance_matrix
  target_dict <- minpatch_data$target_dict
  patch_radius_dict <- minpatch_data$patch_radius_dict

  scores <- numeric(0)

  for (unit_id in names(unit_dict)) {
    if (unit_dict[[unit_id]]$status == 0) {  # Available unit

      # Get all units in the potential patch
      patch_units <- c(unit_id, patch_radius_dict[[unit_id]])

      # Calculate patch cost (only for available units)
      patch_cost <- 0
      patch_feature_amounts <- numeric(length(unmet_targets))
      names(patch_feature_amounts) <- unmet_targets

      for (patch_unit in patch_units) {
        if (patch_unit %in% names(unit_dict) && unit_dict[[patch_unit]]$status == 0) {
          patch_cost <- patch_cost + unit_dict[[patch_unit]]$cost

          # Add feature amounts from this unit
          unit_abundances <- abundance_matrix[[patch_unit]]
          for (feat_id in unmet_targets) {
            if (feat_id %in% names(unit_abundances)) {
              patch_feature_amounts[feat_id] <- patch_feature_amounts[feat_id] + unit_abundances[[feat_id]]
            }
          }
        }
      }

      if (patch_cost > 0) {
        # Calculate BestPatch score using equation from paper
        score <- 0
        for (feat_id in unmet_targets) {
          target_amount <- target_dict[[feat_id]]$target
          current_amount <- feature_amounts[feat_id]
          patch_amount <- patch_feature_amounts[feat_id]

          if (patch_amount > 0) {
            target_gap <- target_amount - current_amount
            if (target_gap > 0) {
              feat_score <- patch_amount / target_gap
              if (feat_score > 1) feat_score <- 1  # Cap at 1 as we only need to meet target
              score <- score + feat_score
            }
          }
        }

        if (score > 0) {
          scores[unit_id] <- score / patch_cost
        }
      }
    }
  }

  return(scores)
}

#' Add patch centered on specified planning unit
#'
#' @param minpatch_data List containing all MinPatch data structures
#' @param center_unit_id ID of the unit to center the patch on
#'
#' @return Updated unit_dict with new patch added
#' @keywords internal
add_patch_centered_on_unit <- function(minpatch_data, center_unit_id) {

  patch_radius_dict <- minpatch_data$patch_radius_dict
  unit_dict <- minpatch_data$unit_dict

  # Get all units in the patch
  patch_units <- c(center_unit_id, patch_radius_dict[[center_unit_id]])

  # Set status to 1 (selected) for all available units in the patch
  for (unit_id in patch_units) {
    if (unit_id %in% names(unit_dict) && unit_dict[[unit_id]]$status == 0) {
      unit_dict[[unit_id]]$status <- 1
    }
  }

  return(unit_dict)
}
